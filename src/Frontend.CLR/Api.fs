namespace Monkey.Frontend.CLR.Api

open System
open System.IO
open System.Threading.Tasks
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open type Microsoft.CodeAnalysis.CSharp.SyntaxFactory

open FsToolkit.ErrorHandling

open Monkey.Frontend.CLR.Api.Errors
open Monkey.Frontend.CLR.HostStubGenerator
open Monkey.Frontend.CLR.Lexer
open Monkey.Frontend.CLR.Parsers
open Monkey.Frontend.CLR.Parsers.ParseErrors

type LogFunctions =
    { Info: string -> unit
      Warning: string -> unit
      Error: string -> unit }
with
    static member Default =
        { Info = (fun _ -> ())
          Warning = (fun _ -> ())
          Error = (fun _ -> ()) }

// We use the below wrapper class so that we don't expose any 'Microsoft.CodeAnalysis' bindings, so we don't have to
// have it as a dependency.

/// <summary>
/// 
/// </summary>
/// <remarks>
/// <ul>
///     <li>
///     </li>
/// </ul>
/// </remarks>
type FrontendApi() =
    let mutable loadedFiles: FileInfo array = [||]
    let mutable loadedCompilation: CSharpCompilation option = None
    let mutable log: LogFunctions = LogFunctions.Default
    
    member this.SetLogFunctions(logFunctions: LogFunctions) : FrontendApi =
        log <- logFunctions
        this
    
    member this.LoadFiles([<ParamArray>] filePaths: string array) : Result<FrontendApi, Exception> =
        result {
            let asFileInfos = filePaths |> Array.map (fun filePath -> FileInfo(filePath))
            let thatExists = asFileInfos |> Array.filter _.Exists
            let doesNotExist = asFileInfos |> Array.except thatExists
            
            return!
                match doesNotExist with
                | [| |] ->
                    loadedFiles <- thatExists
                    Ok this
                | _ ->
                    let formattedFilePaths = doesNotExist |> Array.map _.FullName |> Array.map (fun fileName -> $"{fileName}")
                    let asSingleString = String.Join(", ", formattedFilePaths)
                    let errorMsg = $"Could not find the files {asSingleString}"
                    FileNotFoundException(errorMsg) :> Exception |> Error
        }
        
    member this.CompileFiles() : Result<FrontendApi, Exception> =
        result {
            let fileContentsTasks = loadedFiles |> Array.map (fun fileInfo -> File.ReadAllTextAsync(fileInfo.FullName))
            let fileContents = Task.WhenAll(fileContentsTasks).Result
            
            let fileParseResultsPairs =
                fileContents
                |> Array.map Lexer.parseIntoTokens
                |> Array.map List.toArray
                |> Array.map ModifiedRecursiveDescent.parseTokens
                |> Array.zip loadedFiles
                |> Array.map (fun (a, (b, c)) -> (a, b), (a, c))
                
            do! FrontendApi.assertNoParseErrors (Array.map snd fileParseResultsPairs)
            
            let fileParseStatementsPairs = Array.map fst fileParseResultsPairs
            let compilation = FrontendApi.combineIntoCompilation fileParseStatementsPairs
            loadedCompilation <- Some compilation
            return this
        }
        
    member this.Emit(outputDir: string) : Result<FrontendApi, Exception> =
        result {
            let dirInfo = Directory.CreateDirectory(outputDir)
            
            let! compilation =
                match loadedCompilation with
                | None -> EmitError(message="Nothing was compiled. Please load and compile source files first.") :> Exception |> Error
                | Some value -> Ok value
                
            let fileName = $"{compilation.AssemblyName}.dll"
            use outStream = new FileStream(Path.Join(dirInfo.FullName, fileName), FileMode.Create)
            let emitResult = compilation.Emit(outStream)
            
            return!
                match emitResult.Success with
                | true ->
                    let runtimeConfigJsonPath = Path.Join(dirInfo.FullName, $"{compilation.AssemblyName}.runtimeconfig.json")
                    File.WriteAllText(runtimeConfigJsonPath, FrontendApi.generateRuntimeConfigJson())
                    
                    DotnetProjectGen.generateHostSub compilation.AssemblyName outputDir |> ignore
                    
                    Ok this
                | false -> EmitError(message="Emitting failed, see diagnostics for info.", diagnostics=(Some emitResult.Diagnostics)) :> Exception |> Error
        }
        
    
    static member private assertNoParseErrors (fileAndErrorPairs: (FileInfo * ParseError list) array) : Result<unit, Exception> =
        let pairsWithErrors = 
            fileAndErrorPairs
            |> Array.filter (fun (_, parseErrors) -> parseErrors.Length > 0)
            
        match pairsWithErrors.Length with
        | 0 ->
            Ok ()
        | _ ->
            let pairsWithExceptions =
                pairsWithErrors
                |> Array.map (fun (fileInfo, parseErrors) -> (fileInfo, List.map (fun err -> err :> Exception) parseErrors))
            CompilationError(pairsWithExceptions, message="One or multiple files had compilation errors") :> Exception |> Error
            
            
    static member private combineIntoCompilation (fileParseStatementsPairs: (FileInfo * StatementSyntax list) array) =
        let toCompilationUnit (statements: StatementSyntax list) =
            let tempUsingDirective = UsingDirective(IdentifierName("System"))
            
            let globalStatements =
                statements
                |> List.toArray
                |> Array.map GlobalStatement
                |> Array.map (fun globalStatement -> globalStatement :> MemberDeclarationSyntax)
            CompilationUnit()
                .WithUsings(SyntaxFactory.List([| tempUsingDirective |]))
                .WithMembers(SyntaxFactory.List(globalStatements))
                .NormalizeWhitespace()
                
        let fileSyntaxTreePairs =
            fileParseStatementsPairs
            |> Array.map (fun (fileInfo, statements) -> (fileInfo, (toCompilationUnit statements).SyntaxTree))
            
        let syntaxTrees = Array.map snd fileSyntaxTreePairs
        
        // TEMP, TODO REMOVE
        let temp = CSharpSyntaxTree.ParseText(
            """
            using System;
            
            public class Program
            {
                public static void Main(string[] args)
                {
                    Console.WriteLine("Hello World!");
                }
            }
            
            """)
        let syntaxTrees = [| temp |]
            
        // list of references provided during parsing and 'refined' during binding
        // TODO: find a better way of doin this shi
        let net9DllsDir = DirectoryInfo(@"C:\Program Files\dotnet\shared\Microsoft.NETCore.App\9.0.2")
        let references : MetadataReference array = 
            [|
                MetadataReference.CreateFromFile(typeof<Object>.Assembly.Location)
                MetadataReference.CreateFromFile(typeof<Console>.Assembly.Location)
                MetadataReference.CreateFromFile(Path.Combine(net9DllsDir.FullName, "System.Runtime.dll"))
                MetadataReference.CreateFromFile(Path.Combine(net9DllsDir.FullName, "mscorlib.dll"))
            |]
            
        references |> Seq.iter (fun reference -> printfn $"{reference.Display}")
        
        let options = CSharpCompilationOptions(
            outputKind=OutputKind.ConsoleApplication,
            platform=Platform.AnyCpu
            )
            
        let assemblyName = "MonkeyAssembly"
        let compilation = CSharpCompilation.Create(
            assemblyName=assemblyName,
            syntaxTrees=syntaxTrees,
            references=references,
            options=options
        )
        
        compilation
        
    static member private generateRuntimeConfigJson () : string =
        """
{
  "runtimeOptions": {
    "tfm": "net9.0",
    "framework": {
      "name": "Microsoft.NETCore.App",
      "version": "9.0.0"
    }
  }
}
"""