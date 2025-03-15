namespace rec Monkey.Frontend.CLR.Api

open System
open System.Diagnostics
open System.IO
open System.Threading.Tasks
open Microsoft.Build.Locator
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open type Microsoft.CodeAnalysis.CSharp.SyntaxFactory

open FsToolkit.ErrorHandling

open Microsoft.CodeAnalysis.MSBuild
open Monkey.Frontend.CLR.Api.Errors
open Monkey.Frontend.CLR.HostStubGenerator
open Monkey.Frontend.CLR.Lexer
open Monkey.Frontend.CLR.Parsers
open Monkey.Frontend.CLR.Parsers.ParseErrors

        
        
module CsharpProjectConverter =
    
    type ScanResults =
        { MkprojFileInfo: FileInfo
          SourceFileInfos: FileInfo array
          SourceDirInfo: DirectoryInfo }
    
    let rec scanMonkeyProject (projectDir: string) : Result<ScanResults, Exception> =
        result {
            let dirInfo = DirectoryInfo(projectDir)
            let projectFiles = dirInfo.GetFiles("*.mkproj", SearchOption.TopDirectoryOnly)
            
            let! projectFile = 
                match projectFiles with
                | [| projectFileInfo |] -> Ok projectFileInfo
                | [|  |] ->
                    let errorMsg = "No \"*.mkproj\" file found in the directory."
                    CSharpProjectGenerationError(message=errorMsg) :> Exception |> Error
                | _ -> 
                    let errorMsg = $"Multiple \"*.mkproj\" files ({projectFiles.Length}) were found in the directory."
                    CSharpProjectGenerationError(message=errorMsg) :> Exception |> Error
                    
            let excludedDirectoriesToSearch = [| "bin"; "obj"; ".vscode"; ".idea" |]
            let monkeyFiles = getMonkeyFiles excludedDirectoriesToSearch dirInfo
            return { MkprojFileInfo = projectFile; SourceFileInfos = monkeyFiles; SourceDirInfo = dirInfo }
        }
        
    and private getMonkeyFiles (excludedDirectoryNames: string array) (dirInfo: DirectoryInfo) : FileInfo array =
        let subDirectoriesToSearch =
            dirInfo.GetDirectories()
            |> Array.filter (fun dirInfo -> excludedDirectoryNames |> Array.contains dirInfo.Name |> not )
            
        let subDirectoryMonkeyFiles =
            subDirectoriesToSearch
            |> Array.map (getMonkeyFiles excludedDirectoryNames)
            |> Array.collect id
        
        let monkeyFiles = dirInfo.GetFiles("*.mk", SearchOption.TopDirectoryOnly)
        Array.concat [| monkeyFiles; subDirectoryMonkeyFiles |]
        
       
        
    let rec generateTempCSharpProject (outputDir: string) (scanResults: ScanResults) : Result<FileInfo, Exception> =
        result {
            let sourceDirInfo = scanResults.SourceDirInfo
            
            // generate .csproj file
            let mkprojRelativePath = Path.GetRelativePath(sourceDirInfo.FullName, scanResults.MkprojFileInfo.FullName)
            let outputCsprojFileInfo = FileInfo(Path.Combine(outputDir, Path.ChangeExtension(mkprojRelativePath, ".csproj")))
            
            if not outputCsprojFileInfo.Directory.Exists then
                Directory.CreateDirectory(outputCsprojFileInfo.Directory.FullName) |> ignore
            
            let projFileContents = File.ReadAllText(scanResults.MkprojFileInfo.FullName)
            File.WriteAllText(outputCsprojFileInfo.FullName, projFileContents)
            
            
            // generate .cs files
            let sourceAndOutputFileInfoPairs = getSourceAndOutputFilePathPairs outputDir scanResults
            let parseResultsAndOutputFileInfoPairs = sourceAndOutputFileInfoPairs |> Array.map (fun (sourceFileInfo, outputFileInfo) -> (compileFile sourceFileInfo, outputFileInfo))
            
            let sourceFileInfoAndParseResultsPairs =
                sourceAndOutputFileInfoPairs
                |> Array.zip parseResultsAndOutputFileInfoPairs
                |> Array.map (fun (tuple1, tuple2) -> (fst tuple2, fst tuple1))
            
            let justErrors =
                sourceFileInfoAndParseResultsPairs
                |> Array.choose
                       (function
                        | _, Ok _ -> None
                        | fileInfo, Error errors -> Some (fileInfo, errors))
                       
            do! match justErrors with
                | [| |] -> Ok ()
                | _ -> 
                    let errorMsg = "Some errors occurred during project compilation."
                    MonkeyProjectFilesCompilationError(message=errorMsg, compilationErrors=justErrors) :> Exception |> Error
            
            let compilationResultsAndOutputFileInfoPairs =
                parseResultsAndOutputFileInfoPairs
                |> Array.choose
                       (function
                        | Ok compilationUnit, outputFileInfo -> Some (compilationUnit, outputFileInfo)
                        | Error _, _ -> None)
                       
            compilationResultsAndOutputFileInfoPairs
            |> Array.map (fun (compilationUnit, outputFileInfo) -> emitAsCsFile compilationUnit outputFileInfo)
            |> ignore
            
            return outputCsprojFileInfo
        }
        
    and private getSourceAndOutputFilePathPairs (outputDir: string) (scanResults: ScanResults) : (FileInfo * FileInfo) array =
        let sourceDirInfo = scanResults.SourceDirInfo
        let correspondingOutputCsPath =
            scanResults.SourceFileInfos
            |> Array.map (fun fileInfo -> Path.GetRelativePath(sourceDirInfo.FullName, fileInfo.FullName))
            |> Array.map (fun relativePath -> Path.Combine(outputDir, relativePath))
            |> Array.map (fun relativePath -> Path.ChangeExtension(relativePath, ".cs"))
            |> Array.map (fun outputAbsPath -> FileInfo(outputAbsPath))
            
        Array.zip scanResults.SourceFileInfos correspondingOutputCsPath
        
    and private compileFile (fileInfo: FileInfo) : Result<CompilationUnitSyntax, Exception list> =
        result {
            let monkeySourceCode = File.ReadAllText(fileInfo.FullName)
            let tokens = Lexer.parseIntoTokens monkeySourceCode
            let statements, errors = ModifiedRecursiveDescent.parseTokens (List.toArray tokens)
            
            do! match errors with
                | [] -> Ok ()
                | _ -> errors |> List.map (fun error -> error :> Exception) |> Error
                
            return toCompilationUnit statements
        }
        
    and private toCompilationUnit (statements: StatementSyntax list) =
        let tempUsingDirective = UsingDirective(IdentifierName("System"))  // TODO: This thing
        
        let globalStatements =
            statements
            |> List.toArray
            |> Array.map GlobalStatement
            |> Array.map (fun globalStatement -> globalStatement :> MemberDeclarationSyntax)
        CompilationUnit()
            .WithUsings(SyntaxFactory.List([| tempUsingDirective |]))
            .WithMembers(SyntaxFactory.List(globalStatements))
            .NormalizeWhitespace()
            
    and emitAsCsFile (compilationUnit: CompilationUnitSyntax) (outputFileInfo: FileInfo) =
        let formattedCsCode = compilationUnit.NormalizeWhitespace().ToFullString()
        
        if not outputFileInfo.Directory.Exists then
            Directory.CreateDirectory(outputFileInfo.Directory.FullName) |> ignore
            
        File.WriteAllText(outputFileInfo.FullName, formattedCsCode)
        
        
        
    let runMsBuild (outputPath: string) (tempCsprojFilePath: string) =
        result {
            let vsInstances = MSBuildLocator.QueryVisualStudioInstances() |> Seq.toList
            let vsInstanceOption = filterVsInstances vsInstances
            let! vsInstance =
                match vsInstanceOption with
                | Some value -> Ok value
                | None -> MSBuildToolsNotFound(vsInstances, [ "MSBuild.dll"; "dotnet.dll" ]) :> Exception |> Error
            
            let msbuildDll = Path.Join(vsInstance.MSBuildPath, "MSBuild.dll")
            let dotnetDll = Path.Join(vsInstance.MSBuildPath, "dotnet.dll")
            let argsArr = [|
                $"\"{msbuildDll}\""
                $"-p:OutDir=\"{outputPath}\""
                "-nologo"
                "-consoleloggerparameters:Summary"
                $"-distributedlogger:Microsoft.DotNet.Tools.MSBuild.MSBuildLogger,\"{dotnetDll}\"*Microsoft.DotNet.Tools.MSBuild.MSBuildForwardingLogger,\"{dotnetDll}\""
                "-maxcpucount"
                "-restore"
                "-tlp:default=auto"
                "-verbosity:m"
                "-verbosity:diag"
                tempCsprojFilePath
            |]
            
            let psi = ProcessStartInfo("dotnet", String.Join(" ", argsArr))
            psi.UseShellExecute <- false
            psi.RedirectStandardOutput <- true
            psi.RedirectStandardError <- true

            use proc = Process.Start(psi)
            let output = proc.StandardOutput.ReadToEnd()
            let error = proc.StandardError.ReadToEnd()
            proc.WaitForExit()
            
            // printfn "Output:\n%s" output
            // printfn "Errors:\n%s" error
            
            return ()
        }
        
    let rec filterVsInstances (vsInstances: VisualStudioInstance list) : VisualStudioInstance option =
        match vsInstances with
        | [] -> None
        | head :: tail ->
            let msbuildPath = head.MSBuildPath
            if File.Exists(Path.Join(msbuildPath, "MSBuild.dll")) && File.Exists(Path.Join(msbuildPath, "dotnet.dll")) then
                Some head
            else
                filterVsInstances tail
    
    
    
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
    [<Obsolete>]
    let mutable loadedFiles: FileInfo array = [||]
    
    let mutable loadedCompilation: CSharpCompilation option = None
    let mutable log: LogFunctions = LogFunctions.Default
    
    member this.SetLogFunctions(logFunctions: LogFunctions) : FrontendApi =
        log <- logFunctions
        this
    
    [<Obsolete>]
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
        
    [<Obsolete>]
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
        
    [<Obsolete>]
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
                    
                    // DotnetProjectGen.generateHostStub compilation.AssemblyName outputDir |> ignore
                    Ps1Gen.generateHostStub compilation.AssemblyName outputDir |> ignore
                    
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