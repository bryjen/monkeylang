module Monkey.Codegen.Dotnet.CSharpCompilationGenerator

open System
open System.IO

open FsToolkit.ErrorHandling
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Monkey.Codegen.Dotnet.CSharpProjectGeneratorErrors
open Monkey.Codegen.Dotnet.MonkeyToCSharpAstConverter
open Monkey.Codegen.Dotnet.ProjectFile
open Monkey.Parser.Parser
open Monkey.Parser.Tokenizer

let createCSharpCompilation (projectConfig: ProjectConfig) (compilationUnits: CompilationUnitSyntax array) =
    let syntaxTrees = compilationUnits |> Array.map _.SyntaxTree
    
    let references: MetadataReference array =
        [|
            MetadataReference.CreateFromFile(typeof<obj>.Assembly.Location)
            MetadataReference.CreateFromFile(typeof<Console>.Assembly.Location)
            MetadataReference.CreateFromFile(typeof<obj>.Assembly.Location.Replace("System.Private.CoreLib.dll", "System.Runtime.dll"))
        |]
        
    CSharpCompilation.Create(
        projectConfig.AssemblyName,
        syntaxTrees = syntaxTrees,
        references = references,
        options = CSharpCompilationOptions(OutputKind.ConsoleApplication)
    )
    

let rec compileFiles (sourceFilePaths: FileInfo array) (configFilePath: FileInfo) =
    result {
        let compilationResults = sourceFilePaths |> Array.map compileFile
        let sourceFileErrorsPairs = filterErrors sourceFilePaths compilationResults
        
        do! match sourceFileErrorsPairs with
            | [| |] -> Ok ()
            | _ -> MonkeyProjectFilesCompilationError(compilationErrors=sourceFileErrorsPairs) :> Exception |> Error
            
        // we're guaranteed that all compilation units are valid because of the assertion of the above
        let compilationUnits = compilationResults |> Array.choose (function Error _ -> None | Ok cu -> Some cu)
        
        let projectConfig = ProjectConfig.Default  // TODO: REPLACE WITH ACTUAL PARSING
        let csharpCompilation = createCSharpCompilation projectConfig compilationUnits
        return csharpCompilation
    }
    
    
and private filterErrors (sourceFilePaths: FileInfo array) (compilationResults: Result<CompilationUnitSyntax, Exception array> array) =
    (Array.zip sourceFilePaths compilationResults)
    |> Array.choose
           (function
            | _, Ok _ -> None
            | fileInfo, Error errors -> Some (fileInfo, errors))
    
    
and private compileFile (fileInfo: FileInfo) : Result<CompilationUnitSyntax, Exception array> =
    result {
        let monkeySourceCode = File.ReadAllText(fileInfo.FullName)
        let tokens = tokenize monkeySourceCode
        let statements, parseErrors = parseTokens tokens
        
        do! match parseErrors with
            | [||] -> Ok ()
            | _ -> parseErrors |> Array.map (fun error -> error :> Exception) |> Error
            
        let conversionResults =
            toCSharpCompilationUnit statements.Statements  // TODO: FIX    
            |> Result.mapError (Array.map (fun err -> err :> Exception))
            
        let! compilationUnit = conversionResults
        return compilationUnit
    }

