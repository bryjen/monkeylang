module Monkey.Codegen.Dotnet.Tests.CodegenTestsHelpers

open System.IO
open System.Reflection
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open FsToolkit.ErrorHandling
open Monkey.Parser.Errors

let internal assertNoParseErrors (sourceText: string) (parseErrors: ParseError array) =
    match parseErrors.Length with
    | 0 ->
        Ok ()
    | _ ->
        let filePath = @"C:\Users\Public\Program.mk"
        let sourceText = SourceText.From(sourceText)
        
        let parseErrorStrs = 
            parseErrors
            |> Array.mapi (fun idx parseError -> $"{idx + 1}.\n{parseError.GetFormattedMessage(sourceText, Some filePath)}")
            
        Error (System.String.Join("\n", parseErrorStrs))
        
let internal printTestCases (expectedSyntaxTree: SyntaxTree) (monkeyInput: string) (actualSyntaxTree: SyntaxTree) =
    
    printfn "Input/Output"
    printfn "---------------------------------------------------------"
    
    printfn "```monkey"
    printfn $"{monkeyInput}"
    printfn "```"
    printfn ""
    
    printfn "```csharp (expected)"
    printfn $"{expectedSyntaxTree.ToString()}"
    printfn "```"
    printfn ""
    
    printfn "```csharp (actual)"
    printfn $"{actualSyntaxTree.ToString()}"
    printfn "```"
    printfn "---------------------------------------------------------"

    ()
    
    
    
type internal Input =
    { SourceFileInfos: FileInfo array
      ProjectFileInfo: FileInfo }
    
type internal Expected =
    { ExpectedCsFiles: FileInfo array
      CsprojFileInfo: FileInfo }
    
let rec internal loadSample (sampleId: int) : Result<Input * Expected, string> =
    result {
        let assemblyExecutionDir = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
        let sampleBaseDir = Path.Join(assemblyExecutionDir, "samples", $"{sampleId}")
        
        let inputDirPath = Path.Join(sampleBaseDir, "input") |> Path.GetFullPath
        do! if not (Directory.Exists(inputDirPath)) then Error $"Input .mk files directory \"{inputDirPath}\" could not be found" else Ok ()
        
        let expectedDirPath = Path.Join(sampleBaseDir, "expected") |> Path.GetFullPath
        do! if not (Directory.Exists(expectedDirPath)) then Error $"Expected .cs files directory \"{expectedDirPath}\" could not be found" else Ok ()
        
        let! inputFiles = Directory.GetFiles(inputDirPath, "*.mk", SearchOption.AllDirectories) |> assertExists
        let! inputMkProj = Directory.GetFiles(inputDirPath, "*.mkproj", SearchOption.AllDirectories) |> assertExists 
        let! inputMkProj = inputMkProj |> assertSingleFile "*.mkproj"
        
        let! expectedFiles = Directory.GetFiles(expectedDirPath, "*.cs", SearchOption.AllDirectories) |> assertExists
        let! expectedCsproj = Directory.GetFiles(expectedDirPath, "*.csproj", SearchOption.AllDirectories) |> assertExists 
        let! expectedCsproj = expectedCsproj |> assertSingleFile "*.csproj"
        
        let input =
            { SourceFileInfos = inputFiles
              ProjectFileInfo = inputMkProj }
            
        let expected =
            { ExpectedCsFiles = expectedFiles 
              CsprojFileInfo = expectedCsproj }
            
        return input, expected
    }
    
and private assertExists (filePathStrs: string array) : Result<FileInfo array, string> =
    let fileInfos = filePathStrs |> Array.map FileInfo
    let doesntExist = fileInfos |> Array.choose (function fi when not fi.Exists -> Some fi | _ -> None)
    match doesntExist with
    | [| |] -> Ok fileInfos
    | _ ->
        let errorStr = System.String.Join("\n", doesntExist |> Array.map _.FullName)
        Error $"Could not find the following files:\n{errorStr}"
    
and private assertSingleFile (fileExt: string) (fileInfos: FileInfo array) : Result<FileInfo, string> =
    match fileInfos with
    | [| singularFileInfo |] -> Ok singularFileInfo
    | fs -> Error $"Expected a single \"{fileExt}\" file, found {fs.Length}."