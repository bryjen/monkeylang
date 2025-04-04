module Monkey.Codegen.Dotnet.CSharpProjectGenerator

open System
open System.IO
open System.Diagnostics

open Microsoft.Build.Locator
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp.Syntax
open type Microsoft.CodeAnalysis.CSharp.SyntaxFactory

open FsToolkit.ErrorHandling

open Monkey.Parser.Tokenizer
open Monkey.Parser.Parser
open Monkey.Codegen.Dotnet.MonkeyToCSharpAstConverter
open Monkey.Codegen.Dotnet.CSharpProjectGeneratorErrors



let rec private filterVsInstances (vsInstances: VisualStudioInstance list) : VisualStudioInstance option =
    match vsInstances with
    | [] -> None
    | head :: tail ->
        let msbuildPath = head.MSBuildPath
        if File.Exists(Path.Join(msbuildPath, "MSBuild.dll")) && File.Exists(Path.Join(msbuildPath, "dotnet.dll")) then
            Some head
        else
            filterVsInstances tail
            
            
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
        let parseResultsAndOutputFileInfoPairs =
            sourceAndOutputFileInfoPairs 
            |> Array.map (fun (sourceFileInfo, outputFileInfo) -> (compileFile sourceFileInfo, outputFileInfo))
        
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
                MonkeyProjectFilesCompilationError(compilationErrors=justErrors) :> Exception |> Error
        
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
    
and private emitAsCsFile (compilationUnit: CompilationUnitSyntax) (outputFileInfo: FileInfo) =
    let formattedCsCode = compilationUnit.NormalizeWhitespace().ToFullString()
    
    if not outputFileInfo.Directory.Exists then
        Directory.CreateDirectory(outputFileInfo.Directory.FullName) |> ignore
        
    File.WriteAllText(outputFileInfo.FullName, formattedCsCode)
    
    
    
let runMsBuild (outputPath: string) (tempCsprojFilePath: string) : Result<unit, Exception> =
    result {
        let vsInstances = MSBuildLocator.QueryVisualStudioInstances() |> Seq.toList
        let vsInstanceOption = filterVsInstances vsInstances
        let! vsInstance =
            match vsInstanceOption with
            | Some value -> Ok value
            | None -> MSBuildToolsNotFoundError(vsInstances, [ "MSBuild.dll"; "dotnet.dll" ]) :> Exception |> Error
        
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
    
