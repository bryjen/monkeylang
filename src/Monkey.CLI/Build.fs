﻿module Monkey.CLI.Build


open System
open System.IO
open System.Threading
open Microsoft.CodeAnalysis.Text

open Argu

open FsToolkit.ErrorHandling

open Monkey.CLI
open Monkey.Codegen.Dotnet
open Monkey.Common
open Monkey.Parser.Errors
open Monkey.Codegen.Dotnet.CSharpProjectGeneratorErrors


type BuildError(
        ?message: string,
        ?innerException: Exception) =
    inherit Exception()

let rec performDotnetBuild (buildArguments: ParseResults<BuildArguments>) : int =
    result {
        use logHandle = ProgressTracker.addTasks [|
            "Scanning project files"
            "Compiling Files"
            "Generating C# project"
            "Running MsBuild"
        |]
        
        let! projectFile = tryGetProjectFile buildArguments
        let compileTarget = buildArguments.GetResult (BuildArguments.Target, CompileTarget.Integrated)
        let verbosity = buildArguments.GetResult (BuildArguments.Verbosity, defaultValue=Verbosity.Normal)
        let isStrict = buildArguments.Contains BuildArguments.WarningsAsErrors
        
        let outputDirPath = buildArguments.GetResult (OutputDir, defaultValue="./bin")
        let outputDirInfo = DirectoryInfo(outputDirPath)
        
        let csOutput = Path.Join(outputDirInfo.FullName, "g-cs")
#if ADD_ARTIFICIAL_DELAY
        Thread.Sleep(1000)
#endif
        logHandle.PopTask()
        
        let! scanResults = CSharpProjectGenerator.scanMonkeyProject projectFile.Directory.FullName
#if ADD_ARTIFICIAL_DELAY
        Thread.Sleep(1000)
#endif
        logHandle.PopTask()
        
        let! tempCsprojFileInfo = CSharpProjectGenerator.generateTempCSharpProject csOutput scanResults
#if ADD_ARTIFICIAL_DELAY
        Thread.Sleep(1000)
#endif
        logHandle.PopTask()
        
        do! CSharpProjectGenerator.runMsBuild outputDirInfo.FullName tempCsprojFileInfo.FullName
        logHandle.PopTask()
    }
    |> function
        | Ok _ -> 0
        | Error error ->
            formatError error
            -1
    
and tryGetProjectFile (buildArguments: ParseResults<BuildArguments>) : Result<FileInfo, Exception> =
    match buildArguments.TryGetResult BuildArguments.Project with
    | Some projectFilePath ->
        // validate existence of passed file
        let asFileInfo = FileInfo(projectFilePath)
        match asFileInfo.Exists with
        | true ->
            Ok asFileInfo
        | false -> 
            let errorMsg = $"The file at \"{asFileInfo.FullName}\" isn't a \"*.mkproj\" file."
            BuildError(message=errorMsg) :> Exception |> Error
    | None ->
        // search for ".mkproj" file
        let currentDirInfo = DirectoryInfo("./")
        let projectFiles = currentDirInfo.GetFiles("*.mkproj", SearchOption.TopDirectoryOnly)
        
        match projectFiles with
        | [| projectFileInfo |] -> Ok projectFileInfo
        | [|  |] ->
            let errorMsg = "No \"*.mkproj\" file found in the directory."
            BuildError(message=errorMsg) :> Exception |> Error
        | _ -> 
            let errorMsg = $"Multiple \"*.mkproj\" files ({projectFiles.Length}) were found in the directory. Please specify which project file to build."
            BuildError(message=errorMsg) :> Exception |> Error
            
            
and formatError (ex: Exception) =
    match ex with
    | :? MonkeyProjectFilesCompilationError as e ->
        printfn "Build failed with errors:\n"
        for fileInfo, errors in e.compilationErrors do
            let parseErrors = filterParseErrors errors
            let sourceText = SourceText.From(File.ReadAllText(fileInfo.FullName))
            for (parseError: ParseError) in parseErrors do
                printfn $"{parseError.GetFormattedMessage(sourceText, Some fileInfo.FullName)}\n"
    | _ ->
        printfn $"An unknown error occurred:\n{ex.Message}"
        
and private filterParseErrors (exceptions: Exception array) =
    let parseErrors = ResizeArray<ParseError>()
    for ex in exceptions do
        match ex with
        | :? ParseError as parseError -> parseErrors.Add(parseError)
        | _ -> ()
        
    parseErrors.ToArray()
