module Monkey.CLI.Build


open System
open System.IO
open System.Threading
open Microsoft.CodeAnalysis.Text

open Argu

open FsToolkit.ErrorHandling

open Monkey.CLI
open Monkey.Codegen.Dotnet
open Monkey.Common.CompilerTask
open Monkey.Parser.Errors
open Monkey.Codegen.Dotnet.CSharpProjectGeneratorErrors
open Serilog.Events
open Spectre.Console


type BuildError(
        ?message: string,
        ?innerException: Exception) =
    inherit Exception()

let rec performDotnetBuild (buildArguments: ParseResults<BuildArguments>) : int =
    result {
        let! projectFile = tryGetProjectFile buildArguments
        let compileTarget = buildArguments.GetResult (BuildArguments.Target, CompileTarget.Integrated)
        let verbosity = buildArguments.GetResult (BuildArguments.Verbosity, defaultValue=Verbosity.Normal)
        let isStrict = buildArguments.Contains BuildArguments.WarningsAsErrors
        
        let outputDirPath = buildArguments.GetResult (OutputDir, defaultValue="./bin")
        let outputDirInfo = DirectoryInfo(outputDirPath)
        
        // TODO: Remove & refactor
        CompilerProgression.StartProgress()
        for i in 1 .. 5 do
            let compTask =
                { Msg = $"Doing task {i}"
                  LogLevel = LogEventLevel.Information }
            CompilerProgression.AddTask(compTask) 
            Thread.Sleep(1000)
        Thread.Sleep(5000)
        CompilerProgression.EndProgress()
        
        let csOutput = Path.Join(outputDirInfo.FullName, "g-cs")
        let! scanResults = CSharpProjectGenerator.scanMonkeyProject projectFile.Directory.FullName
        let! tempCsprojFileInfo = CSharpProjectGenerator.generateTempCSharpProject csOutput scanResults
        do! CSharpProjectGenerator.runMsBuild outputDirInfo.FullName tempCsprojFileInfo.FullName
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
