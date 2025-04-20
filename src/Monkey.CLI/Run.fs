module Monkey.CLI.Run

open System
open System.IO
open System.Reflection

open Argu

open FsToolkit.ErrorHandling

open Monkey.CLI
open Monkey.CLI.Build
open Monkey.Codegen.Dotnet
open Monkey.Common


    
type ExecutableNotFoundError(
        targetFile: string,
        searchedDirectories: string array,
        ?innerException: Exception) =
    inherit Exception()
    
type MultipleExecutablesError(
        targetFile: string,
        duplicateFileInfos: FileInfo array,
        ?innerException: Exception) =
    inherit Exception()
    

let rec performRun (runParseResults: ParseResults<RunArguments>) : int =
    result {
        use logHandle = ProgressTracker.addTasks [|
            "Scanning project files"
            "Compiling Files"
            "Generating C# project"
            "Running MsBuild"
        |]
        
        // building project
        let! projectFile = tryGetProjectFile runParseResults
        let compileTarget = runParseResults.GetResult (RunArguments.Target, CompileTarget.Integrated)
        let verbosity = runParseResults.GetResult (RunArguments.Verbosity, defaultValue=Verbosity.Normal)
        let isStrict = runParseResults.Contains RunArguments.WarningsAsErrors
        
        let outputDirPath = runParseResults.GetResult (BuildOutputDir, defaultValue="./bin")
        let outputDirInfo = DirectoryInfo(outputDirPath)
        
        // TODO: Both of the below blocks take in the files, then build them independently of each other.
        // Maybe find a way to share the build results with each other so we only compile once.
        
        // Generate output files, same as `build`
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
#if ADD_ARTIFICIAL_DELAY
        Thread.Sleep(1000)
#endif
        logHandle.PopTask()
        
        // Create assembly information in-memory, then run it
        let! csharpCompilation = DynamicExecution.compileFiles scanResults.SourceFileInfos scanResults.MkprojFileInfo
        
        ProgressTracker.stop()
        DynamicExecution.dynamicallyRunCompilation csharpCompilation
        
        return 0
    }
    |> function
        | Ok _ -> 0
        | Error error ->
            printfn $"Run Failed:\n{error.ToString()}"
            -1

// Changed to accept 'runArguments'
and private tryGetProjectFile (runArguments: ParseResults<RunArguments>) : Result<FileInfo, Exception> =
    match runArguments.TryGetResult RunArguments.Project with
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
           