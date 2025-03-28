module Monkey.CLI.Run



open System
open System.IO
open System.Reflection

open Argu
open Monkey.CLI.Build
open FsToolkit.ErrorHandling

open Monkey.Frontend.CLR.Api
open Monkey.Frontend.CLR.Api.Errors

open Monkey.CLI
open Monkey.CLI.Helpers

    
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
        // building project
        let! projectFile = tryGetProjectFile runParseResults
        let compileTarget = runParseResults.GetResult (RunArguments.Target, CompileTarget.Integrated)
        let verbosity = runParseResults.GetResult (RunArguments.Verbosity, defaultValue=Verbosity.Normal)
        let isStrict = runParseResults.Contains RunArguments.WarningsAsErrors
        
        let outputDirPath = runParseResults.GetResult (BuildOutputDir, defaultValue="./bin")
        let outputDirInfo = DirectoryInfo(outputDirPath)
        
        let csOutput = Path.Join(outputDirInfo.FullName, "g-cs")
        let! scanResults = CsharpProjectConverter.scanMonkeyProject projectFile.Directory.FullName
        let! tempCsprojFileInfo = CsharpProjectConverter.generateTempCSharpProject csOutput scanResults
        do! CsharpProjectConverter.runMsBuild outputDirInfo.FullName tempCsprojFileInfo.FullName
        
        // finding and running the built executables
        let assemblyName = Path.GetFileNameWithoutExtension(projectFile.Name)
        let targetFile = $"{assemblyName}.dll"
        let! targetFileInfo =
            match outputDirInfo.GetFiles(targetFile) with
            | [||] ->
                ExecutableNotFoundError(targetFile=targetFile,searchedDirectories=[| outputDirInfo.FullName |]) :> Exception |> Error
            | [| executableFileInfo |] ->
                Ok executableFileInfo
            | fileInfos ->
                MultipleExecutablesError(targetFile=targetFile,duplicateFileInfos=fileInfos) :> Exception |> Error
                
        // TODO: Add support for argument parsing
        let args: string array = [| |]
        let asm = Assembly.LoadFrom targetFileInfo.FullName
        let entryPoint = asm.EntryPoint
        let parameters = entryPoint.GetParameters()
        if parameters.Length = 0 then
            entryPoint.Invoke(null, [||]) |> ignore
        else
            entryPoint.Invoke(null, [| args |]) |> ignore
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
           