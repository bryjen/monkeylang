module CLI.Run



open System.IO
open System.Text
open System.Text.Json
open System.Text.Json.Serialization

open Argu
open FsToolkit.ErrorHandling

open Monkey.Backend.VM
open Monkey.Backend.Compiler
open Monkey.Frontend.Eval.Object

open Monkey.CLI
open Monkey.CLI.Helpers



let rec performRun (buildParseResults: ParseResults<RunArguments>) : int =
    // further argument post-processing
    let file = buildParseResults.GetResult (File, defaultValue="")
    let compileTarget = buildParseResults.GetResult (RunArguments.Target, CompileTarget.Integrated)
    let isVerbose = buildParseResults.Contains RunArguments.Verbose
    
    result {
        let absFilePath = Path.GetFullPath(file)
        
        if isVerbose then
            printArguments absFilePath compileTarget isVerbose
            
        let! bytecode = getFileContents isVerbose absFilePath
        let! _ = executeBytecode isVerbose bytecode
        return ()
    }
    |> function
       | Ok _ ->
           0
       | Error errorMsg ->
           printfn $"ERROR: {errorMsg}"
           -1
           

and private printArguments filePath compileTarget isVerbose =
    let compileTargetStr =
        match compileTarget with
        | CompileTarget.Integrated -> "integrated"
        | CompileTarget.Dotnet -> "dotnet"
        | _ ->
            Log.info "Failed to determine compile target string."
            ""
            
    Log.info $"output dir:\t\t\"{filePath}\""
    Log.info $"compile target:\t\t{compileTargetStr}"
    Log.info $"is verbose:\t\t{isVerbose}"


and private getFileContents isVerbose filePath : Result<Bytecode, string> =
    try 
        if isVerbose then
            Log.info $"Attempting to load bytecode from \"{filePath}\""
            
        let bytes = File.ReadAllBytes(filePath)
        let asJson = Encoding.UTF8.GetString(bytes)
        
        let options = JsonFSharpOptions.Default().ToJsonSerializerOptions()
        options.WriteIndented <- true
        let bytecode = JsonSerializer.Deserialize<Bytecode>(asJson, options)
        Ok bytecode
    with
    | ex ->
        Error ex.Message
    

and private executeBytecode isVerbose bytecode : Result<Object option, string> =
    result {
        let vm = VM.fromByteCode bytecode
        let! newVm = VM.run vm
        let resultOption = VM.getLastPoppedStackElement newVm
        return resultOption
    }