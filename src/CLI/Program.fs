

open System.IO
open Argu
open CLI.Build
open CLI.Run
open Monkey.CLI

[<EntryPoint>]
let rec main argv =
    let parser = ArgumentParser.Create<ProgramArguments>(programName = "monkey")
    
    if argv.Length = 0 then
        printfn $"{parser.PrintUsage()}"
        System.Environment.Exit(0)
    
    // parse cli args
    let mutable programArgumentsNullable: ParseResults<ProgramArguments> option = None
    try
        programArgumentsNullable <- Some (parser.ParseCommandLine(inputs=argv, raiseOnUsage=true))
    with
    | ex ->
        printfn $"{ex.Message}"
        System.Environment.Exit(-1)
        
    let programArguments = Option.get programArgumentsNullable  // doesn't error because it raises instead of returning 'None'
    
    
    // perform action
    match programArguments with
    | _ when programArguments.Contains Build ->
        let buildParseResults = programArguments.GetResult Build
        // performBuild buildParseResults
        performDotnetBuild buildParseResults
        // performDotnetBuildAlt buildParseResults
    | _ when programArguments.Contains New ->
        printfn "New"
        0
    | _ when programArguments.Contains Run ->
        let runParseResults = programArguments.GetResult Run
        performRun runParseResults
    | _ when programArguments.Contains Version ->
        printfn "Version"
        0
    | _ ->
        printfn "An unexpected error occurred."
        -1
    
    