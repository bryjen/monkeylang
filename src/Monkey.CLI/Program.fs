﻿open System.Diagnostics
open Argu

open Monkey.CLI.Build
open Monkey.CLI.New
open Monkey.CLI.Run
open Monkey.CLI



[<EntryPoint>]
let rec main argv =
#if DEBUG
    printfn "DEBUG (NON-RELEASE) BUILD\n"
#endif
    
    let parser = ArgumentParser.Create<ProgramArguments>(programName = "monkey")
    
    if argv.Length = 0 then
        printfn $"{parser.PrintUsage()}"
        System.Environment.Exit(0)
    
    let mutable programArgumentsNullable: ParseResults<ProgramArguments> option = None
    try
        programArgumentsNullable <- Some (parser.ParseCommandLine(inputs=argv, raiseOnUsage=true))
    with
    | ex ->
        printfn $"{ex.Message}"
        System.Environment.Exit(-1)
        
    let programArguments = Option.get programArgumentsNullable  // doesn't error because it raises instead of returning 'None'
    
    match programArguments with
    | _ when programArguments.Contains Build ->
        performDotnetBuild (programArguments.GetResult Build)
    | _ when programArguments.Contains New ->
        performNew (programArguments.GetResult New)
    | _ when programArguments.Contains Run ->
        performRun (programArguments.GetResult Run)
    | _ when programArguments.Contains Version ->
        printfn "Version"
        0
    | _ ->
        printfn "An unexpected error occurred."
        -1
    
    