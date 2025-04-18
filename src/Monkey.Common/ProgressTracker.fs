[<RequireQualifiedAccess>]
module Monkey.Common.ProgressTracker

open System
open Monkey.Common.SpectreConsole.Spinner
open Serilog.Events
open Spectre.Console
open Spectre.Console.Prompts.Extensions




[<AutoOpen>]
module private LogMsgFormatting =
    let logLevelToString (logLevel: LogEventLevel) =
        match logLevel with
        | LogEventLevel.Verbose ->  "TRC"
        | LogEventLevel.Debug ->    "DBG"
        | LogEventLevel.Warning ->  "WRN"
        | LogEventLevel.Error ->    "ERR"
        | LogEventLevel.Fatal ->    "FTL"
        | _ -> "INF"
        
    let wrapWithMarkup (logLevel: LogEventLevel) (str: string) =
        match logLevel with
        | LogEventLevel.Verbose ->  $"[white on red]{str}[/]"
        | LogEventLevel.Debug ->    $"[blue]{str}[/]"
        | LogEventLevel.Warning ->  $"[yellow]{str}[/]"
        | LogEventLevel.Error ->    $"[red]{str}[/]"
        | LogEventLevel.Fatal ->    $"[pink]{str}[/]"
        | _ -> $"[white]{str}[/]"
        
[<AutoOpen>]
module private ConsoleHelpers =
    let clearLine (line: int) =
        let curLeft = Console.CursorLeft
        let curTop  = Console.CursorTop
        Console.SetCursorPosition(0, line)
        Console.Write(String(' ', Console.WindowWidth))
        Console.SetCursorPosition(curLeft, curTop)
        
        

type ProgressTrackerConfig private =
    { LogLevel: LogEventLevel option  // none indicates no logging
      ShowDateTime: bool
      ClearOnFinish: bool  // clears the console output when finished
       }
    

type Msg private =
    | Log of Log
    | EditStatus of (Tree -> Tree)
    | Stop of AsyncReplyChannel<unit>
and Log private =
    { Msg: string
      LogLevel: LogEventLevel }
        
        
let mutable private alreadyStopped = false
let mutable private statusTree = Tree("Initializing ...")
        
let private mailbox =
    MailboxProcessor.Start(fun inbox ->
        AnsiConsole
            .Status()
            .Spinner(EmptySpinner())
            .StartAsync(AnsiBuilder.Build(statusTree), fun (ctx: StatusContext) -> task {
                while true do
                    let! msg = inbox.Receive() |> Async.StartAsTask
                    match msg with
                    | Log log ->
                        let logLevelPrefix =
                            log.LogLevel
                            |> logLevelToString
                            |> wrapWithMarkup log.LogLevel
                        let markupMsg = $"[[{logLevelPrefix}]]: {log.Msg}"
                        AnsiConsole.MarkupLine(markupMsg)
                        
                    | EditStatus editCallback ->
                        let newStatusTree = editCallback statusTree
                        statusTree <- newStatusTree
                        ctx.Status <- AnsiBuilder.Build(statusTree)
                        
                    | Stop reply ->
                        ctx.Spinner <- EmptySpinner()  // workaround since we cant delete the last line
                        ctx.Status <- "\b"
                        ctx.Refresh()
                        reply.Reply()  // reply so that 
                        return ()
            }
            ) |> Async.AwaitTask)
    |> Lazy<MailboxProcessor<Msg>>
    
    
    
let start () =  // provides an explicit method to init the mailbox from lazy eval
    mailbox.Value |> ignore
    ()
    
let stop () =
    if not alreadyStopped then
        alreadyStopped <- true
        mailbox.Value.PostAndReply(Stop)

let log msg =
    { Msg = msg; LogLevel = LogEventLevel.Information }
    |> Msg.Log
    |> mailbox.Value.Post
    
let changeStatus (status: string) =
    let callback =
        fun (_: Tree) ->
            let tree = Tree(status)
            tree.AddNode("something") |> ignore
            tree
            
            
    callback
    |> Msg.EditStatus
    |> mailbox.Value.Post