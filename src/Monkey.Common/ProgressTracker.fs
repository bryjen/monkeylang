﻿[<RequireQualifiedAccess>]
module Monkey.Common.ProgressTracker

open System
open System.Threading
open System.Threading.Tasks
open Monkey.Common.Spinner
open Monkey.Common.StringTree




[<AutoOpen>]
module private ConsoleHelpers =
    let clearTree (tree: StringTree) =
        let numLines = tree |> _.ToString() |> _.Split('\n') |> _.Length
        let numLines = Math.Max(0, numLines)
        printf $"\x1b[{numLines}F"
        for i = 0 to numLines - 1 do
            printfn "\x1b[2K"
        printf $"\x1b[{numLines}F"
        
    let clearPrintArtifacts () =
        let numLines = 1
        printf $"\x1b[{numLines}F"
        for i = 0 to numLines - 1 do
            printfn "\x1b[2K"
        printf $"\x1b[{numLines}F"
        

type Msg private =
    | SetStatusTree of StringTree
    | Stop of AsyncReplyChannel<unit>


let mutable private isPrinterStopped = false
let mutable private isMailboxStopped = false
let mutable private statusTree = Leaf "Building Project Files"

let spinner = DotsSpinner() :> ISpinner
let updateDelay = TimeSpan.FromSeconds(0.1)


let private ignoreChars = [| ' '; '└'; '├' |]
let mutable private count = 0

let private addCountCallback (str: string) =
    let indentationLen = 1 |> StringTree.identStr |> _.Length
    let mutable currentIdx = 0
    while Array.contains str[currentIdx] ignoreChars do
        currentIdx <- currentIdx  + indentationLen
        
    count <- count + 1
    $"{str[0..currentIdx - 1]}[{count}] {str[currentIdx..]}"

let private startPrinterTask () =
    let addSpinnerCallback (str: string) =
        let indentationLen = 1 |> StringTree.identStr |> _.Length
        let mutable currentIdx = 0
        while Array.contains str[currentIdx] ignoreChars do
            currentIdx <- currentIdx  + indentationLen
            
        $"{str[0..currentIdx - 1]}\u001b[38;2;186;225;255m{spinner.NextFrame()}\u001b[0m {str[currentIdx..]}"
    
    Task.Run(fun () ->
        while not isPrinterStopped do
            let oldStatusTree = statusTree
            Thread.Sleep(updateDelay)
            clearTree oldStatusTree
            
            let newStatusTree = statusTree.ModifyCurrent(addSpinnerCallback)
            printfn $"{newStatusTree.ToString()}"
            ()
        ())
        
let private mailbox =
    MailboxProcessor.Start(fun inbox -> async {
        let mutable printerTask = startPrinterTask ()
        while true do
            let! msg = inbox.Receive()
            match msg with
            | SetStatusTree newStatusTree ->
                statusTree <- newStatusTree
            | Stop reply ->
                isPrinterStopped <- true
                do! printerTask |> Async.AwaitTask
                
                clearTree statusTree
                clearPrintArtifacts ()
                reply.Reply()
                
                return()
    })
    |> Lazy<MailboxProcessor<Msg>>
    
    
    
let start () =  // provides an explicit method to init the mailbox from lazy eval
    mailbox.Value |> ignore
    ()
    
let stop () =
    if not isMailboxStopped then
        isMailboxStopped <- true
        mailbox.Value.PostAndReply(Stop)


/// <summary>
/// Represents a set of tasks relative to the whole program process. Provides methods for managing these tasks. Strongly
/// coupled to the progression display.
/// <br/>
/// <br/>
/// Ideally, all tasks should be done as soon as the caller exits scope, hence the <c>IDisposable</c> implementation.
/// </summary>
type TaskHandle private () =
    let mutable oldStatusTree: StringTree = Unchecked.defaultof<StringTree>
    let mutable newStatusTree: StringTree = Unchecked.defaultof<StringTree>
    
    member this.OldStatusTree
        with private get() = oldStatusTree
        and private set value = oldStatusTree <- value
        
    member this.NewStatusTree
        with private get() = newStatusTree
        and private set value = newStatusTree <- value
        
        
    member this.PopTask() =
        // newStatusTree <- newStatusTree.ModifyParent(addCountCallback)
        newStatusTree <- newStatusTree.PopNode()
        newStatusTree |> Msg.SetStatusTree |> mailbox.Value.Post
        ()
        
    
    interface IDisposable with
        member this.Dispose() =
            statusTree <- oldStatusTree
            oldStatusTree |> Msg.SetStatusTree |> mailbox.Value.Post
            ()
        
    static member internal Init(currentStatusTree: StringTree, tasks: string array) =
        let newStatusTree = currentStatusTree.AddNode(tasks)
        newStatusTree |> Msg.SetStatusTree |> mailbox.Value.Post
        
        let handle = new TaskHandle()
        handle.OldStatusTree <- currentStatusTree
        handle.NewStatusTree <- newStatusTree
        handle
        
        
let addTasks (tasks: string array) = TaskHandle.Init(statusTree, tasks)