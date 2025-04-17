module Monkey.Common.CompilerTask

open System
open System.Collections.Concurrent
open System.Collections.Generic
open System.Threading
open System.Threading.Tasks
open Serilog.Events
open Spectre.Console

// TODO: Refactor with MailboxProcessor
// see: https://en.wikibooks.org/wiki/F_Sharp_Programming/MailboxProcessor

// BUG: builtin `Spectre.Console` spinners not working ? workaround being we need to create our own
// Full list of spectre console spinners available here: https://jsfiddle.net/sindresorhus/2eLtsbey/embedded/result/
type private BinarySpinner () =
    inherit Spinner()
    
    let spinnerFrames =
        ([0..63]
        |> List.map (fun i -> Convert.ToString(i, 2).PadLeft(6, '0'))
        |> List<string>) :> IReadOnlyList<string>
with
    override this.Interval = TimeSpan.FromMilliseconds 50.0
    override this.IsUnicode = false
    override this.Frames = spinnerFrames
        


type CompilerTask =
    { Msg: string
      LogLevel: LogEventLevel }

    
    
let private updateLen = 100;
    


type CompilerProgression private () =
    let mutable compilerTasks: ConcurrentBag<CompilerTask> = ConcurrentBag()
    
    let mutable progressTask: Task | null = null
    let mutable lock: obj = obj()
    let mutable isProgressOngoing: bool = false
    
    
    static let instance = lazy (CompilerProgression())
    static member Instance = instance.Value
    
    member this.CompilerTasks
        with get() = compilerTasks
        and set(value) = compilerTasks <- value
        
    member this.ProgressTask
        with private get() = progressTask
        and private set(value) = progressTask <- value
        
    member this.Lock
        with private get() = lock
        and private set(value) = lock <- value
        
    member this.isProgressOngoing
        with private get() = isProgressOngoing
        and private set(value) = isProgressOngoing <- value
    
        
    ///
    static member StartProgress() =
        let instance = CompilerProgression.Instance
        
        let handler : Func<StatusContext, Task> =
            Func<_,_> (fun (ctx: StatusContext) ->
                ctx.Spinner <- BinarySpinner()
                AnsiConsole.MarkupLine("Started...")
                
                while instance.isProgressOngoing do
                    lock instance.Lock (fun () ->
                        for task in instance.CompilerTasks do
                            AnsiConsole.WriteLine(task.Msg)
                            ()
                        instance.CompilerTasks.Clear()
                        )
                    Thread.Sleep updateLen
                    
                AnsiConsole.MarkupLine("Finished...")
                
                Task.CompletedTask)
        
        instance.ProgressTask <- Task.Run(fun () -> AnsiConsole.Status().StartAsync("something", handler))
        instance.isProgressOngoing <- true
        
        
    ///
    static member AddTask(task: CompilerTask) =
        let instance = CompilerProgression.Instance
        
        Task.Run(fun () ->
            lock instance.Lock (fun () ->
                instance.CompilerTasks.Add(task)
                )
            )
        |> ignore
        
        ()
        
        
    ///
    static member EndProgress() =
        let instance = CompilerProgression.Instance
        
        lock instance.Lock (fun () ->
            instance.isProgressOngoing <- false
            
            if instance.ProgressTask |> isNull |> not then
                instance.ProgressTask.Wait()
                
            instance.ProgressTask <- null
            )
