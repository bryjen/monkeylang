namespace Monkey.Frontend

open System
open Monkey.Frontend.Repl 



module Program = 

    [<EntryPoint>]
    let main _ =
        Repl.startRepl Console.In Console.Out
        