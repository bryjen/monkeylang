open System
open MonkeyInterpreter

module Program = 

    [<EntryPoint>]
    let main _ =
        Repl.startRepl Console.In Console.Out