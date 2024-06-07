namespace MonkeyInterpreter

open System.IO

module Repl =
    let private Prompt = ">> "
    
    
    let rec startRepl (stdIn: TextReader) (stOut: TextWriter) =
        stdout.Write(Prompt) 
        
        match stdIn.ReadLine() with
        | null ->
            ()
        | input ->
            let tokens = Lexer.parseIntoTokens input
            tokens |> List.iter stOut.WriteLine

        startRepl stdIn stdout