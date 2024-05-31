namespace MonkeyInterpreter

open System.IO
open MonkeyInterpreter.Token

module Repl =
    let private Prompt = ">> "
    
    let private processAllTokens (lexer: Lexer) =
        
        let rec processAllTokensCore (currentTokens: Token list) = 
            let token = lexer.NextToken()
            match token.Type with
            | EOF -> List.rev currentTokens 
            | _ -> processAllTokensCore (token :: currentTokens) 
            
        processAllTokensCore []
    
    let rec startRepl (stdIn: TextReader) (stOut: TextWriter) =
        stdout.Write(Prompt) 
        
        match stdIn.ReadLine() with
        | null ->
            ()
        | input ->
            let lexer = Lexer(input)
            let tokens = processAllTokens lexer
            
            tokens
            |> List.iter stOut.WriteLine

        startRepl stdIn stdout 