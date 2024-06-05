namespace MonkeyInterpreter

open MonkeyInterpreter.Token

type Parser(lexer: Lexer) =
    
    let mutable currentToken: Token = lexer.NextToken()
    
    let mutable nextToken: Token = lexer.NextToken()
    
    member this.Lexer = lexer
    
    
    member this.NextToken() =
        currentToken <- nextToken
        nextToken = lexer.NextToken()
        
    member this.ParseProgram() : Result<Program, string> =
        Error ""