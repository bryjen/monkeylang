namespace MonkeyInterpreter

open FsToolkit.ErrorHandling

open MonkeyInterpreter.Token
        

type Parser(lexer: Lexer) =
    
    let mutable currentToken: Token = lexer.NextToken()
    
    let mutable peekNextToken: Token = lexer.NextToken()
    
    member this.Lexer = lexer
    
    
    member this.NextToken() =
        currentToken <- peekNextToken
        peekNextToken <- lexer.NextToken()
        
    member this.ParseProgram() : Program =
        let rec parseProgramStatements statementsList =
            match currentToken.Type with
            | TokenType.EOF ->
                List.rev statementsList
            | _ -> 
                match this.ParseStatement() with
                | None ->
                    parseProgramStatements statementsList 
                | Some statement ->
                    parseProgramStatements (statement :: statementsList)
                    
        { Statements = parseProgramStatements [] }
        
    member this.ParseStatement() : Statement option =
        match currentToken.Type with
        | TokenType.LET ->
            Option.map Statement.LetStatement (this.ParseLetStatement())
        | _ ->
            this.NextToken()
            None
            
    member private this.ContinueUntilSemiColon() =
        let rec continueUntilSemiColonCore () =
            match currentToken.Type with
            | TokenType.SEMICOLON | TokenType.EOF ->
                ()
            | _ ->
                this.NextToken()
                continueUntilSemiColonCore ()
                
        continueUntilSemiColonCore ()
           
    member this.ParseLetStatement() : LetStatement option =
        option {
            let token = currentToken
            
            let! literal = peekNextToken.Type
                           |> function
                               | TokenType.IDENT ->
                                   let identifier: Identifier = { Token = peekNextToken; Value = peekNextToken.Literal }
                                   Some identifier
                               | _ ->
                                   None
                           
            this.NextToken()
            
            do! peekNextToken.Type
                |> function
                   | TokenType.ASSIGN -> Some ()
                   | _ -> None
                   
            this.NextToken()
            
            // TODO: We're skipping parsing the expression for now
            let placeholderExpression: StringLiteral = { Token = token; Value = "" }
            
            this.ContinueUntilSemiColon()
            
            return { Token = token
                     Name = literal
                     Value = Expression.StringLiteral placeholderExpression }
        }