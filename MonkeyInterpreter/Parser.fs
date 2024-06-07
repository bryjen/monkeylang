namespace MonkeyInterpreter

open System.Collections.Generic
open FsToolkit.ErrorHandling

open MonkeyInterpreter.Token
        

type Parser(input: string) =
    
    let defaultToken = { Type = TokenType.EOF; Literal = "" } 
    
    let mutable currentToken: Token = defaultToken 
    
    let mutable peekNextToken: Token = defaultToken 
    
    let tokenQueue =
        let queue = Queue<Token>()
        List.iter queue.Enqueue (Lexer.parseIntoTokens input)
        queue
        
    
    member this.NextToken() =
        currentToken <- peekNextToken
        peekNextToken <- if tokenQueue.Count > 0 then tokenQueue.Dequeue() else defaultToken
        
    member this.ParseProgram() : Program =
        currentToken <- tokenQueue.Dequeue()
        peekNextToken <- tokenQueue.Dequeue() 
        
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