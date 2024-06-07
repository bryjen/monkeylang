namespace MonkeyInterpreter

open System
open FsToolkit.ErrorHandling

open MonkeyInterpreter.Token


[<AutoOpen>]
module private ParserHelpers =
    let peekTokenInArray (tokens: Token array) (index: int) : Token =
        match index with
        | i when  i < 0 || i >= tokens.Length ->
            let errorMsg = $"Attempted to access index \"{i}\" from an array with inclusive bounds [0, {tokens.Length}]"
            raise (IndexOutOfRangeException(errorMsg))
        | i ->
            tokens[i]
            
    let rec continueUntilSemiColon (tokens: Token array) (currentIndex: int) : int =
        let token = peekTokenInArray tokens currentIndex
        match token.Type with
        | TokenType.SEMICOLON | TokenType.EOF ->
            currentIndex
        | _ ->
            continueUntilSemiColon tokens (currentIndex + 1)
            
    let tryParseIdentifier (tokens: Token array) (index: int) : Result<Identifier, int> =
        let token = peekTokenInArray tokens index
        match token.Type with
        | TokenType.IDENT ->
            Ok { Token = token; Value = token.Literal }
        | _ ->
            Error index 
        
    let tryParseAssign (tokens: Token array) (index: int) : Result<Token, int> =
        let token = peekTokenInArray tokens index
        match token.Type with
        | TokenType.ASSIGN -> Ok token 
        | _ -> Error index 
            
        
module Parser =
    
    type private ParserInfo =
        { Tokens: Token array 
          PeekToken: int -> Token }
    
    let rec parseProgram (input: string) : Program =
        let tokens = input |> Lexer.parseIntoTokens |> List.toArray 
        let peekToken = peekTokenInArray tokens
        let parserInfo = { Tokens = tokens; PeekToken = peekToken }
        
        let rec parseProgramStatements currentIndex statementsList : Statement list =
            match peekToken currentIndex with
            | token when token.Type = TokenType.EOF ->
                List.rev statementsList
            | _ ->
                let newIndex, statementOption = tryParseStatement parserInfo currentIndex
                match statementOption with
                | Some statement ->
                    parseProgramStatements newIndex (statement :: statementsList)
                | None ->
                    parseProgramStatements newIndex statementsList 
            
        { Statements = parseProgramStatements 0 [] }
        
    and private tryParseStatement (parserInfo: ParserInfo) (currentIndex: int) : int * Statement Option =
        let currentToken = parserInfo.PeekToken currentIndex
        
        match currentToken.Type with
        | TokenType.LET ->
            let newIndex, letStatementOption = tryParseLetStatement parserInfo currentIndex
            let asStatementOption = Option.map Statement.LetStatement letStatementOption
            newIndex, asStatementOption
        | _ ->
            currentIndex + 1, None
            
    and private tryParseLetStatement (parserInfo: ParserInfo) (currentIndex: int) : int * LetStatement option =
        // Result CE returns new index + let statement if ok, only returns new index if error 
        result {
            let tokens = parserInfo.Tokens
            let letStatementToken = parserInfo.PeekToken currentIndex
            
            let currentIndex = currentIndex + 1
            let! identifier = tryParseIdentifier tokens currentIndex
            
            let currentIndex = currentIndex + 1
            let! _ = tryParseAssign tokens currentIndex
            
            let currentIndex = continueUntilSemiColon parserInfo.Tokens currentIndex
            
            // TODO: We're skipping parsing the expression for now
            let placeholderExpression: StringLiteral = { Token = letStatementToken; Value = "" }
            let letStatement: LetStatement = { Token = letStatementToken; Name = identifier; Value = Expression.StringLiteral placeholderExpression }
            
            return currentIndex, letStatement 
        }
        |> function
            | Ok (newIndex, letStatementOption) ->
                newIndex, Some letStatementOption
            | Error newIndex ->
                newIndex, None