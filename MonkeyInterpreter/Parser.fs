namespace MonkeyInterpreter

open System
open FsToolkit.ErrorHandling

open MonkeyInterpreter.Token
open MonkeyInterpreter.Helpers.Numbers


[<AutoOpen>]
module private ParserHelpers =
    let transformErrorTypeToOptional (result: Result<'a, 'b>) : Result<'a, 'b option> =
        match result with
        | Ok resultValue -> Ok resultValue 
        | Error errorValue -> Error (Some errorValue) 
    
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
            
    let parseExpectedIdentifier (tokens: Token array) (index: int) : Result<Identifier, int * string> =
        let token = peekTokenInArray tokens index
        match token.Type with
        | TokenType.IDENT ->
            Ok { Token = token; Value = token.Literal }
        | _ ->
            let errorMsg = $"Expected an identifier at index \"{index}\", got a \"{TokenType.ToCaseString token.Type}\"."
            Error (index, errorMsg) 
        
    let parseExpectedAssignmentOperator (tokens: Token array) (index: int) : Result<Token, int * string> =
        let token = peekTokenInArray tokens index
        match token.Type with
        | TokenType.ASSIGN ->
            Ok token 
        | _ ->
            let errorMsg = $"Expected an assignment operator \"=\" at index \"{index}\", got a \"{TokenType.ToCaseString token.Type}\"."
            Error (index, errorMsg) 
            
        
module Parser =
    
    type private ParserInfo =
        { Tokens: Token array
          Errors: string list
          PeekToken: int -> Token }
    
    let rec parseProgram (input: string) : Program =
        let tokens = input |> Lexer.parseIntoTokens |> List.toArray 
        let peekToken = peekTokenInArray tokens
        
        let rec parseProgramStatements parserInfo statementsList currentIndex : Program =
            match peekToken currentIndex with
            | token when token.Type = TokenType.EOF ->
                { Statements = List.rev statementsList
                  Errors = List.rev parserInfo.Errors }
            | _ ->
                let newIndex, statementResult = tryParseStatement parserInfo currentIndex
                match statementResult with
                | Ok statement ->
                    parseProgramStatements parserInfo (statement :: statementsList) newIndex
                | Error errorMsgOption ->
                    // In case of parsing error, go to token following the next semi colon 
                    let newIndex = continueUntilSemiColon parserInfo.Tokens currentIndex
                    let newParserInfo = appendErrorMessageToParserInfo parserInfo errorMsgOption 
                    parseProgramStatements newParserInfo statementsList (newIndex + 1)
                    
        and appendErrorMessageToParserInfo parserInfo errorMsgOption : ParserInfo =
            match errorMsgOption with
            | Some errorMsg ->
                { parserInfo with Errors = errorMsg :: parserInfo.Errors }
            | None ->
                parserInfo
            
            
        let parserInfo = { Tokens = tokens; Errors = []; PeekToken = peekToken }
        parseProgramStatements parserInfo [] 0
        
    and private tryParseStatement (parserInfo: ParserInfo) (currentIndex: int) : int * Result<Statement, string option> =
        let currentToken = parserInfo.PeekToken currentIndex
        
        match currentToken.Type with
        | TokenType.LET ->
            let newIndex, letStatementResult = tryParseLetStatement parserInfo currentIndex
            let asStatementResult = Result.map Statement.LetStatement letStatementResult
            newIndex, transformErrorTypeToOptional asStatementResult
        | TokenType.SEMICOLON ->
            currentIndex + 1, Error None
        | _ ->
            currentIndex + 1, Error (Some "Don't recognize token")
            
    and private tryParseLetStatement (parserInfo: ParserInfo) (currentIndex: int) : int * Result<LetStatement, string> =
        // Result CE returns new index + let statement if ok, only returns new index if error 
        result {
            let tokens = parserInfo.Tokens
            let letStatementToken = parserInfo.PeekToken currentIndex
            
            let currentIndex = currentIndex + 1
            let! identifier = parseExpectedIdentifier tokens currentIndex
            
            let currentIndex = currentIndex + 1
            let! _ = parseExpectedAssignmentOperator tokens currentIndex
            
            let currentIndex = continueUntilSemiColon parserInfo.Tokens currentIndex
            
            // TODO: We're skipping parsing the expression for now
            let placeholderExpression: StringLiteral = { Token = letStatementToken; Value = "" }
            let letStatement: LetStatement = { Token = letStatementToken; Name = identifier; Value = Expression.StringLiteral placeholderExpression }
            
            return currentIndex + 1, letStatement 
        }
        |> function
            | Ok (newIndex, letStatement) ->
                newIndex, Ok letStatement
            | Error (newIndex, errorMsg) ->
                newIndex, Error errorMsg 