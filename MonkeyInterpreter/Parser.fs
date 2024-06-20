namespace MonkeyInterpreter

open System
open FsToolkit.ErrorHandling

open MonkeyInterpreter.Token


[<AutoOpen>]
module private ParserHelpers =
    
    type ParseResult<'a> =
        | Some of 'a
        | None
        | ErrorMsg of string
    with
        static member Map 
            (binder: 'someInput -> 'someOutput)
            (input: ParseResult<'someInput>)
            : ParseResult<'someOutput> =
            match input with
            | Some x -> Some (binder x) 
            | None -> None 
            | ErrorMsg errorMsg -> ErrorMsg errorMsg
            
    
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
            let token = peekToken currentIndex
            if token.Type = TokenType.EOF then
                { Statements = List.rev statementsList
                  Errors = List.rev parserInfo.Errors }
            else
                let newIndex, parseResult = tryParseStatement parserInfo currentIndex
                match parseResult with
                | Some statement -> 
                    parseProgramStatements parserInfo (statement :: statementsList) newIndex
                | None -> 
                    parseProgramStatements parserInfo statementsList newIndex
                | ErrorMsg errorMsg -> 
                    let newIndex = continueUntilSemiColon parserInfo.Tokens currentIndex // In case of parsing error, go to token following the next semicolon
                    let newParserInfo = { parserInfo with Errors = errorMsg :: parserInfo.Errors }
                    parseProgramStatements newParserInfo statementsList (newIndex + 1)
            
        let parserInfo = { Tokens = tokens; Errors = []; PeekToken = peekToken }
        parseProgramStatements parserInfo [] 0
        
    and private tryParseStatement
        (parserInfo: ParserInfo)
        (currentIndex: int)
        : int * ParseResult<Statement> =
            
        let currentToken = parserInfo.PeekToken currentIndex
        match currentToken.Type with
        | TokenType.LET ->
            let newIndex, letStatementResult = tryParseLetStatement parserInfo currentIndex
            let asStatementResult = ParseResult.Map<LetStatement, Statement> Statement.LetStatement letStatementResult
            newIndex, asStatementResult
        | TokenType.RETURN ->
            let newIndex, letStatementResult = tryParseReturnStatement parserInfo currentIndex
            let asStatementResult = ParseResult.Map<ReturnStatement, Statement> Statement.ReturnStatement letStatementResult
            newIndex, asStatementResult
        | TokenType.SEMICOLON ->
            currentIndex + 1, None
        | _ ->
            currentIndex + 1, ErrorMsg "Don't recognize token"
            
    and private tryParseLetStatement
        (parserInfo: ParserInfo)
        (currentIndex: int)
        : int * ParseResult<LetStatement> =
            
        result {
            let letStatementToken = parserInfo.PeekToken currentIndex
            
            let currentIndex = currentIndex + 1
            let! identifier = parseExpectedIdentifier parserInfo.Tokens currentIndex
            
            let currentIndex = currentIndex + 1
            let! _ = parseExpectedAssignmentOperator parserInfo.Tokens currentIndex
            
            let currentIndex = continueUntilSemiColon parserInfo.Tokens currentIndex
            
            // TODO: We're skipping parsing the expression for now
            let placeholderExpression: StringLiteral = { Token = letStatementToken; Value = "" }
            let letStatement: LetStatement = { Token = letStatementToken
                                               Name = identifier
                                               Value = Expression.StringLiteral placeholderExpression }
            
            return currentIndex + 1, letStatement 
        }
        |> function
            | Ok (newIndex, letStatement) ->
                newIndex, Some letStatement
            | Error (newIndex, errorMsg) ->
                newIndex, ErrorMsg errorMsg
                
    and private tryParseReturnStatement
        (parserInfo: ParserInfo)
        (currentIndex: int)
        : int * ParseResult<ReturnStatement> =
            
        result {
            let returnStatementToken = parserInfo.PeekToken currentIndex
            
            let currentIndex = continueUntilSemiColon parserInfo.Tokens currentIndex
            
            // TODO: We're skipping parsing the expression for now
            let placeholderExpression: StringLiteral = { Token = returnStatementToken; Value = "" }
            let returnStatement: ReturnStatement = { Token = returnStatementToken
                                                     ReturnValue = Expression.StringLiteral placeholderExpression }
            
            return currentIndex + 1, returnStatement 
        }
        |> function
            | Ok (newIndex, returnStatement) ->
                newIndex, Some returnStatement
            | Error (newIndex, errorMsg) ->
                newIndex, ErrorMsg errorMsg
