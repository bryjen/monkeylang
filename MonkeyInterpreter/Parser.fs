namespace MonkeyInterpreter

open System
open FsToolkit.ErrorHandling

open MonkeyInterpreter.Token
    

type private ParseResult<'a> =
    | Some of 'a
    | None
    | ErrorMsg of string
    
    
module private ParseResult =
    let map 
        (binder: 'someInput -> 'someOutput)
        (input: ParseResult<'someInput>)
        : ParseResult<'someOutput> =
        match input with
        | Some x -> Some (binder x) 
        | None -> None 
        | ErrorMsg errorMsg -> ErrorMsg errorMsg
        
        
type private Precedence =
    | LOWEST = 1
    | EQUALS = 2
    | LESSGREATER = 3
    | SUM = 4
    | PRODUCT = 5
    | PREFIX = 6
    | CALL = 7
        

type private ParserInfo =
    { Tokens: Token array
      Errors: string list
      PeekToken: int -> Token }
        
        
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
          
            
[<AutoOpen>] 
module private PrattParsing =
    let tryParseIdentifier parserInfo currentIndex : int * ParseResult<Expression> =
        let currentToken = parserInfo.PeekToken currentIndex 
        currentIndex + 1, Some (Expression.Identifier { Token = currentToken; Value = currentToken.Literal })
        
    let tryParseIntegerLiteral parserInfo currentIndex : int * ParseResult<Expression> =
        let currentToken = parserInfo.PeekToken currentIndex
        
        match Int64.TryParse(currentToken.Literal) with
        | true, result ->
            currentIndex + 1, Some (Expression.IntegerLiteral { Token = currentToken; Value = result })
        | false, _ ->
            currentIndex + 1, ErrorMsg $"Could not parse \"{currentToken.Literal}\" as an Int64"
            
    let prefixParseFunctionsMap = Map.ofList [
        (TokenType.IDENT, tryParseIdentifier)
        (TokenType.INT, tryParseIntegerLiteral)
    ]
            
        
module Parser =
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
            let newIndex, letStatement = tryParseLetStatement parserInfo currentIndex
            newIndex, ParseResult.map Statement.LetStatement letStatement
        | TokenType.RETURN ->
            let newIndex, returnStatement = tryParseReturnStatement parserInfo currentIndex
            newIndex, ParseResult.map Statement.ReturnStatement returnStatement
        | TokenType.SEMICOLON ->
            currentIndex + 1, None
        | _ ->
            let newIndex, expressionStatement = tryParseExpressionStatement parserInfo currentIndex
            newIndex, ParseResult.map Statement.ExpressionStatement expressionStatement
            
    and private tryParseExpression
        (parserInfo: ParserInfo)
        (currentIndex: int)
        (precedence: Precedence)
        : int * ParseResult<Expression> =
            
        let currentToken = parserInfo.PeekToken currentIndex
        let parseFuncOption = Map.tryFind currentToken.Type prefixParseFunctionsMap
        
        match parseFuncOption with
        | Option.Some parseFunc ->
            parseFunc parserInfo currentIndex 
        | Option.None ->
            currentIndex + 1, None
            
    and private tryParseLetStatement parserInfo currentIndex =
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
                
    and private tryParseReturnStatement parserInfo currentIndex =
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
                
    and private tryParseExpressionStatement
        (parserInfo: ParserInfo)
        (currentIndex: int)
        : int * ParseResult<ExpressionStatement> =
            
        let currentToken = parserInfo.PeekToken currentIndex
        let newIndex, expressionParseResults = tryParseExpression parserInfo currentIndex Precedence.LOWEST
        newIndex, ParseResult.map (fun expr -> { Token = currentToken; Expression = expr } ) expressionParseResults
