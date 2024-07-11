namespace MonkeyInterpreter

open System
open FsToolkit.ErrorHandling

open Microsoft.FSharp.Core
open MonkeyInterpreter.Helpers.Queue
open MonkeyInterpreter.Token
    

type private ParserState =
    { TokensQueue: Token Queue
      Errors: string list
      Statements: Statement list }
    
module private ParserState =
    let addStatement (statement: Statement) (parserState: ParserState) =
        { parserState with Statements = statement :: parserState.Statements }
        
    let addError (errorMsg: string) (parserState: ParserState) =
        { parserState with Errors = errorMsg :: parserState.Errors }
        
    let replaceQueue (tokensQueue: Token Queue) (parserState: ParserState) =
        { parserState with TokensQueue = tokensQueue }
        
    let replaceQueueAndAddStatement (tokensQueue: Token Queue) (statement: Statement) (parserState: ParserState) =
        (replaceQueue tokensQueue >> addStatement statement) parserState
        
    let replaceQueueAndAddError (tokensQueue: Token Queue) (errorMsg: string) (parserState: ParserState) =
        (replaceQueue tokensQueue >> addError errorMsg) parserState
    
        
///
type private Precedence =
    | LOWEST = 1
    | EQUALS = 2
    | LESSGREATER = 3
    | SUM = 4
    | PRODUCT = 5
    | PREFIX = 6
    | CALL = 7
    
    
///
module private Precedence =     
    let tokenTypeToPrecedenceMap = Map.ofList [
        (TokenType.EQ, Precedence.EQUALS)
        (TokenType.NOT_EQ, Precedence.EQUALS)
        (TokenType.LT, Precedence.LESSGREATER)
        (TokenType.GT, Precedence.LESSGREATER)
        (TokenType.PLUS, Precedence.SUM)
        (TokenType.MINUS, Precedence.SUM)
        (TokenType.SLASH, Precedence.PRODUCT)
        (TokenType.ASTERISK, Precedence.PRODUCT)
    ]
    
    let peekPrecedence (tokensQueue: Token Queue) : Precedence =
        option {
            let! peekToken = Queue.peek tokensQueue
            return! Map.tryFind peekToken.Type tokenTypeToPrecedenceMap
        }
        |> function
           | Some precedence -> precedence
           | None -> Precedence.LOWEST
        
        
[<AutoOpen>]
module private ParserHelpers =
    let ofOption (defaultValue: 'b) (input: 'a Option) : Result<'a, 'b> =
        match input with
        | Some value -> Ok value 
        | None -> Error defaultValue
    
    let dequeueToken (errorMsg: string) (tokensQueue: Token Queue) =
        let newTokensQueue, dequeuedTokenResult = Queue.resultDequeue errorMsg tokensQueue
        dequeuedTokenResult
        |> Result.map (fun token -> (newTokensQueue, token))
        |> Result.mapError (fun erMsg -> (newTokensQueue, erMsg))
        
    let assertExpectedTokenType (expectedTokenType: TokenType) (tokensQueue: Token Queue) =
        result {
            let peekTokenResult = Queue.peek tokensQueue |> ofOption "Tokens queue empty."
            let! peekToken = Result.mapError (fun erMsg -> (tokensQueue, erMsg)) peekTokenResult
            return!
                match peekToken.Type with
                | tokenType when tokenType = expectedTokenType -> Ok ()
                | tokenType ->
                    let errorMsg = $"Expected a token type of \"{TokenType.ToCaseString expectedTokenType}\", got \"{TokenType.ToCaseString tokenType}\""
                    Error (Queue.removeTop tokensQueue, errorMsg)
        }
        
    let rec consumeQueueUntilSemicolon (tokensQueue: Token Queue) =
        match (Queue.peek tokensQueue) with
        | None -> tokensQueue 
        | Some token ->
            match token.Type with
            | tokenType when tokenType = SEMICOLON || tokenType = EOF -> tokensQueue
            | _ -> consumeQueueUntilSemicolon (Queue.removeTop tokensQueue)
        
        
module rec Parser =
    
    let rec parseProgram (input: string) : Program =
        let tokens = input |> Lexer.parseIntoTokens |> List.rev 
        let tokensQueue = Queue.enqueueList Queue.empty tokens 
        let initialParserState = { TokensQueue = tokensQueue; Errors = []; Statements = [] }
        
        let finalParserState = parseProgramHelper initialParserState
        let program = { Statements = List.rev finalParserState.Statements; Errors = List.rev finalParserState.Errors }
        program
        
    and private parseProgramHelper parserState : ParserState =
        let peekTokenOption = Queue.peek parserState.TokensQueue
        match peekTokenOption with
        | Some peekToken when peekToken.Type <> TokenType.EOF ->
            let updateParserState =  
                match (tryParseStatement peekToken parserState.TokensQueue) with
                | Ok (newTokensQueue, statement) -> ParserState.replaceQueueAndAddStatement newTokensQueue statement
                | Error (newTokensQueue, errorMsg) -> ParserState.replaceQueueAndAddError newTokensQueue errorMsg
               
            parseProgramHelper (updateParserState parserState)
        | _ ->
            parserState
            
    and private tryParseStatement peekToken tokensQueue
        : Result<Token Queue * Statement, Token Queue * string> =
        match peekToken.Type with
        | TokenType.LET ->
            tryParseLetStatement tokensQueue
        | TokenType.RETURN ->
            tryParseReturnStatement tokensQueue
        | TokenType.SEMICOLON ->
            result {
                let! newTokensQueue, _ = dequeueToken "" tokensQueue  // dequeued token is semicolon
                let peekTokenResult = Queue.peek tokensQueue |> ofOption "Tokens queue empty."
                let! peekToken = Result.mapError (fun erMsg -> (tokensQueue, erMsg)) peekTokenResult
                return! tryParseStatement peekToken newTokensQueue
            }
        | _ ->
            tryParseExpressionStatement tokensQueue
            
    and private tryParseExpression peekToken tokensQueue precedence
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let prefixParseFuncResult = Map.tryFind peekToken.Type prefixParseFunctionsMap
                                        |> ofOption $"No prefix parse function for \"{peekToken.Type}\" found."
            let! prefixParseFunc = Result.mapError (fun erMsg -> (tokensQueue, erMsg)) prefixParseFuncResult
            let! newTokensQueue, leftExpression = prefixParseFunc tokensQueue
            
            return! tryParseExpressionHelper newTokensQueue precedence leftExpression
        }
        
    // TODO: Find if there is a better name for this function
    and private tryParseExpressionHelper tokensQueue precedence leftExpr
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let peekTokenResult = Queue.peek tokensQueue |> ofOption "Tokens queue empty."
            let! peekToken = Result.mapError (fun erMsg -> (tokensQueue, erMsg)) peekTokenResult
            let peekPrecedence = Precedence.peekPrecedence tokensQueue
            
            if peekToken.Type <> TokenType.SEMICOLON && precedence < peekPrecedence then
                let infixParseFuncResult = Map.tryFind peekToken.Type infixParseFunctionsMap
                                           |> ofOption $"No prefix infix function for \"{peekToken.Type}\" found."
                let! infixParseFunc = Result.mapError (fun erMsg -> (tokensQueue, erMsg)) infixParseFuncResult
                let! newTokensQueue, infixExpr = infixParseFunc tokensQueue leftExpr
                return! tryParseExpressionHelper newTokensQueue peekPrecedence infixExpr
            else
                return tokensQueue, leftExpr
        }
        
        
    (* Parsing Statements *)
   
    
    and private tryParseLetStatement (tokensQueue: Token Queue)
        : Result<Token Queue * Statement, Token Queue * string> =
        result {
            let dequeueErMsg = "Tokens queue empty."
            let! newTokensQueue, letStatementToken = dequeueToken dequeueErMsg tokensQueue
            
            do! assertExpectedTokenType TokenType.IDENT tokensQueue
            let! newTokensQueue, identifierToken = dequeueToken dequeueErMsg newTokensQueue
            let identifier: Identifier = { Token = identifierToken; Value = identifierToken.Literal }
            
            do! assertExpectedTokenType TokenType.ASSIGN tokensQueue
            let! newTokensQueue, _ = dequeueToken dequeueErMsg newTokensQueue
            
            let newTokensQueue = consumeQueueUntilSemicolon newTokensQueue |> Queue.removeTop
            
            // TODO: We're skipping parsing the expression for now
            let placeholderExpression = Expression.StringLiteral { Token = letStatementToken; Value = "" }
            let statement = Statement.LetStatement { Token = letStatementToken; Name = identifier; Value = placeholderExpression }
            return newTokensQueue, statement
        }
        
    and private tryParseReturnStatement (tokensQueue: Token Queue)
        : Result<Token Queue * Statement, Token Queue * string> =
        result {
            let dequeueErMsg = "Tokens queue empty."
            let! newTokensQueue, returnStatementToken = dequeueToken dequeueErMsg tokensQueue
            
            let newTokensQueue = consumeQueueUntilSemicolon newTokensQueue |> Queue.removeTop
            
            // TODO: We're skipping parsing the expression for now
            let placeholderExpression = Expression.StringLiteral { Token = returnStatementToken; Value = "" }
            let statement = Statement.ReturnStatement { Token = returnStatementToken; ReturnValue = placeholderExpression }
            return newTokensQueue, statement
        }
        
    and private tryParseExpressionStatement (tokensQueue: Token Queue)
        : Result<Token Queue * Statement, Token Queue * string> =
        result {
            let peekTokenResult = Queue.peek tokensQueue |> ofOption "Tokens queue empty."
            let! peekToken = Result.mapError (fun erMsg -> (tokensQueue, erMsg)) peekTokenResult
            
            let! newTokensQueue, expr = tryParseExpression peekToken tokensQueue Precedence.LOWEST
            let statement = Statement.ExpressionStatement { Token = peekToken; Expression = expr }
            return newTokensQueue, statement 
        }
        
        
    (* Pratt Parsing Stuff *)
    
    and private tryParseIdentifier (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken "Tokens queue empty." tokensQueue
            let expression = Expression.Identifier { Token = dequeuedToken; Value = dequeuedToken.Literal }
            return newTokensQueue, expression
        }
        
    and private tryParseIntegerLiteral (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken "Tokens queue empty." tokensQueue
            let expressionResult =
                match Int64.TryParse(dequeuedToken.Literal) with
                | true, result -> Ok (Expression.IntegerLiteral { Token = dequeuedToken; Value = result })
                | false, _ -> Error $"Could not parse \"{dequeuedToken.Literal}\" as an Int64"
            let! expression = Result.mapError (fun erMsg -> (newTokensQueue, erMsg)) expressionResult
            return newTokensQueue, expression
        }
           
    and private tryParsePrefixExpression (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken "Tokens queue empty." tokensQueue
            let! newTokensQueue, rightExpr = tryParseExpression dequeuedToken newTokensQueue Precedence.PREFIX
            
            let prefixExpr = Expression.PrefixExpression { Token = dequeuedToken; Operator = dequeuedToken.Literal; Right = rightExpr }
            return newTokensQueue, prefixExpr
        }
        
    and private prefixParseFunctionsMap = Map.ofList [
        (TokenType.IDENT, tryParseIdentifier)
        (TokenType.INT, tryParseIntegerLiteral)
        (TokenType.BANG, tryParsePrefixExpression)
        (TokenType.MINUS, tryParsePrefixExpression)
    ]
    
    and private tryParseInfixExpression (tokensQueue: Token Queue) (leftExpr: Expression)
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let precedence = Precedence.peekPrecedence tokensQueue
            
            let! newTokensQueue, dequeuedToken = dequeueToken "Tokens queue empty." tokensQueue
            let! newTokensQueue, rightExpr = tryParseExpression dequeuedToken newTokensQueue precedence
            
            let infixExpr = Expression.InfixExpression { Token = dequeuedToken; Operator = dequeuedToken.Literal
                                                         Left = leftExpr; Right = rightExpr }
            return newTokensQueue, infixExpr
        }
           
    and private infixParseFunctionsMap = Map.ofList [
        (TokenType.PLUS, tryParseInfixExpression)
        (TokenType.MINUS, tryParseInfixExpression)
        (TokenType.SLASH, tryParseInfixExpression)
        (TokenType.ASTERISK, tryParseInfixExpression)
        (TokenType.EQ, tryParseInfixExpression)
        (TokenType.NOT_EQ, tryParseInfixExpression)
        (TokenType.LT, tryParseInfixExpression)
        (TokenType.GT, tryParseInfixExpression)
    ]
    
(*
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
        | Option.Some prefixParseFunc ->
            let newIndex, prefixExprParseResult = prefixParseFunc parserInfo currentIndex
            
            match prefixExprParseResult with
            | Some prefixExpr -> someHelper parserInfo newIndex precedence prefixExpr
            | _ -> newIndex, prefixExprParseResult
        | Option.None ->
            currentIndex + 1, ErrorMsg $"No prefix parse function for \"{currentToken.Type}\" found."
            
    and private someHelper parserInfo currentIndex precedence leftExpr =
        let currentToken = parserInfo.PeekToken currentIndex
        let peekPrecedence = Precedence.peekPrecedence parserInfo currentIndex
        if currentToken.Type <> TokenType.SEMICOLON && precedence < peekPrecedence then
            let infixParseFuncOption = Map.tryFind currentToken.Type infixParseFunctionsMap 
            
            match infixParseFuncOption with
            | Option.Some infixParseFunc ->
                let newIndex, infixExprParseResult = infixParseFunc parserInfo currentIndex leftExpr
                match infixExprParseResult with
                | ErrorMsg errorMsg -> newIndex, ErrorMsg errorMsg
                | Some expr -> someHelper parserInfo newIndex peekPrecedence expr
                | None -> failwith "idk if this is supposed to happen"
            | Option.None ->
                currentIndex + 1, ErrorMsg $"No infix parse function for \"{currentToken.Type}\" found."
        else
            currentIndex, Some leftExpr
            
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
        
    
    (* Pratt Parsing Stuff *)
        
    and private tryParseIdentifier parserInfo currentIndex : int * ParseResult<Expression> =
        let currentToken = parserInfo.PeekToken currentIndex 
        currentIndex + 1, Some (Expression.Identifier { Token = currentToken; Value = currentToken.Literal })
        
    and private tryParseIntegerLiteral parserInfo currentIndex : int * ParseResult<Expression> =
        let currentToken = parserInfo.PeekToken currentIndex
        match Int64.TryParse(currentToken.Literal) with
        | true, result ->
            currentIndex + 1, Some (Expression.IntegerLiteral { Token = currentToken; Value = result })
        | false, _ ->
            currentIndex + 1, ErrorMsg $"Could not parse \"{currentToken.Literal}\" as an Int64"
            
    and private tryParsePrefixExpression parserInfo currentIndex : int * ParseResult<Expression> =
        let currentToken = parserInfo.PeekToken currentIndex
        let newIndex, rightExprParseResult = tryParseExpression parserInfo (currentIndex + 1) Precedence.PREFIX
        
        let prefixExpr =
            rightExprParseResult
            |> ParseResult.map (fun rightExpr -> { Token = currentToken; Operator = currentToken.Literal; Right = rightExpr })
            |> ParseResult.map Expression.PrefixExpression
            
        newIndex, prefixExpr
            
    and private prefixParseFunctionsMap = Map.ofList [
        (TokenType.IDENT, tryParseIdentifier)
        (TokenType.INT, tryParseIntegerLiteral)
        (TokenType.BANG, tryParsePrefixExpression)
        (TokenType.MINUS, tryParsePrefixExpression)
    ]
    
    
    and private tryParseInfixExpression
        (parserInfo: ParserInfo)
        (currentIndex: int)
        (left: Expression)
        : int * ParseResult<Expression> =
            
        let currentToken = parserInfo.PeekToken currentIndex
        let precedence = Precedence.peekPrecedence parserInfo currentIndex
        let newIndex, rightExprParseResult = tryParseExpression parserInfo (currentIndex + 1) precedence
       
        let infixExpr =  
            rightExprParseResult
            |> ParseResult.map (fun rightExpr -> { Token = currentToken; Operator = currentToken.Literal; Left = left; Right = rightExpr })
            |> ParseResult.map Expression.InfixExpression
        
        newIndex, infixExpr 
    
    and private infixParseFunctionsMap = Map.ofList [
        (TokenType.PLUS, tryParseInfixExpression)
        (TokenType.MINUS, tryParseInfixExpression)
        (TokenType.SLASH, tryParseInfixExpression)
        (TokenType.ASTERISK, tryParseInfixExpression)
        (TokenType.EQ, tryParseInfixExpression)
        (TokenType.NOT_EQ, tryParseInfixExpression)
        (TokenType.LT, tryParseInfixExpression)
        (TokenType.GT, tryParseInfixExpression)
    ]
*)