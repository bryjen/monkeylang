namespace MonkeyInterpreter

open System
open FsToolkit.ErrorHandling

open Microsoft.FSharp.Core
open MonkeyInterpreter.Helpers.Queue
    

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
    
        
        
type private Precedence =
    | LOWEST = 1
    | EQUALS = 2
    | LESSGREATER = 3
    | SUM = 4
    | PRODUCT = 5
    | PREFIX = 6
    | CALL = 7
    
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
        
    let queuePeekToken (errorMsg: string) (tokensQueue: Token Queue) =
        match (Queue.peek tokensQueue) with
        | Some peekToken -> Ok peekToken 
        | None -> Error (tokensQueue, errorMsg)
    
    let dequeueToken (errorMsg: string) (tokensQueue: Token Queue) =
        let newTokensQueue, dequeuedTokenResult = Queue.resultDequeue errorMsg tokensQueue
        dequeuedTokenResult
        |> Result.map (fun token -> (newTokensQueue, token))
        |> Result.mapError (fun erMsg -> (newTokensQueue, erMsg))
        
    let assertExpectedTokenType
       (processQueue: Token Queue -> Token Queue)
       (expectedTokenType: TokenType)
       (tokensQueue: Token Queue) =
        result {
            let peekTokenResult = Queue.peek tokensQueue |> ofOption "[assertExpectedTokenType] Tokens queue empty."
            let! peekToken = Result.mapError (fun erMsg -> (tokensQueue, erMsg)) peekTokenResult
            return!
                match peekToken.Type with
                | tokenType when tokenType = expectedTokenType -> Ok ()
                | tokenType ->
                    let errorMsg = $"Expected a token type of \"{TokenType.ToCaseString expectedTokenType}\", got \"{TokenType.ToCaseString tokenType}\""
                    Error (processQueue tokensQueue, errorMsg)
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
                | Ok (newTokensQueue, Some statement) -> // something was parsed
                    ParserState.replaceQueueAndAddStatement newTokensQueue statement
                | Ok (newTokensQueue, None) -> // nothing was parsed, no errors as well
                    ParserState.replaceQueue newTokensQueue
                | Error (newTokensQueue, errorMsg) -> // nothing was parsed, errors occurred
                    ParserState.replaceQueueAndAddError newTokensQueue errorMsg
               
            parseProgramHelper (updateParserState parserState)
        | _ ->
            parserState
            
    and private tryParseStatement peekToken tokensQueue
        : Result<Token Queue * Statement Option, Token Queue * string> =
        let transform = Result.map (fun (tokQueue, statement) -> tokQueue, Some statement) 
            
        match peekToken.Type with
        | LET ->
            tryParseLetStatement tokensQueue |> transform
        | RETURN ->
            tryParseReturnStatement tokensQueue |> transform
        | SEMICOLON | EOF ->
            let newTokensQueue = Queue.removeTop tokensQueue  // we know that there is at least one element
            Ok (newTokensQueue, None)
        | _ ->
            tryParseExpressionStatement tokensQueue |> transform
            
    and private tryParseExpression tokensQueue precedence
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let! peekToken = queuePeekToken "[tryParseExpression] Tokens queue empty." tokensQueue
            
            let noFuncErrMsg = $"No prefix parse function for \"{peekToken.Type}\" found."
            let prefixParseFuncResult = Map.tryFind peekToken.Type prefixParseFunctionsMap |> ofOption noFuncErrMsg
            let! prefixParseFunc = Result.mapError (fun erMsg -> (consumeQueueUntilSemicolon tokensQueue, erMsg)) prefixParseFuncResult
            
            let! newTokensQueue, leftExpression = prefixParseFunc tokensQueue
            return! tryParseExpressionHelper newTokensQueue precedence leftExpression
        }
        
    and private tryParseExpressionHelper tokensQueue precedence leftExpr
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let peekTokenResult = Queue.peek tokensQueue |> ofOption "[tryParseExpressionHelper] Tokens queue empty."
            let! peekToken = Result.mapError (fun erMsg -> (tokensQueue, erMsg)) peekTokenResult
            let peekPrecedence = Precedence.peekPrecedence tokensQueue
            
            if peekToken.Type <> TokenType.SEMICOLON && precedence < peekPrecedence then
                let noFuncErrMsg = $"No prefix infix function for \"{peekToken.Type}\" found."
                let infixParseFuncResult = Map.tryFind peekToken.Type infixParseFunctionsMap |> ofOption noFuncErrMsg 
                let! infixParseFunc = Result.mapError (fun erMsg -> (consumeQueueUntilSemicolon tokensQueue, erMsg)) infixParseFuncResult
                
                let! newTokensQueue, infixExpr = infixParseFunc tokensQueue leftExpr
                return! tryParseExpressionHelper newTokensQueue precedence infixExpr
                // return! tryParseExpressionHelper newTokensQueue peekPrecedence infixExpr
                // learn why above ^ does not work
            else
                return tokensQueue, leftExpr
        }
        
        
    (* Parsing Statements *)
    
    and private tryParseLetStatement (tokensQueue: Token Queue)
        : Result<Token Queue * Statement, Token Queue * string> =
        result {
            let dequeueErMsg = "[tryParseLetStatement] Tokens queue empty."
            let! newTokensQueue, letStatementToken = dequeueToken dequeueErMsg tokensQueue
            
            do! assertExpectedTokenType consumeQueueUntilSemicolon TokenType.IDENT newTokensQueue 
            let! newTokensQueue, identifierToken = dequeueToken dequeueErMsg newTokensQueue
            let identifier: Identifier = { Token = identifierToken; Value = identifierToken.Literal }
            
            do! assertExpectedTokenType consumeQueueUntilSemicolon TokenType.ASSIGN newTokensQueue
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
            let dequeueErMsg = "[tryParseReturnStatement] Tokens queue empty."
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
            let peekTokenResult = Queue.peek tokensQueue |> ofOption "[tryParseExpressionStatement] Tokens queue empty."
            let! peekToken = Result.mapError (fun erMsg -> (tokensQueue, erMsg)) peekTokenResult
            
            let! newTokensQueue, expr = tryParseExpression tokensQueue Precedence.LOWEST
            let statement = Statement.ExpressionStatement { Token = peekToken; Expression = expr }
            return newTokensQueue, statement 
        }
        
        
    (* Pratt Parsing Stuff *)
    
    let private tryParseIdentifier (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken "[tryParseIdentifier] Tokens queue empty." tokensQueue
            let expression = Expression.Identifier { Token = dequeuedToken; Value = dequeuedToken.Literal }
            return newTokensQueue, expression
        }
        
    let private tryParseIntegerLiteral (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken "[tryParseIntegerLiteral] Tokens queue empty." tokensQueue
            let expressionResult =
                match Int64.TryParse(dequeuedToken.Literal) with
                | true, result -> Ok (Expression.IntegerLiteral { Token = dequeuedToken; Value = result })
                | false, _ -> Error $"Could not parse \"{dequeuedToken.Literal}\" as an Int64"
            let! expression = Result.mapError (fun erMsg -> (newTokensQueue, erMsg)) expressionResult
            return newTokensQueue, expression
        }
           
    let private tryParsePrefixExpression (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken "[tryParsePrefixExpression] Tokens queue empty." tokensQueue
            let! newTokensQueue, rightExpr = tryParseExpression newTokensQueue Precedence.PREFIX
            
            let prefixExpr = Expression.PrefixExpression { Token = dequeuedToken; Operator = dequeuedToken.Literal; Right = rightExpr }
            return newTokensQueue, prefixExpr
        }
        
    let private tryParseBooleanLiteral (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken "[tryParseBooleanLiteral] Tokens queue empty." tokensQueue
            let! booleanValue =
                match dequeuedToken.Type with
                | TRUE -> Ok true
                | FALSE -> Ok false
                | _ ->
                    let errorMsg = $"[tryParseBooleanLiteral] Expected a true/false token, got {TokenType.ToCaseString dequeuedToken.Type}"
                    Error (newTokensQueue, errorMsg)
                    
            let booleanLiteral = Expression.BooleanLiteral { Token = dequeuedToken; Value = booleanValue }
            return newTokensQueue, booleanLiteral
        }
        
    
        
    let private prefixParseFunctionsMap = Map.ofList [
        (TokenType.IDENT, tryParseIdentifier)
        (TokenType.INT, tryParseIntegerLiteral)
        (TokenType.BANG, tryParsePrefixExpression)
        (TokenType.MINUS, tryParsePrefixExpression)
        (TokenType.TRUE, tryParseBooleanLiteral)
        (TokenType.FALSE, tryParseBooleanLiteral)
    ]
    
    let private tryParseInfixExpression (tokensQueue: Token Queue) (leftExpr: Expression)
        : Result<Token Queue * Expression, Token Queue * string> =
        result {
            let precedence = Precedence.peekPrecedence tokensQueue
            
            let! newTokensQueue, dequeuedToken = dequeueToken "[tryParseInfixExpression] Tokens queue empty." tokensQueue
            let! newTokensQueue, rightExpr = tryParseExpression newTokensQueue precedence
            
            let infixExpr = Expression.InfixExpression { Token = dequeuedToken; Operator = dequeuedToken.Literal
                                                         Left = leftExpr; Right = rightExpr }
            return newTokensQueue, infixExpr
        }
           
    let private infixParseFunctionsMap = Map.ofList [
        (TokenType.PLUS, tryParseInfixExpression)
        (TokenType.MINUS, tryParseInfixExpression)
        (TokenType.SLASH, tryParseInfixExpression)
        (TokenType.ASTERISK, tryParseInfixExpression)
        (TokenType.EQ, tryParseInfixExpression)
        (TokenType.NOT_EQ, tryParseInfixExpression)
        (TokenType.LT, tryParseInfixExpression)
        (TokenType.GT, tryParseInfixExpression)
    ]
    