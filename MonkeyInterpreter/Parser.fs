namespace MonkeyInterpreter

open System
open System.Diagnostics
open FsToolkit.ErrorHandling

open Microsoft.FSharp.Core
open MonkeyInterpreter.Helpers.Queue
    

type internal ParserState =
    { TokensQueue: Token Queue
      Errors: string list
      Statements: Statement list }
with
    static member CreateEmpty tokensQueue =
        { TokensQueue = tokensQueue; Errors = []; Statements = [] }
        
        
        
type internal Precedence =
    | LOWEST = 1
    | EQUALS = 2
    | LESSGREATER = 3
    | SUM = 4
    | PRODUCT = 5
    | PREFIX = 6
    | CALL = 7
    
module private Precedence =     
    let tokenTypeToPrecedenceMap = Map.ofList [
        (EQ, Precedence.EQUALS)
        (NOT_EQ, Precedence.EQUALS)
        (LT, Precedence.LESSGREATER)
        (GT, Precedence.LESSGREATER)
        (PLUS, Precedence.SUM)
        (MINUS, Precedence.SUM)
        (SLASH, Precedence.PRODUCT)
        (ASTERISK, Precedence.PRODUCT)
        (LPAREN, Precedence.CALL)
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
    let private emptyQueueErrorMsg =
        let stackTrace = StackTrace()
        let frame = stackTrace.GetFrame(2)  // this func is called by other functions in this module, so we go two 'levels' up 
        let callerName = frame.GetMethod().Name
        $"[Fatal @ \"{callerName}\"] Tokens queue empty. This indicates a logical error in the parsing process."
        
    let getInvalidTokenTypeMsg (expectedTokenType: TokenType) (tokensQueue: Token Queue) =
        match Queue.peek tokensQueue with
        | Some token -> $"Expected a token type of \"{TokenType.ToCaseString expectedTokenType}\", got \"{TokenType.ToCaseString token.Type}\""
        | _ -> $"Expected a token type of \"{TokenType.ToCaseString expectedTokenType}\", got nothing." 
    
    let encapsulateIntoCase (transform: 'a -> Statement) =
        Result.map (fun (tokQueue, statement) -> tokQueue, Some (transform statement))
    
    let queuePeekToken (tokensQueue: Token Queue) =
        match (Queue.peek tokensQueue) with
        | Some peekToken -> Ok peekToken 
        | None -> Error (tokensQueue, [ emptyQueueErrorMsg ])
    
    let dequeueToken (tokensQueue: Token Queue) =
        let newTokensQueue, dequeuedTokenResult = Queue.resultDequeue "" tokensQueue
        dequeuedTokenResult
        |> Result.map (fun token -> (newTokensQueue, token))
        |> Result.mapError (fun _ -> (newTokensQueue, [ emptyQueueErrorMsg ]))
        
    let rec consumeUntilSemicolon (tokensQueue: Token Queue) =
        match (Queue.peek tokensQueue) with
        | None -> tokensQueue 
        | Some token ->
            match token.Type with
            | tokenType when tokenType = SEMICOLON || tokenType = EOF -> tokensQueue
            | _ -> consumeUntilSemicolon (Queue.removeTop tokensQueue)
        
        
    let rec tryGetFromMap (map: Map<TokenType, 'a>) (onMissingValue: unit -> 'b) (tokenType: TokenType) =
        match Map.tryFind tokenType map with
        | Some value -> Ok value
        | None -> Error (onMissingValue())
        
    and private getOnMissingValueFunc tokensQueue = (fun () -> consumeUntilSemicolon tokensQueue)
        
    /// Attempts to get the PREFIX parse function based on the next token's type
    and tryGetPrefixParseFunc prefixParseFuncMap tokensQueue =
        match queuePeekToken tokensQueue with
        | Ok token ->
            let noFuncErrMsg = $"No prefix parse function for \"{token.Type}\" found."
            tryGetFromMap prefixParseFuncMap (getOnMissingValueFunc tokensQueue) token.Type
            |> Result.mapError (fun tokQueue -> tokQueue, [ noFuncErrMsg ])
        | Error error -> Error error
        
    /// Attempts to get the INFIX parse function based on the next token's type
    and tryGetInfixParseFunc infixParseFuncMap tokensQueue =
        match queuePeekToken tokensQueue with
        | Ok token ->
            let noFuncErrMsg = $"No infix parse function for \"{token.Type}\" found."
            tryGetFromMap infixParseFuncMap (getOnMissingValueFunc tokensQueue) token.Type
            |> Result.mapError (fun tokQueue -> tokQueue, [ noFuncErrMsg ])
        | Error error -> Error error
            
            
    let rec peekTokenAndExecute onMatch onMismatch onEmpty expectedTokenType tokensQueue =
        match Queue.peek tokensQueue with
        | Some token when token.Type = expectedTokenType ->
            Ok (onMatch tokensQueue)
        | Some _ ->
            Error (onMismatch expectedTokenType tokensQueue)
        | _ ->
            Error (onEmpty tokensQueue)
        
    /// Checks for the type of the next token in queue. If there is mismatch, consume queue until EOF or a semicolon
    let assertNextTokenIsOfType expectedTokenType tokensQueue =
        let unit _ = ()
        let onTypeMismatch expectedTokenType tokensQueue = consumeUntilSemicolon tokensQueue, [ getInvalidTokenTypeMsg expectedTokenType tokensQueue ]
        let onEmptyQueue tokensQueue = tokensQueue, [ emptyQueueErrorMsg ]
        peekTokenAndExecute unit onTypeMismatch onEmptyQueue expectedTokenType tokensQueue
        
        
        
module rec Parser =
    let rec parseProgram (input: string) : Program =
        let tokens = input |> Lexer.parseIntoTokens |> List.rev 
        let tokensQueue = Queue.enqueueList Queue.empty tokens
        
        let statements, errors = parseProgramHelper tokensQueue [] [] 
        let program = { Statements = statements; Errors = errors }
        program
        
    and internal parseProgramHelper tokensQueue statements errors =
        let peekTokenOption = Queue.peek tokensQueue 
        match peekTokenOption with
        | Some peekToken when peekToken.Type <> TokenType.EOF ->
            match (tryParseStatement peekToken tokensQueue) with
            | Ok (newTokensQueue, Some newStatement) -> parseProgramHelper newTokensQueue (newStatement :: statements) errors
            | Ok (newTokensQueue, None) -> parseProgramHelper newTokensQueue statements errors
            | Error (newTokensQueue, newErrorMsgs) -> parseProgramHelper newTokensQueue statements (errors @ newErrorMsgs) 
        | _ ->
            List.rev statements, List.rev errors
            
    let internal tryParseStatement peekToken tokensQueue =
        match peekToken.Type with
        | LET ->
            tokensQueue |> tryParseLetStatement |> encapsulateIntoCase Statement.LetStatement
        | RETURN ->
            tokensQueue |> tryParseReturnStatement |> encapsulateIntoCase Statement.ReturnStatement 
        | SEMICOLON | EOF ->
            let newTokensQueue = Queue.removeTop tokensQueue  // we know that there is at least one element
            Ok (newTokensQueue, None)
        | _ ->
            tokensQueue |> tryParseExpressionStatement |> encapsulateIntoCase Statement.ExpressionStatement
            
    let rec internal tryParseExpression tokensQueue precedence
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let! prefixParseFunc = tryGetPrefixParseFunc prefixParseFunctionsMap tokensQueue
            let! newTokensQueue, leftExpression = prefixParseFunc tokensQueue
            return! tryParseExpressionHelper newTokensQueue precedence leftExpression
        }
        
    and internal tryParseExpressionHelper tokensQueue precedence leftExpr
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let! peekToken = queuePeekToken tokensQueue
            let peekPrecedence = Precedence.peekPrecedence tokensQueue
            
            if peekToken.Type <> TokenType.SEMICOLON && precedence < peekPrecedence then
                let! infixParseFunc = tryGetInfixParseFunc infixParseFunctionsMap tokensQueue
                let! newTokensQueue, infixExpr = infixParseFunc tokensQueue leftExpr
                return! tryParseExpressionHelper newTokensQueue precedence infixExpr
            else
                return tokensQueue, leftExpr
        }
        
    let internal tryParseLetStatement (tokensQueue: Token Queue) =
        result {
            let! newTokensQueue, letStatementToken = dequeueToken tokensQueue
            
            do! assertNextTokenIsOfType IDENT newTokensQueue
            let! newTokensQueue, identifierToken = dequeueToken newTokensQueue
            let identifier: Identifier = { Token = identifierToken; Value = identifierToken.Literal }
            
            do! assertNextTokenIsOfType ASSIGN newTokensQueue
            let! newTokensQueue, _ = dequeueToken newTokensQueue  // we dont need the '=' token
            
            let! newTokensQueue, expression = tryParseExpression newTokensQueue Precedence.LOWEST
            let newTokensQueue = consumeUntilSemicolon newTokensQueue |> Queue.removeTop
            
            let letStatement = { Token = letStatementToken; Name = identifier; Value = expression }
            return newTokensQueue, letStatement
        }
        
    let internal tryParseReturnStatement (tokensQueue: Token Queue) =
        result {
            let! newTokensQueue, returnStatementToken = dequeueToken tokensQueue
            
            let! newTokensQueue, expression = tryParseExpression newTokensQueue Precedence.LOWEST
            let newTokensQueue = consumeUntilSemicolon newTokensQueue |> Queue.removeTop
            
            let returnStatement = { Token = returnStatementToken; ReturnValue = expression }
            return newTokensQueue, returnStatement
        }
        
    let internal tryParseExpressionStatement (tokensQueue: Token Queue) =
        result {
            let! peekToken = queuePeekToken tokensQueue
            let! newTokensQueue, expr = tryParseExpression tokensQueue Precedence.LOWEST
            let expressionStatement = { Token = peekToken; Expression = expr }
            return newTokensQueue, expressionStatement 
        }
        
    // Note, for the sake of simplicity, assume a block statement has the following format:
    // { ... statements ... }
        
    let internal tryParseBlockStatement (stopCondition: Token -> bool) (initialTokensQueue: Token Queue) =
        
        let rec helper tokensQueue statements errors =  // mirror of 'parseProgramHelper' with some changes
            let peekTokenOption = Queue.peek tokensQueue
            match peekTokenOption with
            | Some peekToken when peekToken.Type <> EOF && stopCondition peekToken = false ->
                match (tryParseStatement peekToken tokensQueue) with
                | Ok (newTokensQueue, Some statement) -> helper newTokensQueue (statement :: statements) errors
                | Ok (newTokensQueue, None) -> helper newTokensQueue statements errors
                | Error (newTokensQueue, errorMsg) -> helper newTokensQueue statements (errorMsg @ errors)
            | _ ->
                if errors.Length = 0
                then Ok (tokensQueue, statements)
                else Error (tokensQueue, errors)
                
        result {
            let! peekToken = queuePeekToken initialTokensQueue
            let! newTokensQueue, statements = helper initialTokensQueue [] []
            return (newTokensQueue, { Token = peekToken; Statements = List.rev statements })
        }
        
    (* Pratt Parsing Stuff *)
    
    let internal tryParseIdentifier (tokensQueue: Token Queue) =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken tokensQueue
            let expression = Expression.Identifier { Token = dequeuedToken; Value = dequeuedToken.Literal }
            return newTokensQueue, expression
        }
        
    let internal tryParseIntegerLiteral (tokensQueue: Token Queue) =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken tokensQueue
            let! expression =
                match Int64.TryParse(dequeuedToken.Literal) with
                | true, result -> Ok (Expression.IntegerLiteral { Token = dequeuedToken; Value = result })
                | false, _ -> Error (newTokensQueue, [ $"Could not parse \"{dequeuedToken.Literal}\" as an Int64" ])
            return newTokensQueue, expression
        }
           
    let internal tryParsePrefixExpression (tokensQueue: Token Queue) =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken tokensQueue
            let! newTokensQueue, rightExpr = tryParseExpression newTokensQueue Precedence.PREFIX
            
            let prefixExpr = Expression.PrefixExpression { Token = dequeuedToken; Operator = dequeuedToken.Literal; Right = rightExpr }
            return newTokensQueue, prefixExpr
        }
        
    let internal tryParseBooleanLiteral (tokensQueue: Token Queue) =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken tokensQueue
            let! booleanValue =
                match dequeuedToken.Type with
                | TRUE -> Ok true
                | FALSE -> Ok false
                | _ ->
                    let errorMsg = $"[tryParseBooleanLiteral] Expected a true/false token, got {TokenType.ToCaseString dequeuedToken.Type}"
                    Error (newTokensQueue, [ errorMsg ])
                    
            let booleanLiteral = Expression.BooleanLiteral { Token = dequeuedToken; Value = booleanValue }
            return newTokensQueue, booleanLiteral
        }
        
    let internal tryParseGroupedExpression (tokensQueue: Token Queue) =
        result {
            let newTokensQueue = Queue.removeTop tokensQueue  // consume the left paren
            let! newTokensQueue, expr = tryParseExpression newTokensQueue Precedence.LOWEST
            
            do! assertNextTokenIsOfType RPAREN newTokensQueue
            
            let newTokensQueue = Queue.removeTop newTokensQueue  // consume the right paren
            return newTokensQueue, expr
        }
        
    let rec internal tryParseIfExpression (tokensQueue: Token Queue) =
        result {
            let! newTokensQueue, ifStatementToken = dequeueToken tokensQueue
            
            // Parsing the condition
            do! assertNextTokenIsOfType LPAREN newTokensQueue
            let newTokensQueue = Queue.removeTop newTokensQueue
            
            let! newTokensQueue, condition = tryParseExpression newTokensQueue Precedence.LOWEST
            
            do! assertNextTokenIsOfType RPAREN newTokensQueue
            let newTokensQueue = Queue.removeTop newTokensQueue
            
            // Parsing the consequence
            let stopCondition token = token.Type = RBRACE
            do! assertNextTokenIsOfType LBRACE newTokensQueue
            let newTokensQueue = Queue.removeTop newTokensQueue
            
            let! newTokensQueue, consequenceBlocksStatement = tryParseBlockStatement stopCondition newTokensQueue
            
            do! assertNextTokenIsOfType RBRACE newTokensQueue
            let newTokensQueue = Queue.removeTop newTokensQueue
            
            // Parsing the alternative, if any
            let! newTokensQueue, alternativeBlockStatementOption = tryParseAlternativeBlockStatement stopCondition newTokensQueue
            let ifExpression = { Token = ifStatementToken; Condition = condition
                                 Consequence = consequenceBlocksStatement; Alternative = alternativeBlockStatementOption } 
            return newTokensQueue, Expression.IfExpression ifExpression 
        }
        
    and private tryParseAlternativeBlockStatement stopCondition tokensQueue =
        match (Queue.peek tokensQueue) with
        | Some peekToken when peekToken.Type = ELSE ->
            result {
                let newTokensQueue = Queue.removeTop tokensQueue // to consume the 'else' token
                
                do! assertNextTokenIsOfType LBRACE newTokensQueue
                let newTokensQueue = Queue.removeTop newTokensQueue 
                
                let! newTokensQueue, consequenceBlocksStatement = tryParseBlockStatement stopCondition newTokensQueue
                
                do! assertNextTokenIsOfType RBRACE newTokensQueue
                let newTokensQueue = Queue.removeTop newTokensQueue
                return newTokensQueue, Some consequenceBlocksStatement
            }
        | _ -> Ok (tokensQueue, None)
        
    let rec internal tryParseFunctionLiteral (tokensQueue: Token Queue) = 
        result {
            let! newTokensQueue, functionLiteralToken = dequeueToken tokensQueue
            
            do! assertNextTokenIsOfType LPAREN newTokensQueue
            let newTokensQueue = Queue.removeTop newTokensQueue
            let! newTokensQueue, identifiersList = tryParseFunctionParameters newTokensQueue [] // rparen consumed inside 'tryParseFunctionParameters'
            
            // parsing body
            let stopCondition token = token.Type = RBRACE
            do! assertNextTokenIsOfType LBRACE newTokensQueue
            let newTokensQueue = Queue.removeTop newTokensQueue
            
            let! newTokensQueue, funcBlockStatement = tryParseBlockStatement stopCondition newTokensQueue
            
            do! assertNextTokenIsOfType RBRACE newTokensQueue
            let newTokensQueue = Queue.removeTop newTokensQueue
            
            let functionLiteral: FunctionLiteral = { Token = functionLiteralToken; Parameters = identifiersList; Body = funcBlockStatement }
            return (newTokensQueue, Expression.FunctionLiteral functionLiteral) 
        }
        
    and tryParseFunctionParameters (tokensQueue: Token Queue) (identifiers: Identifier list) =
        result {
            match dequeueToken tokensQueue with
            | Ok (newTokensQueue, dequeuedToken) when dequeuedToken.Type = IDENT ->
                // Case for function with >= 1 parameters
                let identifier: Identifier = { Token = dequeuedToken; Value = dequeuedToken.Literal }
                let! newTokensQueue, dequeuedToken = dequeueToken newTokensQueue 
                match dequeuedToken.Type with
                | tokType when tokType = COMMA ->
                    return! tryParseFunctionParameters newTokensQueue (identifier :: identifiers)
                | tokType when tokType = RPAREN ->
                    return! Ok (newTokensQueue, List.rev (identifier :: identifiers))
                | _ ->
                    let errorMsg = $"[tryParseFunctionParameters] Expected a semicolon or right paren, got {TokenType.ToCaseString dequeuedToken.Type}"
                    return! Error (newTokensQueue, [ errorMsg ] )
                    
            | Ok (newTokensQueue, dequeuedToken) when dequeuedToken.Type = RPAREN ->
                // Case for function with 0 parameters
                return! Ok (newTokensQueue, [])
                
            | Ok (newTokensQueue, dequeuedToken) ->
                let errorMsg = $"Expected a token type of \"IDENT\" or \"RPAREN\", got \"{TokenType.ToCaseString dequeuedToken.Type}\""
                return! Error (newTokensQueue, [ errorMsg ])
                
            | Error dequeueError ->
                return! Error dequeueError
        }
        
    let internal prefixParseFunctionsMap
        : Map<TokenType, Token Queue -> Result<Token Queue * Expression, Token Queue * string list>> =
        Map.ofList [
            (TokenType.IDENT, tryParseIdentifier)
            (TokenType.INT, tryParseIntegerLiteral)
            (TokenType.BANG, tryParsePrefixExpression)
            (TokenType.MINUS, tryParsePrefixExpression)
            (TokenType.TRUE, tryParseBooleanLiteral)
            (TokenType.FALSE, tryParseBooleanLiteral)
            (TokenType.LPAREN, tryParseGroupedExpression)
            (TokenType.IF, tryParseIfExpression)
            (TokenType.FUNCTION, tryParseFunctionLiteral)
        ]
    
    let internal tryParseInfixExpression (tokensQueue: Token Queue) (leftExpr: Expression) = 
        result {
            let precedence = Precedence.peekPrecedence tokensQueue
            
            let! newTokensQueue, dequeuedToken = dequeueToken tokensQueue
            let! newTokensQueue, rightExpr = tryParseExpression newTokensQueue precedence
            
            let infixExpr = Expression.InfixExpression { Token = dequeuedToken; Operator = dequeuedToken.Literal
                                                         Left = leftExpr; Right = rightExpr }
            return newTokensQueue, infixExpr
        }
        
    let rec internal tryParseCallExpression (tokensQueue: Token Queue) (leftExpr: Expression) =
        let validateLeftExpr _leftExpr =
            match CallExpr.FromExpression _leftExpr with
            | Some callExpr -> Ok callExpr 
            | None -> Error $"Left expr expected to be either \"Identifier\" or \"FunctionLiteral\", got {_leftExpr.GetType()}"
        
        result {
            let boxErrorMsgAlt errorMsg = (tokensQueue, [ errorMsg ])
            let! callExprFunc = validateLeftExpr leftExpr |> Result.mapError boxErrorMsgAlt
            
            let! newTokensQueue, dequeuedToken = dequeueToken tokensQueue
            
            let! newTokensQueue, callArguments = tryParseCallArguments newTokensQueue []
            let callExpression = { Token = dequeuedToken; Function = callExprFunc; Arguments = callArguments }
            return (newTokensQueue, Expression.CallExpression callExpression)
        }
        
    and private tryParseCallArguments (tokensQueue: Token Queue) (arguments: Expression list) =
        // after parsing the expression, the next valid tokens are COMMA or RPAREN
        let assertNextTokenType (_tokensQueue, expression) =
            match (queuePeekToken _tokensQueue) with
            | Ok peekToken when peekToken.Type = COMMA -> Ok (Queue.removeTop _tokensQueue, expression)
            | Ok peekToken when peekToken.Type = RPAREN -> Ok (_tokensQueue, expression)
            | Ok peekToken -> Error (_tokensQueue, [ $"Expected token \"COMMA\" or \"RPAREN\" after expression, got {TokenType.ToCaseString peekToken.Type}" ])
            | Error errorValue -> Error errorValue
            
        match (queuePeekToken tokensQueue) with
        | Ok peekToken when peekToken.Type = RPAREN ->
            Ok (Queue.removeTop tokensQueue, List.rev arguments)
        | Ok _ ->
            result {
                let! newTokensQueue, expr = tryParseExpression tokensQueue Precedence.LOWEST |> Result.bind assertNextTokenType
                let updatedArguments = expr :: arguments
                return! tryParseCallArguments newTokensQueue updatedArguments
            }
        | Error err ->
            Error err
            
    let internal infixParseFunctionsMap
        : Map<TokenType, Token Queue -> Expression -> Result<Token Queue * Expression, Token Queue * string list>> =
        Map.ofList [
            (TokenType.LPAREN, tryParseCallExpression) // parse call expr
            (TokenType.PLUS, tryParseInfixExpression)
            (TokenType.MINUS, tryParseInfixExpression)
            (TokenType.SLASH, tryParseInfixExpression)
            (TokenType.ASTERISK, tryParseInfixExpression)
            (TokenType.EQ, tryParseInfixExpression)
            (TokenType.NOT_EQ, tryParseInfixExpression)
            (TokenType.LT, tryParseInfixExpression)
            (TokenType.GT, tryParseInfixExpression)
        ]
    