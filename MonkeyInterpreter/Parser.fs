namespace MonkeyInterpreter

open System
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
    let internal boxErrorMsg (tokensQueue, errorMsg) = tokensQueue, [ errorMsg ]
    
    let ofOption (defaultValue: 'b) (input: 'a Option) : Result<'a, 'b> =
        match input with
        | Some value -> Ok value 
        | None -> Error defaultValue
        
    let queuePeekToken (errorMsg: string) (tokensQueue: Token Queue) =
        match (Queue.peek tokensQueue) with
        | Some peekToken -> Ok peekToken 
        | None -> Error (tokensQueue, errorMsg)
        |> Result.mapError boxErrorMsg
    
    let dequeueToken (errorMsg: string) (tokensQueue: Token Queue) =
        let newTokensQueue, dequeuedTokenResult = Queue.resultDequeue errorMsg tokensQueue
        dequeuedTokenResult
        |> Result.map (fun token -> (newTokensQueue, token))
        |> Result.mapError (fun erMsg -> (newTokensQueue, erMsg))
        |> Result.mapError boxErrorMsg
        
    let isNextTokenOfType (expectedTokenType: TokenType) (tokensQueue: Token Queue) =
        match Queue.peek tokensQueue with
        | Some token when token.Type = expectedTokenType -> true
        | _ -> false
        
    let getInvalidTokenTypeMsg (expectedTokenType: TokenType) (tokensQueue: Token Queue) =
        match Queue.peek tokensQueue with
        | Some token -> $"Expected a token type of \"{TokenType.ToCaseString expectedTokenType}\", got \"{TokenType.ToCaseString token.Type}\""
        | _ -> $"Expected a token type of \"{TokenType.ToCaseString expectedTokenType}\", got nothing." 
        
    let rec consumeUntilSemicolon (tokensQueue: Token Queue) =
        match (Queue.peek tokensQueue) with
        | None -> tokensQueue 
        | Some token ->
            match token.Type with
            | tokenType when tokenType = SEMICOLON || tokenType = EOF -> tokensQueue
            | _ -> consumeUntilSemicolon (Queue.removeTop tokensQueue)
        
        
        
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
            | Ok (newTokensQueue, Some newStatement) ->
                parseProgramHelper newTokensQueue (newStatement :: statements) errors
            | Ok (newTokensQueue, None) ->
                parseProgramHelper newTokensQueue statements errors
            | Error (newTokensQueue, newErrorMsgs) ->
                parseProgramHelper newTokensQueue statements (errors @ newErrorMsgs) 
        | _ ->
            List.rev statements, List.rev errors
            
    and internal tryParseStatement peekToken tokensQueue
        : Result<Token Queue * Statement Option, Token Queue * string list> =
            
        let encapsulateIntoCase (transform: 'a -> Statement) =
            Result.map (fun (tokQueue, statement) -> tokQueue, Some (transform statement))
            
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
            
    and internal tryParseExpression tokensQueue precedence
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let! peekToken = queuePeekToken "[tryParseExpression] Tokens queue empty." tokensQueue
            
            let noFuncErrMsg = $"No prefix parse function for \"{peekToken.Type}\" found."
            let prefixParseFuncResult = Map.tryFind peekToken.Type prefixParseFunctionsMap |> ofOption noFuncErrMsg
            let! prefixParseFunc = Result.mapError (fun erMsg -> (consumeUntilSemicolon tokensQueue, [ erMsg ])) prefixParseFuncResult
            
            let! newTokensQueue, leftExpression = prefixParseFunc tokensQueue
            return! tryParseExpressionHelper newTokensQueue precedence leftExpression
        }
        
    and internal tryParseExpressionHelper tokensQueue precedence leftExpr
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let peekTokenResult = Queue.peek tokensQueue |> ofOption "[tryParseExpressionHelper] Tokens queue empty."
            let! peekToken = Result.mapError (fun erMsg -> (tokensQueue, [ erMsg ])) peekTokenResult
            let peekPrecedence = Precedence.peekPrecedence tokensQueue
            
            if peekToken.Type <> TokenType.SEMICOLON && precedence < peekPrecedence then
                let noFuncErrMsg = $"No prefix infix function for \"{peekToken.Type}\" found."
                let infixParseFuncResult = Map.tryFind peekToken.Type infixParseFunctionsMap |> ofOption noFuncErrMsg 
                let! infixParseFunc = Result.mapError (fun erMsg -> (consumeUntilSemicolon tokensQueue, [ erMsg] )) infixParseFuncResult
                
                let! newTokensQueue, infixExpr = infixParseFunc tokensQueue leftExpr
                return! tryParseExpressionHelper newTokensQueue precedence infixExpr
            else
                return tokensQueue, leftExpr
        }
        
    (* Parsing Statements *)
    
    let internal tryParseLetStatement (tokensQueue: Token Queue)
        : Result<Token Queue * LetStatement, Token Queue * string list> =
        result {
            let dequeueErMsg = "[tryParseLetStatement] Tokens queue empty."
            let! newTokensQueue, letStatementToken = dequeueToken dequeueErMsg tokensQueue
            
            do! if isNextTokenOfType IDENT newTokensQueue
                then Ok()
                else Error (consumeUntilSemicolon newTokensQueue, [ getInvalidTokenTypeMsg IDENT newTokensQueue ])
            let! newTokensQueue, identifierToken = dequeueToken dequeueErMsg newTokensQueue
            let identifier: Identifier = { Token = identifierToken; Value = identifierToken.Literal }
            
            do! if isNextTokenOfType ASSIGN newTokensQueue
                then Ok()
                else Error (consumeUntilSemicolon newTokensQueue, [ getInvalidTokenTypeMsg ASSIGN newTokensQueue ])
            let! newTokensQueue, _ = dequeueToken dequeueErMsg newTokensQueue
            
            let newTokensQueue = consumeUntilSemicolon newTokensQueue |> Queue.removeTop
            
            // TODO: We're skipping parsing the expression for now
            let placeholderExpression = Expression.StringLiteral { Token = letStatementToken; Value = "" }
            let letStatement = { Token = letStatementToken; Name = identifier; Value = placeholderExpression }
            return newTokensQueue, letStatement
        }
        
    let internal tryParseReturnStatement (tokensQueue: Token Queue)
        : Result<Token Queue * ReturnStatement, Token Queue * string list> =
        result {
            let dequeueErMsg = "[tryParseReturnStatement] Tokens queue empty."
            let! newTokensQueue, returnStatementToken = dequeueToken dequeueErMsg tokensQueue
            
            let newTokensQueue = consumeUntilSemicolon newTokensQueue |> Queue.removeTop
            
            // TODO: We're skipping parsing the expression for now
            let placeholderExpression = Expression.StringLiteral { Token = returnStatementToken; Value = "" }
            let returnStatement = { Token = returnStatementToken; ReturnValue = placeholderExpression }
            return newTokensQueue, returnStatement
        }
        
    let internal tryParseExpressionStatement (tokensQueue: Token Queue)
        : Result<Token Queue * ExpressionStatement, Token Queue * string list> =
        result {
            let peekTokenResult = Queue.peek tokensQueue |> ofOption "[tryParseExpressionStatement] Tokens queue empty."
            let! peekToken = Result.mapError (fun erMsg -> (tokensQueue, [ erMsg ])) peekTokenResult
            
            let! newTokensQueue, expr = tryParseExpression tokensQueue Precedence.LOWEST
            let expressionStatement = { Token = peekToken; Expression = expr }
            return newTokensQueue, expressionStatement 
        }
        
    // Note, for the sake of simplicity, assume a block statement has the following format:
    // { ... statements ... }
        
    let internal tryParseBlockStatement (stopCondition: Token -> bool) (initialTokensQueue: Token Queue)
        : Result<Token Queue * BlockStatement, Token Queue * string list> =
        
        let rec helper tokensQueue statements errors =
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
            let peekTokenResult = Queue.peek initialTokensQueue |> ofOption "[tryParseBlockStatement] Tokens queue empty."
            let! peekToken = Result.mapError (fun erMsg -> (initialTokensQueue, [ erMsg ])) peekTokenResult
            
            let! newTokensQueue, statements = helper initialTokensQueue [] []
            return (newTokensQueue, { Token = peekToken; Statements = List.rev statements })
        }
        
    (* Pratt Parsing Stuff *)
    
    let internal tryParseIdentifier (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken "[tryParseIdentifier] Tokens queue empty." tokensQueue
            let expression = Expression.Identifier { Token = dequeuedToken; Value = dequeuedToken.Literal }
            return newTokensQueue, expression
        }
        
    let internal tryParseIntegerLiteral (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken "[tryParseIntegerLiteral] Tokens queue empty." tokensQueue
            let! expression =
                match Int64.TryParse(dequeuedToken.Literal) with
                | true, result -> Ok (Expression.IntegerLiteral { Token = dequeuedToken; Value = result })
                | false, _ -> Error (newTokensQueue, [ $"Could not parse \"{dequeuedToken.Literal}\" as an Int64" ])
            return newTokensQueue, expression
        }
           
    let internal tryParsePrefixExpression (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken "[tryParsePrefixExpression] Tokens queue empty." tokensQueue
            let! newTokensQueue, rightExpr = tryParseExpression newTokensQueue Precedence.PREFIX
            
            let prefixExpr = Expression.PrefixExpression { Token = dequeuedToken; Operator = dequeuedToken.Literal; Right = rightExpr }
            return newTokensQueue, prefixExpr
        }
        
    let internal tryParseBooleanLiteral (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken "[tryParseBooleanLiteral] Tokens queue empty." tokensQueue
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
        
    let internal tryParseGroupedExpression (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let newTokensQueue = Queue.removeTop tokensQueue  // consume the left paren
            let! newTokensQueue, expr = tryParseExpression newTokensQueue Precedence.LOWEST
            
            do! if isNextTokenOfType RPAREN newTokensQueue
                then Ok()
                else Error (consumeUntilSemicolon newTokensQueue, [ getInvalidTokenTypeMsg RPAREN newTokensQueue ])
            
            let newTokensQueue = Queue.removeTop newTokensQueue  // consume the right paren
            return newTokensQueue, expr
        }
        
    let rec internal tryParseIfExpression (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let dequeueErrorMsg = "[tryParseIfExpression] Tokens queue empty."
            let! newTokensQueue, ifStatementToken = dequeueToken dequeueErrorMsg tokensQueue
            
            // Parsing the condition
            do! if isNextTokenOfType LPAREN newTokensQueue
                then Ok()
                else Error (consumeUntilSemicolon newTokensQueue, [ getInvalidTokenTypeMsg LPAREN newTokensQueue ])
            let newTokensQueue = Queue.removeTop newTokensQueue
            
            let! newTokensQueue, condition = tryParseExpression newTokensQueue Precedence.LOWEST
            
            do! if isNextTokenOfType RPAREN newTokensQueue
                then Ok()
                else Error (consumeUntilSemicolon newTokensQueue, [ getInvalidTokenTypeMsg RPAREN newTokensQueue ])
            let newTokensQueue = Queue.removeTop newTokensQueue
            
            // Parsing the consequence
            let stopCondition token = token.Type = RBRACE
            do! if isNextTokenOfType LBRACE newTokensQueue
                then Ok()
                else Error (consumeUntilSemicolon newTokensQueue, [ getInvalidTokenTypeMsg LBRACE newTokensQueue ])
            let newTokensQueue = Queue.removeTop newTokensQueue
            
            let! newTokensQueue, consequenceBlocksStatement = tryParseBlockStatement stopCondition newTokensQueue
            
            do! if isNextTokenOfType RBRACE newTokensQueue
                then Ok()
                else Error (consumeUntilSemicolon newTokensQueue, [ getInvalidTokenTypeMsg RBRACE newTokensQueue ])
            let newTokensQueue = Queue.removeTop newTokensQueue
            
            // Parsing the alternative, if any
            let! newTokensQueue, alternativeBlockStatementOption = tryParseAlternativeBlockStatement stopCondition newTokensQueue
            let ifExpression = { Token = ifStatementToken; Condition = condition
                                 Consequence = consequenceBlocksStatement; Alternative = alternativeBlockStatementOption } 
            return newTokensQueue, Expression.IfExpression ifExpression 
        }
        
    and internal tryParseAlternativeBlockStatement stopCondition tokensQueue 
        : Result<Token Queue * BlockStatement Option, Token Queue * string list> =
        match (Queue.peek tokensQueue) with
        | Some peekToken when peekToken.Type = ELSE ->
            result {
                let newTokensQueue = Queue.removeTop tokensQueue // to consume the 'else' token
                
                do! if isNextTokenOfType LBRACE newTokensQueue
                    then Ok()
                    else Error (consumeUntilSemicolon newTokensQueue, [ getInvalidTokenTypeMsg LBRACE newTokensQueue ])
                let newTokensQueue = Queue.removeTop newTokensQueue 
                
                let! newTokensQueue, consequenceBlocksStatement = tryParseBlockStatement stopCondition newTokensQueue
                
                do! if isNextTokenOfType RBRACE newTokensQueue
                    then Ok()
                    else Error (consumeUntilSemicolon newTokensQueue, [ getInvalidTokenTypeMsg RBRACE newTokensQueue ])
                let newTokensQueue = Queue.removeTop newTokensQueue
                return newTokensQueue, Some consequenceBlocksStatement
            }
        | _ -> Ok (tokensQueue, None)
        
    let rec internal tryParseFunctionLiteral (tokensQueue: Token Queue)
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let dequeueErrorMsg = "[tryParseFunctionLiteral] Tokens queue empty."
            let! newTokensQueue, functionLiteralToken = dequeueToken dequeueErrorMsg tokensQueue
            
            do! if isNextTokenOfType LPAREN newTokensQueue
                then Ok()
                else Error (consumeUntilSemicolon newTokensQueue, [ getInvalidTokenTypeMsg LPAREN newTokensQueue ])
            let newTokensQueue = Queue.removeTop newTokensQueue
            let! newTokensQueue, identifiersList = tryParseFunctionParameters newTokensQueue [] // rparen consumed inside 'tryParseFunctionParameters'
            
            // parsing body
            let stopCondition token = token.Type = RBRACE
            do! if isNextTokenOfType LBRACE newTokensQueue
                then Ok()
                else Error (consumeUntilSemicolon newTokensQueue, [ getInvalidTokenTypeMsg LPAREN newTokensQueue ])
            let newTokensQueue = Queue.removeTop newTokensQueue
            
            let! newTokensQueue, funcBlockStatement = tryParseBlockStatement stopCondition newTokensQueue
            
            do! if isNextTokenOfType RBRACE newTokensQueue
                then Ok()
                else Error (consumeUntilSemicolon newTokensQueue, [ getInvalidTokenTypeMsg RBRACE newTokensQueue ])
            let newTokensQueue = Queue.removeTop newTokensQueue
            
            let functionLiteral: FunctionLiteral = { Token = functionLiteralToken; Parameters = identifiersList; Body = funcBlockStatement }
            return (newTokensQueue, Expression.FunctionLiteral functionLiteral) 
        }
        
    and tryParseFunctionParameters (tokensQueue: Token Queue) (identifiers: Identifier list)
        : Result<Token Queue * Identifier list, Token Queue * string list> =
        let dequeueErrorMsg = "[tryParseFunctionParameters] Tokens queue empty."
        result {
            let! newTokensQueue, dequeuedToken = dequeueToken dequeueErrorMsg tokensQueue
            match dequeuedToken.Type with
            | tokType when tokType = IDENT ->
                let identifier: Identifier = { Token = dequeuedToken; Value = dequeuedToken.Literal }
                let! newTokensQueue, dequeuedToken = dequeueToken dequeueErrorMsg newTokensQueue 
                match dequeuedToken.Type with
                | tokType when tokType = COMMA ->
                    return! tryParseFunctionParameters newTokensQueue (identifier :: identifiers)
                | tokType when tokType = RPAREN ->
                    return! Ok (newTokensQueue, List.rev (identifier :: identifiers))
                | _ ->
                    let errorMsg = $"[tryParseFunctionParameters] Expected a semicolon or right paren, got {TokenType.ToCaseString dequeuedToken.Type}"
                    return! Error (newTokensQueue, [ errorMsg ] )
                    
            | tokType when tokType = RPAREN ->  // rparen here means a func w no parameters
                    return! Ok (newTokensQueue, [])
                
            | _ ->
                return! Error (newTokensQueue, [ dequeueErrorMsg ] )
        }
        
    let internal prefixParseFunctionsMap = Map.ofList [
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
    
    let internal tryParseInfixExpression (tokensQueue: Token Queue) (leftExpr: Expression)
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let precedence = Precedence.peekPrecedence tokensQueue
            
            let! newTokensQueue, dequeuedToken = dequeueToken "[tryParseInfixExpression] Tokens queue empty." tokensQueue
            let! newTokensQueue, rightExpr = tryParseExpression newTokensQueue precedence
            
            let infixExpr = Expression.InfixExpression { Token = dequeuedToken; Operator = dequeuedToken.Literal
                                                         Left = leftExpr; Right = rightExpr }
            return newTokensQueue, infixExpr
        }
        
    let rec internal tryParseCallExpression (tokensQueue: Token Queue) (leftExpr: Expression)
        : Result<Token Queue * Expression, Token Queue * string list> =
        result {
            let boxErrorMsgAlt errorMsg = (tokensQueue, [ errorMsg ])
            let! callExprFunc = validateLeftExpr leftExpr |> Result.mapError boxErrorMsgAlt
            
            let! newTokensQueue, dequeuedToken = dequeueToken "[tryParseCallExpression] Tokens queue empty." tokensQueue
            
            let! newTokensQueue, callArguments = tryParseCallArguments newTokensQueue []
            let callExpression = { Token = dequeuedToken; Function = callExprFunc; Arguments = callArguments }
            return (newTokensQueue, Expression.CallExpression callExpression)
        }
        
    and private validateLeftExpr (leftExpr: Expression) =
        match CallExpr.FromExpression leftExpr with
        | Some callExpr -> Ok callExpr 
        | None -> Error $"Left expr expected to be either \"Identifier\" or \"FunctionLiteral\", got {leftExpr.GetType()}"
        
    and private tryParseCallArguments (tokensQueue: Token Queue) (arguments: Expression list) =
        let peekErrorMsg = "[tryParseCallArguments] Tokens queue empty."
        
        // after parsing the expression, the next valid tokens are COMMA or RPAREN
        let assertNextTokenType (_tokensQueue, expression) =
            match (queuePeekToken peekErrorMsg _tokensQueue) with
            | Ok peekToken when peekToken.Type = COMMA -> Ok (Queue.removeTop _tokensQueue, expression)
            | Ok peekToken when peekToken.Type = RPAREN -> Ok (_tokensQueue, expression)
            | Ok peekToken -> Error (_tokensQueue, [ $"Expected token \"COMMA\" or \"RPAREN\" after expression, got {TokenType.ToCaseString peekToken.Type}" ])
            | Error errorValue -> Error errorValue
            
        match (queuePeekToken peekErrorMsg tokensQueue) with
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
           
    let internal infixParseFunctionsMap = Map.ofList [
        (TokenType.LPAREN, tryParseCallExpression)
        
        (TokenType.PLUS, tryParseInfixExpression)
        (TokenType.MINUS, tryParseInfixExpression)
        (TokenType.SLASH, tryParseInfixExpression)
        (TokenType.ASTERISK, tryParseInfixExpression)
        (TokenType.EQ, tryParseInfixExpression)
        (TokenType.NOT_EQ, tryParseInfixExpression)
        (TokenType.LT, tryParseInfixExpression)
        (TokenType.GT, tryParseInfixExpression)
    ]
    