module rec Monkey.Frontend.CLR.Parsers.ModifiedRecursiveDescent

open Frontend.CLR.Parsers.ParseErrors
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Monkey.Frontend.CLR.Helpers
open Monkey.Frontend.CLR.Helpers.Queue
open Monkey.Frontend.CLR.Token
open FsToolkit.ErrorHandling


// TODO: DEQUEUE MEANT POP, REPLACE ALL OCCURRENCES
    
        
type internal ParserState(tokens: Token array) =
    let mutable currentIdx: int = 0
    let mutable parseErrors: ParseError list = []
    let mutable statements: StatementSyntax list = []
    
    member this.Tokens = tokens
    
    member this.CurrentIdx
        with get() = currentIdx
        and set(value) = currentIdx <- value

    member this.ParseErrors
        with get() = parseErrors
        and set(value) = parseErrors <- value
        
    member this.Statements
        with get() = statements
        and set(value) = statements <- value
    
with
    member this.TokensLength() =
        Array.length this.Tokens
        
    member this.IsEof() =
        let isEof =
            match this.Tokens[this.CurrentIdx].Type with
            | EOF -> true
            | _ -> false
        
        (this.CurrentIdx >= Array.length this.Tokens) || isEof
        
    member this.PeekToken() =
        this.Tokens[this.CurrentIdx]
        
    member this.PopToken() =
        let token = this.Tokens[this.CurrentIdx]
        this.CurrentIdx <- this.CurrentIdx + 1
        token
        
    member this.GetTokenAt(index: int) =
        this.Tokens[index]
        
        
type internal Precedence =
    | LOWEST = 1
    | EQUALS = 2
    | LESSGREATER = 3
    | SUM = 4
    | PRODUCT = 5
    | PREFIX = 6
    | CALL = 7
    | INDEX = 8
    
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
        (LBRACKET, Precedence.INDEX)
    ]
    
    let getPrecedence (token: Token) : Precedence =
        match Map.tryFind token.Type tokenTypeToPrecedenceMap with
        | Some precedence -> precedence
        | None -> Precedence.LOWEST
            
    
    let peekPrecedence (tokensQueue: Token Queue) : Precedence =
        option {
            let! peekToken = Queue.peek tokensQueue
            return! Map.tryFind peekToken.Type tokenTypeToPrecedenceMap
        }
        |> function
           | Some precedence -> precedence
           | None -> Precedence.LOWEST
           

// TODO: Re-write to use iteration once we know stuff works
let rec internal consumeUntilSemicolon (parserState: ParserState) =
    match parserState.IsEof() with
    | true ->
        parserState
    | false ->
        let token = parserState.PeekToken()
        match token.Type with
        | tokenType when tokenType = SEMICOLON || tokenType = EOF ->
            parserState
        | _ ->
            parserState.CurrentIdx <- parserState.CurrentIdx + 1
            consumeUntilSemicolon parserState
        


// TODO: Make 'SyntaxTree' out of 'StatementSyntax' list
let rec parseTokens (tokens: Token array) : Result<StatementSyntax list, ParseError list> =
    let parserState = ParserState(tokens)
    let newParserState: ParserState = parseTokensHelper parserState
    Ok newParserState.Statements
    
and internal parseTokensHelper (parserState: ParserState) =
    match parserState.IsEof() with
    | true ->
        parserState
    | false ->
        match tryParseStatement parserState with
        | Ok statementSyntaxOption ->
            match statementSyntaxOption with
            | Some statementSyntax ->
                let newStatements = statementSyntax :: parserState.Statements
                parserState.Statements <- newStatements
                parserState
            | None ->
                parserState
        | Error errorValue ->
            let newErrors = errorValue :: parserState.ParseErrors
            parserState.ParseErrors <- newErrors
            parserState
        
and internal tryParseStatement (parserState: ParserState) : Result<StatementSyntax option, ParseError> =
    let token = parserState.PeekToken()
    match token.Type with
    | LET ->
        failwith "todo"
        // tokensQueue |> tryParseLetStatement |> encapsulateIntoCase Statement.LetStatement
    | RETURN ->
        failwith "todo"
        // tokensQueue |> tryParseReturnStatement |> encapsulateIntoCase Statement.ReturnStatement 
    | SEMICOLON | EOF ->
        parserState.PopToken() |> ignore  // we know that there is at least one element
        Ok None  // no parsing happens, jus continue type shi
    | _ ->
        let mapOkType (exprStatementSyntax: ExpressionStatementSyntax) =
            let asSyntaxNode = exprStatementSyntax :> StatementSyntax
            Some asSyntaxNode
            
        parserState |> tryParseExpressionStatement |> (Result.map mapOkType)
        

(* Statement parsing *)

let internal tryParseExpressionStatement (parserState: ParserState) : Result<ExpressionStatementSyntax, ParseError> =
    result {
        // let peekToken = parserState.PeekToken()
        let! expr = tryParseExpression parserState Precedence.LOWEST
        // TODO: roslyn global statment here?
        return SyntaxFactory.ExpressionStatement(expr)
    }
    
        
(* Simple expression parsing *)     
        
/// 
let internal tryParseIdentifier (parserState: ParserState) : ExpressionSyntax =
    let currentToken = parserState.PopToken()
    SyntaxFactory.IdentifierName(currentToken.Literal)

///    
let internal tryParseIntegerLiteral (parserState: ParserState) : Result<ExpressionSyntax, ParseError> =
    result {
        let currentToken = parserState.PopToken()
        let intValueOption = tryParseInt currentToken.Literal
        let! intValue =
            match intValueOption with
            | Some value ->
                Ok value
            | None ->
                let literalParseError = LiteralExpressionParseError(SyntaxKind.NumericLiteralExpression, token=Some currentToken, innerException=IntParseError(currentToken.Literal))
                Error (ParseError(innerException=literalParseError))
                
        return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(intValue))
    }

///    
let internal tryParseBooleanLiteral (parserState: ParserState) : Result<ExpressionSyntax, ParseError> =
    result {
        let currentToken = parserState.PopToken()
        let! syntaxKind =
            match currentToken.Type with
            | TRUE ->
                Ok SyntaxKind.TrueKeyword
            | FALSE ->
                Ok SyntaxKind.FalseKeyword
            | _ ->
                let literalParseError = LiteralExpressionParseError(message=($"Failed to parse boolean literal, expected a \"bool\" type, got \"{currentToken.Type}\""), token=Some currentToken)
                Error (ParseError(innerException=literalParseError))
                
        return SyntaxFactory.LiteralExpression(syntaxKind)
    }

///    
let internal tryParseStringLiteral (parserState: ParserState) : ExpressionSyntax =
    let currentToken = parserState.PopToken()
    SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, SyntaxFactory.Literal(currentToken.Literal))
   
///  
let rec internal tryParseExpression (parserState: ParserState) (precedence: Precedence) : Result<ExpressionSyntax, ParseError> =
    result {
        let! prefixParseFunc = tryGetPrefixParseFunc prefixParseFunctionsMap parserState 
        let! leftExpression = prefixParseFunc parserState
        return leftExpression
        
        // TODO: you need to uncomment this to continue
        // return! tryParseExpressionHelper parserState precedence leftExpression
    }
    
/// Attempts to get the PREFIX parse function based on the next token's type
and internal tryGetPrefixParseFunc prefixParseFuncMap parserState : Result<ParserState -> Result<ExpressionSyntax, ParseError>, ParseError> =
    match parserState.IsEof() with
    | true ->
        let message = "Tokens queue empty. This indicates a logical error in the parsing process."
        failwith message
        // Error (ParseError(message))
    | false ->
        let onMissingValue parserState =  // i.e. basically go to the next statement
            consumeUntilSemicolon parserState |> ignore  // pass by reference, properties change 'in-place'
            ParseError()
            
        let token = parserState.PeekToken()
        match Map.tryFind token.Type prefixParseFuncMap with
        | Some value -> Ok value
        | None -> Error (onMissingValue parserState)
        
        
    
and internal tryParseExpressionHelper parserState precedence leftExpr =
    failwith "todo"
    
    (*
    result {
        let peekToken = parserState.PeekToken()
        let peekPrecedence =
            match Map.tryFind peekToken.Type Precedence.tokenTypeToPrecedenceMap with
            | Some precedence -> precedence
            | None -> Precedence.LOWEST
            
        if peekToken.Type <> TokenType.SEMICOLON && precedence < peekPrecedence then
            let! infixParseFunc = tryGetInfixParseFunc infixParseFunctionsMap tokensQueue
            let! newTokensQueue, infixExpr = infixParseFunc tokensQueue leftExpr
            return! tryParseExpressionHelper newTokensQueue precedence infixExpr
        else
            return tokensQueue, leftExpr
    }
    *)
    
///  
let internal tryParsePrefixExpression (parserState: ParserState) =
    result {
        let currentToken = parserState.PopToken()
        let! rightExpr = tryParseExpression parserState Precedence.PREFIX
       
        let! syntaxKind = 
            match currentToken.Type with
            | BANG ->
                Ok SyntaxKind.UnaryMinusExpression
            | MINUS ->
                Ok SyntaxKind.LogicalNotExpression
            | _ ->
                let invalidPrefixOperatorError = InvalidPrefixOperatorError(token=currentToken)
                let literalParseError = LiteralExpressionParseError(message="Failed to parse prefix expression", token=Some currentToken, innerException=invalidPrefixOperatorError)
                Error (ParseError(innerException=literalParseError))
                
        return SyntaxFactory.PrefixUnaryExpression(syntaxKind, rightExpr)
    }

let internal prefixParseFunctionsMap
    : Map<TokenType, ParserState -> Result<ExpressionSyntax, ParseError>> =
    Map.ofList [
        (TokenType.IDENT, tryParseIdentifier >> Ok)
        (TokenType.STRING, tryParseStringLiteral >> Ok)
        (TokenType.INT, tryParseIntegerLiteral)
        (TokenType.TRUE, tryParseBooleanLiteral)
        (TokenType.FALSE, tryParseBooleanLiteral)
        
        (*
        (TokenType.BANG, tryParsePrefixExpression)
        (TokenType.MINUS, tryParsePrefixExpression)
        (TokenType.LPAREN, tryParseGroupedExpression)
        (TokenType.IF, tryParseIfExpression)
        (TokenType.FUNCTION, tryParseFunctionLiteral)
        (TokenType.LBRACKET, tryParseArrayLiteral)
        (TokenType.LBRACE, tryParseHashLiteral)
        *)
    ]
    
let internal infixParseFunctionsMap
    : Map<TokenType, Token Queue -> ExpressionSyntax -> Result<ExpressionSyntax, ParseError>> =
    Map.ofList [
        (*
        (TokenType.LPAREN, tryParseCallExpression) // parse call expr
        (TokenType.LBRACKET, tryParseIndexExpression) // parse index expr 
        (TokenType.PLUS, tryParseInfixExpression)
        (TokenType.MINUS, tryParseInfixExpression)
        (TokenType.SLASH, tryParseInfixExpression)
        (TokenType.ASTERISK, tryParseInfixExpression)
        (TokenType.EQ, tryParseInfixExpression)
        (TokenType.NOT_EQ, tryParseInfixExpression)
        (TokenType.LT, tryParseInfixExpression)
        (TokenType.GT, tryParseInfixExpression)
        *)
    ]