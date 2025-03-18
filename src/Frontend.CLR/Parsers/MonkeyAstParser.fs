// ReSharper disable FSharpRedundantParens
[<RequireQualifiedAccess>]
module rec Monkey.Frontend.CLR.Parsers.MonkeyAstParser

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

open FsToolkit.ErrorHandling

open Monkey.Frontend.CLR.Syntax.Ast
open Monkey.Frontend.CLR.Parsers.CSharpAstErrors
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeySyntaxTokenFactory
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeyStatementSyntaxFactory
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeyExpressionSyntaxFactory



[<AutoOpen>]
module ParserStateHelpers =
    let isSemicolon (tokenType: SyntaxKind) = tokenType = SyntaxKind.SemicolonToken

    let isRParen (tokenType: SyntaxKind) = tokenType = SyntaxKind.CloseParenToken
            
    let rec internal consumeUntilTokenType (tokenTypePredicate: SyntaxKind -> bool) (parserState: MonkeyAstParserState) =
        match parserState.IsEof() with
        | true ->
            parserState
        | false ->
            let token = parserState.PeekToken()
            match token.Kind with
            | tokenType when (tokenTypePredicate tokenType) || tokenType = SyntaxKind.EndOfFileToken ->
                parserState
            | _ ->
                parserState.CurrentIdx <- parserState.CurrentIdx + 1
                consumeUntilTokenType tokenTypePredicate parserState
                
                

/// <summary>
/// Operator precedence when determining how to order nested infix expressions.
/// </summary>
type internal Precedence =
    | LOWEST = 1
    | EQUALS = 2
    | LESSGREATER = 3
    | SUM = 4
    | PRODUCT = 5
    | PREFIX = 6
    | CALL = 7
    | INDEX = 8

let internal tokenTypeToPrecedenceMap = Map.ofList [
    (SyntaxKind.EqualsToken, Precedence.EQUALS)
    (SyntaxKind.ExclamationEqualsToken, Precedence.EQUALS)
    (SyntaxKind.LessThanToken, Precedence.LESSGREATER)
    (SyntaxKind.GreaterThanToken, Precedence.LESSGREATER)
    (SyntaxKind.PlusToken, Precedence.SUM)
    (SyntaxKind.MinusToken, Precedence.SUM)
    (SyntaxKind.SlashToken, Precedence.PRODUCT)
    (SyntaxKind.AsteriskToken, Precedence.PRODUCT)
    (SyntaxKind.OpenParenToken, Precedence.CALL)
    (SyntaxKind.OpenBracketToken, Precedence.INDEX)
]
    
    
    
/// <summary>
/// Mutable type containing the main parser state.
/// </summary>
type internal MonkeyAstParserState(tokens: SyntaxToken array) =
    let mutable currentIdx: int = 0
    let mutable errors: ResizeArray<ParseError> = ResizeArray<ParseError>()
    let mutable statements: ResizeArray<StatementSyntax> = ResizeArray<StatementSyntax>()
    
    member this.Tokens = tokens
    
    member this.CurrentIdx
        with get() = currentIdx
        and set(value) = currentIdx <- value

    member this.Errors
        with get() = errors
        
    member this.Statements
        with get() = statements
        
with
    member this.TokensLength() =
        Array.length this.Tokens
        
    member this.IsEof() =
        let isEof =
            match this.Tokens[this.CurrentIdx].Kind with
            | SyntaxKind.EndOfFileToken -> true
            | _ -> false
        
        (this.CurrentIdx >= Array.length this.Tokens) || isEof
        
    member this.PeekToken() : SyntaxToken =
        this.Tokens[this.CurrentIdx]
        
    member this.PopToken() : SyntaxToken =
        let token = this.Tokens[this.CurrentIdx]
        this.CurrentIdx <- this.CurrentIdx + 1
        token
        
    member this.GetTokenAt(index: int) : SyntaxToken =
        this.Tokens[index]
        
        

let rec parseTokens (tokens: SyntaxToken array) : StatementSyntax array * ParseError array =
    let parserState = MonkeyAstParserState(tokens)
    while not (parserState.IsEof()) do
        match tryParseStatement parserState with
        | Ok statementSyntaxOption ->
            if statementSyntaxOption.IsSome then
                parserState.Statements.Add(statementSyntaxOption.Value)
        | Error error ->
            parserState.Errors.Add(error)
    
    parserState.Statements.ToArray(), parserState.Errors.ToArray()
            
        
let private tryParseStatement (parserState: MonkeyAstParserState) : Result<StatementSyntax option, ParseError> =
    let token = parserState.PeekToken()
    match token.Kind with
    | SyntaxKind.LetKeyword ->
        // parserState |> tryParseLetStatement |> (Result.map Some)
        failwith "todo"
    | SyntaxKind.SemicolonToken | SyntaxKind.EndOfFileToken ->
        parserState.PopToken() |> ignore  // we know that there is at least one element
        Ok None  // no parsing happens, jus continue type shi
    | _ ->
        parserState |> tryParseExpressionStatement |> (Result.map Some)
        

let rec private tryParseExpression (parserState: MonkeyAstParserState) (precedence: Precedence) : Result<ExpressionSyntax, ParseError> =
    result {
        let! prefixParseFunc = tryGetPrefixParseFunc prefixParseFunctionsMap parserState 
        let! expression = prefixParseFunc parserState
        return! tryParseExpressionOrStatementHelper parserState precedence expression
    }
    
and private tryParseExpressionOrStatementHelper (parserState: MonkeyAstParserState) (precedence: Precedence) (leftExpr: ExpressionSyntax) : Result<ExpressionSyntax, ParseError> =
    result {
        let peekToken = parserState.PeekToken()
        let peekPrecedence =
            match Map.tryFind peekToken.Kind tokenTypeToPrecedenceMap with
            | Some precedence -> precedence
            | None -> Precedence.LOWEST
            
        if peekToken.Kind <> SyntaxKind.SemicolonToken && precedence < peekPrecedence then
            let! infixParseFunc = tryGetInfixParseFunc infixParseFunctionsMap parserState
            let! infixExpr = infixParseFunc parserState leftExpr
            return! tryParseExpressionOrStatementHelper parserState precedence infixExpr
        else
            return leftExpr
    }

[<AutoOpen>]    
module private Statements =
    let internal tryParseExpressionStatement (parserState: MonkeyAstParserState) : Result<StatementSyntax, ParseError> =
        result {
            let! expression = tryParseExpression parserState Precedence.LOWEST
            let! semicolonToken = 
                match parserState.PeekToken() with
                | syntaxToken when syntaxToken.Kind = SyntaxKind.SemicolonToken ->
                    parserState.PopToken() |> Ok
                | _ -> failwith "todo"
            
            return { Expression = expression; SemicolonToken = semicolonToken } |> StatementSyntax.ExpressionStatementSyntax
        }
        


[<AutoOpen>]
module internal PrefixExpressions =
    // TODO: Change to parse identifier or call expr
    let tryParseIdentifier (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        let currentToken = parserState.PopToken()
        { Kind = SyntaxKind.StringLiteralExpression; Token = currentToken } |> ExpressionSyntax.LiteralExpressionSyntax |> Ok
        
    let tryParseStringLiteralExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        let currentToken = parserState.PopToken()
        { Token = currentToken } |> ExpressionSyntax.IdentifierNameSyntax |> Ok
        
    let tryParseNumericLiteralExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        let currentToken = parserState.PopToken()
        { Kind = SyntaxKind.NumericLiteralExpression; Token = currentToken } |> ExpressionSyntax.LiteralExpressionSyntax |> Ok
        
    let tryParseBooleanLiteralExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        let currentToken = parserState.PopToken()
        match currentToken.Kind with
        | SyntaxKind.TrueKeyword -> 
            { Kind = SyntaxKind.TrueLiteralExpression; Token = currentToken } |> ExpressionSyntax.LiteralExpressionSyntax |> Ok
        | SyntaxKind.FalseKeyword ->
            { Kind = SyntaxKind.FalseLiteralExpression; Token = currentToken } |> ExpressionSyntax.LiteralExpressionSyntax |> Ok
        | _ ->
            failwith "todo"
            
    let tryParsePrefixExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        result {
            let operatorToken = parserState.PopToken()
            let! expression = tryParseExpression parserState Precedence.PREFIX
            let! prefixExpr = 
                match operatorToken.Kind with
                | SyntaxKind.ExclamationToken ->
                    LogicalNotPrefixExpression(expression, operatorToken) |> Ok
                | SyntaxKind.MinusToken -> 
                    MinusPrefixExpression(expression, operatorToken) |> Ok
                | _ -> failwith "todo"
            return prefixExpr
        }
        
    let tryParseParenthesizedExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        result {
            let openParenToken = parserState.PopToken()
            let! expr = tryParseExpression parserState Precedence.LOWEST
            
            consumeUntilTokenType (fun tt -> isSemicolon tt || isRParen tt) parserState |> ignore
            let closeParenToken = parserState.PopToken()
            
            return ParenthesizedExpression(openParenToken, expr, closeParenToken)
        }
    
    /// <summary>
    /// Map containing token types and the corresponding function required to parse them.
    /// </summary>
    let prefixParseFunctionsMap
        : Map<SyntaxKind, MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>> =
        Map.ofList [
            // (TokenType.IDENT,   tryParseIdentifier              >> Ok >> castToCSharpSyntaxNode)
            (SyntaxKind.IdentifierToken, tryParseIdentifier)
            (SyntaxKind.StringLiteralToken,  tryParseStringLiteralExpression)
            (SyntaxKind.NumericLiteralToken, tryParseNumericLiteralExpression)
            (SyntaxKind.TrueKeyword, tryParseBooleanLiteralExpression)
            (SyntaxKind.FalseKeyword, tryParseBooleanLiteralExpression)
            (SyntaxKind.ExclamationToken, tryParsePrefixExpression)
            (SyntaxKind.MinusToken, tryParsePrefixExpression)
            (SyntaxKind.OpenParenToken, tryParseParenthesizedExpression)
            
            (*
            (TokenType.IF,      tryParseIfExpression                >> castToCSharpSyntaxNode)
            (TokenType.FUNCTION, tryParseFunctionExpression         >> castToCSharpSyntaxNode)
            
            (TokenType.LBRACKET, tryParseArrayLiteral)
            (TokenType.LBRACE, tryParseHashLiteral)
            *)
        ]
        
        
    /// Attempts to get the PREFIX parse function based on the next token's type
    let tryGetPrefixParseFunc prefixParseFuncMap parserState : Result<MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>, ParseError> =
        match parserState.IsEof() with
        | true ->
            let message = "FATAL. Tokens queue empty. This indicates a logical error in the parsing process."
            failwith message
            // TODO HERE MAYBE
        | false ->
            let token = parserState.PeekToken()
            
            let onMissingValue parserState =  // i.e. basically go to the next statement
                consumeUntilTokenType isSemicolon parserState |> ignore  // pass by reference, properties change 'in-place'
                ParseError($"Could not find a prefix parse function for the token type \"{token.Kind}\"")
                
            match Map.tryFind token.Kind prefixParseFuncMap with
            | Some value -> Ok value
            | None -> Error (onMissingValue parserState)
            
            
            
[<AutoOpen>]
module internal InfixExpressions =
    
    let tryParseInfixExpression (parserState: MonkeyAstParserState) (leftExpr: ExpressionSyntax) : Result<ExpressionSyntax, ParseError> =
        result {
            let token = parserState.PopToken()
            let precedence = 
                match Map.tryFind token.Kind tokenTypeToPrecedenceMap with
                | Some precedence -> precedence
                | None -> Precedence.LOWEST
                
            let! rightExpr = tryParseExpression parserState precedence
            return { Left = leftExpr; OperatorToken = token; Right = rightExpr } |> ExpressionSyntax.BinaryExpression
        }
        
        
    let infixParseFunctionsMap
        : Map<SyntaxKind, MonkeyAstParserState -> ExpressionSyntax -> Result<ExpressionSyntax, ParseError>> =
        Map.ofList [
            (*
            (TokenType.LPAREN, tryParseCallExpression) // parse call expr
            (TokenType.LBRACKET, tryParseIndexExpression) // parse index expr 
            *)
            
            (SyntaxKind.PlusToken, tryParseInfixExpression)
            (SyntaxKind.MinusToken, tryParseInfixExpression)
            (SyntaxKind.SlashToken, tryParseInfixExpression)
            (SyntaxKind.AsteriskToken, tryParseInfixExpression)
            (SyntaxKind.EqualsToken, tryParseInfixExpression)
            (SyntaxKind.ExclamationEqualsToken, tryParseInfixExpression)
            (SyntaxKind.LessThanToken, tryParseInfixExpression)
            (SyntaxKind.GreaterThanToken, tryParseInfixExpression)
        ]
        
        
        
    /// Attempts to get the INFIX parse function based on the next token's type
    let tryGetInfixParseFunc
            (infixParseFuncMap: Map<SyntaxKind, MonkeyAstParserState -> ExpressionSyntax -> Result<ExpressionSyntax, ParseError>>)
            (parserState: MonkeyAstParserState)
            : Result<MonkeyAstParserState -> ExpressionSyntax -> Result<ExpressionSyntax, ParseError>, ParseError> =
                
        match parserState.IsEof() with
        | true ->
            let message = "FATAL. Tokens queue empty. This indicates a logical error in the parsing process."
            failwith message  // TODO
        | false ->
            let onMissingValue parserState =  // i.e. basically go to the next statement
                consumeUntilTokenType isSemicolon parserState |> ignore  // pass by reference, properties change 'in-place'
                ParseError()
                
            let token = parserState.PeekToken()
            match Map.tryFind token.Kind infixParseFuncMap with
            | Some value -> Ok value
            | None -> Error (onMissingValue parserState)
