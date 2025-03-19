// ReSharper disable FSharpRedundantParens
[<RequireQualifiedAccess>]
module rec Monkey.Frontend.CLR.Parsers.MonkeyAstParser

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

    let isCloseParen (tokenType: SyntaxKind) = tokenType = SyntaxKind.CloseParenToken
    
    let isCloseBracket (tokenType: SyntaxKind) = tokenType = SyntaxKind.CloseBracketToken
            
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
                
    let internal onUnexpectedSyntaxKind (expectedKind: SyntaxKind) (actualKind: SyntaxKind) =
        let errorMsg = $"Expected a/an \"{expectedKind}\", but received \"{actualKind}\""
        Error (ParseError(message=errorMsg))
        
    let internal assertNoParseErrors (parseErrors: ParseError array) =
        match parseErrors with
        | [|  |] ->
            Ok ()
        | _ ->
            Error parseErrors[0]  // we're guaranteed at least one element
                

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
    (SyntaxKind.EqualsEqualsToken, Precedence.EQUALS)
    (SyntaxKind.ExclamationEqualsToken, Precedence.EQUALS)
    (SyntaxKind.LessThanToken, Precedence.LESSGREATER)
    (SyntaxKind.GreaterThanToken, Precedence.LESSGREATER)
    (SyntaxKind.LessThanEqualsToken, Precedence.LESSGREATER)
    (SyntaxKind.GreaterThanEqualsToken, Precedence.LESSGREATER)
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
        
        

let parseTokens (tokens: SyntaxToken array) : StatementSyntax array * ParseError array =
    let parserState = MonkeyAstParserState(tokens)
    while not (parserState.IsEof()) do
        match tryParseStatement parserState with
        | Ok statementSyntaxOption ->
            if statementSyntaxOption.IsSome then
                parserState.Statements.Add(statementSyntaxOption.Value)
        | Error error ->
            parserState.Errors.Add(error)
    
    parserState.Statements.ToArray(), parserState.Errors.ToArray()
    
    
let private enterNewScopeAndParseTokens (stopCondition: MonkeyAstParserState -> bool) (parserState: MonkeyAstParserState) : StatementSyntax array * ParseError array =
    let statements = ResizeArray<StatementSyntax>()
    let errors = ResizeArray<ParseError>()
    
    while not (stopCondition parserState) do
        match tryParseStatement parserState with
        | Ok statementSyntaxOption ->
            if statementSyntaxOption.IsSome then
                statements.Add(statementSyntaxOption.Value)
        | Error error ->
            errors.Add(error)
    
    statements.ToArray(), errors.ToArray()
            
        
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
        let! prefixParseFunc = getPrefixParseFunc parserState 
        let! expression = prefixParseFunc parserState
        return! tryParseExpressionHelper parserState precedence expression
    }
    
and private tryParseExpressionHelper (parserState: MonkeyAstParserState) (precedence: Precedence) (leftExpr: ExpressionSyntax) : Result<ExpressionSyntax, ParseError> =
    result {
        let peekToken = parserState.PeekToken()
        let peekPrecedence =
            match Map.tryFind peekToken.Kind tokenTypeToPrecedenceMap with
            | Some precedence -> precedence
            | None -> Precedence.LOWEST
            
        if peekToken.Kind <> SyntaxKind.SemicolonToken && precedence < peekPrecedence then
            let! infixParseFunc = tryGetInfixParseFunc infixParseFunctionsMap parserState
            let! infixExpr = infixParseFunc parserState leftExpr
            return! tryParseExpressionHelper parserState precedence infixExpr
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
        { Token = currentToken } |> ExpressionSyntax.IdentifierNameSyntax |> Ok
        
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
            let updatedToken = { currentToken with Value = Some true }
            { Kind = SyntaxKind.TrueLiteralExpression; Token = updatedToken } |> ExpressionSyntax.LiteralExpressionSyntax |> Ok
        | SyntaxKind.FalseKeyword ->
            let updatedToken = { currentToken with Value = Some false }
            { Kind = SyntaxKind.FalseLiteralExpression; Token = updatedToken } |> ExpressionSyntax.LiteralExpressionSyntax |> Ok
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
            
            consumeUntilTokenType (fun tt -> isSemicolon tt || isCloseParen tt) parserState |> ignore
            let closeParenToken = parserState.PopToken()
            
            return ParenthesizedExpression(openParenToken, expr, closeParenToken)
        }
        
    let rec tryParseIfExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        result {
            let ifKeywordToken = parserState.PopToken()  // we assert this from the syntax kind to (prefix) parse function map
            
            // #1. Parsing the condition
            let openParenToken = parserState.PopToken()
            do! match openParenToken.Kind with
                | SyntaxKind.OpenParenToken -> Ok ()
                | kind ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    onUnexpectedSyntaxKind SyntaxKind.OpenParenToken kind

            let! conditionExpression = tryParseExpression parserState Precedence.LOWEST
                
            let closeParenToken = parserState.PopToken()
            do! match closeParenToken.Kind with
                | SyntaxKind.CloseParenToken -> Ok ()
                | kind ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    onUnexpectedSyntaxKind SyntaxKind.CloseParenToken kind
                       
            // #2. Parsing the main block
            let openBraceToken = parserState.PopToken()
            do! match openBraceToken.Kind with
                | SyntaxKind.OpenBraceToken -> Ok ()
                | kind ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    onUnexpectedSyntaxKind SyntaxKind.OpenBraceToken kind
                       
            let stopCondition (parserState: MonkeyAstParserState) = parserState.PeekToken().Kind = SyntaxKind.CloseBraceToken || parserState.IsEof()
            let statements, parseErrors = enterNewScopeAndParseTokens stopCondition parserState
            do! assertNoParseErrors parseErrors
            
            let closeBraceToken = parserState.PopToken()
            do! match closeBraceToken.Kind with
                | SyntaxKind.CloseBraceToken -> Ok ()
                | kind ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    onUnexpectedSyntaxKind SyntaxKind.CloseBraceToken kind
                       
            let mainBlock = { OpenBraceToken = openBraceToken; Statements = statements; CloseBraceToken = closeBraceToken }
            
            // TODO: Parse else if statements here
                       
                       
            // #3. Parsing the else statement, if any
            let! elseClauseOption =
                match parserState.PeekToken().Kind with
                | SyntaxKind.ElseKeyword ->
                    Result.map Some (parseElseBlock parserState)
                | _ ->
                    Ok None
                    
            return IfExpression(ifKeywordToken, openParenToken, conditionExpression, closeParenToken, mainBlock, [| |], elseClauseOption)
        }
        
    // TODO:
    // Right now, parsing a block can yield multiple parse errors, but we are only checking for one cause I don't
    // want to change the API as of right now 2025/03/11, 5:31PM
        
    and private parseElseBlock (parserState: MonkeyAstParserState) : Result<ElseClauseSyntax, ParseError> =
        result {
            let elseKeywordToken = parserState.PopToken()  // we assert this from the pattern match directly before the invocation
                    
            let openBraceToken = parserState.PopToken()
            do! match openBraceToken.Kind with
                | SyntaxKind.OpenBraceToken -> Ok ()
                | kind ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    onUnexpectedSyntaxKind SyntaxKind.OpenBraceToken kind
                       
            let stopCondition (parserState: MonkeyAstParserState) = parserState.PeekToken().Kind = SyntaxKind.CloseBraceToken || parserState.IsEof()
            let statements, parseErrors = enterNewScopeAndParseTokens stopCondition parserState
            do! assertNoParseErrors parseErrors
            
            let closeBraceToken = parserState.PopToken()
            do! match closeBraceToken.Kind with
                | SyntaxKind.CloseBraceToken -> Ok ()
                | kind ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    onUnexpectedSyntaxKind SyntaxKind.CloseBraceToken kind
                       
            let elseBlock = { OpenBraceToken = openBraceToken; Statements = statements; CloseBraceToken = closeBraceToken }
            return ElseClause(elseKeywordToken, elseBlock)
        }
        
        
    
    let rec internal tryParseTypeSyntax (onInvalid: ParseError) (parserState: MonkeyAstParserState) : Result<TypeSyntax, ParseError> =
        let currentToken = parserState.PopToken()
        match currentToken.Kind with
        | SyntaxKind.OpenBracketToken ->
            result {
                let openBracketToken = currentToken
                let! functionParsingResults = tryParseFunctionTypeSyntax [] [] onInvalid parserState
                let types, arrows = functionParsingResults
                let closeBracketToken = parserState.PopToken()  // we assert this from 'tryParseFunctionTypeSyntax'
                return FunctionType(openBracketToken, types, arrows, closeBracketToken)
            }
        | SyntaxKind.IntKeyword | SyntaxKind.StringKeyword ->
            BuiltinSyntax(currentToken) |> Ok
        | SyntaxKind.IdentifierToken ->
            NameSyntax(currentToken) |> Ok
        | _ ->
            Error onInvalid
            
    /// <example>
    /// example input:
    /// <code>
    /// "int -> int -> int]"
    /// </code>
    /// the 'lbracket' is assumed to be consumed by the caller function
    /// </example>
    /// <returns>
    /// <code>(TypeSyntax array) * (SyntaxToken array)</code>
    /// (Type signatures * Arrows between those type signatures)
    /// </returns>
    and private tryParseFunctionTypeSyntax
            (arrowTokens: SyntaxToken list)
            (funcSigTypes: TypeSyntax list)
            (onInvalid: ParseError)
            (parserState: MonkeyAstParserState)
            : Result<(TypeSyntax array) * (SyntaxToken array), ParseError> =  
                
        tryParseTypeSyntax onInvalid parserState
        |> function
            | Ok typeSyntax ->
                let newFuncSigTypes = typeSyntax :: funcSigTypes
                let peekToken = parserState.PeekToken()
                match peekToken.Kind with
                | SyntaxKind.CloseBracketToken ->
                    parserState.PopToken() |> ignore  // consume the ']'
                    let typeSyntaxArr = newFuncSigTypes |> List.toArray |> Array.rev
                    let arrowTokensArr = arrowTokens |> List.toArray |> Array.rev
                    Ok (typeSyntaxArr, arrowTokensArr)
                | SyntaxKind.MinusGreaterThanToken ->  // arrow token '->'
                    let arrowToken = parserState.PopToken()
                    tryParseFunctionTypeSyntax (arrowToken :: arrowTokens) newFuncSigTypes onInvalid parserState
                | tokenType ->
                   Error (ParseError(message=($"Invalid token type \"{tokenType}\" detected")))
            | Error parseError ->
                // TODO: see what you can do with composite errors
                Error parseError

    
    let rec tryParseFunctionExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        result {
            let fnKeywordToken = parserState.PopToken()  // we assert this from the syntax kind to (prefix) parse function map
            
            
            // #1. parse parameters list
            let openParenToken = parserState.PopToken()
            do! match openParenToken.Kind with
                | SyntaxKind.OpenParenToken -> Ok ()
                | kind ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    onUnexpectedSyntaxKind SyntaxKind.OpenParenToken kind
            
            let! syntaxSeparatedList =
                match parserState.PeekToken().Kind with
                | SyntaxKind.CloseParenToken -> Ok ([||], [||])  // no parameters
                | _ -> parseParametersList [] [] parserState
                
            let closeParenToken = parserState.PopToken()
            do! match closeParenToken.Kind with
                | SyntaxKind.CloseParenToken -> Ok ()
                | kind ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    onUnexpectedSyntaxKind SyntaxKind.CloseParenToken kind
                    
            let parameterSyntaxArr, commas = syntaxSeparatedList
            let parameterList = ParameterList(openParenToken, parameterSyntaxArr, commas, closeParenToken)
            
            
            // #2. parse return type
            let colonToken = parserState.PopToken()
            do! match colonToken.Kind with
                | SyntaxKind.ColonToken -> Ok ()
                | kind ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    onUnexpectedSyntaxKind SyntaxKind.ColonToken kind
            
            let retTypeToken = parserState.PeekToken()
            let onInvalidReturnTypeError = ParseError(message=($"Expected a valid return type, but received \"{retTypeToken.Text}\""))
            let! returnTypeSyntax = tryParseTypeSyntax onInvalidReturnTypeError parserState
            
            
            // #3. parse function block
            let openBraceToken = parserState.PopToken()
            do! match openBraceToken.Kind with
                | SyntaxKind.OpenBraceToken -> Ok ()
                | kind ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    onUnexpectedSyntaxKind SyntaxKind.OpenBraceToken kind
            
            let stopCondition (parserState: MonkeyAstParserState) = parserState.PeekToken().Kind = SyntaxKind.CloseBraceToken || parserState.IsEof()
            let statements, parseErrors = enterNewScopeAndParseTokens stopCondition parserState
            do! assertNoParseErrors parseErrors
            
            let closeBraceToken = parserState.PopToken()
            do! match closeBraceToken.Kind with
                | SyntaxKind.CloseBraceToken -> Ok ()
                | kind ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    onUnexpectedSyntaxKind SyntaxKind.CloseBraceToken kind
                    
            let block = BlockStatementNoBox(openBraceToken, statements, closeBraceToken)
            return FunctionExpressionNoBox(fnKeywordToken, parameterList, colonToken, returnTypeSyntax, block) |> ExpressionSyntax.FunctionExpressionSyntax
        }
        
    /// Expected input ""
    /// <example>
    /// example input:
    /// <code>
    /// TYPEDEF_1 ARG_NAME_1, TYPEDEF_2 ARG_NAME_2, ..., TYPEDEF_N ARG_NAME_N
    /// </code>
    /// the 'closeParenToken' is assumed to be consumed by the caller function
    /// </example>
    /// <returns>
    /// <code>(ParameterSyntax array) * (SyntaxToken array)</code>
    /// (Function parameter syntax * commas between those type signatures)
    /// </returns>
    and private parseParametersList
        (commas: SyntaxToken list)
        (parameters: ParameterSyntax list)
        (parserState: MonkeyAstParserState)
        : Result<(ParameterSyntax array) * (SyntaxToken array), ParseError> =
        result {
            let currentToken = parserState.PeekToken()
            let onInvalidTypeSyntaxError = ParseError(message=($"Expected a valid return type, but received \"{currentToken.Text}\""))
            let! typeSyntax = tryParseTypeSyntax onInvalidTypeSyntaxError parserState
            
            let currentToken = parserState.PopToken()
            let! parameterNameToken = 
                match currentToken.Kind with
                | SyntaxKind.IdentifierToken -> IdentifierNameNoBox(currentToken) |> Ok
                | _ -> Error (ParseError(message=($"[arg #{List.length parameters}] Expected an identifier for the argument name, but received \"{currentToken.Text}\"")))
            
            return Parameter(typeSyntax, parameterNameToken)
        }
        |> function
           | Ok parameterSyntax ->
               let newParameters = parameterSyntax :: parameters
               match parserState.PeekToken().Kind with
               | SyntaxKind.CommaToken ->
                   let commaToken = parserState.PopToken()
                   parseParametersList (commaToken :: commas) newParameters parserState
               | SyntaxKind.CloseParenToken ->
                   let parametersArr = newParameters |> List.toArray |> Array.rev
                   let commasArr = commas |> List.toArray |> Array.rev
                   Ok (parametersArr, commasArr)
               | tokenType ->
                   Error (ParseError(message=($"Invalid token type \"{tokenType}\" detected")))
           | Error error ->
               Error error

        
    let rec getPrefixParseFunc (parserState: MonkeyAstParserState) : Result<MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>, ParseError> =
        match parserState.IsEof() with
        | true ->
            let message = "FATAL. Tokens queue empty. This indicates a logical error in the parsing process."
            failwith message
            // TODO HERE MAYBE
        | false ->
            let token = parserState.PeekToken()
            match token.Kind, token.Value with
            | SyntaxKind.IdentifierToken, value when isFnKeyword value -> tryParseFunctionExpression |> Ok
            | SyntaxKind.IdentifierToken, _ ->  tryParseIdentifier |> Ok
            | SyntaxKind.StringLiteralToken, _ ->  tryParseStringLiteralExpression |> Ok
            | SyntaxKind.NumericLiteralToken, _ ->  tryParseNumericLiteralExpression |> Ok
            | SyntaxKind.TrueKeyword, _ ->  tryParseBooleanLiteralExpression |> Ok
            | SyntaxKind.FalseKeyword, _ ->  tryParseBooleanLiteralExpression |> Ok
            | SyntaxKind.ExclamationToken, _ ->  tryParsePrefixExpression |> Ok
            | SyntaxKind.MinusToken, _ ->  tryParsePrefixExpression |> Ok
            | SyntaxKind.OpenParenToken, _ ->  tryParseParenthesizedExpression |> Ok
            | SyntaxKind.IfKeyword, _ ->  tryParseIfExpression |> Ok
            | _ -> 
                consumeUntilTokenType isSemicolon parserState |> ignore  // pass by reference, properties change 'in-place'
                Error (ParseError($"Could not find a prefix parse function for the token type \"{token.Kind}\""))
                
    and isFnKeyword (value: obj option) : bool =
        match value with
        | Some value ->
            match value with
            | :? string as str when str = "fn" -> true
            | _ -> false
        | None -> false
                
                
            
[<AutoOpen>]
module internal InfixExpressions =
    
    let private operatorTokenKindToBinaryExpressionFunc (tokenSyntaxKind: SyntaxKind) =
        match tokenSyntaxKind with
        | SyntaxKind.PlusToken ->
            Ok (fun leftExpr operatorToken rightExpr -> AddExpression(leftExpr, operatorToken, rightExpr))
        | SyntaxKind.MinusToken -> 
            Ok (fun leftExpr operatorToken rightExpr -> SubtractExpression(leftExpr, operatorToken, rightExpr))
        | SyntaxKind.AsteriskToken -> 
            Ok (fun leftExpr operatorToken rightExpr -> MultiplicationExpression(leftExpr, operatorToken, rightExpr))
        | SyntaxKind.SlashToken -> 
            Ok (fun leftExpr operatorToken rightExpr -> DivideExpression(leftExpr, operatorToken, rightExpr))
        | SyntaxKind.EqualsEqualsToken -> 
            Ok (fun leftExpr operatorToken rightExpr -> EqualsExpression(leftExpr, operatorToken, rightExpr))
        | SyntaxKind.ExclamationEqualsToken -> 
            Ok (fun leftExpr operatorToken rightExpr -> NotEqualsExpression(leftExpr, operatorToken, rightExpr))
        | SyntaxKind.GreaterThanToken -> 
            Ok (fun leftExpr operatorToken rightExpr -> GreaterThanExpression(leftExpr, operatorToken, rightExpr))
        | SyntaxKind.LessThanToken -> 
            Ok (fun leftExpr operatorToken rightExpr -> LessThanExpression(leftExpr, operatorToken, rightExpr))
        | SyntaxKind.GreaterThanEqualsToken -> 
            Ok (fun leftExpr operatorToken rightExpr -> GreaterThanOrEqExpression(leftExpr, operatorToken, rightExpr))
        | SyntaxKind.LessThanEqualsToken -> 
            Ok (fun leftExpr operatorToken rightExpr -> LessThanOrEqExpression(leftExpr, operatorToken, rightExpr))
        | _ ->
            failwith "todo"
        
    let tryParseInfixExpression (parserState: MonkeyAstParserState) (leftExpr: ExpressionSyntax) : Result<ExpressionSyntax, ParseError> =
        result {
            let operatorToken = parserState.PopToken()
            let precedence = 
                match Map.tryFind operatorToken.Kind tokenTypeToPrecedenceMap with
                | Some precedence -> precedence
                | None -> Precedence.LOWEST
                
            let! rightExpr = tryParseExpression parserState precedence
            let! toBinaryExpressionSyntaxFunc = operatorTokenKindToBinaryExpressionFunc operatorToken.Kind
            return (toBinaryExpressionSyntaxFunc leftExpr operatorToken rightExpr)
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
            (SyntaxKind.EqualsEqualsToken, tryParseInfixExpression)
            (SyntaxKind.ExclamationEqualsToken, tryParseInfixExpression)
            (SyntaxKind.LessThanToken, tryParseInfixExpression)
            (SyntaxKind.GreaterThanToken, tryParseInfixExpression)
            (SyntaxKind.LessThanEqualsToken, tryParseInfixExpression)
            (SyntaxKind.GreaterThanEqualsToken, tryParseInfixExpression)
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
