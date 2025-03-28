﻿// ReSharper disable FSharpRedundantParens
(*
Converts source monkey text into an equivalent C# Roslyn AST.
Deprecated in favor of a new parser that converts source monkey text into an equivalent monkey AST.
*)
[<System.Obsolete>]
module rec Monkey.Frontend.CLR.Parsers.DirectCSharpAstParser

open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open type Microsoft.CodeAnalysis.CSharp.SyntaxFactory

open FsToolkit.ErrorHandling

open Monkey.Frontend.CLR.Token
open Monkey.Frontend.CLR.Helpers
open Monkey.Frontend.CLR.Parsers
open Monkey.Frontend.CLR.Parsers.CSharpAstErrors



/// <summary>
/// Mutable type containing the main parser state.
/// </summary>
type internal CSharpAstParserState(tokens: Token array) =
    let mutable currentIdx: int = 0
    let mutable parseErrors: CSharpParseError list = []
    let mutable statements: StatementSyntax list = []
    let mutable lambdaTypeMap: Map<string, LambdaFunctionSignature> = Map.empty
    
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
        
    /// Map containing function signatures of lambda expressions (since their syntax does not contain any type info as
    /// compared to normal member methods.
    member this.LambdaTypeMap
        with get() = lambdaTypeMap
        and set(value) = lambdaTypeMap <- value
    
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
        
and LambdaFunctionSignature =
    { ParameterTypes: TypeSyntax array
      ReturnType: TypeSyntax }
with
    member this.ToFuncTypeSyntax() =
        let types = Array.append this.ParameterTypes [| this.ReturnType |]
        let commas = Array.create ((Array.length types) - 1) (Token(SyntaxKind.CommaToken))
        GenericName(Identifier("Func"))
            .WithTypeArgumentList(
                TypeArgumentList(
                    SeparatedList<TypeSyntax>(types, commas)))


[<AutoOpen>]
module ParserStateHelpers =
    let isSemicolon (tokenType: TokenType) = tokenType = TokenType.SEMICOLON

    let isRParen (tokenType: TokenType) = tokenType = TokenType.RPAREN
            
    let rec internal consumeUntilTokenType (tokenTypePredicate: TokenType -> bool) (parserState: CSharpAstParserState) =
        match parserState.IsEof() with
        | true ->
            parserState
        | false ->
            let token = parserState.PeekToken()
            match token.Type with
            | tokenType when (tokenTypePredicate tokenType) || tokenType = EOF ->
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


let internal defaultHashLen = 16

let internal generateRandomStringHash n =
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    let random = Random()
    Array.init n (fun _ -> chars.[random.Next(chars.Length)]) |> String
    
    
let internal onUnexpectedToken (expectedTokenType: TokenType) (actualToken: Token) =
    let errorMsg = $"Expected a/an \"{expectedTokenType}\", but received \"{actualToken.Literal}\" of type \"{actualToken.Type}\""
    Error (CSharpParseError(message=errorMsg))

let internal assertAndPop (expectedTokenType: TokenType) (parserState: CSharpAstParserState) =
    let currentToken = parserState.PopToken()
    match currentToken.Type with
    | tokenType when tokenType = expectedTokenType -> Ok ()
    | _ -> consumeUntilTokenType isSemicolon parserState |> ignore
           onUnexpectedToken expectedTokenType currentToken

let internal assertNoParseErrors (parseErrors: CSharpParseError list) =
    match parseErrors with
    | [ ] ->
        Ok ()
    | head :: _ ->
        Error head


type ParseOptions =
    { OutputFormat: OutputFormat
      UseGlobalStatements: bool }
with
    static member Default =
        { OutputFormat = AsFullSyntaxTree
          UseGlobalStatements = true }
        
    static member DevelopmentOptions =
        { OutputFormat = JustStatements 
          UseGlobalStatements = false }
    
and OutputFormat =
    | JustStatements
    | AsFullSyntaxTree
    

let rec parseTokens (tokens: Token array) : StatementSyntax list * CSharpParseError list =
    let parserState = CSharpAstParserState(tokens)
    
    let parseStopCondition (parserState: CSharpAstParserState) = parserState.IsEof()
    let statements, parseErrors = parseTokensHelper parseStopCondition parserState [] []
    statements, parseErrors
    
    
let rec internal parseTokensHelper
        (stopCondition: CSharpAstParserState -> bool)
        (parserState: CSharpAstParserState)
        (statements: StatementSyntax list)
        (parseErrors: CSharpParseError list)
        : StatementSyntax list * CSharpParseError list =
    match stopCondition parserState with
    | true -> statements, parseErrors
    | false ->
        match tryParseStatement parserState with
        | Ok statementSyntaxOption ->
            match statementSyntaxOption with
            | Some newStatements ->
                let newStatements = List.append statements (Array.toList newStatements)
                parseTokensHelper stopCondition parserState newStatements parseErrors
            | None ->
                parseTokensHelper stopCondition parserState statements parseErrors
        | Error errorValue ->
            parseTokensHelper stopCondition parserState statements (errorValue :: parseErrors)
            
        
and internal tryParseStatement (parserState: CSharpAstParserState) : Result<StatementSyntax[] option, CSharpParseError> =
    let token = parserState.PeekToken()
    match token.Type with
    | LET ->
        parserState |> tryParseLetStatement |> (Result.map Some)
    | RETURN ->
        failwith "todo"
        // tokensQueue |> tryParseReturnStatement |> encapsulateIntoCase Statement.ReturnStatement 
    | SEMICOLON | EOF ->
        parserState.PopToken() |> ignore  // we know that there is at least one element
        Ok None  // no parsing happens, jus continue type shi
    | _ ->
        parserState |> tryParseExpressionStatement |> (Result.map (fun stat -> Some [| stat |]))
        


/// <remarks>
/// Mainly parses expressions. However, since we treat 'if' clauses as expressions and C# treats them as statements, we
/// need to work around this by being able to take both expressions and statements.
/// <br/>
/// <br/>
/// Since we cannot inherit 'ExpressionSyntax' to represent these cases as an expression (due to ROSLYN's design), this
/// seems like one of the few solutions without re-writing the api (2025-03-11).
/// </remarks>
let rec internal tryParseExpressionOrStatement (parserState: CSharpAstParserState) (precedence: Precedence) : Result<CSharpSyntaxNode, CSharpParseError> =
    result {
        let! prefixParseFunc = tryGetPrefixParseFunc prefixParseFunctionsMap parserState 
        let! csSyntaxNode = prefixParseFunc parserState
        
        return!
            match csSyntaxNode with
            | :? ExpressionSyntax as leftExpression ->
                tryParseExpressionOrStatementHelper parserState precedence leftExpression
            | :? StatementSyntax as stat ->
                Ok stat
            | _ ->
                Error (CSharpParseError())  // TODO
    }
    
and internal tryParseExpressionOrStatementHelper (parserState: CSharpAstParserState) (precedence: Precedence) (leftExpr: ExpressionSyntax) : Result<CSharpSyntaxNode, CSharpParseError> =
    result {
        let peekToken = parserState.PeekToken()
        let peekPrecedence =
            match Map.tryFind peekToken.Type tokenTypeToPrecedenceMap with
            | Some precedence -> precedence
            | None -> Precedence.LOWEST
            
        if peekToken.Type <> TokenType.SEMICOLON && precedence < peekPrecedence then
            let! infixParseFunc = tryGetInfixParseFunc infixParseFunctionsMap parserState
            let! infixExpr = infixParseFunc parserState leftExpr
            return! tryParseExpressionOrStatementHelper parserState precedence infixExpr
        else
            return leftExpr
    }
    
    
    
[<AutoOpen>]    
module private Statements =
    let internal tryParseExpressionStatement (parserState: CSharpAstParserState) : Result<StatementSyntax, CSharpParseError> =
        result {
            let! csSyntaxNode = tryParseExpressionOrStatement parserState Precedence.LOWEST
            let! statementSyntax =
                match csSyntaxNode with
                | :? ExpressionSyntax as expressionSyntax ->
                    Ok (ExpressionStatement(expressionSyntax) :> StatementSyntax)
                | :? IfStatementSyntax as ifStatementSyntax ->
                    Ok ifStatementSyntax  // regular if statement parsing
                | _ ->
                    Error (CSharpParseError())  // TODO
                
            return statementSyntax
        }
        
    let rec internal tryParseLetStatement (parserState: CSharpAstParserState) : Result<StatementSyntax array, CSharpParseError> =
        result {
            // is the 'let' keyword
            // if ever we have more modifiers (ex. types and 'const' keyword for example), this needs to be modified
            let _ = parserState.PopToken()
            
            // #1. parse variable name
            let peekToken = parserState.PeekToken()
            let! variableName = 
                match peekToken.Type with
                | IDENT ->
                    let identToken = parserState.PopToken()
                    Ok identToken.Literal
                | _ ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    let errorMsg = $"Expected an identifier after 'let' keyword, but received \"{peekToken.Literal}\" of type \"{peekToken.Type}\""
                    Error (CSharpParseError(innerException=LetStatementParseError(message=errorMsg)))
                    
                    
            // #2. trying to parse explicit type annotation, if it exists
            let! explicitTypeSyntaxOption =
                match parserState.PeekToken().Type with
                | COLON ->
                    parserState.PopToken() |> ignore  // consume the ':' token
                    let explicitTypeSyntaxResult = tryParseTypeSyntax (CSharpParseError(innerException=LetStatementParseError(message="Failed to parse explicit variable type annotation."))) parserState
                    Result.map Some explicitTypeSyntaxResult
                | _ -> Ok None
                
            let varTypeSyntax =
                match explicitTypeSyntaxOption with
                | Some typeSyntax -> typeSyntax
                | None -> SyntaxFactory.IdentifierName("var") :> TypeSyntax  // default to 'var'
                    
                    
            // #3. asserting equals token
            do! match parserState.PeekToken().Type with
                | ASSIGN ->
                    parserState.PopToken() |> ignore  // consume the 'equals' token
                    Ok ()
                | _ ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    let errorMsg = $"Expected an assignment operator ('='), but received \"{peekToken.Literal}\" of type \"{peekToken.Type}\""
                    Error (CSharpParseError(innerException=LetStatementParseError(message=errorMsg)))
                    
                    
            // #4. parsing actual expression
            let! csSyntaxNode = tryParseExpressionOrStatement parserState Precedence.LOWEST
            consumeUntilTokenType isSemicolon parserState |> ignore
            
            
            // #5. forming the let statement, as well as performing any modifications as necessary
            let! statementSyntax =
                match csSyntaxNode with
                | :? ExpressionSyntax as expr ->
                        
                    let typeSyntax =
                        match isFunction expr with
                        | Some functionHash when parserState.LambdaTypeMap.ContainsKey functionHash ->
                            let functionSignature = parserState.LambdaTypeMap[functionHash]
                            functionSignature.ToFuncTypeSyntax() :> TypeSyntax
                        | _ ->
                            varTypeSyntax
                            
                    let variableDeclarator =
                        SyntaxFactory
                            .VariableDeclarator(SyntaxFactory.Identifier(variableName))
                            .WithInitializer(SyntaxFactory.EqualsValueClause(expr))
                    let variableDeclaration =
                        SyntaxFactory.VariableDeclaration(
                            typeSyntax,
                            SyntaxFactory.SeparatedList([| variableDeclarator |]))
                    let localDeclarationStatement = SyntaxFactory.LocalDeclarationStatement(variableDeclaration) :> StatementSyntax
                    Ok [| localDeclarationStatement |]
                    
                | :? IfStatementSyntax as ifStatementSyntax ->
                    
                    // reformat if statement into separate blocks of code
                    let varDeclarationStatement =
                        LocalDeclarationStatement(
                            VariableDeclaration(
                                PredefinedType(Token(SyntaxKind.ObjectKeyword)),
                                SeparatedList(
                                [|
                                    VariableDeclarator(Identifier(variableName))
                                |])
                                )
                            ) :> StatementSyntax
                        
                    transformIfStatement variableName ifStatementSyntax
                    |> Result.map (fun transformedIfStatement -> [| varDeclarationStatement; transformedIfStatement :> StatementSyntax |])
                | _ ->
                    Error (CSharpParseError())  // TODO
                    
            return statementSyntax
        }
        
        
    /// <remarks>
    /// We need this to determine whether the expression we're assigning to the variable is a function.
    /// </remarks>
    and private isFunction (expression: ExpressionSyntax) : string option =
        option {
            let! parenLambdaExpr =
                match expression with
                | :? ParenthesizedLambdaExpressionSyntax as parenLambdaExpr -> Some parenLambdaExpr
                | _ -> None
            let! annotationOption = Seq.tryHead (parenLambdaExpr.GetAnnotations("FunctionSignature"))
            return! annotationOption.Data
        }
        
    /// <summary>
    /// Transforms an inline if-assignment expression into a multi-line one.
    /// <code>
    /// var foobar = 5 > 2 ? 5 : 2;
    /// </code>
    /// Becomes
    /// <code>
    /// object foobar;
    /// if (5 > 2)
    /// {
    ///     foobar = 5;
    /// }
    /// else
    /// {
    ///     foobar = 2;
    /// }
    /// </code>
    /// </summary>
    /// <remarks>
    /// <ul>
    /// <li>
    /// As of right now (2025/03/11), the assigned type is <c>object</c>, but this could be changed later on to infer
    /// the actual type of the returned if expression.
    /// </li>
    /// <li>
    /// It needs to be done this way since we want to put multiple expressions and/or statements.
    /// </li>
    /// </ul>
    /// </remarks>
    and private transformIfStatement (variableName: string) (ifStatement: IfStatementSyntax) =
        let tryCastToBlockStatement (statementSyntax: StatementSyntax) =
            match statementSyntax with
            | :? BlockSyntax as blockSyntax -> Ok blockSyntax
            | _ -> 
                let errMsg = $"Expected the clause of an inline 'if expression to be \"{nameof(BlockSyntax)}\", received \"{statementSyntax.GetType()}\" instead"
                Error (InlineIfExpressionParseError(message=errMsg) :> CSharpParseError)
        
        let tryGetExpression (syntaxNode: CSharpSyntaxNode) : Result<ExpressionSyntax, CSharpParseError> =
            match syntaxNode with
            | :? ExpressionStatementSyntax as expressionStatement ->
                Ok expressionStatement.Expression
            | _ ->
                let errMsg = $"Expected an expression statement as the last statement in an inline 'if expression', received \"{syntaxNode.GetType()}\" instead"
                Error (InlineIfExpressionParseError(message=errMsg) :> CSharpParseError)
                
        try
            result {
                // #1. Asserts on given if statement
                do! match isNull ifStatement.Else with
                    | true -> Error (InlineIfExpressionParseError(message="An inline 'if expression' requires at least both branches of an if statement") :> CSharpParseError)
                    | false -> Ok ()
                
                // #2. Attempting to convert the last expression of each clause to an assignment statement (to the variable)
                let! primaryClause = tryCastToBlockStatement ifStatement.Statement
                let lastStatement = primaryClause.Statements.Item(primaryClause.Statements.Count - 1)
                let! primaryClauseLastExpr = tryGetExpression lastStatement
                let primaryAssignmentStatement = 
                    ExpressionStatement(
                        AssignmentExpression(
                            SyntaxKind.SimpleAssignmentExpression,
                            IdentifierName(variableName),
                            primaryClauseLastExpr
                            )
                        ) :> StatementSyntax
                
                let! elseClause = tryCastToBlockStatement ifStatement.Else.Statement
                let lastStatement = elseClause.Statements.Item(elseClause.Statements.Count - 1)
                let! elseClauseLastExpr = tryGetExpression lastStatement
                let elseAssignmentStatement = 
                    ExpressionStatement(
                        AssignmentExpression(
                            SyntaxKind.SimpleAssignmentExpression,
                            IdentifierName(variableName),
                            elseClauseLastExpr
                            )
                        ) :> StatementSyntax
                    
                // #3. Rebuilding the if statement
                let newPrimaryClauseStatements =
                    primaryClause.Statements
                    |> Seq.pairwise |> Seq.map fst  // get every item but last
                    |> (fun s -> Seq.append s (Seq.singleton primaryAssignmentStatement))   // append to end
                let newPrimaryClauseStatement = Block(newPrimaryClauseStatements)
                
                let newElseClauseStatements =
                    elseClause.Statements
                    |> Seq.pairwise |> Seq.map fst  // get every item but last
                    |> (fun s -> Seq.append s (Seq.singleton elseAssignmentStatement))   // append to end
                let newElseClauseStatement = Block(newElseClauseStatements)
                
                return IfStatement(
                    Token(SyntaxKind.IfKeyword),
                    Token(SyntaxKind.OpenParenToken),
                    ifStatement.Condition,
                    Token(SyntaxKind.CloseParenToken),
                    newPrimaryClauseStatement,
                    ElseClause(newElseClauseStatement))
            }
        with
        | ex ->
            Error (InlineIfExpressionParseError(message="An unexpected error occurred.") :> CSharpParseError)
    

    
let rec internal tryParseTypeSyntax
        (onInvalid: CSharpParseError)
        (parserState: CSharpAstParserState)
        : Result<TypeSyntax, CSharpParseError> =
            
    let currentToken = parserState.PopToken()
    match currentToken.Type, currentToken.Literal with
    | LBRACKET, "[" ->
        tryParseFunctionTypeSyntax [] onInvalid parserState
    | IDENT, "int" ->
        Token(SyntaxKind.IntKeyword)         |> PredefinedType |> (fun t -> t :> TypeSyntax) |> Ok
    | IDENT, "string" ->
        Token(SyntaxKind.StringKeyword)   |> PredefinedType |> (fun t -> t :> TypeSyntax) |> Ok
    | IDENT, _ ->
        IdentifierName(currentToken.Literal) :> TypeSyntax |> Ok
    | _ ->
        Error onInvalid
        
/// example input:  "int -> int -> int]"
/// the 'lbracket' is assumed to be consumed by the caller function
and private tryParseFunctionTypeSyntax 
        (funcSigTypes: TypeSyntax list)
        (onInvalid: CSharpParseError)
        (parserState: CSharpAstParserState)
        : Result<TypeSyntax, CSharpParseError> =
            
    // modified to also stop at rbrackets
    let assertAndPopAlt (expectedTokenType: TokenType) (parserState: CSharpAstParserState) =
        let isRBracket tokenType = tokenType = RBRACKET
        let isRbBracketOrSemicolon tokenType = isSemicolon tokenType || isRBracket tokenType
        
        let currentToken = parserState.PopToken()
        match currentToken.Type with
        | tokenType when tokenType = expectedTokenType -> Ok ()
        | _ -> consumeUntilTokenType isRbBracketOrSemicolon parserState |> ignore
               onUnexpectedToken expectedTokenType currentToken
            
    tryParseTypeSyntax onInvalid parserState
    |> function
        | Ok typeSyntax ->
            let newFuncSigTypes = typeSyntax :: funcSigTypes
            let peekToken = parserState.PeekToken()
            match peekToken.Type with
            | RBRACKET ->
                parserState.PopToken() |> ignore  // consume the ']'
                let typeSyntaxArr = newFuncSigTypes |> List.toArray |> Array.rev
                let commas = Array.create ((Array.length typeSyntaxArr) - 1) (Token(SyntaxKind.CommaToken))
                let typeSyntax = GenericName(Identifier("Func")).WithTypeArgumentList(TypeArgumentList(SeparatedList<TypeSyntax>(typeSyntaxArr, commas)))
                Ok typeSyntax
            | RARROW ->
                parserState.PopToken() |> ignore
                tryParseFunctionTypeSyntax newFuncSigTypes onInvalid parserState
            | tokenType ->
               Error (CSharpParseError(message=($"Invalid token type \"{tokenType}\" detected")))
        | Error parseError ->
            // TODO: see what you can do with composite errors
            Error parseError
    
    
    
    

    
[<AutoOpen>]
module internal PrefixExpressions =
    /// <summary>
    /// Consumes the current token and parses it as a string literal.
    /// </summary>
    let tryParseStringLiteral
            (parserState: CSharpAstParserState)
            : ExpressionSyntax =
        let currentToken = parserState.PopToken()
        SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, SyntaxFactory.Literal(currentToken.Literal))
        
        
    /// <summary>
    /// Consumes the current token and parses it as an identifier.
    /// </summary>
    let tryParseIdentifier
            (parserState: CSharpAstParserState)
            : ExpressionSyntax =
        let currentToken = parserState.PopToken()
        SyntaxFactory.IdentifierName(currentToken.Literal)
        
        
    let rec tryParseIdentifierOrCallExpression
            (parserState: CSharpAstParserState)
            : Result<ExpressionSyntax, CSharpParseError> =
                
        tryParseIdentifierOrCallExpressionHelper [] parserState
        
    and private tryParseIdentifierOrCallExpressionHelper
            (identifiers: IdentifierNameSyntax list)
            (parserState: CSharpAstParserState)
            : Result<ExpressionSyntax, CSharpParseError> =
        
        let currentToken = parserState.PopToken()
        let identifier = SyntaxFactory.IdentifierName(currentToken.Literal)
        
        match parserState.PeekToken().Type with
        | DOT ->
            parserState.PopToken() |> ignore
            tryParseIdentifierOrCallExpressionHelper (identifier :: identifiers) parserState
        | LPAREN ->
            tryParseFunctionCallExpression (identifier :: identifiers) parserState
            |> Result.map (fun invocationExpression -> invocationExpression :> ExpressionSyntax)
        | _ ->
            identifier :> ExpressionSyntax |> Ok
        
    and private tryParseFunctionCallExpression
            (identifiers: IdentifierNameSyntax list)
            (parserState: CSharpAstParserState)
            : Result<InvocationExpressionSyntax, CSharpParseError> =
                
        let rec buildMemberAccessExpressionTree (_identifiers: IdentifierNameSyntax list) : ExpressionSyntax =
            match _identifiers with
            | [] ->
                failwith "this is really unexpected"
            | [ last ] ->
                last :> ExpressionSyntax
            | head :: tail ->
                MemberAccessExpression(
                    SyntaxKind.SimpleMemberAccessExpression,
                    buildMemberAccessExpressionTree tail,
                    Token(SyntaxKind.DotToken),
                    head) 
                
        result {
            parserState.PopToken() |> ignore
            let! argumentListSyntax = tryParseFunctionCallArgumentsList [] parserState
            do! assertAndPop RPAREN parserState
            let expression = buildMemberAccessExpressionTree identifiers
            return InvocationExpression(expression, argumentListSyntax)
        }
        
    and private tryParseFunctionCallArgumentsList
            (arguments: ArgumentSyntax list)
            (parserState: CSharpAstParserState)
            : Result<ArgumentListSyntax, CSharpParseError> =
                
        let assertIsExpression (syntaxNode: CSharpSyntaxNode) : Result<ExpressionSyntax, CSharpParseError> =
            match syntaxNode with
            | :? ExpressionSyntax as expressionSyntax -> Ok expressionSyntax
            | _ -> FunctionCallExpressionParseError($"Expected an expression, but received \"{syntaxNode.GetType()}\"") :> CSharpParseError |> Error
                
        tryParseExpressionOrStatement parserState Precedence.LOWEST
        |> Result.bind assertIsExpression
        |> Result.map Argument
        |> function
           | Ok argument ->
               let newArguments = argument :: arguments
               match parserState.PeekToken().Type with
               | COMMA ->
                   parserState.PopToken() |> ignore  // consume the comma
                   tryParseFunctionCallArgumentsList newArguments parserState
               | RPAREN ->
                   let asArray = newArguments |> List.toArray |> Array.rev
                   SeparatedList<ArgumentSyntax>(asArray) |> ArgumentList |> Ok
               | tokenType ->
                   FunctionCallExpressionParseError($"Invalid token type \"{tokenType}\" detected") :> CSharpParseError |> Error
           | Error parseError ->
               Error parseError
        

    
    /// <summary>
    /// Consumes the current token and attempts to parse it as an integer number.
    /// </summary>
    let tryParseIntegerLiteral
            (parserState: CSharpAstParserState)
            : Result<ExpressionSyntax, CSharpParseError> =
                
        let getError (token: Token) =
            let literalParseError = LiteralExpressionParseError(SyntaxKind.NumericLiteralExpression, token=Some token, innerException=IntParseError(token.Literal))
            Error (CSharpParseError(innerException=literalParseError))
            
        result {
            let currentToken = parserState.PopToken()
            let intValueOption = tryParseInt currentToken.Literal
            let! intValue =
                match intValueOption with
                | Some value -> Ok value
                | None -> getError currentToken
            return SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(intValue))
        }

    
    /// <summary>
    /// Consumes the current token and attempts to parse it as a boolean type.
    /// </summary>
    let tryParseBooleanLiteral
            (parserState: CSharpAstParserState)
            : Result<ExpressionSyntax, CSharpParseError> =
                
        let getError (token: Token) =
            let literalParseError = LiteralExpressionParseError(message=($"Failed to parse boolean literal, expected a \"bool\" type, got \"{token.Type}\""), token=Some token)
            Error (CSharpParseError(innerException=literalParseError))
                
        result {
            let currentToken = parserState.PopToken()
            let! syntaxKind =
                match currentToken.Type with
                | TRUE ->  Ok SyntaxKind.TrueLiteralExpression
                | FALSE -> Ok SyntaxKind.FalseLiteralExpression
                | _ ->     getError currentToken
            return SyntaxFactory.LiteralExpression(syntaxKind)
        }
    
    
    /// <summary>
    /// Consumes the current token and attempts to parse it as an expression surrounded by parentheses.
    /// </summary>
    let tryParseParenthesisExpression
            (parserState: CSharpAstParserState)
            : Result<ExpressionSyntax, CSharpParseError> =
        result {
            parserState.PopToken() |> ignore  // consume the left paren
            
            let! csSyntaxNode = tryParseExpressionOrStatement parserState Precedence.LOWEST
            let! expr =
                match csSyntaxNode with
                | :? ExpressionSyntax as expr -> Ok expr
                | _ -> Error (CSharpParseError())  // TODO
                
            let wrappedExpr = SyntaxFactory.ParenthesizedExpression(SyntaxFactory.Token(SyntaxKind.OpenParenToken), expr, SyntaxFactory.Token(SyntaxKind.CloseParenToken))
            consumeUntilTokenType (fun tt -> isSemicolon tt || isRParen tt) parserState |> ignore
            
            parserState.PopToken() |> ignore  // consume the right paren
            return wrappedExpr
        }
        
        
    /// <summary>
    /// Consumes the current token and attempts to parse it as a prefix expression.
    /// </summary>
    /// <remarks>
    /// <ul>
    /// <li>Note that the expression is not tied to a specific type, and can handle multiple expression types.</li>
    /// </ul>
    /// </remarks>
    let tryParsePrefixExpression
            (parserState: CSharpAstParserState)
            : Result<ExpressionSyntax, CSharpParseError> =
                
        let getError (token: Token) =
            let invalidPrefixOperatorError = InvalidPrefixOperatorError(token=token)
            let literalParseError = LiteralExpressionParseError(message=($"Failed to parse prefix expression with type \"{token.Type}\""), token=Some token, innerException=invalidPrefixOperatorError)
            Error (CSharpParseError(innerException=literalParseError))
            
        result {
            let currentToken = parserState.PopToken()
            let! csSyntaxNode = tryParseExpressionOrStatement parserState Precedence.PREFIX
            let! rightExpr =
                match csSyntaxNode with
                | :? ExpressionSyntax as expr -> Ok expr
                | _ -> Error (CSharpParseError())  // TODO
                
            let! syntaxKind = 
                match currentToken.Type with
                | BANG -> Ok SyntaxKind.LogicalNotExpression
                | MINUS -> Ok SyntaxKind.UnaryMinusExpression
                | _ -> getError currentToken
            return SyntaxFactory.PrefixUnaryExpression(syntaxKind, rightExpr) :> ExpressionSyntax
        }
        
        
    /// <summary>
    /// Parses an if expression (can be-inline).
    /// </summary>
    let rec tryParseIfExpression
        (parserState: CSharpAstParserState)
        : Result<IfStatementSyntax, CSharpParseError> =
        result {
            parserState.PopToken() |> ignore  // 'if' keyword
            
            // #1. Parsing the condition
            let currentToken = parserState.PopToken()
            do! match currentToken.Type with
                | LPAREN -> Ok ()
                | _ -> consumeUntilTokenType isSemicolon parserState |> ignore
                       onUnexpectedToken LPAREN currentToken

            let! csSyntaxNode = tryParseExpressionOrStatement parserState Precedence.LOWEST
            let! condition =
                match csSyntaxNode with
                | :? ExpressionSyntax as expr -> Ok expr
                | other -> Error (CSharpParseError(message=($"Expected an expression for the condition, but received \"{other.GetType()}\"")))
                
            let currentToken = parserState.PopToken()
            do! match currentToken.Type with
                | RPAREN -> Ok ()
                | _ -> consumeUntilTokenType isSemicolon parserState |> ignore
                       onUnexpectedToken RPAREN currentToken
                       
                       
            // #2. Parsing the main block
            let currentToken = parserState.PopToken()
            do! match currentToken.Type with
                | LBRACE -> Ok ()
                | _ -> consumeUntilTokenType isSemicolon parserState |> ignore
                       onUnexpectedToken LBRACE currentToken
                       
            let stopCondition (parserState: CSharpAstParserState) = parserState.PeekToken().Type = TokenType.RBRACE || parserState.IsEof()
            let statements, parseErrors = parseTokensHelper stopCondition parserState [] []
            do! assertNoParseErrors parseErrors
            
            let mainBlock = Block(List.toArray statements)
            
            let currentToken = parserState.PopToken()
            do! match currentToken.Type with
                | RBRACE -> Ok ()
                | _ -> consumeUntilTokenType isSemicolon parserState |> ignore
                       onUnexpectedToken RBRACE currentToken
                       
                       
            // #3. Parsing the else statement, if any
            let! elseClause =
                match parserState.PeekToken().Type with
                | ELSE ->
                    parserState.PopToken() |> ignore
                    Result.map Some (parseElseBlock parserState)
                | _ ->
                    Ok None
                    
            let elseClauseNullable : ElseClauseSyntax | null =
                match elseClause with
                | None -> null
                | Some value -> value
                    
            // #4. Construct the thang
            let ifStatement = 
                IfStatement(
                    Token(SyntaxKind.IfKeyword),
                    Token(SyntaxKind.OpenParenToken),
                    condition,
                    Token(SyntaxKind.CloseParenToken),
                    mainBlock,
                    elseClauseNullable)
            return ifStatement
        }
        
        
    // TODO:
    // Right now, parsing a block can yield multiple parse errors, but we are only checking for one cause I don't
    // want to change the API as of right now 2025/03/11, 5:31PM
        
    and private parseElseBlock (parserState: CSharpAstParserState) : Result<ElseClauseSyntax, CSharpParseError> =
        result {
            let currentToken = parserState.PopToken()
            do! match currentToken.Type with
                | LBRACE -> Ok ()
                | _ -> consumeUntilTokenType isSemicolon parserState |> ignore
                       onUnexpectedToken LBRACE currentToken
            
            let stopCondition (parserState: CSharpAstParserState) = parserState.PeekToken().Type = TokenType.RBRACE || parserState.IsEof()
            let statements, parseErrors = parseTokensHelper stopCondition parserState [] []
            do! assertNoParseErrors parseErrors
            
            let currentToken = parserState.PopToken()
            do! match currentToken.Type with
                | RBRACE -> Ok ()
                | _ -> consumeUntilTokenType isSemicolon parserState |> ignore
                       onUnexpectedToken RBRACE currentToken
                       
            let asBlock = Block(List.toArray statements)
            let elseClause = ElseClause(Token(SyntaxKind.ElseKeyword), asBlock)
            return elseClause
        }
        
        
    /// <summary>
    /// Parses a function literal to its C# equivalent. Below are sample equivalent examples:
    /// <br/>
    /// Monkey:
    /// <code>
    /// fn(int x, int y) : int {
    ///     let z = 10;
    ///     x + y + z;
    /// }
    /// </code>
    ///
    /// C# equivalent:
    /// <code>
    /// (int)((int x, int y) =>
    /// {
    ///     int z = 10;
    ///     return x + y + z;
    /// });
    /// </code>
    /// </summary>
    /// <remarks>
    /// Note that the 'cast' in the C# equivalent code 'enforces' the return type of our declared function.
    /// </remarks>
    let rec tryParseFunctionExpression
        (parserState: CSharpAstParserState)
        : Result<ParenthesizedLambdaExpressionSyntax, CSharpParseError> =
            
            
        result {
            parserState.PopToken() |> ignore  // consume 'fn' keyword
            
            
            // #1. parse parameters list
            do! assertAndPop LPAREN parserState
            
            let! syntaxSeparatedList =
                match parserState.PeekToken().Type with
                | RPAREN -> Ok (SeparatedSyntaxList<ParameterSyntax>())  // no parameters
                | _ -> parseArgumentsList [] parserState
            let parameterList = ParameterList(syntaxSeparatedList)
            do! assertAndPop RPAREN parserState
            
            
            // #2. parse return type
            do! assertAndPop COLON parserState
            let retTypeToken = parserState.PeekToken()
            let onInvalidReturnTypeError = CSharpParseError(message=($"Expected a valid return type, but received \"{retTypeToken.Literal}\""))
            let! returnTypeSyntax = tryParseTypeSyntax onInvalidReturnTypeError parserState
            
            let isUnitReturn =  // equivalent to 'void' method
                match retTypeToken.Type, retTypeToken.Literal with
                | IDENT, "unit" -> true
                | _ -> false
                
                
            // #3. parse function block
            do! assertAndPop LBRACE parserState
            
            let stopCondition (parserState: CSharpAstParserState) = parserState.PeekToken().Type = TokenType.RBRACE || parserState.IsEof()
            let statements, parseErrors = parseTokensHelper stopCondition parserState [] []
            do! assertNoParseErrors parseErrors
            
            let! modifiedStatements =
                match isUnitReturn with
                | true -> statements |> appendUnitReturn |> Ok
                | false -> tryTransformLastStatementToReturn statements
                
            
            let asBlock = modifiedStatements |> Block
            
            do! assertAndPop RBRACE parserState
            
            
            // #4. create a type signature for the function (since it's a lambda, there's no inherent property for typing)
            let parameterTypes =
                parameterList.Parameters
                |> Seq.choose (fun p -> if isNull p.Type then None else Some p.Type)
                |> Seq.toArray
            
            let functionSignature =
                { ParameterTypes = parameterTypes
                  ReturnType = returnTypeSyntax }
                
            let functionHashId = generateRandomStringHash defaultHashLen
            parserState.LambdaTypeMap <- parserState.LambdaTypeMap.Add(functionHashId, functionSignature)
            
            let lambdaExpression =
                ParenthesizedLambdaExpression(parameterList, asBlock)
                    .WithAdditionalAnnotations(SyntaxAnnotation("FunctionSignature", functionHashId))
                    
            return lambdaExpression
        }
        
    /// Expected input "TYPEDEF_1 ARG_NAME_1, TYPEDEF_2 ARG_NAME_2, ..., TYPEDEF_N ARG_NAME_N) ... ~ REST OF THE TOKENS"
    and private parseArgumentsList
            (parameters: ParameterSyntax list)
            (parserState: CSharpAstParserState)
            : Result<SeparatedSyntaxList<ParameterSyntax>, CSharpParseError> =
        result {
            let currentToken = parserState.PeekToken()
            let onInvalidTypeSyntaxError = CSharpParseError(message=($"Expected a valid return type, but received \"{currentToken.Literal}\""))
            let! typeSyntax = tryParseTypeSyntax onInvalidTypeSyntaxError parserState
            
            let currentToken = parserState.PopToken()
            let! argNameToken = 
                match currentToken.Type with
                | IDENT ->  Identifier(currentToken.Literal)    |> Ok
                | _ ->      Error (CSharpParseError(message=($"[arg #{List.length parameters}] Expected an identifier for the argument name, but received \"{currentToken.Literal}\"")))
            
            return Parameter(argNameToken).WithType(typeSyntax)
        }
        |> function
           | Ok parameterSyntax ->
               let newParameters = parameterSyntax :: parameters
               match parserState.PeekToken().Type with
               | COMMA ->
                   parserState.PopToken() |> ignore  // consume the comma
                   parseArgumentsList newParameters parserState
               | RPAREN ->
                   Ok (SeparatedList<ParameterSyntax>(List.rev newParameters))
               | tokenType ->
                   Error (CSharpParseError(message=($"Invalid token type \"{tokenType}\" detected")))
           | Error error ->
               Error error

    /// Transforms the last statement into a return statement if applicable, errors otherwise
    and private tryTransformLastStatementToReturn (statements: StatementSyntax seq) : Result<StatementSyntax seq, CSharpParseError> =
        let everythingButLastStatement = statements |> Seq.pairwise |> Seq.map fst
        let lastStatement = Seq.last statements
        
        match lastStatement with
        | :? ExpressionStatementSyntax as expressionStatementSyntax ->
            let newLastStatement = expressionStatementSyntax.Expression |> ReturnStatement
            let newStatements = Seq.append everythingButLastStatement (Seq.singleton newLastStatement)
            newStatements |> Ok
        | _ -> 
            Error (CSharpParseError(message=($"Expected the last statement of a function to be a return statement or an expression statement, but got \"{lastStatement.GetType()}\"")))
            
    /// Adds a 'unit' return to the end of the statements
    and private appendUnitReturn (statements: StatementSyntax seq) : StatementSyntax seq =
        let unitReturnStatement =
            ReturnStatement(
                ObjectCreationExpression(
                    Token(SyntaxKind.NewKeyword),
                    IdentifierName("unit"),
                    ArgumentList(),
                    null)
                )
        Seq.append statements (Seq.singleton unitReturnStatement)
        
        
        
    let private castToCSharpSyntaxNode (result: Result<'T, 'E>) : Result<CSharpSyntaxNode, 'E> =
        Result.map (fun t -> t :> CSharpSyntaxNode) result
        
    /// <summary>
    /// Map containing token types and the corresponding function required to parse them.
    /// </summary>
    let prefixParseFunctionsMap
        : Map<TokenType, CSharpAstParserState -> Result<CSharpSyntaxNode, CSharpParseError>> =
        Map.ofList [
            // (TokenType.IDENT,   tryParseIdentifier              >> Ok >> castToCSharpSyntaxNode)
            (TokenType.IDENT,   tryParseIdentifierOrCallExpression  >> castToCSharpSyntaxNode)
            
            (TokenType.STRING,  tryParseStringLiteral               >> Ok >> castToCSharpSyntaxNode)
            (TokenType.INT,     tryParseIntegerLiteral              >> castToCSharpSyntaxNode)
            (TokenType.TRUE,    tryParseBooleanLiteral              >> castToCSharpSyntaxNode)
            (TokenType.FALSE,   tryParseBooleanLiteral              >> castToCSharpSyntaxNode)
            (TokenType.BANG,    tryParsePrefixExpression            >> castToCSharpSyntaxNode)
            (TokenType.MINUS,   tryParsePrefixExpression            >> castToCSharpSyntaxNode)
            (TokenType.LPAREN,  tryParseParenthesisExpression       >> castToCSharpSyntaxNode)
            (TokenType.IF,      tryParseIfExpression                >> castToCSharpSyntaxNode)
            (TokenType.FUNCTION, tryParseFunctionExpression         >> castToCSharpSyntaxNode)
            
            (*
            (TokenType.LBRACKET, tryParseArrayLiteral)
            (TokenType.LBRACE, tryParseHashLiteral)
            *)
        ]
        
    /// Attempts to get the INFIX parse function based on the next token's type
    let tryGetInfixParseFunc
            (infixParseFuncMap: Map<TokenType, CSharpAstParserState -> ExpressionSyntax -> Result<ExpressionSyntax, CSharpParseError>>)
            (parserState: CSharpAstParserState)
            : Result<CSharpAstParserState -> ExpressionSyntax -> Result<ExpressionSyntax, CSharpParseError>, CSharpParseError> =
                
        match parserState.IsEof() with
        | true ->
            let message = "FATAL. Tokens queue empty. This indicates a logical error in the parsing process."
            failwith message
            // TODO HERE MAYBE
        | false ->
            let onMissingValue parserState =  // i.e. basically go to the next statement
                consumeUntilTokenType isSemicolon parserState |> ignore  // pass by reference, properties change 'in-place'
                CSharpParseError()
                
            let token = parserState.PeekToken()
            match Map.tryFind token.Type infixParseFuncMap with
            | Some value -> Ok value
            | None -> Error (onMissingValue parserState)
        
        
        
[<AutoOpen>]
module internal InfixExpressions =
    
    let tryParseInfixExpression (parserState: CSharpAstParserState) (leftExpr: ExpressionSyntax) : Result<ExpressionSyntax, CSharpParseError> =
        let getError (token: Token) =
            let invalidInfixOperatorError = InvalidInfixOperatorError(token=token)
            let literalParseError = LiteralExpressionParseError(message="Failed to parse infix expression", token=Some token, innerException=invalidInfixOperatorError)
            Error (CSharpParseError(innerException=literalParseError))
        
        result {
            let token = parserState.PopToken()
            let precedence = 
                match Map.tryFind token.Type tokenTypeToPrecedenceMap with
                | Some precedence -> precedence
                | None -> Precedence.LOWEST
                
            let! syntaxKindAndOperatorToken =
                match token.Type with
                | PLUS ->       Ok (SyntaxKind.AddExpression,           SyntaxFactory.Token(SyntaxKind.PlusToken))
                | MINUS ->      Ok (SyntaxKind.SubtractExpression,      SyntaxFactory.Token(SyntaxKind.MinusToken))
                | ASTERISK ->   Ok (SyntaxKind.MultiplyExpression,      SyntaxFactory.Token(SyntaxKind.AsteriskToken))
                | SLASH ->      Ok (SyntaxKind.DivideExpression,        SyntaxFactory.Token(SyntaxKind.SlashToken))
                | GT ->         Ok (SyntaxKind.GreaterThanExpression,   SyntaxFactory.Token(SyntaxKind.GreaterThanToken))
                | LT ->         Ok (SyntaxKind.LessThanExpression,      SyntaxFactory.Token(SyntaxKind.LessThanToken))
                | EQ ->         Ok (SyntaxKind.EqualsExpression,        SyntaxFactory.Token(SyntaxKind.EqualsEqualsToken))
                | NOT_EQ ->     Ok (SyntaxKind.NotEqualsExpression,     SyntaxFactory.Token(SyntaxKind.ExclamationEqualsToken))
                | _ -> getError token
                    
            let syntaxKind, operatorToken = syntaxKindAndOperatorToken
            let! csSyntaxNode = tryParseExpressionOrStatement parserState precedence
            let! rightExpr =
                match csSyntaxNode with
                | :? ExpressionSyntax as expr -> Ok expr
                | _ -> Error (CSharpParseError())  // TODO
                
            return SyntaxFactory.BinaryExpression(syntaxKind, leftExpr, operatorToken, rightExpr)
        }
        
        
    let infixParseFunctionsMap
        : Map<TokenType, CSharpAstParserState -> ExpressionSyntax -> Result<ExpressionSyntax, CSharpParseError>> =
        Map.ofList [
            (*
            (TokenType.LPAREN, tryParseCallExpression) // parse call expr
            (TokenType.LBRACKET, tryParseIndexExpression) // parse index expr 
            *)
            
            (TokenType.PLUS, tryParseInfixExpression)
            (TokenType.MINUS, tryParseInfixExpression)
            (TokenType.SLASH, tryParseInfixExpression)
            (TokenType.ASTERISK, tryParseInfixExpression)
            (TokenType.EQ, tryParseInfixExpression)
            (TokenType.NOT_EQ, tryParseInfixExpression)
            (TokenType.LT, tryParseInfixExpression)
            (TokenType.GT, tryParseInfixExpression)
        ]
        
        
    /// Attempts to get the PREFIX parse function based on the next token's type
    let tryGetPrefixParseFunc prefixParseFuncMap parserState : Result<CSharpAstParserState -> Result<CSharpSyntaxNode, CSharpParseError>, CSharpParseError> =
        match parserState.IsEof() with
        | true ->
            let message = "FATAL. Tokens queue empty. This indicates a logical error in the parsing process."
            failwith message
            // TODO HERE MAYBE
        | false ->
            let token = parserState.PeekToken()
            
            let onMissingValue parserState =  // i.e. basically go to the next statement
                consumeUntilTokenType isSemicolon parserState |> ignore  // pass by reference, properties change 'in-place'
                CSharpParseError($"Could not find a prefix parse function for the token type \"{token.Type}\"")
                
            match Map.tryFind token.Type prefixParseFuncMap with
            | Some value -> Ok value
            | None -> Error (onMissingValue parserState)
