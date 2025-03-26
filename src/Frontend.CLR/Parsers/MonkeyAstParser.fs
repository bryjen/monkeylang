// ReSharper disable FSharpRedundantParens
[<RequireQualifiedAccess>]
module rec Monkey.Frontend.CLR.Parsers.MonkeyAstParser

open Frontend.CLR.Parsers.ParsingErrors.ParameterListErrors
open Microsoft.CodeAnalysis.CSharp

open FsToolkit.ErrorHandling

open Monkey.Frontend.CLR.Syntax.Ast
open Monkey.Frontend.CLR.Parsers.ParsingErrors
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeySyntaxTokenFactory
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeyStatementSyntaxFactory
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeyExpressionSyntaxFactory


        

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
        parserState |> tryParseLetStatement |> (Result.map Some)
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
                | _ ->
                    parserState.RecoverFromParseError()
                    AbsentSemicolonError(expression.TextSpan(), AbsentSemicolonAt.ExpressionStatement) :> ParseError |> Error
            
            return { Expression = expression; SemicolonToken = semicolonToken } |> StatementSyntax.ExpressionStatementSyntax
        }
        
    
    let rec internal tryParseLetStatement (parserState: MonkeyAstParserState) : Result<StatementSyntax, ParseError> =
        result {
            let letKeywordToken = parserState.PopToken()
            
            // #1. parse variable name
            let! variableName = 
                match parserState.PopToken() with
                | token when token.Kind = SyntaxKind.IdentifierToken ->
                    Ok token
                | token ->
                    parserState.RecoverFromParseError()
                    VariableAssignmentStatementErrors.InvalidVariableNameError(token) :> ParseError |> Error
                    
                    
            // #2. trying to parse explicit type annotation, if it exists
            let! explicitTypeSyntaxOption =
                match parserState.PeekToken() with
                | token when token.Kind = SyntaxKind.ColonToken ->
                    let colonToken = parserState.PopToken()
                    let explicitTypeSyntaxResult = tryParseTypeSyntax (PlaceholderError()) parserState
                    
                    explicitTypeSyntaxResult
                    |> Result.map (fun typeSyntax -> { ColonToken = colonToken; Type = typeSyntax })
                    |> Result.map Some
                | _ ->
                    Ok None
                    
                    
            // #3. asserting equals token
            let! equalsToken = 
                match parserState.PopToken() with
                | token when token.Kind = SyntaxKind.EqualsToken ->
                    Ok token
                | _ ->
                    parserState.RecoverFromParseError()
                    let span =
                        match explicitTypeSyntaxOption with
                        | Some value -> value.Type.TextSpan()
                        | None -> variableName.TextSpan
                    VariableAssignmentStatementErrors.AbsentEqualsError(span) :> ParseError |> Error
                    
                    
            // #4. parsing actual expression
            let! expression = tryParseExpression parserState Precedence.LOWEST
            
            let! semicolonToken = 
                match parserState.PeekToken() with
                | token when token.Kind = SyntaxKind.SemicolonToken ->
                    parserState.PopToken() |> Ok
                | _ ->
                    parserState.RecoverFromParseError()
                    AbsentSemicolonError(expression.TextSpan(), AbsentSemicolonAt.LetStatement) :> ParseError |> Error
                    
                    
            return VariableDeclarationStatement(letKeywordToken, variableName, equalsToken, expression, semicolonToken, explicitTypeSyntaxOption)
        }


[<AutoOpen>]
module internal PrefixExpressions =
    
    let tryParseIdentifier (parserState: MonkeyAstParserState) : Result<IdentifierSyntax, ParseError> =
        let rec tryParseIdentifierCore
                (identifiers: SyntaxToken list)
                (dots: SyntaxToken list)
                : Result<SyntaxToken array * SyntaxToken array, ParseError> =
            match parserState.PeekToken() with
            | token when token.Kind = SyntaxKind.IdentifierToken ->
                let identifierToken = parserState.PopToken()
                match parserState.PeekToken() with
                | nextToken when nextToken.Kind = SyntaxKind.DotToken ->
                    let dotToken = parserState.PopToken()
                    tryParseIdentifierCore (identifierToken :: identifiers) (dotToken :: dots)
                | _ -> 
                    let identifiersArr = (identifierToken :: identifiers) |> List.toArray |> Array.rev
                    let dotsArr = dots |> List.toArray |> Array.rev
                    Ok (identifiersArr, dotsArr)
            | _ ->
                let identifiersArr = identifiers |> List.toArray |> Array.rev
                let dotsArr = dots |> List.toArray |> Array.rev
                Ok (identifiersArr, dotsArr)
                
        result {
            // in case the core helper method returns an empty array, for whatever reason, this will be the base case
            let backupToken = parserState.PeekToken()
            
            let! identifierParseResults = tryParseIdentifierCore [] []
            let identifiers, dots = identifierParseResults
            return
                match identifiers with
                | [| |] -> 
                    { Token = backupToken }
                    |> IdentifierSyntax.SimpleIdentifier
                | [| token |] -> 
                    { Token = token }
                    |> IdentifierSyntax.SimpleIdentifier
                | arr ->
                    { Tokens = arr; Dots = dots }
                    |> IdentifierSyntax.QualifiedIdentifier
        }
        
    let tryParseStringLiteralExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        let currentToken = parserState.PopToken()
        { Kind = SyntaxKind.StringLiteralExpression; Token = currentToken } |> ExpressionSyntax.LiteralExpressionSyntax |> Ok
        
    let tryParseNumericLiteralExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        let currentToken = parserState.PopToken()
        { Kind = SyntaxKind.NumericLiteralExpression; Token = currentToken } |> ExpressionSyntax.LiteralExpressionSyntax |> Ok
        
    let tryParseTrueLiteralExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        let currentToken = parserState.PopToken()
        let updatedToken = { currentToken with Value = true }
        { Kind = SyntaxKind.TrueLiteralExpression; Token = updatedToken } |> ExpressionSyntax.LiteralExpressionSyntax |> Ok
        
    let tryParseFalseLiteralExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        let currentToken = parserState.PopToken()
        let updatedToken = { currentToken with Value = false }
        { Kind = SyntaxKind.FalseLiteralExpression; Token = updatedToken } |> ExpressionSyntax.LiteralExpressionSyntax |> Ok
        
    let tryParseLogicalNotPrefixExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        result {
            let operatorToken = parserState.PopToken()
            let! expression = tryParseExpression parserState Precedence.PREFIX
            return LogicalNotPrefixExpression(expression, operatorToken)
        }
        
    let tryParseMinusPrefixExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        result {
            let operatorToken = parserState.PopToken()
            let! expression = tryParseExpression parserState Precedence.PREFIX
            return MinusPrefixExpression(expression, operatorToken)
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
                | _ ->
                    parserState.RecoverFromParseError()
                    AbsentOrInvalidTokenError(ifKeywordToken.TextSpan, [| SyntaxKind.OpenParenToken |], AbsentTokenAt.IfExpression) :> ParseError |> Error

            let! conditionExpression = tryParseExpression parserState Precedence.LOWEST
                
            let closeParenToken = parserState.PopToken()
            do! match closeParenToken.Kind with
                | SyntaxKind.CloseParenToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    AbsentOrInvalidTokenError(conditionExpression.TextSpan(), [| SyntaxKind.CloseParenToken |], AbsentTokenAt.IfExpression) :> ParseError |> Error
                       
            // #2. Parsing the main block
            let openBraceToken = parserState.PopToken()
            do! match openBraceToken.Kind with
                | SyntaxKind.OpenBraceToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    AbsentOrInvalidTokenError(closeParenToken.TextSpan, [| SyntaxKind.OpenBraceToken |], AbsentTokenAt.IfExpression) :> ParseError |> Error
                       
            let stopCondition (parserState: MonkeyAstParserState) = parserState.PeekToken().Kind = SyntaxKind.CloseBraceToken || parserState.IsEof()
            let statements, parseErrors = enterNewScopeAndParseTokens stopCondition parserState
            do! assertNoParseErrors parseErrors
            
            let closeBraceToken = parserState.PopToken()
            do! match closeBraceToken.Kind with
                | SyntaxKind.CloseBraceToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    let textSpan =
                        match statements with
                        | [| |] -> openBraceToken.TextSpan
                        | arr -> arr[arr.Length - 1].TextSpan()
                    AbsentOrInvalidTokenError(textSpan, [| SyntaxKind.CloseBraceToken |], AbsentTokenAt.IfExpression) :> ParseError |> Error
                       
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
                | _ ->
                    parserState.RecoverFromParseError()
                    AbsentOrInvalidTokenError(elseKeywordToken.TextSpan, [| SyntaxKind.OpenBraceToken |], AbsentTokenAt.IfExpression) :> ParseError |> Error
                       
            let stopCondition (parserState: MonkeyAstParserState) = parserState.PeekToken().Kind = SyntaxKind.CloseBraceToken || parserState.IsEof()
            let statements, parseErrors = enterNewScopeAndParseTokens stopCondition parserState
            do! assertNoParseErrors parseErrors
            
            let closeBraceToken = parserState.PopToken()
            do! match closeBraceToken.Kind with
                | SyntaxKind.CloseBraceToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    let textSpan =
                        match statements with
                        | [| |] -> openBraceToken.TextSpan
                        | arr -> arr[arr.Length - 1].TextSpan()
                    AbsentOrInvalidTokenError(textSpan, [| SyntaxKind.CloseBraceToken |], AbsentTokenAt.IfExpression) :> ParseError |> Error
                       
            let elseBlock = { OpenBraceToken = openBraceToken; Statements = statements; CloseBraceToken = closeBraceToken }
            return ElseClause(elseKeywordToken, elseBlock)
        }
        
        
    
    let rec internal tryParseTypeSyntax (onInvalid: ParseError) (parserState: MonkeyAstParserState) : Result<TypeSyntax, ParseError> =
        result {
            let! typeSyntax = 
                match parserState.PeekToken() with
                | token when token.Kind = SyntaxKind.OpenBracketToken ->
                    result {
                        let currentToken = parserState.PopToken()
                        let openBracketToken = currentToken
                        let! functionParsingResults = tryParseFunctionTypeSyntax [] [] onInvalid parserState
                        let types, arrows = functionParsingResults
                        let closeBracketToken = parserState.PopToken()  // we assert this from 'tryParseFunctionTypeSyntax'
                        return FunctionType(openBracketToken, types, arrows, closeBracketToken)
                    }
                | token when token.Kind = SyntaxKind.IntKeyword
                             || token.Kind = SyntaxKind.StringKeyword
                             || token.Kind = SyntaxKind.BoolKeyword ->
                    let currentToken = parserState.PopToken()
                    BuiltinType(currentToken) |> Ok
                | token when token.Kind = SyntaxKind.IdentifierToken ->
                    tryParseIdentifier parserState
                    |> Result.map NameType
                    
                    (*
                    let currentToken = parserState.PopToken()
                    NameType(IdentifierNameNoBox(currentToken)) |> Ok
                    *)
                | _ ->
                    Error onInvalid
                    
            return! typeSyntaxFurtherProcessing onInvalid parserState typeSyntax
        }
        
    /// helper method to process types with postfix context tokens.
    /// Ex. when parsing array types, the brackets come after the type itself is declared. In this method, we process
    /// the prefix/preceding type to the composite type including the postfix token context.
    /// <br/><br/>
    /// Recursively calls itself to handle nested cases, ex: 'int[][][]'
    and private typeSyntaxFurtherProcessing (onInvalid: ParseError) (parserState: MonkeyAstParserState) (typeSyntax: TypeSyntax) : Result<TypeSyntax, ParseError> =
        match parserState.PeekToken() with
        | token when token.Kind = SyntaxKind.OpenBracketToken ->  // array case
            tryParseArrayType onInvalid parserState typeSyntax
            |> Result.bind (typeSyntaxFurtherProcessing onInvalid parserState)
        | token when token.Kind = SyntaxKind.LessThanToken ->  // generic case
            result {
                let lessThanToken = parserState.PopToken()
                let! genericTypeParseResults = tryParseGenericType onInvalid [] [] parserState
                let types, commas = genericTypeParseResults
                
                let! greaterThanToken = 
                    match parserState.PeekToken() with
                    | token when token.Kind = SyntaxKind.GreaterThanToken ->
                        parserState.PopToken() |> Ok
                    | _ ->
                        parserState.RecoverFromParseError()
                        let textSpan =
                            match types with
                            | [| |] -> lessThanToken.TextSpan
                            | arr -> arr[arr.Length - 1].TextSpan()
                        AbsentOrInvalidTokenError(textSpan, [| SyntaxKind.GreaterThanToken |], AbsentTokenAt.GenericTypeSyntax) :> ParseError |> Error
                
                return GenericType(typeSyntax, types, commas, lessThanToken, greaterThanToken)
            }
            |> Result.bind (typeSyntaxFurtherProcessing onInvalid parserState)
        | _ ->
            Ok typeSyntax  // base case
        
    /// Expected input ""
    /// <example>
    /// example input:
    /// <code>
    /// TYPE_1, TYPE_2, ..., TYPE_N
    /// </code>
    /// the 'closeParenToken' is assumed to be consumed by the caller function
    /// </example>
    /// <returns>
    /// <code>(TypeSyntax array) * (SyntaxToken array)</code>
    /// (types * commas between those expressions)
    /// </returns>
    and private tryParseGenericType
        (onInvalid: ParseError)
        (commas: SyntaxToken list)
        (types: TypeSyntax list)
        (parserState: MonkeyAstParserState)
        : Result<(TypeSyntax array) * (SyntaxToken array), ParseError> =
            
        tryParseTypeSyntax onInvalid parserState
        |> function
           | Ok typeSyntax ->
               let newTypes = typeSyntax :: types
               match parserState.PeekToken().Kind with
               | SyntaxKind.CommaToken ->
                   let commaToken = parserState.PopToken()
                   tryParseGenericType onInvalid (commaToken :: commas) newTypes parserState
               | SyntaxKind.GreaterThanToken ->
                   let typesArr = newTypes |> List.toArray |> Array.rev
                   let commasArr = commas |> List.toArray |> Array.rev
                   Ok (typesArr, commasArr)
               | _ ->
                    parserState.RecoverFromParseError()
                    AbsentOrInvalidTokenError(typeSyntax.TextSpan(), [| SyntaxKind.CommaToken; SyntaxKind.GreaterThanToken |], AbsentTokenAt.GenericTypeSyntax) :> ParseError |> Error
           | Error error ->
               Error error
            
    /// <example>
    /// example input:
    /// <code>
    /// "`SOME_TYPE`[]"
    /// </code>
    /// the 'SOME_TYPE' is assumed to be consumed by the caller function. Called when the type is followed by an open
    /// bracket token.
    /// </example>
    and tryParseArrayType (onInvalid: ParseError) (parserState: MonkeyAstParserState) (typeSyntax: TypeSyntax) : Result<TypeSyntax, ParseError> =
        let openBracketToken = parserState.PopToken()
        match parserState.PeekToken() with
        | token when token.Kind = SyntaxKind.CloseBracketToken ->
            let closeBracketToken = parserState.PopToken()
            ArrayType(typeSyntax, openBracketToken, closeBracketToken) |> Ok
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
                    let typeSyntaxArr = newFuncSigTypes |> List.toArray |> Array.rev
                    let arrowTokensArr = arrowTokens |> List.toArray |> Array.rev
                    Ok (typeSyntaxArr, arrowTokensArr)
                | SyntaxKind.MinusGreaterThanToken ->  // arrow token '->'
                    let arrowToken = parserState.PopToken()
                    tryParseFunctionTypeSyntax (arrowToken :: arrowTokens) newFuncSigTypes onInvalid parserState
                | _ ->
                    parserState.RecoverFromParseError()
                    AbsentOrInvalidTokenError(typeSyntax.TextSpan(), [| SyntaxKind.CloseBracketToken; SyntaxKind.MinusGreaterThanToken |], AbsentTokenAt.IfExpression) :> ParseError |> Error
            | Error parseError ->
                Error parseError

    
    let rec tryParseFunctionExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        result {
            let fnKeywordToken = parserState.PopToken()  // we assert this from the syntax kind to (prefix) parse function map
            
            // #1. parse parameters list
            let openParenToken = parserState.PopToken()
            do! match openParenToken.Kind with
                | SyntaxKind.OpenParenToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    FunctionExpressionErrors.UnexpectedTokenError(fnKeywordToken, "(") :> ParseError |> Error
            
            let! syntaxSeparatedList =
                match parserState.PeekToken().Kind with
                | SyntaxKind.CloseParenToken -> Ok ([||], [||])  // no parameters
                | _ -> parseParametersList [] [] parserState
                
            let closeParenToken = parserState.PopToken()
            do! match closeParenToken.Kind with
                | SyntaxKind.CloseParenToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    FunctionExpressionErrors.UnexpectedTokenError(openParenToken, ")") :> ParseError |> Error
                    
            let parameterSyntaxArr, commas = syntaxSeparatedList
            let parameterList = ParameterList(openParenToken, parameterSyntaxArr, commas, closeParenToken)
            
            
            // #2. parse return type
            let colonToken = parserState.PopToken()
            do! match colonToken.Kind with
                | SyntaxKind.ColonToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    FunctionExpressionErrors.UnexpectedTokenError(closeParenToken, ":", detailedHelpMessage=FunctionExpressionErrors.onNoReturnTypeDefinedHelpMessage) :> ParseError |> Error
            
            
            let! returnTypeSyntax = tryParseTypeSyntax (PlaceholderError()) parserState
            
            
            // #3. parse function block
            let openBraceToken = parserState.PopToken()
            do! match openBraceToken.Kind with
                | SyntaxKind.OpenBraceToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    FunctionExpressionErrors.UnexpectedTokenError(colonToken, "{") :> ParseError |> Error
            
            let stopCondition (parserState: MonkeyAstParserState) = parserState.PeekToken().Kind = SyntaxKind.CloseBraceToken || parserState.IsEof()
            let statements, parseErrors = enterNewScopeAndParseTokens stopCondition parserState
            do! assertNoParseErrors parseErrors
            
            let closeBraceToken = parserState.PopToken()
            do! match closeBraceToken.Kind with
                | SyntaxKind.CloseBraceToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    FunctionExpressionErrors.UnexpectedTokenError(openBraceToken, "}") :> ParseError |> Error
                    
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
            let! typeSyntax = tryParseTypeSyntax (PlaceholderError()) parserState
            
            let currentToken = parserState.PopToken()
            let! parameterNameToken = 
                match currentToken.Kind with
                | SyntaxKind.IdentifierToken ->
                    SimpleIdentifier(currentToken) |> Ok
                | _ ->
                    parserState.RecoverFromParseError()
                    InvalidParameterNameError(currentToken) :> ParseError |> Error
            
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
               | _ ->
                    parserState.RecoverFromParseError()
                    AbsentOrInvalidTokenError(parameterSyntax.TextSpan(), [| SyntaxKind.CommaToken; SyntaxKind.CloseParenToken |], AbsentTokenAt.ParameterList) :> ParseError |> Error
           | Error error ->
               Error error
               
    let rec tryParseInvocationExpression (parserState: MonkeyAstParserState) (expression: InvocationExpressionLeftExpression) : Result<ExpressionSyntax, ParseError> =
        result {
            // parse parameters list
            let openParenToken = parserState.PopToken()
            do! match openParenToken.Kind with
                | SyntaxKind.OpenParenToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    let textSpan = parserState.PeekToken(-2).TextSpan  // we need the previous token to point to the invalid token, kinda illegal what we're doing here
                    AbsentOrInvalidTokenError(textSpan, [| SyntaxKind.OpenParenToken |], AbsentTokenAt.InvocationExpression) :> ParseError |> Error
            
            let! argumentListParseResults =
                match parserState.PeekToken().Kind with
                | SyntaxKind.CloseParenToken -> Ok ([||], [||])  // no parameters
                | _ -> parseArgumentsList [] [] parserState
                
            let arguments, commas = argumentListParseResults
            
            let closeParenToken = parserState.PopToken()
            do! match closeParenToken.Kind with
                | SyntaxKind.CloseParenToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    let textSpan =
                        match arguments with
                        | [| |] -> openParenToken.TextSpan
                        | arr -> arr[arr.Length - 1].TextSpan()
                    AbsentOrInvalidTokenError(textSpan, [| SyntaxKind.CloseParenToken |], AbsentTokenAt.InvocationExpression) :> ParseError |> Error
                    
            let argumentList = ArgumentList(openParenToken, arguments, commas, closeParenToken)
            return InvocationExpression(expression, argumentList)
        }
        
    /// Expected input ""
    /// <example>
    /// example input:
    /// <code>
    /// EXPRESSION_1, EXPRESSION_2, ..., EXPRESSION_N
    /// </code>
    /// the 'closeParenToken' is assumed to be consumed by the caller function
    /// </example>
    /// <returns>
    /// <code>(ExpressionSyntax array) * (SyntaxToken array)</code>
    /// (Argument expressions * commas between those expressions)
    /// </returns>
    and private parseArgumentsList
        (commas: SyntaxToken list)
        (arguments: ExpressionSyntax list)
        (parserState: MonkeyAstParserState)
        : Result<(ExpressionSyntax array) * (SyntaxToken array), ParseError> =
            
        tryParseExpression parserState Precedence.LOWEST
        |> function
           | Ok expression ->
               let newArguments = expression :: arguments 
               match parserState.PeekToken().Kind with
               | SyntaxKind.CommaToken ->
                   let commaToken = parserState.PopToken()
                   parseArgumentsList (commaToken :: commas) newArguments parserState
               | SyntaxKind.CloseParenToken ->
                   let parametersArr = newArguments |> List.toArray |> Array.rev
                   let commasArr = commas |> List.toArray |> Array.rev
                   Ok (parametersArr, commasArr)
               | _ ->
                    parserState.RecoverFromParseError()
                    AbsentOrInvalidTokenError(expression.TextSpan(), [| SyntaxKind.CommaToken; SyntaxKind.CloseParenToken |], AbsentTokenAt.ArgumentsList) :> ParseError |> Error
           | Error error ->
               Error error
               
               
    let rec tryParseListArrayInitializationExpression (parserState: MonkeyAstParserState) : Result<ExpressionSyntax, ParseError> =
        result {
            let openBracketToken = parserState.PopToken()
            do! match openBracketToken.Kind with
                | SyntaxKind.OpenBracketToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    let textSpan = parserState.PeekToken(-2).TextSpan  // we need the previous token to point to the invalid token, kinda illegal what we're doing here
                    AbsentOrInvalidTokenError(textSpan, [| SyntaxKind.OpenBracketToken |], AbsentTokenAt.ListArrayInitialization) :> ParseError |> Error
                    
            let! listInitializationParseResults =
                match parserState.PeekToken() with
                | token when token.Kind = SyntaxKind.CloseBracketToken ->
                    Ok ([| |], [| |])
                | _ ->
                    tryParseArrayInitializationContents [] [] parserState
                    
            let values, commas = listInitializationParseResults
                    
            let closeBracketToken = parserState.PopToken()
            do! match closeBracketToken.Kind with
                | SyntaxKind.CloseBracketToken -> Ok ()
                | _ ->
                    parserState.RecoverFromParseError()
                    let textSpan =
                        match values with
                        | [| |] -> openBracketToken.TextSpan
                        | arr -> arr[arr.Length - 1].TextSpan()
                    AbsentOrInvalidTokenError(textSpan, [| SyntaxKind.CloseParenToken |], AbsentTokenAt.InvocationExpression) :> ParseError |> Error
                    
            return
                ArrayListInitialization(openBracketToken, values, commas, closeBracketToken)
                |> ArrayExpressionSyntax.ListInitialization
                |> ExpressionSyntax.ArrayExpressionSyntax 
        }
        
    and private tryParseArrayInitializationContents
            (commas: SyntaxToken list)
            (values: ExpressionSyntax list)
            (parserState: MonkeyAstParserState)
            : Result<ExpressionSyntax array * SyntaxToken array, ParseError> =
        match (tryParseExpression parserState Precedence.LOWEST) with
        | Ok expression ->
            let newValues = expression :: values
            match parserState.PeekToken() with
            | token when token.Kind = SyntaxKind.CommaToken ->
                let commaToken = parserState.PopToken()
                tryParseArrayInitializationContents (commaToken :: commas) newValues parserState
            | token when token.Kind = SyntaxKind.CloseBracketToken ->
               let valuesArr = newValues |> List.toArray |> Array.rev
               let commasArr = commas |> List.toArray |> Array.rev
               Ok (valuesArr, commasArr)
            | _ ->
                parserState.RecoverFromParseError()
                AbsentOrInvalidTokenError(expression.TextSpan(), [| SyntaxKind.CommaToken; SyntaxKind.CloseBracketToken |], AbsentTokenAt.InvocationExpression) :> ParseError |> Error
        | Error error ->
            Error error

        
    let rec getPrefixParseFunc (parserState: MonkeyAstParserState) : Result<MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>, ParseError> =
        match parserState.IsEof() with
        | true ->
            let message = "FATAL. Tokens queue empty. This indicates a logical error in the parsing process."
            failwith message
        | false ->
            let token = parserState.PeekToken()
            match token.Kind, token.Value with
            | SyntaxKind.IdentifierToken, value when isFnKeyword value ->
                tryParseFunctionExpression
                >> (Result.bind (tryParseInvocationExpressionIfPrecedesCallExpr parserState)) |> Ok
            | SyntaxKind.IdentifierToken, _ ->
                tryParseIdentifier
                >> (Result.map ExpressionSyntax.IdentifierSyntax)
                >> (Result.bind (tryParseInvocationExpressionIfPrecedesCallExpr parserState)) |> Ok
            | SyntaxKind.OpenParenToken, _ ->  
                tryParseParenthesizedExpression
                >> (Result.bind (tryParseInvocationExpressionIfPrecedesCallExpr parserState)) |> Ok
                
            | SyntaxKind.OpenBracketToken, _ ->  
                tryParseListArrayInitializationExpression |> Ok
            | SyntaxKind.StringLiteralToken, _ ->  
                tryParseStringLiteralExpression |> Ok
            | SyntaxKind.NumericLiteralToken, _ ->  
                tryParseNumericLiteralExpression |> Ok
            | SyntaxKind.TrueKeyword, _ ->  
                tryParseTrueLiteralExpression |> Ok
            | SyntaxKind.FalseKeyword, _ ->  
                tryParseFalseLiteralExpression |> Ok
            | SyntaxKind.ExclamationToken, _ ->  
                tryParseLogicalNotPrefixExpression |> Ok
            | SyntaxKind.MinusToken, _ ->  
                tryParseMinusPrefixExpression |> Ok
            | SyntaxKind.IfKeyword, _ ->  
                tryParseIfExpression |> Ok
            | _ -> 
                consumeUntilTokenType isSemicolon parserState |> ignore  // pass by reference, properties change 'in-place'
                // Error (ParseError($"Could not find a prefix parse function for the token type \"{token.Kind}\""))
                // failwith "todo"
                PlaceholderError() :> ParseError |> Error
                
    and private isFnKeyword (value: obj) : bool =
        match value with
        | :? string as str when str = "fn" -> true
        | _ -> false
        
    and private tryParseInvocationExpressionIfPrecedesCallExpr
            (parserState: MonkeyAstParserState)
            (expressionSyntax: ExpressionSyntax)
            : Result<ExpressionSyntax, ParseError> =
        let isNextTokenOpenParen = parserState.PeekToken().Kind = SyntaxKind.OpenParenToken
        match expressionSyntax, isNextTokenOpenParen with
        | ExpressionSyntax.FunctionExpressionSyntax functionExpressionSyntax, true ->
            tryParseInvocationExpression parserState (InvocationExpressionLeftExpression.FunctionExpressionSyntax functionExpressionSyntax)
        | ExpressionSyntax.IdentifierSyntax identifierNameSyntax, true ->
            tryParseInvocationExpression parserState (InvocationExpressionLeftExpression.IdentifierSyntax identifierNameSyntax)
        | ExpressionSyntax.ParenthesizedExpressionSyntax parenthesizedExpressionSyntax, true ->
            match InvocationExpressionLeftExpression.FromParenthesizedExpression(parenthesizedExpressionSyntax) with
            | Some value ->
                tryParseInvocationExpression parserState (InvocationExpressionLeftExpression.ParenthesizedFunctionExpressionSyntax value)
            | None ->
                // ERROR HERE
                failwith "todo"
        | _ -> Ok expressionSyntax
        
        
                
                
            
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
                PlaceholderError()
                
            let token = parserState.PeekToken()
            match Map.tryFind token.Kind infixParseFuncMap with
            | Some value -> Ok value
            | None -> Error (onMissingValue parserState)
