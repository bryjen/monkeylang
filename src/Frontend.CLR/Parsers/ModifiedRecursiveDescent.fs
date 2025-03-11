module rec Monkey.Frontend.CLR.Parsers.ModifiedRecursiveDescent

open Frontend.CLR.Parsers
open Frontend.CLR.Parsers.ParseErrors
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Monkey.Frontend.CLR.Helpers
open Monkey.Frontend.CLR.Helpers.Queue
open Monkey.Frontend.CLR.Token
open FsToolkit.ErrorHandling


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
    

let rec parseTokens (tokens: Token array) : StatementSyntax list * ParseError list =
    let parserState = ParserState(tokens)
    let newParserState: ParserState = parseTokensHelper parserState
    newParserState.Statements, newParserState.ParseErrors
    
    
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
        parserState |> tryParseLetStatement |> (Result.map Some)
    | RETURN ->
        failwith "todo"
        // tokensQueue |> tryParseReturnStatement |> encapsulateIntoCase Statement.ReturnStatement 
    | SEMICOLON | EOF ->
        parserState.PopToken() |> ignore  // we know that there is at least one element
        Ok None  // no parsing happens, jus continue type shi
    | _ ->
        parserState |> tryParseExpressionStatement |> (Result.map Some)
    
[<AutoOpen>]    
module private Statements =
    let internal tryParseExpressionStatement (parserState: ParserState) : Result<StatementSyntax, ParseError> =
        result {
            let! expr = tryParseExpression parserState Precedence.LOWEST
            return SyntaxFactory.ExpressionStatement(expr)
        }
        
    let internal tryParseLetStatement (parserState: ParserState) : Result<StatementSyntax, ParseError> =
        result {
            // is the 'let' keyword
            // if ever we have more modifiers (ex. types and 'const' keyword for example), this needs to be modified
            let _ = parserState.PopToken()
            
            let peekToken = parserState.PeekToken()
            let! variableName = 
                match peekToken.Type with
                | IDENT ->
                    let identToken = parserState.PopToken()
                    Ok identToken.Literal
                | _ ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    let errorMsg = $"Expected an identifier after 'let' keyword, but received \"{peekToken.Literal}\" of type \"{peekToken.Type}\""
                    Error (ParseError(innerException=LetStatementParseError(message=errorMsg)))
                    
            do! match parserState.PeekToken().Type with
                | ASSIGN ->
                    parserState.PopToken() |> ignore  // consume the 'equals' token
                    Ok ()
                | _ ->
                    consumeUntilTokenType isSemicolon parserState |> ignore
                    let errorMsg = $"Expected an assignment operator ('='), but received \"{peekToken.Literal}\" of type \"{peekToken.Type}\""
                    Error (ParseError(innerException=LetStatementParseError(message=errorMsg)))
                    
            let! expr = tryParseExpression parserState Precedence.LOWEST
            consumeUntilTokenType isSemicolon parserState |> ignore
            
            let variableDeclarator =
                SyntaxFactory
                    .VariableDeclarator(SyntaxFactory.Identifier(variableName))
                    .WithInitializer(SyntaxFactory.EqualsValueClause(expr))
            let variableDeclaration =
                SyntaxFactory.VariableDeclaration(
                    SyntaxFactory.IdentifierName("var"),
                    SyntaxFactory.SeparatedList([| variableDeclarator |]))
            return SyntaxFactory.LocalDeclarationStatement(variableDeclaration)
        }
    
   
let rec internal tryParseExpression (parserState: ParserState) (precedence: Precedence) : Result<CSharpSyntaxNode, ParseError> =
    result {
        let! prefixParseFunc = tryGetPrefixParseFunc prefixParseFunctionsMap parserState 
        let! leftExpression = prefixParseFunc parserState
        return! tryParseExpressionHelper parserState precedence leftExpression
    }
    
and internal tryParseExpressionHelper (parserState: ParserState) (precedence: Precedence) (leftExpr: CSharpSyntaxNode) : Result<CSharpSyntaxNode, ParseError> =
    result {
        let peekToken = parserState.PeekToken()
        let peekPrecedence =
            match Map.tryFind peekToken.Type tokenTypeToPrecedenceMap with
            | Some precedence -> precedence
            | None -> Precedence.LOWEST
            
        if peekToken.Type <> TokenType.SEMICOLON && precedence < peekPrecedence then
            let! infixParseFunc = tryGetInfixParseFunc infixParseFunctionsMap parserState
            let! infixExpr = infixParseFunc parserState leftExpr
            return! tryParseExpressionHelper parserState precedence infixExpr
        else
            return leftExpr
    }
    
    
    
    

    
[<AutoOpen>]
module internal PrefixExpressions =
    /// <summary>
    /// Consumes the current token and parses it as a string literal.
    /// </summary>
    let tryParseStringLiteral
            (parserState: ParserState)
            : ExpressionSyntax =
        let currentToken = parserState.PopToken()
        SyntaxFactory.LiteralExpression(SyntaxKind.StringLiteralExpression, SyntaxFactory.Literal(currentToken.Literal))
        
        
    /// <summary>
    /// Consumes the current token and parses it as an identifier.
    /// </summary>
    let tryParseIdentifier
            (parserState: ParserState)
            : ExpressionSyntax =
        let currentToken = parserState.PopToken()
        SyntaxFactory.IdentifierName(currentToken.Literal)

    
    /// <summary>
    /// Consumes the current token and attempts to parse it as an integer number.
    /// </summary>
    let tryParseIntegerLiteral
            (parserState: ParserState)
            : Result<ExpressionSyntax, ParseError> =
                
        let getError (token: Token) =
            let literalParseError = LiteralExpressionParseError(SyntaxKind.NumericLiteralExpression, token=Some token, innerException=IntParseError(token.Literal))
            Error (ParseError(innerException=literalParseError))
            
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
            (parserState: ParserState)
            : Result<ExpressionSyntax, ParseError> =
                
        let getError (token: Token) =
            let literalParseError = LiteralExpressionParseError(message=($"Failed to parse boolean literal, expected a \"bool\" type, got \"{token.Type}\""), token=Some token)
            Error (ParseError(innerException=literalParseError))
                
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
            (parserState: ParserState)
            : Result<ExpressionSyntax, ParseError> =
        result {
            parserState.PopToken() |> ignore  // consume the left paren
            
            let! expr = tryParseExpression parserState Precedence.LOWEST
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
            (parserState: ParserState)
            : Result<ExpressionSyntax, ParseError> =
                
        let getError (token: Token) =
            let invalidPrefixOperatorError = InvalidPrefixOperatorError(token=token)
            let literalParseError = LiteralExpressionParseError(message=($"Failed to parse prefix expression with type \"{token.Type}\""), token=Some token, innerException=invalidPrefixOperatorError)
            Error (ParseError(innerException=literalParseError))
            
        result {
            let currentToken = parserState.PopToken()
            let! rightExpr = tryParseExpression parserState Precedence.PREFIX
            let! syntaxKind = 
                match currentToken.Type with
                | BANG -> Ok SyntaxKind.LogicalNotExpression
                | MINUS -> Ok SyntaxKind.UnaryMinusExpression
                | _ -> getError currentToken
            return SyntaxFactory.PrefixUnaryExpression(syntaxKind, rightExpr) :> ExpressionSyntax
        }
        
    let tryParseIfExpression
        (parserState: ParserState)
        : Result<ExpressionSyntax, ParseError> =
        failwith "todo"
        
    let private castToCSharpSyntaxNode (result: Result<'T, 'E>) : Result<CSharpSyntaxNode, 'E> =
        Result.map (fun t -> t :> CSharpSyntaxNode) result
        
    /// <summary>
    /// Map containing token types and the corresponding function required to parse them.
    /// </summary>
    let prefixParseFunctionsMap
        : Map<TokenType, ParserState -> Result<CSharpSyntaxNode, ParseError>> =
        Map.ofList [
            (TokenType.IDENT,   tryParseIdentifier              >> Ok >> castToCSharpSyntaxNode)
            (TokenType.STRING,  tryParseStringLiteral           >> Ok >> castToCSharpSyntaxNode)
            (TokenType.INT,     tryParseIntegerLiteral          >> castToCSharpSyntaxNode)
            (TokenType.TRUE,    tryParseBooleanLiteral          >> castToCSharpSyntaxNode)
            (TokenType.FALSE,   tryParseBooleanLiteral          >> castToCSharpSyntaxNode)
            (TokenType.BANG,    tryParsePrefixExpression        >> castToCSharpSyntaxNode)
            (TokenType.MINUS,   tryParsePrefixExpression        >> castToCSharpSyntaxNode)
            (TokenType.LPAREN,  tryParseParenthesisExpression   >> castToCSharpSyntaxNode)
            (TokenType.IF,      tryParseIfExpression            >> castToCSharpSyntaxNode)
            
            (*
            (TokenType.FUNCTION, tryParseFunctionLiteral)
            (TokenType.LBRACKET, tryParseArrayLiteral)
            (TokenType.LBRACE, tryParseHashLiteral)
            *)
        ]
        
    /// Attempts to get the INFIX parse function based on the next token's type
    let tryGetInfixParseFunc (infixParseFuncMap: Map<TokenType, ParserState -> ExpressionSyntax -> Result<ExpressionSyntax, ParseError>>) (parserState: ParserState) : Result<ParserState -> ExpressionSyntax -> Result<ExpressionSyntax, ParseError>, ParseError> =
        match parserState.IsEof() with
        | true ->
            let message = "FATAL. Tokens queue empty. This indicates a logical error in the parsing process."
            failwith message
            // TODO HERE MAYBE
        | false ->
            let onMissingValue parserState =  // i.e. basically go to the next statement
                consumeUntilTokenType isSemicolon parserState |> ignore  // pass by reference, properties change 'in-place'
                ParseError()
                
            let token = parserState.PeekToken()
            match Map.tryFind token.Type infixParseFuncMap with
            | Some value -> Ok value
            | None -> Error (onMissingValue parserState)
        
        
        
[<AutoOpen>]
module internal InfixExpressions =
    
    let tryParseInfixExpression (parserState: ParserState) (leftExpr: ExpressionSyntax) : Result<ExpressionSyntax, ParseError> =
        let getError (token: Token) =
            let invalidInfixOperatorError = InvalidInfixOperatorError(token=token)
            let literalParseError = LiteralExpressionParseError(message="Failed to parse infix expression", token=Some token, innerException=invalidInfixOperatorError)
            Error (ParseError(innerException=literalParseError))
        
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
            let! rightExpr = tryParseExpression parserState precedence
            return SyntaxFactory.BinaryExpression(syntaxKind, leftExpr, operatorToken, rightExpr)
        }
    
    let infixParseFunctionsMap
        : Map<TokenType, ParserState -> ExpressionSyntax -> Result<ExpressionSyntax, ParseError>> =
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
    let tryGetPrefixParseFunc prefixParseFuncMap parserState : Result<ParserState -> Result<CSharpSyntaxNode, ParseError>, ParseError> =
        match parserState.IsEof() with
        | true ->
            let message = "FATAL. Tokens queue empty. This indicates a logical error in the parsing process."
            failwith message
            // TODO HERE MAYBE
        | false ->
            let token = parserState.PeekToken()
            
            let onMissingValue parserState =  // i.e. basically go to the next statement
                consumeUntilTokenType isSemicolon parserState |> ignore  // pass by reference, properties change 'in-place'
                ParseError($"Could not find a prefix parse function for the token type \"{token.Type}\"")
                
            match Map.tryFind token.Type prefixParseFuncMap with
            | Some value -> Ok value
            | None -> Error (onMissingValue parserState)
