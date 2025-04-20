﻿namespace Monkey.AST.SyntaxFactory

open Microsoft.CodeAnalysis.CSharp

open Monkey.AST
open type Monkey.AST.SyntaxFactory.MonkeySyntaxTokenFactory



[<AbstractClass; Sealed>]
type MonkeyExpressionSyntaxFactory() =
    static member NumericLiteralExpression(value: int) =
        { Kind = SyntaxKind.NumericLiteralExpression; Token = NumericLiteral(value) } |> ExpressionSyntax.LiteralExpressionSyntax
        
    static member NumericLiteralExpression(value: float) =
        { Kind = SyntaxKind.NumericLiteralExpression; Token = NumericLiteral(value) } |> ExpressionSyntax.LiteralExpressionSyntax
        
    static member TrueLiteralExpression() =
        { Kind = SyntaxKind.TrueLiteralExpression; Token = TrueKeyword() } |> ExpressionSyntax.LiteralExpressionSyntax
        
    static member FalseLiteralExpression() =
        { Kind = SyntaxKind.FalseLiteralExpression; Token = FalseKeyword() } |> ExpressionSyntax.LiteralExpressionSyntax
        
    static member StringLiteralExpression(value: string) =
        { Kind = SyntaxKind.StringLiteralExpression; Token = StringLiteral(value) } |> ExpressionSyntax.LiteralExpressionSyntax
        
        
    static member IdentifierName(token: SyntaxToken) =
        MonkeyExpressionSyntaxFactory.IdentifierNameNoBox(token) |> ExpressionSyntax.IdentifierSyntax
        
    static member IdentifierNameNoBox(token: SyntaxToken) =
        { Token = token }
        |> IdentifierSyntax.SimpleIdentifier
        
        
    static member SimpleIdentifierNoBox(token: SyntaxToken) : SimpleIdentifier =
        { Token = token }
        
    static member SimpleIdentifier(token: SyntaxToken) : IdentifierSyntax =
        { Token = token } |> IdentifierSyntax.SimpleIdentifier
        
        
    static member internal QualifiedNameNoBox(tokens: SyntaxToken array) : QualifiedIdentifier =
        let periods = Array.create (tokens.Length - 1) (DotToken())
        { Tokens = tokens; Dots = periods }
        
    static member internal QualifiedName(tokens: SyntaxToken array) : IdentifierSyntax =
        let periods = Array.create (tokens.Length - 1) (DotToken())
        { Tokens = tokens; Dots = periods }
        |> IdentifierSyntax.QualifiedIdentifier
        
        
        
    static member MinusPrefixExpression(expression: ExpressionSyntax, operatorToken: SyntaxToken) =
        let prefixExpr: PrefixExpressionSyntax = { Kind = SyntaxKind.UnaryMinusExpression; OperatorToken = operatorToken; Operand = expression }
        prefixExpr |> ExpressionSyntax.PrefixExpressionSyntax
        
    static member MinusPrefixExpression(expression: ExpressionSyntax) =
        let prefixExpr: PrefixExpressionSyntax =
            { Kind = SyntaxKind.UnaryMinusExpression
              OperatorToken = Token(SyntaxKind.MinusToken, text="-", value="-")
              Operand = expression }
        prefixExpr |> ExpressionSyntax.PrefixExpressionSyntax
        
        
        
    static member LogicalNotPrefixExpression(expression: ExpressionSyntax, operatorToken: SyntaxToken) =
        let prefixExpr: PrefixExpressionSyntax = { Kind = SyntaxKind.LogicalNotExpression; OperatorToken = operatorToken; Operand = expression }
        prefixExpr |> ExpressionSyntax.PrefixExpressionSyntax
        
    static member LogicalNotPrefixExpression(expression: ExpressionSyntax) =
        let prefixExpr: PrefixExpressionSyntax =
            { Kind = SyntaxKind.LogicalNotExpression
              OperatorToken = Token(SyntaxKind.ExclamationToken, text="!", value="!")
              Operand = expression }
        prefixExpr |> ExpressionSyntax.PrefixExpressionSyntax
        
        
        
    static member ParenthesizedExpression(openParenToken: SyntaxToken, expression: ExpressionSyntax, closeParenToken: SyntaxToken) =
        { OpenParenToken = openParenToken; Expression = expression; CloseParenToken = closeParenToken }
        |> ExpressionSyntax.ParenthesizedExpressionSyntax
        
    static member ParenthesizedExpression(expression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.ParenthesizedExpression(
            Token(SyntaxKind.OpenParenToken, text="(", value="("),
            expression,
            Token(SyntaxKind.CloseParenToken, text=")", value=")"))
        
        
    static member BinaryExpression(syntaxKind: SyntaxKind, leftExpression: ExpressionSyntax, operatorToken: SyntaxToken, rightExpression: ExpressionSyntax) =
        { Kind = syntaxKind; Left = leftExpression; OperatorToken = operatorToken; Right = rightExpression }
        |> ExpressionSyntax.BinaryExpressionSyntax
        
        
    static member AddExpression(leftExpression: ExpressionSyntax, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.AddExpression, leftExpression, PlusToken(), rightExpression)
        
    static member AddExpression(leftExpression: ExpressionSyntax, operatorToken: SyntaxToken, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.AddExpression, leftExpression, operatorToken, rightExpression)
        
        
    static member SubtractExpression(leftExpression: ExpressionSyntax, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.SubtractExpression, leftExpression, MinusToken(), rightExpression)
        
    static member SubtractExpression(leftExpression: ExpressionSyntax, operatorToken: SyntaxToken, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.SubtractExpression, leftExpression, operatorToken, rightExpression)
        
        
    static member MultiplicationExpression(leftExpression: ExpressionSyntax, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.MultiplyExpression, leftExpression, AsteriskToken(), rightExpression)
        
    static member MultiplicationExpression(leftExpression: ExpressionSyntax, operatorToken: SyntaxToken, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.MultiplyExpression, leftExpression, operatorToken, rightExpression)
        
        
    static member DivideExpression(leftExpression: ExpressionSyntax, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.DivideExpression, leftExpression, SlashToken(), rightExpression)
        
    static member DivideExpression(leftExpression: ExpressionSyntax, operatorToken: SyntaxToken, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.DivideExpression, leftExpression, operatorToken, rightExpression)
        
        
    static member EqualsExpression(leftExpression: ExpressionSyntax, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.EqualsExpression, leftExpression, DoubleEqualsToken(), rightExpression)
        
    static member EqualsExpression(leftExpression: ExpressionSyntax, operatorToken: SyntaxToken, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.EqualsExpression, leftExpression, operatorToken, rightExpression)
        
        
    static member NotEqualsExpression(leftExpression: ExpressionSyntax, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.NotEqualsExpression, leftExpression, NotEqualsToken(), rightExpression)
        
    static member NotEqualsExpression(leftExpression: ExpressionSyntax, operatorToken: SyntaxToken, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.NotEqualsExpression, leftExpression, operatorToken, rightExpression)
        
        
    static member GreaterThanExpression(leftExpression: ExpressionSyntax, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.GreaterThanExpression, leftExpression, GreaterThanToken(), rightExpression)
        
    static member GreaterThanExpression(leftExpression: ExpressionSyntax, operatorToken: SyntaxToken, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.GreaterThanExpression, leftExpression, operatorToken, rightExpression)
        
        
    static member LessThanExpression(leftExpression: ExpressionSyntax, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.LessThanExpression, leftExpression, LessThanToken(), rightExpression)
        
    static member LessThanExpression(leftExpression: ExpressionSyntax, operatorToken: SyntaxToken, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.LessThanExpression, leftExpression, operatorToken, rightExpression)

    
    static member GreaterThanOrEqExpression(leftExpression: ExpressionSyntax, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression, leftExpression, GreaterThanOrEqToken(), rightExpression)
        
    static member GreaterThanOrEqExpression(leftExpression: ExpressionSyntax, operatorToken: SyntaxToken, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.GreaterThanOrEqualExpression, leftExpression, operatorToken, rightExpression)
        
        
    static member LessThanOrEqExpression(leftExpression: ExpressionSyntax, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression, leftExpression, LessThanOrEqToken(), rightExpression)
        
    static member LessThanOrEqExpression(leftExpression: ExpressionSyntax, operatorToken: SyntaxToken, rightExpression: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.BinaryExpression(SyntaxKind.LessThanOrEqualExpression, leftExpression, operatorToken, rightExpression)
        
        
    static member IfExpression(
                ifKeyword: SyntaxToken,
                openParenToken: SyntaxToken, 
                condition: ExpressionSyntax,
                closeParenToken: SyntaxToken,
                clause: BlockSyntax,
                elseIfClauses: ElseIfClauseSyntax array,
                elseClause: ElseClauseSyntax option) =
        { IfKeyword = ifKeyword
          OpenParenToken = openParenToken
          Condition = condition
          CloseParenToken = closeParenToken
          Clause = clause
          ElseIfClauses = elseIfClauses
          ElseClause = elseClause }
        |> ExpressionSyntax.IfExpressionSyntax
        
    static member IfExpression(condition: ExpressionSyntax, clause: BlockSyntax) =
        MonkeyExpressionSyntaxFactory.IfExpression(IfKeyword(), OpenParenToken(), condition, CloseParenToken(), clause, [| |], None)
        
    static member IfExpression(condition: ExpressionSyntax, clause: BlockSyntax, elseClause: ElseClauseSyntax) =
        MonkeyExpressionSyntaxFactory.IfExpression(IfKeyword(), OpenParenToken(), condition, CloseParenToken(), clause, [| |], Some elseClause)
        
        
    static member ElseClause(elseKeyword: SyntaxToken, clause: BlockSyntax) : ElseClauseSyntax =
        { ElseKeyword = elseKeyword; ElseClause = clause }
        
    static member ElseClause(clause: BlockSyntax) : ElseClauseSyntax =
        { ElseKeyword = ElseKeyword(); ElseClause = clause }
        

    static member ArrayTypeNoNox(arrayType: TypeSyntax, openBracketToken: SyntaxToken, closeBracketToken: SyntaxToken) : ArrayTypeSyntax =
        { Type = arrayType; OpenBracketToken = openBracketToken; CloseBracketToken = closeBracketToken }
        
    static member ArrayTypeNoNox(arrayType: TypeSyntax) : ArrayTypeSyntax =
        { Type = arrayType; OpenBracketToken = OpenBracketToken(); CloseBracketToken = CloseBracketToken() }
        
    static member ArrayType(arrayType: TypeSyntax, openBracketToken: SyntaxToken, closeBracketToken: SyntaxToken) : TypeSyntax =
        MonkeyExpressionSyntaxFactory.ArrayTypeNoNox(arrayType, openBracketToken, closeBracketToken)
        |> TypeSyntax.ArrayTypeSyntax
        
    static member ArrayType(arrayType: TypeSyntax) : TypeSyntax =
        MonkeyExpressionSyntaxFactory.ArrayTypeNoNox(arrayType, OpenBracketToken(), CloseBracketToken())
        |> TypeSyntax.ArrayTypeSyntax
        
        
    static member GenericTypeNoBox(typeSyntax: TypeSyntax, genericTypes: TypeSyntax array, commas: SyntaxToken array, lessThanToken: SyntaxToken, greaterThanToken: SyntaxToken) : GenericTypeSyntax =
        { Type = typeSyntax; LessThanToken = lessThanToken; GenericTypes = genericTypes; Commas = commas; GreaterThanToken = greaterThanToken }
        
    static member GenericTypeNoBox(typeSyntax: TypeSyntax, genericTypes: TypeSyntax array) : GenericTypeSyntax =
        let commas =
            match genericTypes.Length with
            | i when i >= 2 -> Array.create (i - 1) (CommaToken())
            | _ -> [| |]
        MonkeyExpressionSyntaxFactory.GenericTypeNoBox(typeSyntax, genericTypes, commas, LessThanToken(), GreaterThanToken())
        
    static member GenericType(typeSyntax: TypeSyntax, genericTypes: TypeSyntax array, commas: SyntaxToken array, lessThanToken: SyntaxToken, greaterThanToken: SyntaxToken) : TypeSyntax =
        MonkeyExpressionSyntaxFactory.GenericTypeNoBox(typeSyntax, genericTypes, commas, lessThanToken, greaterThanToken)
        |> TypeSyntax.GenericTypeSyntax
        
    static member GenericType(typeSyntax: TypeSyntax, genericTypes: TypeSyntax array) : TypeSyntax =
        let commas =
            match genericTypes.Length with
            | i when i >= 2 -> Array.create (i - 1) (CommaToken())
            | _ -> [| |]
        MonkeyExpressionSyntaxFactory.GenericTypeNoBox(typeSyntax, genericTypes, commas, LessThanToken(), GreaterThanToken())
        |> TypeSyntax.GenericTypeSyntax
        
        
    
    static member BuiltinTypeNoBox(token: SyntaxToken) : BuiltinTypeSyntax =
        { Token = token }
        
    static member BuiltinType(token: SyntaxToken) : TypeSyntax =
        MonkeyExpressionSyntaxFactory.BuiltinTypeNoBox(token) |>  TypeSyntax.BuiltinTypeSyntax
        
    static member NameTypeNoBox(identifier: IdentifierSyntax) : NameSyntax =
        { Identifier = identifier }
        
    static member NameType(identifier: IdentifierSyntax) : TypeSyntax =
        MonkeyExpressionSyntaxFactory.NameTypeNoBox(identifier) |>  TypeSyntax.NameSyntax
        
        
    static member FunctionTypeNoBox(
            openBracketToken: SyntaxToken,
            parameterTypes: TypeSyntax array,
            arrowTokens: SyntaxToken array,
            closeBracketToken: SyntaxToken)
            : FunctionTypeSyntax =
        { OpenBracketToken = openBracketToken
          ParameterTypes = parameterTypes
          ArrowTokens = arrowTokens
          CloseBracketToken = closeBracketToken }
        
    static member FunctionType(
            openBracketToken: SyntaxToken,
            parameterTypes: TypeSyntax array,
            arrowTokens: SyntaxToken array,
            closeBracketToken: SyntaxToken)
            : TypeSyntax =
        MonkeyExpressionSyntaxFactory.FunctionTypeNoBox(openBracketToken, parameterTypes, arrowTokens, closeBracketToken)
        |>  TypeSyntax.FunctionTypeSyntax
        
    static member FunctionType(parameterTypes: TypeSyntax array) =
        let arrowTokens = Array.create (parameterTypes.Length - 1) (ArrowToken())
        MonkeyExpressionSyntaxFactory.FunctionTypeNoBox(OpenBracketToken(), parameterTypes, arrowTokens, CloseBracketToken())
        |> TypeSyntax.FunctionTypeSyntax
        
        
        
    static member ParameterList(
            openParenToken: SyntaxToken,
            parameters: ParameterSyntax array,
            commas: SyntaxToken array,
            closeParenToken: SyntaxToken)
            : ParameterListSyntax =
        { OpenParenToken = openParenToken 
          Parameters = parameters
          Commas = commas
          CloseParenToken = closeParenToken }
        
    static member ParameterList(parameters: ParameterSyntax array) : ParameterListSyntax =
        let commas =
            match parameters.Length with
            | i when i >= 2 -> Array.create ((Array.length parameters) - 1) (CommaToken())
            | _ -> [| |]
        MonkeyExpressionSyntaxFactory.ParameterList(OpenParenToken(), parameters, commas, CloseParenToken())
        
        
    static member ArgumentList(
            openParenToken: SyntaxToken,
            arguments: ExpressionSyntax array,
            commas: SyntaxToken array,
            closeParenToken: SyntaxToken)
            : ArgumentListSyntax =
        { OpenParenToken = openParenToken 
          Arguments = arguments
          Commas = commas
          CloseParenToken = closeParenToken }
        
    static member ArgumentList(arguments: ExpressionSyntax array) : ArgumentListSyntax =
        let commas =
            match arguments.Length with
            | i when i >= 2 -> Array.create ((Array.length arguments) - 1) (CommaToken())
            | _ -> [| |]
        MonkeyExpressionSyntaxFactory.ArgumentList(OpenParenToken(), arguments, commas, CloseParenToken())
        
        
    static member FunctionExpression(
            functionKeywordToken: SyntaxToken,
            parameterList: ParameterListSyntax,
            colonToken: SyntaxToken,
            returnType: TypeSyntax,
            body: BlockSyntax)
            : ExpressionSyntax =
        MonkeyExpressionSyntaxFactory.FunctionExpressionNoBox(functionKeywordToken, parameterList, colonToken, returnType, body)
        |> ExpressionSyntax.FunctionExpressionSyntax
        
        
    static member FunctionExpression(parameterList: ParameterListSyntax, returnType: TypeSyntax, body: BlockSyntax) : ExpressionSyntax =
        MonkeyExpressionSyntaxFactory.FunctionExpression(FnKeyword(), parameterList, ColonToken(), returnType, body)
        
        
    static member FunctionExpressionNoBox(
            functionKeywordToken: SyntaxToken,
            parameterList: ParameterListSyntax,
            colonToken: SyntaxToken,
            returnType: TypeSyntax,
            body: BlockSyntax)
            : FunctionExpressionSyntax =
        { FunctionKeywordToken = functionKeywordToken
          ParameterList = parameterList
          ColonToken = colonToken 
          ReturnType = returnType 
          Body = body }
        
    static member FunctionExpressionNoBox(parameterList: ParameterListSyntax, colonToken: SyntaxToken, returnType: TypeSyntax, body: BlockSyntax) : FunctionExpressionSyntax =
        MonkeyExpressionSyntaxFactory.FunctionExpressionNoBox(FnKeyword(), parameterList, colonToken, returnType, body)
        
    static member FunctionExpressionNoBox(parameterList: ParameterListSyntax, returnType: TypeSyntax, body: BlockSyntax) : FunctionExpressionSyntax =
        MonkeyExpressionSyntaxFactory.FunctionExpressionNoBox(FnKeyword(), parameterList, ColonToken(), returnType, body)
        
        
    static member InvocationExpressionNoBox(expression: InvocationExpressionLeftExpression, arguments: ArgumentListSyntax) : InvocationExpressionSyntax =
        { Expression = expression; Arguments = arguments }
        
    static member InvocationExpression(expression: InvocationExpressionLeftExpression, arguments: ArgumentListSyntax) : ExpressionSyntax =
        MonkeyExpressionSyntaxFactory.InvocationExpressionNoBox(expression, arguments) |> ExpressionSyntax.InvocationExpressionSyntax
        
        
    static member internal InvocationExpressionIdentifierName(token: SyntaxToken) : InvocationExpressionLeftExpression =
        { Token = token }
        |> IdentifierSyntax.SimpleIdentifier
        |> InvocationExpressionLeftExpression.IdentifierSyntax
        
    static member internal InvocationExpressionQualifiedName(tokens: SyntaxToken array) : InvocationExpressionLeftExpression =
        let periods = Array.create (tokens.Length - 1) (DotToken())
        { Tokens = tokens; Dots = periods }
        |> IdentifierSyntax.QualifiedIdentifier
        |> InvocationExpressionLeftExpression.IdentifierSyntax
        
    static member internal InvocationExpressionFunctionExpression(parameterList: ParameterListSyntax, returnType: TypeSyntax, body: BlockSyntax) : InvocationExpressionLeftExpression =
        MonkeyExpressionSyntaxFactory.FunctionExpressionNoBox(parameterList, returnType, body)
        |> InvocationExpressionLeftExpression.FunctionExpressionSyntax
        
    static member internal InvocationExpressionParenthesizedExpression(expression: InvocationExpressionLeftExpression) : InvocationExpressionLeftExpression =
        ({ OpenParenToken = OpenParenToken() 
           Expression = expression
           CloseParenToken = CloseParenToken() }: InvocationParenthesizedExpressionSyntax)
        |> InvocationExpressionLeftExpression.ParenthesizedFunctionExpressionSyntax
        
    
    
    static member VariableTypeAnnotation(colonToken: SyntaxToken, typeSyntax: TypeSyntax) =
        { ColonToken = colonToken; Type = typeSyntax }
        
    static member VariableTypeAnnotation(typeSyntax: TypeSyntax) =
        { ColonToken = ColonToken(); Type = typeSyntax }
        
        
    static member Parameter(paramType: TypeSyntax, identifierName: SimpleIdentifier) =
        { Type = paramType; Identifier = identifierName }
        


    static member ValueInitArrayExpression(openBracketToken: SyntaxToken, values: ExpressionSyntax array, commas: SyntaxToken array, closeBracketToken: SyntaxToken) =
        { OpenBracketToken = openBracketToken; Values = values; Commas = commas; CloseBracketToken = closeBracketToken }
        
    static member ValueInitArrayExpression(values: ExpressionSyntax array, commas: SyntaxToken array) =
        { OpenBracketToken = OpenBracketToken(); Values = values; Commas = commas; CloseBracketToken = CloseBracketToken() }
        
    static member ValueInitArrayExpression(values: ExpressionSyntax array) =
        let commas =
            match values.Length with
            | i when i >= 2 -> Array.create ((Array.length values) - 1) (CommaToken())
            | _ -> [| |]
        { OpenBracketToken = OpenBracketToken(); Values = values; Commas = commas; CloseBracketToken = CloseBracketToken() }
        
        
    static member SizeInitArrayExpression(typeSyntax: TypeSyntax, openBracketToken: SyntaxToken, sizeExpr: ExpressionSyntax, closeBracketToken: SyntaxToken) =
        { Type = typeSyntax; OpenBracketToken = openBracketToken; Size = sizeExpr; CloseBracketToken = closeBracketToken }
        
    static member SizeInitArrayExpression(typeSyntax: TypeSyntax, sizeExpr: ExpressionSyntax) =
        MonkeyExpressionSyntaxFactory.SizeInitArrayExpression(typeSyntax, OpenBracketToken(), sizeExpr, CloseBracketToken())
        
    
    
    static member InterpolatedStringTextNoBox(textToken: SyntaxToken) : InterpolatedStringText =
        { TextToken = textToken }
        
    static member InterpolatedStringText(textToken: SyntaxToken) : InterpolatedStringContent =
        MonkeyExpressionSyntaxFactory.InterpolatedStringTextNoBox(textToken) |> InterpolatedStringContent.InterpolatedStringText
    
    
    static member InterpolationNoBox(expression: ExpressionSyntax) : Interpolation =
        { OpenBraceToken = OpenBraceToken(); Expression=expression; CloseBraceToken = CloseBraceToken() }
        
    static member Interpolation(expression: ExpressionSyntax) : InterpolatedStringContent =
        MonkeyExpressionSyntaxFactory.InterpolationNoBox(expression) |> InterpolatedStringContent.Interpolation
    
    
    static member InterpolatedStringNoBox(dollarToken: SyntaxToken, contents: InterpolatedStringContent array) =
        { InterpolatedStringStartToken = dollarToken; Contents = contents }
        
    static member InterpolatedString(dollarToken: SyntaxToken, contents: InterpolatedStringContent array) =
        MonkeyExpressionSyntaxFactory.InterpolatedStringNoBox(dollarToken, contents) |> ExpressionSyntax.InterpolatedStringExpressionSyntax