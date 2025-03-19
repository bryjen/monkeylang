namespace Monkey.Frontend.CLR.Syntax.SyntaxFactory

open System.Linq.Expressions
open Microsoft.CodeAnalysis.CSharp
open Monkey.Frontend.CLR.Syntax.Ast
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeySyntaxTokenFactory


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
        
        
    static member IdentifierName(token: SyntaxToken) =
        MonkeyExpressionSyntaxFactory.IdentifierNameNoBox(token) |> ExpressionSyntax.IdentifierNameSyntax
        
    static member IdentifierNameNoBox(token: SyntaxToken) =
        { Token = token }
        
        
        
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
        

    
    static member BuiltinSyntaxNoBox(token: SyntaxToken) : BuiltinTypeSyntax =
        { Token = token }
        
    static member BuiltinSyntax(token: SyntaxToken) : TypeSyntax =
        MonkeyExpressionSyntaxFactory.BuiltinSyntaxNoBox(token) |>  TypeSyntax.BuiltinTypeSyntax
        
        
    static member NameSyntaxNoBox(identifierToken: SyntaxToken) : NameSyntax =
        { Identifier = identifierToken }
        
    static member NameSyntax(identifierToken: SyntaxToken) : TypeSyntax =
        MonkeyExpressionSyntaxFactory.NameSyntaxNoBox(identifierToken) |>  TypeSyntax.NameSyntax
        
        
    static member FunctionType(
            openBracketToken: SyntaxToken,
            parameterTypes: TypeSyntax array,
            arrowTokens: SyntaxToken array,
            closeBracketToken: SyntaxToken)
            : TypeSyntax =
        { OpenBracketToken = openBracketToken
          ParameterTypes = parameterTypes
          ArrowTokens = arrowTokens
          CloseBracketToken = closeBracketToken }
        |>  TypeSyntax.FunctionTypeSyntax
        
        
        
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
        let commas = Array.create ((Array.length parameters) - 1) (CommaToken())
        MonkeyExpressionSyntaxFactory.ParameterList(OpenParenToken(), parameters, commas, CloseParenToken())
        
        
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
        
        
    static member Parameter(paramType: TypeSyntax, identifierName: IdentifierNameSyntax) =
        { Type = paramType; Identifier = identifierName }
