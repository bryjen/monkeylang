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
