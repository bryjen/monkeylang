namespace Monkey.Frontend.CLR.Syntax.SyntaxFactory

open Microsoft.CodeAnalysis.CSharp
open Monkey.Frontend.CLR.Syntax.Ast
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeySyntaxTokenFactory



[<AbstractClass; Sealed>]
type MonkeyStatementSyntaxFactory() =
    static member ExpressionStatement(expression: ExpressionSyntax, semicolonToken: SyntaxToken) =
        { Expression = expression; SemicolonToken = semicolonToken }
        
    static member ExpressionStatement(expression: ExpressionSyntax) =
        MonkeyStatementSyntaxFactory.ExpressionStatement(expression, Token(SyntaxKind.SemicolonToken, text=";", value=";"))
