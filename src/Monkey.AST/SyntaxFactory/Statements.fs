namespace Monkey.AST.SyntaxFactory

open Microsoft.CodeAnalysis.CSharp

open Monkey.AST
open type Monkey.AST.SyntaxFactory.MonkeySyntaxTokenFactory



[<AbstractClass; Sealed>]
type MonkeyStatementSyntaxFactory() =
    static member ExpressionStatementNoBox(expression: ExpressionSyntax, semicolonToken: SyntaxToken) =
        { Expression = expression; SemicolonToken = semicolonToken }
        
    static member ExpressionStatementNoBox(expression: ExpressionSyntax) =
        MonkeyStatementSyntaxFactory.ExpressionStatementNoBox(expression, Token(SyntaxKind.SemicolonToken, text=";", value=";"))
        
        
    static member ExpressionStatement(expression: ExpressionSyntax, semicolonToken: SyntaxToken) =
        MonkeyStatementSyntaxFactory.ExpressionStatementNoBox(expression, semicolonToken)
        |> StatementSyntax.ExpressionStatementSyntax
        
    static member ExpressionStatement(expression: ExpressionSyntax) =
        MonkeyStatementSyntaxFactory.ExpressionStatement(expression, Token(SyntaxKind.SemicolonToken, text=";", value=";"))
        
        
        
    static member BlockStatement(openBraceToken: SyntaxToken, statements: StatementSyntax array, closeBraceToken: SyntaxToken) =
        { OpenBraceToken = openBraceToken; Statements = statements; CloseBraceToken = closeBraceToken }
        |> StatementSyntax.BlockSyntax

    static member BlockStatement(statements: StatementSyntax array) =
        MonkeyStatementSyntaxFactory.BlockStatement(OpenBraceToken(), statements, CloseBraceToken())
        
        
    static member BlockStatementNoBox(openBraceToken: SyntaxToken, statements: StatementSyntax array, closeBraceToken: SyntaxToken) =
        { OpenBraceToken = openBraceToken; Statements = statements; CloseBraceToken = closeBraceToken }

    static member BlockStatementNoBox(statements: StatementSyntax array) =
        MonkeyStatementSyntaxFactory.BlockStatementNoBox(OpenBraceToken(), statements, CloseBraceToken())
        
        
        
    static member VariableDeclarationStatement(letKeyword: SyntaxToken, name: SyntaxToken, equalsToken: SyntaxToken, expression: ExpressionSyntax, semicolonToken: SyntaxToken, typeAnnotation: VariableTypeAnnotation option) =
        MonkeyStatementSyntaxFactory.VariableDeclarationStatementNoBox(letKeyword , name, equalsToken, expression, semicolonToken, typeAnnotation)
        |> StatementSyntax.VariableDeclarationStatementSyntax

    static member VariableDeclarationStatement(name: SyntaxToken, expression: ExpressionSyntax, typeAnnotation: VariableTypeAnnotation option) =
        MonkeyStatementSyntaxFactory.VariableDeclarationStatement(LetKeyword(), name, EqualsToken(), expression, SemicolonToken(), typeAnnotation)
        
    static member VariableDeclarationStatement(name: SyntaxToken, expression: ExpressionSyntax) =
        MonkeyStatementSyntaxFactory.VariableDeclarationStatement(LetKeyword(), name, EqualsToken(), expression, SemicolonToken(), None)
        
        
    static member VariableDeclarationStatementNoBox(letKeyword: SyntaxToken, name: SyntaxToken, equalsToken: SyntaxToken, expression: ExpressionSyntax, semicolonToken: SyntaxToken, typeAnnotation: VariableTypeAnnotation option) : VariableDeclarationStatementSyntax =
        { LetKeyword = letKeyword
          Name = name
          EqualsToken = equalsToken
          TypeAnnotation = typeAnnotation
          Expression = expression
          SemicolonToken = semicolonToken }

    static member VariableDeclarationStatementNoBox(name: SyntaxToken, expression: ExpressionSyntax, typeAnnotation: VariableTypeAnnotation option) =
        MonkeyStatementSyntaxFactory.VariableDeclarationStatementNoBox(LetKeyword(), name, EqualsToken(), expression, SemicolonToken(), typeAnnotation)
