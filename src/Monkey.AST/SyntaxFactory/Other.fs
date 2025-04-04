namespace Monkey.AST.SyntaxFactory

open Monkey.AST
open type Monkey.AST.SyntaxFactory.MonkeySyntaxTokenFactory
open type Monkey.AST.SyntaxFactory.MonkeyExpressionSyntaxFactory



[<AbstractClass; Sealed>]
type MonkeyOtherSyntaxFactory() =
    static member UsingDirective(identifierSyntax: IdentifierSyntax) =
        { UsingToken = UsingKeyword()
          Name = identifierSyntax
          SemicolonToken = SemicolonToken() }
        
    static member NamespaceDeclaration(identifierSyntax: IdentifierSyntax) =
        { NamespaceToken = NamespaceKeyword()
          Name = identifierSyntax
          SemicolonToken = SemicolonToken() }
