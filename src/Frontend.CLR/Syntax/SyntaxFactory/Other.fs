namespace Monkey.Frontend.CLR.Syntax.SyntaxFactory

open Microsoft.CodeAnalysis.CSharp
open Monkey.Frontend.CLR.Syntax.Ast

open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeySyntaxTokenFactory
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeyExpressionSyntaxFactory
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeySyntaxTokenFactory

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
