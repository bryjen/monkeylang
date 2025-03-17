namespace Monkey.Frontend.CLR.Syntax.SyntaxFactory

open System.Runtime.CompilerServices
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text
open Monkey.Frontend.CLR.Syntax.Ast

type private CSharpToken = Microsoft.CodeAnalysis.SyntaxToken
type private MonkeyToken = Monkey.Frontend.CLR.Syntax.Ast.SyntaxToken

[<AutoOpen>]
module private SyntaxTokenFactoryHelpers =
    let temp () =
        ()

[<AbstractClass; Sealed>]
type SyntaxTokenFactory () =
    static member Token(
            syntaxKind: SyntaxKind,
            ?text: string,
            ?value: obj,
            ?textSpan: TextSpan,
            ?fullTextSpan: TextSpan,
            ?leadingTrivia: SyntaxTriviaList,
            ?trailingTrivia: SyntaxTriviaList)
            : SyntaxToken =
                
        let textValue = Option.defaultValue "" text
        let textSpanValue = Option.defaultValue Unchecked.defaultof<TextSpan> textSpan
        let fullTextSpanValue = Option.defaultValue Unchecked.defaultof<TextSpan> fullTextSpan
        let leadingTriviaValue = Option.defaultValue (SyntaxTriviaList()) leadingTrivia
        let trailingTriviaValue = Option.defaultValue (SyntaxTriviaList()) trailingTrivia 
                
        { Kind = syntaxKind
          Text = textValue
          Value = value
          TextSpan = textSpanValue
          FullTextSpan = fullTextSpanValue
          LeadingTrivia = leadingTriviaValue 
          TrailingTrivia = trailingTriviaValue }
        
    static member Identifier(value: string, textSpan: TextSpan, fullTextSpan: TextSpan, leadingTrivia: SyntaxTriviaList) =
        SyntaxTokenFactory.Token(SyntaxKind.IdentifierToken, text=value, value=value, textSpan=textSpan, fullTextSpan=fullTextSpan, leadingTrivia=leadingTrivia)
        
    static member StringLiteral(value: string, textSpan: TextSpan, fullTextSpan: TextSpan, leadingTrivia: SyntaxTriviaList) =
        SyntaxTokenFactory.Token(SyntaxKind.StringLiteralToken, text=($"\"{value}\""), value=value, textSpan=textSpan, fullTextSpan=fullTextSpan, leadingTrivia=leadingTrivia)
        
    static member NumericLiteral(value: obj, textSpan: TextSpan, fullTextSpan: TextSpan, leadingTrivia: SyntaxTriviaList) =
        SyntaxTokenFactory.Token(SyntaxKind.NumericLiteralToken, text=(string value), value=value, textSpan=textSpan, fullTextSpan=fullTextSpan, leadingTrivia=leadingTrivia)
       
       
       
[<Extension>] 
type SyntaxTokenExtensions =
    /// <summary>
    /// 
    /// </summary>
    /// <remarks>
    /// <ul>
    ///     <li>
    ///     </li>
    /// </ul>
    /// </remarks>
    [<Extension>]
    static member ToRoslynSyntaxToken (syntaxToken: SyntaxToken) : Microsoft.CodeAnalysis.SyntaxToken =
        SyntaxFactory.Token(SyntaxKind.Argument).WithLeadingTrivia(SyntaxTriviaList()) |> ignore
        failwith "todo"
