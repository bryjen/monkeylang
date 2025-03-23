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
type MonkeySyntaxTokenFactory () =
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
        MonkeySyntaxTokenFactory.Token(SyntaxKind.IdentifierToken, text=value, value=value, textSpan=textSpan, fullTextSpan=fullTextSpan, leadingTrivia=leadingTrivia)
        
    static member Identifier(value: string) =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.IdentifierToken, text=value, value=value)
        
    static member StringLiteral(value: string, text: string, textSpan: TextSpan, fullTextSpan: TextSpan, leadingTrivia: SyntaxTriviaList) =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.StringLiteralToken, text=text, value=value, textSpan=textSpan, fullTextSpan=fullTextSpan, leadingTrivia=leadingTrivia)
        
    /// <remarks>
    /// Assumes that the passed string is the contents inside the double quotes
    /// </remarks>
    static member internal StringLiteral(value: string) =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.StringLiteralToken, text=($"\"{value}\""), value=value)
        
        
    static member NumericLiteral(value: obj, textSpan: TextSpan, fullTextSpan: TextSpan, leadingTrivia: SyntaxTriviaList) =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.NumericLiteralToken, text=(string value), value=value, textSpan=textSpan, fullTextSpan=fullTextSpan, leadingTrivia=leadingTrivia)
        
    static member NumericLiteral(value: obj) =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.NumericLiteralToken, text=(string value), value=value)
        
        
    static member LetKeyword() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.LetKeyword, text="let", value="let")
        
    static member BoolKeyword() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.BoolKeyword, text="bool", value="bool")
        
    static member StringKeyword() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.StringKeyword, text="string", value="string")
        
    static member IntKeyword() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.IntKeyword, text="int", value="int")
        
    static member FalseKeyword() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.FalseKeyword, text="false", value=false)
        
    static member TrueKeyword() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.TrueKeyword, text="true", value=true)
        
    static member IfKeyword() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.IfKeyword, text="if", value="if")
        
    static member ElseKeyword() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.ElseKeyword, text="else", value="else")
        
    static member FnKeyword() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.IdentifierToken, text="fn", value="fn")
        
        
    static member DotToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.DotToken, text=".", value=".")
        
    static member SemicolonToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.SemicolonToken, text=";", value=";")
        
    static member EqualsToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.EqualsToken, text="=", value="=")
        
    static member PlusToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.PlusToken, text="+", value="+")
        
    static member MinusToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.MinusToken, text="-", value="-")
        
    static member AsteriskToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.AsteriskToken, text="*", value="*")
        
    static member SlashToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.SlashToken, text="/", value="/")
        
    static member DoubleEqualsToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.EqualsEqualsToken, text="==", value="==")
        
    static member NotEqualsToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.ExclamationEqualsToken, text="!=", value="!=")
        
    static member GreaterThanToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.GreaterThanToken, text=">", value=">")
        
    static member LessThanToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.LessThanToken, text="<", value="<")
        
    static member GreaterThanOrEqToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.GreaterThanEqualsToken, text=">=", value=">=")
        
    static member LessThanOrEqToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.LessThanEqualsToken, text="<=", value="<=")
        
        
    static member OpenParenToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.OpenParenToken, text="(", value="(")
        
    static member CloseParenToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.CloseParenToken, text=")", value=")")
        
        
    static member OpenBraceToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.OpenBraceToken, text="{", value="{")
        
    static member CloseBraceToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.CloseBraceToken, text="}", value="}")
        
    static member OpenBracketToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.OpenBracketToken, text="[", value="[")
        
    static member CloseBracketToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.CloseBracketToken, text="]", value="]")
        
    static member ArrowToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.MinusGreaterThanToken, text="->", value=">")
        
        
    static member CommaToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.CommaToken, text=",", value=",")
        
    static member ColonToken() =
        MonkeySyntaxTokenFactory.Token(SyntaxKind.ColonToken, text=":", value=":")
