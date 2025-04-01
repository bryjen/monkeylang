namespace Monkey.AST

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text

type SyntaxToken = Monkey.AST.SyntaxToken



module SyntaxFactory =
    let something () =
        ""
        
    let token (kind: SyntaxKind) (text: string option) (value: obj option) (textSpan: TextSpan option) (fullTextSpan: TextSpan option) (leadingTrivia: SyntaxTriviaList option) (trailingTrivia: SyntaxTriviaList option) : SyntaxToken =
        let textValue = Option.defaultValue "" text
        let value = Option.defaultValue Unchecked.defaultof<obj> value
        let textSpanValue = Option.defaultValue Unchecked.defaultof<TextSpan> textSpan
        let fullTextSpanValue = Option.defaultValue Unchecked.defaultof<TextSpan> fullTextSpan
        let leadingTriviaValue = Option.defaultValue (SyntaxTriviaList()) leadingTrivia
        let trailingTriviaValue = Option.defaultValue (SyntaxTriviaList()) trailingTrivia
        
        SyntaxToken(kind, textValue, value, textSpanValue, fullTextSpanValue, leadingTriviaValue, trailingTriviaValue)
        
        
    let internal token1 (kind: SyntaxKind) (text: string option) (value: obj option) : SyntaxToken =
        let textValue = Option.defaultValue "" text
        let value = Option.defaultValue Unchecked.defaultof<obj> value
        let textSpanValue = Unchecked.defaultof<TextSpan>
        let fullTextSpanValue = Unchecked.defaultof<TextSpan>
        let leadingTriviaValue = SyntaxTriviaList()
        let trailingTriviaValue = SyntaxTriviaList()
        token kind textValue value textSpanValue fullTextSpanValue leadingTriviaValue trailingTriviaValue
