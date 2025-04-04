namespace Monkey.Semantics.SemanticErrors

open System
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text
open Monkey.AST


[<AutoOpen>]
module ParseErrorHelpers =
    let spaces n = new string(' ', n)
    
    let repeat str n = new string(str, n)
    
    let normalizeKindWord (syntaxKind: SyntaxKind) =
        match syntaxKind.ToString() with
        | kindWord when kindWord.EndsWith("Token") -> kindWord.Substring(0, kindWord.Length - "Token".Length)
        | kindWord -> kindWord
        
    let isVowel (c: char) =
        let c = System.Char.ToLower(c)
        c = 'a' ||  c = 'e' ||  c = 'i' ||  c = 'o' ||  c = 'u'
        

[<AbstractClass>]
type SemanticErrorBase() =
    inherit Exception()
    
    abstract member GetFormattedMessage : SourceText * string option -> string  // source text -> source text file path -> formatted message
    
    abstract member ErrorType : unit -> string
    
    abstract member ErrorMessage : unit -> string
    
    abstract member DetailedHelpMessage : unit -> string option
    
    member internal this.Format(sourceText: SourceText, textSpan: TextSpan, filePath: string option) =
        let line = sourceText.Lines.GetLineFromPosition(textSpan.Start)
        let lineNumber = line.LineNumber;
        let column = textSpan.Start - line.Start
        
        let lineNumCharacters = lineNumber.ToString().ToCharArray().Length
        let margin = lineNumCharacters + 1
        let padding = 1
        
        let lines =
            [|
                $"error: {this.ErrorType()}"
                sprintf "%s|" (spaces margin)
                sprintf "%d%s|%s%s" lineNumber (spaces (margin - lineNumCharacters)) (spaces padding) (line.ToString())
                sprintf "%s|%s%s%s %s" (spaces margin) (spaces padding) (spaces column) (repeat '^' textSpan.Length) (this.ErrorMessage())
            |]
            
        // add filepath info, if it exists
        let lines =
            match filePath with
            | None -> lines
            | Some value ->
                let filePathStr = sprintf "%s┌─ %s:%d:%d" (spaces margin) value (lineNumber + 1) (column + 1)
                Array.insertAt 1 filePathStr lines
                
        // add detailed helper message, if it exists
        let lines =
            match this.DetailedHelpMessage() with
            | None -> lines
            | Some detailedHelpMessage -> Array.append lines [| $"\n{detailedHelpMessage}" |]
            
        System.String.Join("\n", lines)


type InternalError(syntaxNode: MonkeySyntaxNode) =
    inherit SemanticErrorBase()

    override this.DetailedHelpMessage() = None
    
    override this.ErrorType() = "An unknown error occurred."
    
    override this.ErrorMessage() = "An unknown error occurred."
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        base.Format(sourceText, syntaxNode.TextSpan(), filePath)

