namespace Monkey.Frontend.CLR.Parsers.ParsingErrors

open Microsoft.CodeAnalysis.Text

[<AutoOpen>]
module ParseErrorHelpers =
    let spaces n = new string(' ', n)
    
    let repeat str n = new string(str, n)

[<AbstractClass>]
type ParseError () =
    abstract member GetFormattedMessage : SourceText -> string
    
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
                let filePathStr = sprintf "%s┌─ %s:%d:%d" (spaces margin) value lineNumber column
                Array.insertAt 1 filePathStr lines
                
        // add detailed helper message, if it exists
        let lines =
            match this.DetailedHelpMessage() with
            | None -> lines
            | Some detailedHelpMessage -> Array.append lines [| $"\n{detailedHelpMessage}" |]
            
        System.String.Join("\n", lines)
        
        
type PlaceholderError () =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText) = ""
    
    override this.ErrorType() = "Placeholder Error"
    
    override this.ErrorMessage() = "Placeholder Error"
    
    override this.DetailedHelpMessage() = None
