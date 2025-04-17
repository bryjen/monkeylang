namespace Monkey.Parser.Errors

open Microsoft.CodeAnalysis.Text
open Monkey.AST
open Monkey.Parser.Errors.ErrorDumper

type InternalParseError private
    (
        sourceText: SourceText,
        textSpan: TextSpan,
        detailedHelpMessageOption: string option,
        dumpText: string option 
    ) =
    inherit ParseError()
with
    member val SourceText: SourceText = sourceText with get, set
    
    member val TextSpan: TextSpan = textSpan with get, set
    
    member val DetailedHelpMessageValue: string option = detailedHelpMessageOption with get, set
    
    member val DumpText: string option = dumpText with get, set
    
    
    override this.GetFormattedMessage(_: SourceText, filePath: string option) =
        // we instead use the source text provided during creation
        base.Format(this.SourceText, this.TextSpan, filePath)

    override this.ErrorType() =
        "An unexpected error occurred."
    
    override this.ErrorMessage() =
        "An unexpected error occurred while trying to parse the following line(s)."
    
    override this.DetailedHelpMessage() =
        this.DetailedHelpMessageValue
    
    
    static member InitAndDump(reason: string option, tokens: SyntaxToken array, textSpan: TextSpan) =
        let mutable stackTrace = ""
        try failwith "An internal error occurred."
        with | ex -> stackTrace <- ex.StackTrace
        
        let tokensAsStrings = Array.map _.ToString() tokens
        let sourceTextRaw = System.String.Join("", tokensAsStrings)
        let sourceText = SourceText.From(sourceTextRaw)
        
        let reasonFormatted = reason |> Option.map (fun str -> $"""
reason:
```
{str}
```

        """)
        
        // we create a temp error so that we can get a formatted error to put in the dump
        let tempError = InternalParseError(sourceText, textSpan, None, None)
        
        let contents = $"""
An unexpected error occurred during parsing.
Try issuing an issue here:
https://github.com/bryjen/monkeylang/issues

{reasonFormatted}
stack trace:
```
{stackTrace}
```

source text:
```
{sourceTextRaw}
```

error occurred:
```
{tempError.Format(sourceText, textSpan, None)}
```
"""

        let dumpFileInfoOption = dump contents
        let detailedHelpMsg =
            match dumpFileInfoOption with
            | Some fileInfo -> Some $"For more details, see dump at \"{fileInfo.FullName}\"."
            | None -> None
            
        InternalParseError(sourceText, textSpan, detailedHelpMsg, Some contents)

