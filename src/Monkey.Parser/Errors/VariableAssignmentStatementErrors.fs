namespace Monkey.Parser.Errors

open System.Xml.Serialization
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.CSharp

open Monkey.AST
open Monkey.Parser.Errors



[<AutoOpen>]
module private Helpers =
    let errorType = "Invalid variable assignment statement"

    
    
type AbsentEqualsError(textSpan: TextSpan) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        let updatedTextSpan = TextSpan(textSpan.End, 1)
        this.Format(sourceText, updatedTextSpan, filePath)

    override this.ErrorType() = errorType
    
    override this.ErrorMessage() = "Expected an equals token '='."
    
    override this.DetailedHelpMessage() = None


type InvalidVariableNameError(token: SyntaxToken) =
    inherit ParseError()
with
    let isDigit (c: char) = '0' <= c && c <= '9'
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        this.Format(sourceText, token.TextSpan, filePath)

    override this.ErrorType() = errorType
    
    override this.ErrorMessage() =
        match token with
        | token when SyntaxFacts.IsKeywordKind(token.Kind) ->
            $"\"{token.Text}\" is a keyword. It cannot be used as a variable name."
        | token when isDigit (token.Text.Trim()[0]) ->
            "Variable names cannot start with a digit."
        | _ ->
            "An unknown error occurred."
    
    override this.DetailedHelpMessage() = None
    
    
    
type InvalidVarTypeAnnotError(textSpan: TextSpan) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        base.Format(sourceText, textSpan, filePath)

    override this.ErrorType() =
        "Invalid variable type annotation."
    
    override this.ErrorMessage() =
        "Could not parse the following type."
    
    override this.DetailedHelpMessage() = None
