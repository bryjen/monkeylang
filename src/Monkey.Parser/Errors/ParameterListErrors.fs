namespace Monkey.Parser.Errors

open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.CSharp

open Monkey.AST
open Monkey.Parser.Errors



type InvalidParameterNameError(actualToken: SyntaxToken) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        this.Format(sourceText, actualToken.TextSpan, filePath)

    override this.ErrorType() = "Invalid parameter name."
    
    override this.ErrorMessage() =
        match actualToken.Kind with
        | syntaxKind when SyntaxFacts.IsKeywordKind(syntaxKind) ->
            $"\"{actualToken.Text.Trim()}\" is a keyword, it cannot be used as a variable name"
        | _ ->
            "The token is an invalid parameter name."
    
    override this.DetailedHelpMessage() = None
