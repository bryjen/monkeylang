module Monkey.Frontend.CLR.Parsers.ParsingErrors.ParameterListErrors

open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text
open Monkey.Frontend.CLR.Parsers.ParsingErrors
open Monkey.Frontend.CLR.Syntax.Ast

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
