[<RequireQualifiedAccess>]
module Frontend.CLR.Parsers.ParsingErrors.FunctionExpression

open System
open Microsoft.CodeAnalysis.Text
open Monkey.Frontend.CLR.Parsers.ParsingErrors
open Monkey.Frontend.CLR.Syntax.Ast

///
type ExpectedConditionOpenParenTokenError(fnKeywordToken: SyntaxToken) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText) =
        let updatedTextSpan = TextSpan(fnKeywordToken.TextSpan.End, 1)  // points to the character after the 'fn' keyword
        this.Format(sourceText, updatedTextSpan, Some "FILEPATH")

    override this.ErrorType() = "Invalid function definition"
    
    override this.ErrorMessage() = "Expected a '(' token"
    
    override this.DetailedHelpMessage() = None