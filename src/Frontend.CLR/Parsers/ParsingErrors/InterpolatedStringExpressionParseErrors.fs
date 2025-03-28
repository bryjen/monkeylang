module Monkey.Frontend.CLR.Parsers.ParsingErrors.InterpolatedStringExpressionParseError

open Microsoft.CodeAnalysis.Text
open Monkey.Frontend.CLR.Syntax.Ast


type MissingStringLiteralExpressionError(invalidToken: SyntaxToken) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        this.Format(sourceText, invalidToken.TextSpan, filePath)

    override this.ErrorType() = "Invalid interpolated string."
    
    override this.ErrorMessage() = "Expected a string literal."
    
    override this.DetailedHelpMessage() = None
