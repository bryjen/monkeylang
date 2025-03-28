namespace Monkey.Parser.Errors

open Microsoft.CodeAnalysis.Text

open Monkey.AST
open Monkey.Parser.Errors



type MissingStringLiteralExpressionError(invalidToken: SyntaxToken) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        this.Format(sourceText, invalidToken.TextSpan, filePath)

    override this.ErrorType() = "Invalid interpolated string."
    
    override this.ErrorMessage() = "Expected a string literal."
    
    override this.DetailedHelpMessage() = None


type InvalidInterpolationError(invalidToken: SyntaxToken, errorMessage: string) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        this.Format(sourceText, invalidToken.TextSpan, filePath)

    override this.ErrorType() = "Invalid interpolated string."
    
    override this.ErrorMessage() = errorMessage
    
    override this.DetailedHelpMessage() = None
