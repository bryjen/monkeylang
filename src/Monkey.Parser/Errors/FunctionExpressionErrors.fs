namespace Monkey.Parser.Errors

open Microsoft.CodeAnalysis.Text

open Monkey.Parser.Errors
open Monkey.AST


module internal FunctionExpressionHelpMessages =
    let onNoReturnTypeDefined =
        """Functions must be strongly typed.
Specify a return type by adding ': RETURN_TYPE' after the parameter list."""

type UnexpectedTokenError(previousToken: SyntaxToken, expected: string, ?detailedHelpMessage: string) =
    inherit ParseError()
with
    let errorType = "Invalid function definition"

    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        let updatedTextSpan = TextSpan(previousToken.TextSpan.End, 1)
        this.Format(sourceText, updatedTextSpan, filePath)

    override this.ErrorType() = errorType
    
    override this.ErrorMessage() = $"Expected a '{expected}' token"
    
    override this.DetailedHelpMessage() = detailedHelpMessage
    
    
    
type InvalidFuncReturnTypeExpr(textSpan: TextSpan) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        base.Format(sourceText, textSpan, filePath)

    override this.ErrorType() =
        "Invalid function definition."
    
    override this.ErrorMessage() =
        "Could not parse the following type."
    
    override this.DetailedHelpMessage() = None
