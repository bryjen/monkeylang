[<RequireQualifiedAccess>]
module Monkey.Frontend.CLR.Parsers.ParsingErrors.FunctionExpressionErrors

open System
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text
open Monkey.Frontend.CLR.Parsers.ParsingErrors
open Monkey.Frontend.CLR.Syntax.Ast

let private errorType = "Invalid function definition"

let internal onNoReturnTypeDefinedHelpMessage =
    """Functions must be strongly typed.
Specify a return type by adding ': RETURN_TYPE' after the parameter list.
"""

type UnexpectedTokenError(previousToken: SyntaxToken, expected: string, ?detailedHelpMessage: string) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        let updatedTextSpan = TextSpan(previousToken.TextSpan.End, 1)
        this.Format(sourceText, updatedTextSpan, filePath)

    override this.ErrorType() = errorType
    
    override this.ErrorMessage() = $"Expected a '{expected}' token"
    
    override this.DetailedHelpMessage() = detailedHelpMessage