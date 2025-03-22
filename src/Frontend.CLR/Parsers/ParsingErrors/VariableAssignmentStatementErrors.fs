[<RequireQualifiedAccess>]
module Monkey.Frontend.CLR.Parsers.ParsingErrors.VariableAssignmentStatementErrors

open Microsoft.CodeAnalysis.Text

let private errorType = "Invalid variable assignment statement"


type AbsentSemicolonError(expressionTextSpan: TextSpan) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        let updatedTextSpan = TextSpan(expressionTextSpan.End, 1)
        this.Format(sourceText, updatedTextSpan, filePath)

    override this.ErrorType() = errorType
    
    override this.ErrorMessage() = "Expected a semicolon ';'"
    
    override this.DetailedHelpMessage() = None
