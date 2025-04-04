namespace Monkey.Semantics.Diagnostics.Errors

open Microsoft.CodeAnalysis.Text
open Monkey.AST
open Monkey.Semantics.SemanticErrors
open Monkey.Semantics.Symbols


type InvalidVariableAnnotation(variableTypeAnnotation: VariableTypeAnnotation) =
    inherit SemanticErrorBase()

    override this.DetailedHelpMessage() =
        Some "Make sure that the type of variable is declared beforehand, or, if it is declared in another module, make sure that it is referenced correctly."
    
    override this.ErrorType() = "Invalid invocation expression."
    
    override this.ErrorMessage() =
        ""
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        base.Format(sourceText, variableTypeAnnotation.Type.TextSpan(), filePath)
        
        
type VariableAnnotationMismatch(expression: ExpressionSyntax, expectedType: TypeSymbol, actualType: TypeSymbol) =
    inherit SemanticErrorBase()

    override this.DetailedHelpMessage() =
        Some "Make sure that the type of variable is declared beforehand, or, if it is declared in another module, make sure that it is referenced correctly."
    
    override this.ErrorType() = "Invalid invocation expression."
    
    override this.ErrorMessage() =
        ""
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        base.Format(sourceText, expression.TextSpan(), filePath)
