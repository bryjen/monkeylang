namespace Monkey.Semantics.Diagnostics.Errors

open Microsoft.CodeAnalysis.Text
open Monkey.AST
open Monkey.Semantics.SemanticErrors


type UnresolvedIdentifier(identifier: IdentifierSyntax) =
    inherit SemanticErrorBase()

    override this.DetailedHelpMessage() = Some "Make sure that the type of variable is declared beforehand, or, if it is declared in another module, make sure that it is referenced correctly."
    
    override this.ErrorType() = "Unable to resolve identifier."
    
    override this.ErrorMessage() =
        $"Could not resolve the identifier \"{identifier.ToString().Trim()}\""
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        base.Format(sourceText, identifier.TextSpan(), filePath)
