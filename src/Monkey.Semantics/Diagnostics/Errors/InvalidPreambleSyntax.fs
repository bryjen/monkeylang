namespace Monkey.Semantics.Diagnostics.Errors

open Microsoft.CodeAnalysis.Text
open Monkey.AST
open Monkey.Semantics.SemanticErrors

type MisplacedUsingDirective(usingDirective: UsingDirectiveSyntax) =
    inherit SemanticErrorBase()

    override this.DetailedHelpMessage() = None
    
    override this.ErrorType() = "Invalid 'using' directive."
    
    override this.ErrorMessage() = "Using directives should precede all statements and expressions in the file."
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        base.Format(sourceText, usingDirective.TextSpan(), filePath)


type MisplacedNamespaceDeclaration(namespaceDeclaration: NamespaceDeclarationSyntax) =
    inherit SemanticErrorBase()

    override this.DetailedHelpMessage() = None
    
    override this.ErrorType() = "Invalid namespace declaration."
    
    override this.ErrorMessage() = "The namespace declaration should precede all statements and expressions in the file."
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        base.Format(sourceText, namespaceDeclaration.TextSpan(), filePath)


// TODO
type MissingNamespaceDeclaration() =
    class end
    
    
type MultipleNamespaceDeclarations(currentNamespaceDeclaration: NamespaceDeclarationSyntax) =
    inherit SemanticErrorBase()

    override this.DetailedHelpMessage() = None
    
    override this.ErrorType() = "Multiple namespace declarations detected."
    
    override this.ErrorMessage() = "Only one namespace declaration is requried per file.."
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        base.Format(sourceText, currentNamespaceDeclaration.TextSpan(), filePath)

