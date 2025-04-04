namespace Monkey.Parser.Errors

open Microsoft.CodeAnalysis.Text

open Monkey.AST
open Monkey.Parser.Errors


type MisplacedUsingDirectiveError(usingDirectiveSyntax: UsingDirectiveSyntax) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        this.Format(sourceText, usingDirectiveSyntax.TextSpan(), filePath)

    override this.ErrorType() = "Misplaced 'Using' directive error."
    
    override this.ErrorMessage() = "Misplaced 'Using' directive error."
    
    override this.DetailedHelpMessage() = Some "A using directive must precede all other elements in the source text."
    
    
type MisplacedNamespaceDirectiveError(namespaceDeclarationSyntax: NamespaceDeclarationSyntax) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        this.Format(sourceText, namespaceDeclarationSyntax.TextSpan(), filePath)

    override this.ErrorType() = "Misplaced namespace declaration error."
    
    override this.ErrorMessage() = "Misplaced namespace declaration error."
    
    override this.DetailedHelpMessage() = Some "File-scoped namespace declaration must precede all statements in the source text." 
