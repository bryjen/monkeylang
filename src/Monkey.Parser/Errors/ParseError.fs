namespace Monkey.Parser.Errors

open System
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text

[<AutoOpen>]
module ParseErrorHelpers =
    let spaces n = new string(' ', n)
    
    let repeat str n = new string(str, n)
    
    let normalizeKindWord (syntaxKind: SyntaxKind) =
        match syntaxKind.ToString() with
        | kindWord when kindWord.EndsWith("Token") -> kindWord.Substring(0, kindWord.Length - "Token".Length)
        | kindWord -> kindWord
        
    let isVowel (c: char) =
        let c = System.Char.ToLower(c)
        c = 'a' ||  c = 'e' ||  c = 'i' ||  c = 'o' ||  c = 'u' 
        

/// <summary>
/// 
/// </summary>
[<AbstractClass>]
type ParseError () =
    inherit Exception()
    abstract member GetFormattedMessage : SourceText * string option -> string  // source text -> source text file path -> formatted message
    
    abstract member ErrorType : unit -> string
    
    abstract member ErrorMessage : unit -> string
    
    abstract member DetailedHelpMessage : unit -> string option
    
    
    member internal this.Format(sourceText: SourceText, textSpan: TextSpan, filePath: string option) =
        let line = sourceText.Lines.GetLineFromPosition(textSpan.Start)
        let lineNumber = line.LineNumber;
        let column = textSpan.Start - line.Start
        
        let lineNumCharacters = lineNumber.ToString().ToCharArray().Length
        let margin = lineNumCharacters + 1
        let padding = 1
        
        let lines =
            [|
                $"error: {this.ErrorType()}"
                sprintf "%s|" (spaces margin)
                sprintf "%d%s|%s%s" lineNumber (spaces (margin - lineNumCharacters)) (spaces padding) (line.ToString())
                sprintf "%s|%s%s%s %s" (spaces margin) (spaces padding) (spaces column) (repeat '^' textSpan.Length) (this.ErrorMessage())
            |]
            
        // add filepath info, if it exists
        let lines =
            match filePath with
            | None -> lines
            | Some value ->
                let filePathStr = sprintf "%s┌─ %s:%d:%d" (spaces margin) value (lineNumber + 1) (column + 1)
                Array.insertAt 1 filePathStr lines
                
        // add detailed helper message, if it exists
        let lines =
            match this.DetailedHelpMessage() with
            | None -> lines
            | Some detailedHelpMessage -> Array.append lines [| $"\n{detailedHelpMessage}" |]
            
        System.String.Join("\n", lines)
        


/// <summary>
/// 
/// </summary>
type AbsentSemicolonError(textSpan: TextSpan, from: AbsentSemicolonAt) =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        let updatedTextSpan = TextSpan(textSpan.End, 1)
        this.Format(sourceText, updatedTextSpan, filePath)

    override this.ErrorType() =
        match from with
        | LetStatement -> "Invalid variable assignment statement."
        | ExpressionStatement -> "Invalid expression statement."
    
    override this.ErrorMessage() = "Expected a semicolon ';'."
    
    override this.DetailedHelpMessage() = None
    
and AbsentSemicolonAt =
    | LetStatement
    | ExpressionStatement
    
    
type At =
    | UsingDirective
    | NamespaceDeclaration
    | InterpolatedStringExpression
    | IfExpression
    | GenericTypeSyntax
    | ParameterList
    | ArgumentsList 
    | InvocationExpression
    | ListArrayInitialization
    | QualifiedIdentifier
with
    static member DefaultErrorType (at: At) =
        match at with
        | UsingDirective -> "Invalid using directive."
        | NamespaceDeclaration -> "Invalid namespace declaration."
        | IfExpression -> "Invalid 'If' expression."
        | GenericTypeSyntax -> "Invalid generic type."
        | ParameterList -> "Invalid parameter list."
        | ArgumentsList -> "Invalid arguments list."
        | InvocationExpression -> "Invalid function call expression."
        | ListArrayInitialization -> "Invalid array expression."
        | QualifiedIdentifier -> "Invalid qualified identifier."
        | InterpolatedStringExpression -> "Invalid interpolated string."
    

/// <summary>
/// 
/// </summary>
type AbsentOrInvalidTokenError(textSpan: TextSpan, expectedKinds: SyntaxKind array, at: At) =
    inherit ParseError()
with
    let getExpectedKindString (expectedKind: SyntaxKind) =
        let kindWord = normalizeKindWord expectedKind
        let indefiniteArticle =
            match kindWord.ToCharArray() with
            | [|  |] -> "a"
            | arr when isVowel arr[0] -> "an"
            | _ -> "a"
        $"{indefiniteArticle} {kindWord} '{SyntaxFacts.GetText(expectedKind)}'"
        
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        let updatedTextSpan = TextSpan(textSpan.End, 1)
        this.Format(sourceText, updatedTextSpan, filePath)

    override this.ErrorType() =
        At.DefaultErrorType(at)
    
    override this.ErrorMessage() =
        let strings = expectedKinds |> Array.map getExpectedKindString
        let asSingleString = System.String.Join(", or ", strings)
        $"Expected {asSingleString}'."
    
    override this.DetailedHelpMessage() = None
    
    
    
/// <summary>
/// 
/// </summary>
type CompositeParseError(parseErrors: ParseError array) =
    inherit ParseError()
with
    member val ParseErrors = parseErrors
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        let formattedMessages = parseErrors |> Array.map _.GetFormattedMessage(sourceText, filePath)
        System.String.Join("\n\n", formattedMessages)

    override this.ErrorType() = ""
    
    override this.ErrorMessage() = ""
    
    override this.DetailedHelpMessage() = None
        
        
        
type PlaceholderError () =
    inherit ParseError()
with
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) = ""
    
    override this.ErrorType() = "Placeholder Error"
    
    override this.ErrorMessage() = "Placeholder Error"
    
    override this.DetailedHelpMessage() = None
