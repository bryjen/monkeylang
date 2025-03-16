module Frontend.CLR.Syntax.Tokenizer

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text


type private TokenizerState () =
    let mutable currentIdx: int = 0
    
    member this.CurrentIdx
        with get() = currentIdx
        and set(value) = currentIdx <- value
        

[<AutoOpen>]
module private Helpers =
    let isWhitespace (c: char) = c = ' ' || c = '\t' || c = '\n'
    // || c = '\r'
    
    let isLetter (c: char) = 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c = '_'
        
    let isDigit (c: char) = '0' <= c && c <= '9'
        
    let isIndexValid (array: 'a array) index = index >= 0 && index < array.Length
    
    let peekCharInArray (characters: char array) (index: int) : char =
        match index with
        | i when isIndexValid characters i -> characters[i]
        | _ -> '\u0000' // null character
    
    
    /// Increments the index until the current index is not a whitespace character
    let rec parseTriviaCharacters
            (examinedChars: char ResizeArray)
            (characters: char array)
            (tokenizerState: TokenizerState)
            : SyntaxTriviaList =
        match tokenizerState.CurrentIdx with 
        | i when isIndexValid characters i && isWhitespace characters[i] ->
            examinedChars.Add(characters[i])
            tokenizerState.CurrentIdx <- tokenizerState.CurrentIdx + 1
            parseTriviaCharacters examinedChars characters tokenizerState
        | _ ->
            // 'ParseLeadingTrivia' and 'ParseTrailingTrivia' are functionally identical since we're just parsing trivia.
            examinedChars.ToArray() |> string |> SyntaxFactory.ParseLeadingTrivia
            
            
    /// Reads a continuous sequence of characters that satisfy the predicate.
    /// Returns a tuple containing the new current index and the parsed character sequence. 
    let readCharacterSequence
            (characters: char array)
            (predicate: char -> bool)
            (tokenizerState: TokenizerState)
            : int * int =
        let startingIndex = tokenizerState.CurrentIdx
        while (isIndexValid characters tokenizerState.CurrentIdx && predicate characters[tokenizerState.CurrentIdx]) do
            tokenizerState.CurrentIdx <- tokenizerState.CurrentIdx + 1
            
        // System.String(characters, startingIndex, currentIndex - startingIndex)
        startingIndex, tokenizerState.CurrentIdx

        
        
        
let rec tokenize (source: string) =
    let sourceText = SourceText.From(source)
    let chars = source.ToCharArray()
    ()
    
and private parseToken (tokenizerState: TokenizerState) (chars: char array) : SyntaxToken =
    // since we parse trivia first, this will be used as leading trivia
    let leadingTriviaList = parseTriviaCharacters (ResizeArray<char>()) chars tokenizerState
    
    match (peekCharInArray chars tokenizerState.CurrentIdx) with
    | c when isLetter c ->
        let startIndex, endIndex = readCharacterSequence chars isLetter tokenizerState
        let asString = System.String(chars, startIndex, tokenizerState.CurrentIdx - startIndex)
        SyntaxFactory.Literal(, asString, asString).WithLeadingTrivia(leadingTriviaList)
    | c when isDigit c ->
        let startIndex, endIndex = readCharacterSequence chars isDigit tokenizerState
        let asString = System.String(chars, startIndex, tokenizerState.CurrentIdx - startIndex)
        SyntaxFactory.Literal(asString, asString).WithLeadingTrivia(leadingTriviaList)
    | c when c = '"' ->
        // TODO: add support for character escapes
        // the '+ 1' is to consume the semicolons
        let stringPredicate c = (c <> '"')
        let newIndex, stringLiteral = readCharacterSequence characters stringPredicate (currentIndex + 1)
        newIndex + 1, { Type = TokenType.STRING; Literal = stringLiteral }
    | _ ->
        let newIndex, token = tryParseAsOperatorOrDelimiter characters currentIndex
        newIndex, token
