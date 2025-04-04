module Monkey.Parser.Tokenizer

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.CSharp

open FsToolkit.ErrorHandling

open type Monkey.AST.SyntaxFactory.MonkeySyntaxTokenFactory


type private TokenizerState (sourceText: SourceText) =
    let mutable currentIdx: int = 0
    
    member this.CurrentIdx
        with get() = currentIdx
        
    member this.SourceText =  sourceText
    member this.Characters =  sourceText.ToString().ToCharArray()
        
    member this.Next() : TokenizerState =
        currentIdx <- currentIdx + 1
        this
        
    member this.Next(inc: int) : TokenizerState =
        currentIdx <- currentIdx + inc
        this
        

[<AutoOpen>]
module private Helpers =
    let isWhitespace (c: char) = c = ' ' || c = '\t' || c = '\n' || c = '\r'
    
    let isLetter (c: char) = 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c = '_'
        
    let isDigit (c: char) = '0' <= c && c <= '9'
        
    let isIndexValid (array: 'a array) index = index >= 0 && index < array.Length
    
    let peekCharInArray (characters: char array) (index: int) : char =
        match index with
        | i when isIndexValid characters i -> characters[i]
        | _ -> '\u0000' // null character
    
    
    /// Increments the index until the current index is not a whitespace character
    let rec parseTriviaCharacters
            (characters: char array)
            (tokenizerState: TokenizerState)
            : SyntaxTriviaList * TextSpan =
                
        let startingIdx = tokenizerState.CurrentIdx
        let examinedChars = ResizeArray<char>()
        while isIndexValid characters tokenizerState.CurrentIdx && isWhitespace characters[tokenizerState.CurrentIdx] do
            examinedChars.Add(characters[tokenizerState.CurrentIdx])
            tokenizerState.Next() |> ignore
        
        let syntaxTriviaList = System.String.Join("", examinedChars.ToArray()) |> SyntaxFactory.ParseLeadingTrivia
        let textSpan = TextSpan(startingIdx, tokenizerState.CurrentIdx - startingIdx)
        syntaxTriviaList, textSpan
            
            
            
    /// Reads a continuous sequence of characters that satisfy the predicate.
    /// Returns a tuple containing the new current index and the parsed character sequence. 
    let readCharacterSequence
            (characters: char array)
            (predicate: char -> bool)
            (tokenizerState: TokenizerState)
            : int * int =
        let startingIndex = tokenizerState.CurrentIdx
        while (isIndexValid characters tokenizerState.CurrentIdx && predicate characters[tokenizerState.CurrentIdx]) do
            tokenizerState.Next() |> ignore
            
        // System.String(characters, startingIndex, currentIndex - startingIndex)
        startingIndex, tokenizerState.CurrentIdx
        
       
    // takes into account current and previous char, separate methods to prevent changing the whole logic 
    let readCharacterSequenceWithLookback
            (characters: char array)
            (predicate: char -> char -> bool)
            (tokenizerState: TokenizerState)
            : int * int =
        let startingIndex = tokenizerState.CurrentIdx
        let mutable previousToken = '\u0000'
        while (isIndexValid characters tokenizerState.CurrentIdx && predicate previousToken characters[tokenizerState.CurrentIdx]) do
            previousToken <- characters[tokenizerState.CurrentIdx]
            tokenizerState.Next() |> ignore
            
        // System.String(characters, startingIndex, currentIndex - startingIndex)
        startingIndex, tokenizerState.CurrentIdx
        

        
let rec tokenize (source: string) =
    let sourceText = SourceText.From(source)
    let chars = source.ToCharArray()
    let tokenizerState = TokenizerState(sourceText)
    
    // basically a while loop
    let tokens = ResizeArray<Monkey.AST.SyntaxToken>()
    let rec parseAllTokens () : unit =
        let syntaxToken = parseToken tokenizerState chars
        tokens.Add(syntaxToken)
        
        match syntaxToken.Kind with
        | SyntaxKind.EndOfFileToken ->
            ()
        | _ ->
            parseAllTokens ()

    parseAllTokens ()
    tokens.ToArray()
    
    
and private parseToken (tokenizerState: TokenizerState) (chars: char array) : Monkey.AST.SyntaxToken =
    // since we parse trivia first, this will be used as leading trivia
    let leadingTriviaList, triviaTextSpan = parseTriviaCharacters chars tokenizerState
    
    match (peekCharInArray chars tokenizerState.CurrentIdx) with
    | c when isLetter c ->
        let startIndex, endIndex = readCharacterSequence chars isLetter tokenizerState
        let textSpan = TextSpan(startIndex, endIndex - startIndex)
        let asString = tokenizerState.SourceText.GetSubText(textSpan).ToString()
        let fullTextSpan = TextSpan(triviaTextSpan.Start, triviaTextSpan.Length + textSpan.Length)
        
        match SyntaxFacts.GetKeywordKind(asString) with
        | SyntaxKind.None ->
            match asString with  // further processing for keyword tokens unrecognized by 'SyntaxFacts'
            | "let" -> Token(SyntaxKind.LetKeyword, text=asString, value=asString, textSpan=textSpan, fullTextSpan=fullTextSpan, leadingTrivia=leadingTriviaList)
            | _ ->  Identifier(asString, textSpan, fullTextSpan, leadingTriviaList)
        | syntaxKind -> Token(syntaxKind, text=asString, value=asString, textSpan=textSpan, fullTextSpan=fullTextSpan, leadingTrivia=leadingTriviaList)
        
    | c when isDigit c ->
        let startIndex, endIndex = readCharacterSequence chars isDigit tokenizerState
        let textSpan = TextSpan(startIndex, endIndex - startIndex)
        let numericLiteralValue = tokenizerState.SourceText.GetSubText(textSpan).ToString() |> int  // differentiate between differnt numeric types here
        let fullTextSpan = TextSpan(triviaTextSpan.Start, triviaTextSpan.Length + textSpan.Length)
        NumericLiteral(numericLiteralValue, textSpan, fullTextSpan, leadingTriviaList)
        
    | c when c = '"' ->
        tokenizerState.Next() |> ignore  // consume the starting double quote
        
        let continueReadingWhen prev curr = not (curr = '"' && prev <> '\\')
        let startIndex, endIndex = readCharacterSequenceWithLookback chars continueReadingWhen tokenizerState
        
        let endIndex = System.Math.Min(endIndex, tokenizerState.SourceText.Length - 1)
        
        let textSpan = TextSpan(startIndex - 1, endIndex - startIndex + 2)  // to include the quotation marks
        let fullTextSpan = TextSpan(triviaTextSpan.Start, triviaTextSpan.Length + textSpan.Length)
        let valueSpan = TextSpan(startIndex, endIndex - startIndex)
        
        let text = tokenizerState.SourceText.GetSubText(textSpan).ToString()
        let value = tokenizerState.SourceText.GetSubText(valueSpan).ToString()
        
        tokenizerState.Next() |> ignore  // consume the ending double quote
        StringLiteral(value, text, textSpan, fullTextSpan, leadingTriviaList)
    | _ ->
        tryParseAsOperatorOrDelimiter chars leadingTriviaList triviaTextSpan tokenizerState
        
        
and private tryParseAsOperatorOrDelimiter
        (chars: char array)
        (leadingTriviaList: SyntaxTriviaList)
        (triviaTextSpan: TextSpan)
        (tokenizerState: TokenizerState)
        : Monkey.AST.SyntaxToken =
    let currentChar = peekCharInArray chars tokenizerState.CurrentIdx
    
    match tryParseMultiCharacterOperator chars leadingTriviaList triviaTextSpan tokenizerState with
    | Some syntaxToken ->
        syntaxToken
    | None -> 
        let syntaxKind = 
            match currentChar with
            // Operators
            | '=' -> SyntaxKind.EqualsToken
            | '+' -> SyntaxKind.PlusToken
            | '-' -> SyntaxKind.MinusToken
            | '!' -> SyntaxKind.ExclamationToken
            | '*' -> SyntaxKind.AsteriskToken
            | '/' -> SyntaxKind.SlashToken
            | '<' -> SyntaxKind.LessThanToken
            | '>' -> SyntaxKind.GreaterThanToken
                
            // Delimiters
            | '$' -> SyntaxKind.DollarToken
            | '.' -> SyntaxKind.DotToken 
            | ',' -> SyntaxKind.CommaToken
            | ':' -> SyntaxKind.ColonToken 
            | ';' -> SyntaxKind.SemicolonToken
            | '(' -> SyntaxKind.OpenParenToken
            | ')' -> SyntaxKind.CloseParenToken
            | '{' -> SyntaxKind.OpenBraceToken
            | '}' -> SyntaxKind.CloseBraceToken
            | '[' -> SyntaxKind.OpenBracketToken
            | ']' -> SyntaxKind.CloseBracketToken
                
            // Other
            | '\u0000' -> SyntaxKind.EndOfFileToken
            | _ -> SyntaxKind.BadToken
            
        let value =
            match currentChar with
            | c when c = '\u0000' -> ""
            | c -> c.ToString()
            
        let textSpan = TextSpan(tokenizerState.CurrentIdx, 1)
        let fullTextSpan = TextSpan(triviaTextSpan.Start, triviaTextSpan.Length + textSpan.Length)
        let token = Token(syntaxKind, value, value, textSpan, fullTextSpan, leadingTriviaList)
        
        tokenizerState.Next() |> ignore
            
        token

and private tryParseMultiCharacterOperator
        (chars: char array)
        (leadingTriviaList: SyntaxTriviaList)
        (triviaTextSpan: TextSpan)
        (tokenizerState: TokenizerState)
        : Monkey.AST.SyntaxToken option =
    let currentChar = peekCharInArray chars tokenizerState.CurrentIdx
    let nextChar = peekCharInArray chars (tokenizerState.CurrentIdx + 1)
    
    option {
        let value = $"{currentChar}{nextChar}"
        let! syntaxKind =
            match value with
            | "==" -> Some SyntaxKind.EqualsEqualsToken
            | "!=" -> Some SyntaxKind.ExclamationEqualsToken
            | "->" -> Some SyntaxKind.MinusGreaterThanToken
            | ">=" -> Some SyntaxKind.GreaterThanEqualsToken
            | "<=" -> Some SyntaxKind.LessThanEqualsToken
            | _ -> None
            
        let textSpan = TextSpan(tokenizerState.CurrentIdx, 2)
        let fullTextSpan = TextSpan(triviaTextSpan.Start, triviaTextSpan.Length + textSpan.Length)
        let token = Token(syntaxKind, value, value, textSpan, fullTextSpan, leadingTriviaList)
        
        tokenizerState.Next(2) |> ignore
        
        return token
    }
    
