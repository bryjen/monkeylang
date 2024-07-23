namespace MonkeyInterpreter

[<AutoOpen>]
module private LexerHelpers =
    let isWhitespace (c: char) = c = ' ' || c = '\t' || c = '\n' || c = '\r'
    
    let isLetter (c: char) = 'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c = '_'
        
    let isDigit (c: char) = '0' <= c && c <= '9'
        
    let isIndexValid (array: 'a array) index = index >= 0 && index < array.Length
        
    let peekCharInArray (characters: char array) (index: int) : char =
        match index with
        | i when isIndexValid characters i -> characters[i]
        | _ -> '\u0000' // null character
            
            
    /// Increments the index until the current index is not a whitespace character
    let rec skipWhiteSpaceCharacters (characters: char array) currentIndex : int =
        match currentIndex with 
        | i when isIndexValid characters i && isWhitespace characters[i] -> 
            skipWhiteSpaceCharacters characters (currentIndex + 1)
        | _ ->
            currentIndex
            
            
    /// Reads a continuous sequence of characters that satisfy the predicate.
    /// Returns a tuple containing the new current index and the parsed character sequence. 
    let readCharacterSequence (characters: char array) (predicate: char -> bool) startingIndex : int * string =
        let rec readCharacterSequenceCore currentIndex : int * string =
            match currentIndex with 
            | i when isIndexValid characters i && predicate characters[i] -> 
                readCharacterSequenceCore (currentIndex + 1)
            | _ -> 
                currentIndex, System.String(characters, startingIndex, currentIndex - startingIndex)
            
        readCharacterSequenceCore startingIndex
        
        
    /// Attempts to parse the token as an operator or delimiter. 
    /// Returns a tuple containing the new current index and the parsed token. 
    let tryParseAsOperatorOrDelimiter characters currentIndex : int * Token =
        let peekChar = peekCharInArray characters
        let currentChar = peekChar currentIndex
        let nextChar = peekChar (currentIndex + 1) 
        
        if currentChar = '=' && nextChar = '=' then
            currentIndex + 2, { Type = EQ; Literal = "==" }
        elif currentChar = '!' && nextChar = '=' then
            currentIndex + 2, { Type = NOT_EQ; Literal = "!=" }
        else
            let tokenType = 
                match currentChar with
                // Operators
                | '=' -> ASSIGN 
                | '+' -> PLUS
                | '-' -> MINUS
                | '!' -> BANG 
                | '*' -> ASTERISK
                | '/' -> SLASH
                | '<' -> LT
                | '>' -> GT
                    
                // Delimiters
                | ',' -> COMMA
                | ';' -> SEMICOLON
                | '(' -> LPAREN
                | ')' -> RPAREN
                | '{' -> LBRACE
                | '}' -> RBRACE
                | '[' -> LBRACKET 
                | ']' -> RBRACKET 
                    
                // Other
                | '\u0000' -> EOF
                | _ -> ILLEGAL
                
            let literal =
                match currentChar with
                | c when c = '\u0000' -> ""
                | c -> c.ToString()
                
            currentIndex + 1, { Type = tokenType; Literal = literal }
        
        
        
module Lexer =
    type LexerInfo =
        { Characters: char array
          PeekChar: int -> char }
    
    let rec parseIntoTokens (input: string) : Token list =
        let characters = input.ToCharArray()
        let peekChar = peekCharInArray characters
        let lexerInfo = { Characters = characters; PeekChar = peekChar }
        
        let rec parseAllTokens currentIndex listOfTokens : Token list =
            let newIndex, token = parseToken lexerInfo currentIndex
            let newListOfTokens = (token :: listOfTokens)
            
            match token.Type with
            | TokenType.EOF ->
                List.rev newListOfTokens
            | _ ->
                parseAllTokens newIndex newListOfTokens
         
        parseAllTokens 0 []
        
        
    /// Parses the next token.
    /// Returns a tuple containing the new current index and the parsed token.
    and private parseToken (lexerInfo: LexerInfo) (currentIndex: int) : int * Token =
        let characters = lexerInfo.Characters
        let currentIndex = skipWhiteSpaceCharacters characters currentIndex
        
        match (lexerInfo.PeekChar currentIndex) with
        | c when isLetter c ->
            let newIndex, literal = readCharacterSequence characters isLetter currentIndex
            newIndex, { Type = Keywords.lookupIdent literal; Literal = literal }
        | c when isDigit c ->
            let newIndex, numberLiteral = readCharacterSequence characters isDigit currentIndex 
            newIndex, { Type = TokenType.INT; Literal = numberLiteral }
        | c when c = '"' ->
            // TODO: add support for character escapes
            // the '+ 1' is to consume the semicolons
            let stringPredicate c = (c <> '"')
            let newIndex, stringLiteral = readCharacterSequence characters stringPredicate (currentIndex + 1)
            newIndex + 1, { Type = TokenType.STRING; Literal = stringLiteral }
        | _ ->
            let newIndex, token = tryParseAsOperatorOrDelimiter characters currentIndex
            newIndex, token