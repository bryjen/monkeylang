namespace MonkeyInterpreter

[<AutoOpen>]
module private LexerHelpers = 
    let isLetter (c: char) =
        'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c = '_'
        
    let isDigit (c: char) =
        '0' <= c && c <= '9'
        
    let peekCharInArray (characters: char array) (index: int) : char =
        match index with
        | i when  i < 0 || i >= characters.Length ->
            '\u0000' // null character
        | i ->
            characters[i]
        
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
        let peekChar = lexerInfo.PeekChar
        
        /// Increments the index until the current index is not a whitespace character
        // TODO: Decompile to IL and see if you need a helper rec function for TCO
        let rec skipWhiteSpaceCharacters currentIndex : int =
            match (peekChar currentIndex) with
            | c when c = ' ' || c = '\t' || c = '\n' || c = '\r' ->
                skipWhiteSpaceCharacters (currentIndex + 1)
            | _ ->
                currentIndex
                
        /// Reads a continuous sequence of characters that satisfy the predicate.
        /// Returns a tuple containing the new current index and the parsed character sequence. 
        and readCharacterSequence (predicate: char -> bool) startingIndex : int * string =
            let rec readCharacterSequenceCore currentIndex : int * string =
                match (peekChar currentIndex) with
                | c when predicate c ->
                    readCharacterSequenceCore (currentIndex + 1)
                | _ ->
                    currentIndex, System.String(characters, startingIndex, currentIndex - startingIndex)
                
            readCharacterSequenceCore startingIndex
            
        /// Attempts to parse the token as an operator or delimiter. 
        /// Returns a tuple containing the new current index and the parsed token. 
        and tryParseAsOperatorOrDelimiter currentIndex : int * Token =
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
                        
                    // Other
                    | '\u0000' -> EOF
                    | _ -> ILLEGAL
                    
                let literal =
                    match currentChar with
                    | c when c = '\u0000' -> ""
                    | c -> c.ToString()
                    
                currentIndex + 1, { Type = tokenType; Literal = literal }
                
                
        let currentIndex = skipWhiteSpaceCharacters currentIndex
        
        let readIdentifier = readCharacterSequence isLetter 
        let readNumber = readCharacterSequence isDigit 
        
        match (lexerInfo.PeekChar currentIndex) with
        | c when isLetter c ->
            let newIndex, literal = readIdentifier currentIndex
            newIndex, { Type = Keywords.lookupIdent literal; Literal = literal }
        | c when isDigit c ->
            let newIndex, numberLiteral = readNumber currentIndex 
            newIndex, { Type = TokenType.INT; Literal = numberLiteral }
        | _ ->
            let newIndex, token = tryParseAsOperatorOrDelimiter currentIndex
            newIndex, token
            
