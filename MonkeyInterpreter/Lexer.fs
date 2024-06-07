namespace MonkeyInterpreter

open MonkeyInterpreter.Token

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
    let rec parseIntoTokens (input: string) : Token list =
        let characters = input.ToCharArray()
        let peekChar = peekCharInArray characters
       
        /// Parses the next token.
        /// Returns a tuple containing the new current index and the parsed token.
        let rec parseToken (currentIndex: int) : int * Token =
            let currentIndex = skipWhiteSpaceCharacters currentIndex
            
            let readIdentifier = readCharacterSequence isLetter 
            let readNumber = readCharacterSequence isDigit 
            
            match (peekChar currentIndex) with
            | c when isLetter c ->
                let newIndex, literal = readIdentifier currentIndex
                newIndex, { Type = Keywords.lookupIdent literal; Literal = literal }
            | c when isDigit c ->
                let newIndex, numberLiteral = readNumber currentIndex 
                newIndex, { Type = TokenType.INT; Literal = numberLiteral }
            | _ ->
                let newIndex, token = tryParseAsOperatorOrDelimiter currentIndex
                newIndex, token
                
        /// Increments the index until the current index is not a whitespace character
        // TODO: Decompile to IL and see if you need a helper rec function for TCO
        and skipWhiteSpaceCharacters currentIndex : int =
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
                
        let rec parseAllTokens currentIndex listOfTokens : Token list =
            let newIndex, token = parseToken currentIndex
            let newListOfTokens = (token :: listOfTokens)
            
            match token.Type with
            | TokenType.EOF ->
                List.rev newListOfTokens
            | _ ->
                parseAllTokens newIndex newListOfTokens
         
        parseAllTokens 0 []
        
        
        
(*
type Lexer(input: string) =
    let mutable position: int = 0
    
    let characters = input.ToCharArray()
    
    
    member this.Input = input
    member this.Position with get() = position
    member this.ReadPosition with get() = position + 1
    member this.CurrentChar with get() =
        if position >= this.Input.Length then
            '\u0000' // null character
        else
            characters[position]
            
            
    member this.NextChar() : unit =
        position <- position + 1
        
    member this.PeekNextChar() : char =
        if this.ReadPosition >= this.Input.Length then
            '\u0000' // null character
        else
            characters[this.ReadPosition]
        
    member this.NextToken() : Token =
        
        this.SkipWhiteSpace()
        
        match this.CurrentChar with
        | c when isLetter c ->
            let literal = this.ReadIdentifier()
            { Type = Keywords.lookupIdent literal; Literal = literal }
        | c when isDigit c ->
            let literal = this.ReadNumber()
            { Type = TokenType.INT; Literal = literal }
        | _ ->
            this.TryParseAsOperatorOrDelimiter()
        
    member private this.ReadIdentifier() : string =
        let mutable startingPosition = this.Position
        
        while isLetter this.CurrentChar do
            this.NextChar()
            
        this.Input.Substring(startingPosition, this.Position - startingPosition)
        
    member private this.ReadNumber() : string =
        let mutable startingPosition = this.Position
        
        while isDigit this.CurrentChar do
            this.NextChar()
            
        this.Input.Substring(startingPosition, this.Position - startingPosition)
        
    member private this.SkipWhiteSpace() : unit =
        while this.CurrentChar = ' '
              || this.CurrentChar = '\t'
              || this.CurrentChar = '\n'
              || this.CurrentChar = '\r' do
            this.NextChar()
            
    member private this.TryParseAsOperatorOrDelimiter() : Token =
        if this.CurrentChar = '=' && this.PeekNextChar() = '=' then
            position <- position + 2
            { Type = EQ; Literal = "==" }
        elif this.CurrentChar = '!' && this.PeekNextChar() = '=' then
            position <- position + 2
            { Type = NOT_EQ; Literal = "!=" }
        else
            let tokenType = 
                match this.CurrentChar with
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
                match this.CurrentChar with
                | c when c = '\u0000' -> ""
                | c -> c.ToString()
                
            this.NextChar()
            { Type = tokenType; Literal = literal }
*)