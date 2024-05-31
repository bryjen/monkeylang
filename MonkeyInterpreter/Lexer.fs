namespace MonkeyInterpreter.Lexer

open MonkeyInterpreter.Token

[<AutoOpen>]
module private LexerHelpers = 
    let isLetter (c: char) =
        'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c = '_'
        
    let isDigit (c: char) =
        '0' <= c && c <= '9'


type Lexer(input: string) =
    let mutable position: int = 0
    
    let characters = input.ToCharArray()
    
    
    member this.Input = input
    member this.Position with get() = position
    member this.ReadPosition with get() = position + 1
    member this.CurrentChar with get() =
        if this.ReadPosition > this.Input.Length then
            '\u0000' // null character
        else
            characters[position]
            
            
    member this.NextChar() : unit =
        position <- position + 1
        
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
        if this.CurrentChar = '=' || this.CurrentChar = '!' then
            let previousChar = this.CurrentChar
            this.NextChar()
            
            match this.CurrentChar with
            | '=' ->
                this.NextChar()
                { Type = if previousChar = '=' then EQ else NOT_EQ
                  Literal = $"{previousChar}=" }
            | _ ->
                { Type = if previousChar = '=' then ASSIGN else BANG 
                  Literal = previousChar.ToString() }
                
        else
            let literal = $"{this.CurrentChar}" 
            let token =
                match this.CurrentChar with
                // Operators
                | '+' ->
                    { Type = PLUS; Literal = literal } 
                | '-' ->
                    { Type = MINUS; Literal = literal }
                | '*' ->
                    { Type = ASTERISK; Literal = literal } 
                | '/' ->
                    { Type = SLASH; Literal = literal } 
                | '<' ->
                    { Type = LT; Literal = literal } 
                | '>' ->
                    { Type = GT; Literal = literal }
                    
                // Delimiters
                | ',' ->
                    { Type = COMMA; Literal = literal } 
                | ';' ->
                    { Type = SEMICOLON; Literal = literal } 
                | '(' ->
                    { Type = LPAREN; Literal = literal } 
                | ')' ->
                    { Type = RPAREN; Literal = literal } 
                | '{' ->
                    { Type = LBRACE; Literal = literal } 
                | '}' ->
                    { Type = RBRACE; Literal = literal }
                    
                // Other
                | '\u0000' ->
                    { Type = EOF; Literal = "" }
                | _ ->
                    { Type = ILLEGAL; Literal = literal }
            
            this.NextChar()
            token
