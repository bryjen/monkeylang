namespace MonkeyInterpreter

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