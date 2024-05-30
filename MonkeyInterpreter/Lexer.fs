namespace MonkeyInterpreter.Lexer

open MonkeyInterpreter.Token

type Lexer(input: string) =
    let mutable position: int = 0
    
    let characters = input.ToCharArray()
    
    let isLetter (c: char) =
        'a' <= c && c <= 'z' || 'A' <= c && c <= 'Z' || c = '_'
        
    let isDigit (c: char) =
        '0' <= c && c <= '9'
    
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
        
    member this.ReadIdentifier() : string =
        let mutable startingPosition = this.Position
        
        while isLetter this.CurrentChar do
            this.NextChar()
            
        this.Input.Substring(startingPosition, this.Position - startingPosition)
        
    member this.ReadNumber() : string =
        let mutable startingPosition = this.Position
        
        while isDigit this.CurrentChar do
            this.NextChar()
            
        this.Input.Substring(startingPosition, this.Position - startingPosition)
        
    member this.SkipWhiteSpace() : unit =
        while this.CurrentChar = ' '
              || this.CurrentChar = '\t'
              || this.CurrentChar = '\n'
              || this.CurrentChar = '\r' do
            this.NextChar()
        
    member this.NextToken() : Token =
        
        this.SkipWhiteSpace()
        
        let token =
            match this.CurrentChar with
            | c when isLetter c ->
                let literal = this.ReadIdentifier()
                { Type = Keywords.lookupIdent literal; Literal = literal }
            | c when isDigit c ->
                let literal = this.ReadNumber()
                { Type = TokenType.INT; Literal = literal }
            | _ ->
                let token =
                    match this.CurrentChar with
                    | '=' ->
                        { Type = ASSIGN; Literal = $"{this.CurrentChar}" } 
                    | '+' ->
                        { Type = PLUS; Literal = $"{this.CurrentChar}" } 
                    | ',' ->
                        { Type = COMMA; Literal = $"{this.CurrentChar}" } 
                    | ';' ->
                        { Type = SEMICOLON; Literal = $"{this.CurrentChar}" } 
                    | '(' ->
                        { Type = LPAREN; Literal = $"{this.CurrentChar}" } 
                    | ')' ->
                        { Type = RPAREN; Literal = $"{this.CurrentChar}" } 
                    | '{' ->
                        { Type = LBRACE; Literal = $"{this.CurrentChar}" } 
                    | '}' ->
                        { Type = RBRACE; Literal = $"{this.CurrentChar}" }
                    | '\u0000' ->
                        { Type = EOF; Literal = "" }
                    | _ ->
                        { Type = ILLEGAL; Literal = $"{this.CurrentChar}" }
                
                this.NextChar()
                token
                
        token