namespace MonkeyInterpreter

open Microsoft.FSharp.Reflection

type TokenType =
    | ILLEGAL
    | EOF
    
    // Identifiers + literals
    | IDENT // add, foobar, x, ... 
    | INT // 10, 12, ...
    | STRING
    
    // Operators 
    | ASSIGN
    | PLUS
    | MINUS
    | BANG
    | ASTERISK
    | SLASH
    | EQ
    | NOT_EQ
    | LT
    | GT
    
    // Delimiters
    | COMMA
    | SEMICOLON
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    
    // Keywords
    | FUNCTION
    | LET
    | TRUE
    | FALSE
    | IF
    | ELSE
    | RETURN
    
    
    /// Returns the name of the 'TokenType' case as coded
    static member ToCaseString tokenType =
        match FSharpValue.GetUnionFields(tokenType, typeof<TokenType>) with
        | case, _ -> case.Name
     
     
module Keywords =
    
    let lookupIdent (ident: string) =
        match ident with
        | "fn" -> FUNCTION
        | "let" -> LET
        | "true" -> TRUE 
        | "false" -> FALSE
        | "if" -> IF
        | "else" -> ELSE
        | "return" -> RETURN
        | _ -> IDENT
    
        
type Token =
    { Type: TokenType
      Literal: string }