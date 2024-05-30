namespace MonkeyInterpreter.Token

open System
open Microsoft.FSharp.Reflection

type TokenType =
    | ILLEGAL
    | EOF
    | IDENT 
    | INT
    | ASSIGN
    | PLUS
    | COMMA
    | SEMICOLON
    | LPAREN
    | RPAREN
    | LBRACE
    | RBRACE
    | FUNCTION
    | LET
    
    /// Returns the name of the 'TokenType' case as coded
    static member ToCaseString tokenType =
        match FSharpValue.GetUnionFields(tokenType, typeof<TokenType>) with
        | case, _ -> case.Name
    
    static member ToString tokenType =
        match tokenType with
        | ILLEGAL -> "ILLEGAL" 
        | EOF -> "EOF" 
        | IDENT -> "IDENT" 
        | INT -> "INT" 
        | ASSIGN -> "=" 
        | PLUS -> "+" 
        | COMMA -> "," 
        | SEMICOLON -> ";" 
        | LPAREN ->  "("
        | RPAREN ->  ")"
        | LBRACE ->  "{"
        | RBRACE ->  "}"
        | FUNCTION ->  "FUNCTION"
        | LET -> "LET"
        
    static member FromString tokenAsString =
        match tokenAsString with
        | "ILLEGAL" -> ILLEGAL 
        | "EOF" -> EOF 
        | "INDENT" -> IDENT 
        | "INT" -> INT 
        | "=" -> ASSIGN 
        | "+" -> PLUS 
        | "," -> COMMA 
        | ";" -> SEMICOLON 
        | "(" -> LPAREN
        | ")" -> RPAREN
        | "{" -> LBRACE
        | "}" -> RBRACE
        | "FUNCTION" -> FUNCTION 
        | "LET" -> LET
        | _ -> raise (ArgumentException($"The token \"{tokenAsString}\" is not a valid token."))
     
     
module Keywords =
    let keywordToTokenTypeDict = dict [
        "fn", FUNCTION
        "let", LET
    ]
    
    let lookupIdent (ident: string) =
        match keywordToTokenTypeDict.TryGetValue ident with
        | (true, tokenType) -> tokenType
        | _ -> IDENT 
    
        
type Token =
    { Type: TokenType
      Literal: string }