[<AutoOpen>]
module Frontend.CLR.Parsers.ModifiedRecursiveDescentHelpers

open Frontend.CLR.Parsers.ParseErrors
open Microsoft.CodeAnalysis.CSharp.Syntax
open Monkey.Frontend.CLR.Token


/// <summary>
/// Mutable type containing the main parser state.
/// </summary>
type internal ParserState(tokens: Token array) =
    let mutable currentIdx: int = 0
    let mutable parseErrors: ParseError list = []
    let mutable statements: StatementSyntax list = []
    
    member this.Tokens = tokens
    
    member this.CurrentIdx
        with get() = currentIdx
        and set(value) = currentIdx <- value

    member this.ParseErrors
        with get() = parseErrors
        and set(value) = parseErrors <- value
        
    member this.Statements
        with get() = statements
        and set(value) = statements <- value
    
with
    member this.TokensLength() =
        Array.length this.Tokens
        
    member this.IsEof() =
        let isEof =
            match this.Tokens[this.CurrentIdx].Type with
            | EOF -> true
            | _ -> false
        
        (this.CurrentIdx >= Array.length this.Tokens) || isEof
        
    member this.PeekToken() =
        this.Tokens[this.CurrentIdx]
        
    member this.PopToken() =
        let token = this.Tokens[this.CurrentIdx]
        this.CurrentIdx <- this.CurrentIdx + 1
        token
        
    member this.GetTokenAt(index: int) =
        this.Tokens[index]


[<AutoOpen>]
module ParserStateHelpers =
    let isSemicolon (tokenType: TokenType) = tokenType = TokenType.SEMICOLON

    let isRParen (tokenType: TokenType) = tokenType = TokenType.RPAREN
            
    let rec internal consumeUntilTokenType (tokenTypePredicate: TokenType -> bool) (parserState: ParserState) =
        match parserState.IsEof() with
        | true ->
            parserState
        | false ->
            let token = parserState.PeekToken()
            match token.Type with
            | tokenType when (tokenTypePredicate tokenType) || tokenType = EOF ->
                parserState
            | _ ->
                parserState.CurrentIdx <- parserState.CurrentIdx + 1
                consumeUntilTokenType tokenTypePredicate parserState


/// <summary>
/// Operator precedence when determining how to order nested infix expressions.
/// </summary>
type internal Precedence =
    | LOWEST = 1
    | EQUALS = 2
    | LESSGREATER = 3
    | SUM = 4
    | PRODUCT = 5
    | PREFIX = 6
    | CALL = 7
    | INDEX = 8
    
let internal tokenTypeToPrecedenceMap = Map.ofList [
    (EQ, Precedence.EQUALS)
    (NOT_EQ, Precedence.EQUALS)
    (LT, Precedence.LESSGREATER)
    (GT, Precedence.LESSGREATER)
    (PLUS, Precedence.SUM)
    (MINUS, Precedence.SUM)
    (SLASH, Precedence.PRODUCT)
    (ASTERISK, Precedence.PRODUCT)
    (LPAREN, Precedence.CALL)
    (LBRACKET, Precedence.INDEX)
]
