[<AutoOpen>]
module rec Monkey.Frontend.CLR.Parsers.ParserHelpers

open Microsoft.CodeAnalysis.CSharp
open Monkey.Frontend.CLR.Parsers.ParsingErrors
open Monkey.Frontend.CLR.Syntax.Ast
                

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
    (SyntaxKind.EqualsEqualsToken, Precedence.EQUALS)
    (SyntaxKind.ExclamationEqualsToken, Precedence.EQUALS)
    (SyntaxKind.LessThanToken, Precedence.LESSGREATER)
    (SyntaxKind.GreaterThanToken, Precedence.LESSGREATER)
    (SyntaxKind.LessThanEqualsToken, Precedence.LESSGREATER)
    (SyntaxKind.GreaterThanEqualsToken, Precedence.LESSGREATER)
    (SyntaxKind.GreaterThanToken, Precedence.LESSGREATER)
    (SyntaxKind.PlusToken, Precedence.SUM)
    (SyntaxKind.MinusToken, Precedence.SUM)
    (SyntaxKind.SlashToken, Precedence.PRODUCT)
    (SyntaxKind.AsteriskToken, Precedence.PRODUCT)
    (SyntaxKind.OpenParenToken, Precedence.CALL)
    (SyntaxKind.OpenBracketToken, Precedence.INDEX)
]
    
    
    
/// <summary>
/// Mutable type containing the main parser state.
/// </summary>
type internal MonkeyAstParserState(tokens: SyntaxToken array) =
    let mutable currentIdx: int = 0
    let mutable errors: ResizeArray<ParseError> = ResizeArray<ParseError>()
    let mutable statements: ResizeArray<StatementSyntax> = ResizeArray<StatementSyntax>()
    
    member this.Tokens = tokens
    
    member this.CurrentIdx
        with get() = currentIdx
        and set(value) = currentIdx <- value

    member this.Errors
        with get() = errors
        
    member this.Statements
        with get() = statements
        
with
    member this.TokensLength() =
        Array.length this.Tokens
        
    member this.IsEof() =
        let isEof =
            match this.Tokens[this.CurrentIdx].Kind with
            | SyntaxKind.EndOfFileToken -> true
            | _ -> false
        
        (this.CurrentIdx >= Array.length this.Tokens) || isEof
        
    member this.PeekToken() : SyntaxToken =
        this.Tokens[this.CurrentIdx]
        
    member this.PeekToken(offset: int) : SyntaxToken =
        this.Tokens[this.CurrentIdx + offset]
        
    member this.CanPeek(offset: int) : bool =
        match offset + this.CurrentIdx with
        | i when i >= 0 && i <= this.Tokens.Length - 1 -> true
        | _ -> false
        
    member this.PopToken() : SyntaxToken =
        let token = this.Tokens[this.CurrentIdx]
        this.CurrentIdx <- this.CurrentIdx + 1
        token
        
    member this.GetTokenAt(index: int) : SyntaxToken =
        this.Tokens[index]
       
        
    member this.RecoverFromParseError() : unit =
        consumeUntilTokenType (fun kind -> kind = SyntaxKind.SemicolonToken || kind = SyntaxKind.EndOfFileToken) this
        |> ignore



let isSemicolon (tokenType: SyntaxKind) = tokenType = SyntaxKind.SemicolonToken

let isCloseParen (tokenType: SyntaxKind) = tokenType = SyntaxKind.CloseParenToken

let isCloseBracket (tokenType: SyntaxKind) = tokenType = SyntaxKind.CloseBracketToken
        
        
let rec internal consumeUntilTokenType (tokenTypePredicate: SyntaxKind -> bool) (parserState: MonkeyAstParserState) =
    match parserState.IsEof() with
    | true ->
        parserState
    | false ->
        let token = parserState.PeekToken()
        match token.Kind with
        | tokenType when (tokenTypePredicate tokenType) || tokenType = SyntaxKind.EndOfFileToken ->
            parserState
        | _ ->
            parserState.CurrentIdx <- parserState.CurrentIdx + 1
            consumeUntilTokenType tokenTypePredicate parserState
            
    
let internal assertNoParseErrors (parseErrors: ParseError array) =
    match parseErrors with
    | [|  |] ->
        Ok ()
    | _ ->
        Error parseErrors[0]  // we're guaranteed at least one element
