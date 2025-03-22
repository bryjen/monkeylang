[<AutoOpen>]
[<System.Obsolete>]
module Monkey.Frontend.CLR.Parsers.ParserHelpers

open System

open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.CSharp
open type Microsoft.CodeAnalysis.CSharp.SyntaxFactory

open FsToolkit.ErrorHandling

open Monkey.Frontend.CLR.Token
open Monkey.Frontend.CLR.Parsers.CSharpAstErrors


/// <summary>
/// Mutable type containing the main parser state.
/// </summary>
type internal CSharpAstParserState(tokens: Token array) =
    let mutable currentIdx: int = 0
    let mutable parseErrors: CSharpParseError list = []
    let mutable statements: StatementSyntax list = []
    let mutable lambdaTypeMap: Map<string, LambdaFunctionSignature> = Map.empty
    
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
        
    /// Map containing function signatures of lambda expressions (since their syntax does not contain any type info as
    /// compared to normal member methods.
    member this.LambdaTypeMap
        with get() = lambdaTypeMap
        and set(value) = lambdaTypeMap <- value
    
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
        
and LambdaFunctionSignature =
    { ParameterTypes: TypeSyntax array
      ReturnType: TypeSyntax }
with
    member this.ToFuncTypeSyntax() =
        let types = Array.append this.ParameterTypes [| this.ReturnType |]
        let commas = Array.create ((Array.length types) - 1) (Token(SyntaxKind.CommaToken))
        GenericName(Identifier("Func"))
            .WithTypeArgumentList(
                TypeArgumentList(
                    SeparatedList<TypeSyntax>(types, commas)))


[<AutoOpen>]
module ParserStateHelpers =
    let isSemicolon (tokenType: TokenType) = tokenType = TokenType.SEMICOLON

    let isRParen (tokenType: TokenType) = tokenType = TokenType.RPAREN
            
    let rec internal consumeUntilTokenType (tokenTypePredicate: TokenType -> bool) (parserState: CSharpAstParserState) =
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


let internal defaultHashLen = 16

let internal generateRandomStringHash n =
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ0123456789"
    let random = Random()
    Array.init n (fun _ -> chars.[random.Next(chars.Length)]) |> String
    
    
let internal onUnexpectedToken (expectedTokenType: TokenType) (actualToken: Token) =
    let errorMsg = $"Expected a/an \"{expectedTokenType}\", but received \"{actualToken.Literal}\" of type \"{actualToken.Type}\""
    Error (CSharpParseError(message=errorMsg))

let internal assertAndPop (expectedTokenType: TokenType) (parserState: CSharpAstParserState) =
    let currentToken = parserState.PopToken()
    match currentToken.Type with
    | tokenType when tokenType = expectedTokenType -> Ok ()
    | _ -> consumeUntilTokenType isSemicolon parserState |> ignore
           onUnexpectedToken expectedTokenType currentToken

let internal assertNoParseErrors (parseErrors: CSharpParseError list) =
    match parseErrors with
    | [ ] ->
        Ok ()
    | head :: _ ->
        Error head
