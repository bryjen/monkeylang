module Monkey.Frontend.CLR.Parsers.ModifiedRecursiveDescent

open Microsoft.CodeAnalysis
open Monkey.Frontend.CLR.Helpers.Queue
open Monkey.Frontend.CLR.Token

type ParseError =
    { temp: string }

[<AutoOpen>]
module private Helpers =
    type MutableParserState =
        { Tokens: Token array
          mutable CurrentToken: int
          mutable ParseErrors: ParseError list }

let rec parseTokens (tokens: Token array) : SyntaxTree =
    let parserState = { Tokens = tokens; CurrentToken = 0; ParseErrors = [] }
    
    
    failwith "todo"
    
