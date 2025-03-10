module Frontend.CLR.Parsers.ParseErrors

open System
open Microsoft.CodeAnalysis.CSharp
open Monkey.Frontend.CLR.Token

let nullIfNone (opt: 'T option) : 'T  =
    match opt with
    | Some value -> value
    | None -> null
    
let defaultIfNone (opt: 'T option) (defaultValue: 'T) : 'T =
    match opt with
    | Some value -> value
    | None -> defaultValue
    
    
    
(* General Errors *)
    
type IntParseError(failedParseStr: string) =
    inherit Exception(IntParseError.FormatMessage(failedParseStr), null)
with
    static member private FormatMessage(failedParseStr) =
        $"Failed to parse \"{failedParseStr}\" to an int."
        
type InvalidPrefixOperatorError(token: Token) =
    inherit Exception(InvalidPrefixOperatorError.FormatMessage(token), null)
with
    static member private FormatMessage(token) =
        $"Unrecognized prefix operator \"{token.Literal}\" of type \"{token.Type}\""
    
    
    
(* Parser Errors *)

/// Exception type indicating a general exception.
type ParseError(
        ?message: string,
        ?token: Token option,
        ?innerException: Exception) =
    inherit Exception(nullIfNone message, nullIfNone innerException)
  
/// 
type LiteralExpressionParseError(
        ?syntaxKind: SyntaxKind,
        ?message: string,
        ?token: Token option,
        ?innerException: Exception) =
    inherit ParseError(nullIfNone message, None, nullIfNone innerException)
