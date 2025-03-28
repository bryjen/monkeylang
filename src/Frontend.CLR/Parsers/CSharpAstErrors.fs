﻿module Monkey.Frontend.CLR.Parsers.CSharpAstErrors

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
        
type InvalidInfixOperatorError(token: Token) =
    inherit Exception(InvalidInfixOperatorError.FormatMessage(token), null)
with
    static member private FormatMessage(token) =
        $"Unrecognized infix operator \"{token.Literal}\" of type \"{token.Type}\""
    
    
    
(* Parser Errors *)

/// Exception type indicating a general exception.
type CSharpParseError(
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
    inherit CSharpParseError(nullIfNone message, None, nullIfNone innerException)
    
///
type LetStatementParseError(
        ?message: string,
        ?innerException: Exception) =
    inherit CSharpParseError(nullIfNone message, None, nullIfNone innerException)
    
///
type InlineIfExpressionParseError(
        ?message: string,
        ?innerException: Exception) =
    inherit CSharpParseError(nullIfNone message, None, nullIfNone innerException)
    

///
type FunctionCallExpressionParseError(
        ?message: string,
        ?innerException: Exception) =
    inherit CSharpParseError(nullIfNone message, None, nullIfNone innerException)
    
///
type FunctionExpressionParseError(
        ?message: string,
        ?innerException: Exception) =
    inherit CSharpParseError(nullIfNone message, None, nullIfNone innerException)
