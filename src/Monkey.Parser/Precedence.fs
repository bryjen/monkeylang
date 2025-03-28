module Monkey.Parser.Precedence

open Microsoft.CodeAnalysis.CSharp

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
    
let internal getSyntaxKindPrecedence (syntaxKind: SyntaxKind) : Precedence =
    match syntaxKind with
    | SyntaxKind.EqualsEqualsToken -> Precedence.EQUALS
    | SyntaxKind.ExclamationEqualsToken -> Precedence.EQUALS
    | SyntaxKind.LessThanToken -> Precedence.LESSGREATER
    | SyntaxKind.GreaterThanToken -> Precedence.LESSGREATER
    | SyntaxKind.LessThanEqualsToken -> Precedence.LESSGREATER
    | SyntaxKind.GreaterThanEqualsToken -> Precedence.LESSGREATER
    | SyntaxKind.PlusToken -> Precedence.SUM
    | SyntaxKind.MinusToken -> Precedence.SUM
    | SyntaxKind.SlashToken -> Precedence.PRODUCT
    | SyntaxKind.AsteriskToken -> Precedence.PRODUCT
    | SyntaxKind.OpenParenToken -> Precedence.CALL
    | SyntaxKind.OpenBracketToken -> Precedence.INDEX
    | _ -> Precedence.LOWEST
