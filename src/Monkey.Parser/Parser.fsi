module Monkey.Parser.Parser

open Monkey.AST
open Monkey.Parser.Errors


    
/// <summary>
/// Constructs a syntax tree from a <c>SyntaxToken</c> array. 
/// </summary>
val parseTokens : SyntaxToken array -> MonkeyCompilationUnit * ParseError array


/// <summary>
/// Type encapsulating the parser's state as it builds the syntax tree using the given syntax tokens.
/// </summary>
/// <remarks>
/// <ul>
///     <li>
///         Exposed solely for testing purposes.
///     </li>
/// </ul>
/// </remarks>
[<Sealed>]
type internal MonkeyAstParserState =
   new : SyntaxToken array -> MonkeyAstParserState
   
   
   
/// <summary>
/// Module 
/// </summary>
/// <remarks>
/// <ul>
///     <li>
///         Exposed solely for testing purposes.
///     </li>
/// </ul>
/// </remarks>
module internal PrefixExpressions =
    
    val internal tryParseIdentifier : MonkeyAstParserState -> Result<IdentifierSyntax, ParseError>
    
    val internal tryParseStringLiteralExpression : MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>
    
    val internal tryParseNumericLiteralExpression : MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>
    
    val internal tryParseTrueLiteralExpression : MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>
    
    val internal tryParseFalseLiteralExpression : MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>
    
    val internal tryParseLogicalNotPrefixExpression : MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>
    
    val internal tryParseMinusPrefixExpression : MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>
    
    val internal tryParseParenthesizedExpression : MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>
    
    val internal tryParseIfExpression : MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>
    
    val internal tryParseTypeSyntax : ParseError -> MonkeyAstParserState -> Result<TypeSyntax, ParseError>
    
    val internal tryParseFunctionExpression : MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>
    
    val internal tryParseInvocationExpression : MonkeyAstParserState -> InvocationExpressionLeftExpression -> Result<ExpressionSyntax, ParseError>
    
    val internal tryParseListArrayInitializationExpression : MonkeyAstParserState -> Result<ExpressionSyntax, ParseError>
    
    val internal tryParseInterpolatedStringExpression : MonkeyAstParserState -> Result<InterpolatedStringExpressionSyntax, ParseError>
        
        
