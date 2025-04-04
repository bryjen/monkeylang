module Monkey.Parser.Tokenizer

/// <summary>
/// Parses an input string into an array of Monkey syntax tokens.
/// </summary>
/// <remarks>
/// <ul>
///     <li>
///         Syntax trivia is assigned to the following token as preceding syntax trivia.
///     </li>
/// </ul>
/// </remarks>
val tokenize : string -> Monkey.AST.SyntaxToken array
