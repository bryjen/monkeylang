// Due to F# limitations, we can not declare types across several files for a better project structure.
module rec Monkey.Frontend.CLR.Syntax.Ast

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text



type SyntaxToken =
    { Kind: SyntaxKind
      Text: string
      Value: obj option
      
      TextSpan: TextSpan  // excludes trivia
      FullTextSpan: TextSpan  // includes trivia
      
      LeadingTrivia: SyntaxTriviaList
      TrailingTrivia: SyntaxTriviaList }
with
    override this.ToString() =
        $"{this.LeadingTrivia.ToFullString()}{this.Text}{this.TrailingTrivia.ToFullString()}"
    


/// <summary>
/// Represents a non-terminal node in the syntax tree.
/// </summary>
type MonkeySyntaxNode =
    | CompilationUnitSyntax
    | UsingDeclarationSyntax
    | ArgumentListSyntax of ArgumentListSyntax
    | ParameterListSyntax of ParameterListSyntax
    | ExpressionSyntax of ExpressionSyntax
    | StatementSyntax of StatementSyntax



/// <summary>
/// The parent type for all expression syntax types.
/// </summary>
type ExpressionSyntax =
    // | ArrayInitializationExpression
    // | ParenthesizedExpressionSyntax
    | FunctionExpression of FunctionExpression
    | BinaryExpression of BinaryExpression
    | InterpolatedStringExpression of InterpolatedStringExpression
    | InvocationExpressionSyntax of InvocationExpressionSyntax
    | LiteralExpressionSyntax of LiteralExpressionSyntax
    | PostfixExpressionSyntax of PostfixExpressionSyntax
    | PrefixExpressionSyntax of PrefixExpressionSyntax
    | TypeSyntax of TypeSyntax
    
    
    
/// <summary>
/// The parent type for all statement syntax types.
/// </summary>
type StatementSyntax =
    | BlockSyntax of BlockSyntax
    | ExpressionStatementSyntax of ExpressionStatementSyntax
    | IfStatementSyntax of IfStatementSyntax
    | VariableDeclarationStatementSyntax of VariableDeclarationStatementSyntax
    
    
    
(* #REGION Supporting Types *)

type TypeSyntax =
    | NameSyntax of NameSyntax
    | BuiltinTypeSyntax of BuiltinTypeSyntax
    | FunctionTypeSyntax of FunctionTypeSyntax
/// <remarks>
/// Example: <code>TypeSyntax</code>
/// </remarks>
and NameSyntax =
    { Identifier: SyntaxToken }
/// <remarks>
/// Example: <code>int</code>
/// </remarks>
and BuiltinTypeSyntax =
    { Identifier: SyntaxToken }
/// <remarks>
/// Example: <code>[int -> int]</code>
/// </remarks>
and FunctionTypeSyntax =
    { ParameterTypes: TypeSyntax array }
    
    
/// <remarks>
/// Example: <code>&lt;&lt;fn&gt;&gt;(PARAM_TYPE_1 PARAM_1_NAME, PARAM_TYPE_2 PARAM_2_NAME, ..., PARAM_TYPE_N PARAM_N_NAME)</code>
/// </remarks>
type ParameterListSyntax =
    { OpenParenToken: SyntaxToken
      ParameterSyntax: ParameterSyntax array
      CloseParenToken: SyntaxToken }
/// <remarks>
/// Example: <code>PARAM_TYPE PARAM_NAME)</code>
/// </remarks>
and ParameterSyntax =
    { Type: TypeSyntax
      Identifier: SyntaxToken }
    

type ArgumentListSyntax =
    { OpenParenToken: SyntaxToken
      ArgumentSyntax: ArgumentSyntax array
      CloseParenToken: SyntaxToken }
and ArgumentSyntax =
    ExpressionSyntax
    
    
    
(* #REGION Expressions *)

type FunctionExpression =
    { ReturnType: TypeSyntax
      ParameterList: ParameterListSyntax
      Body: BlockSyntax }
    
    
type BinaryExpression =
    { Left: ExpressionSyntax
      OperatorToken: SyntaxToken
      Right: ExpressionSyntax }
    
    
type InterpolatedStringExpression =
    { InterpolatedStringStartToken: SyntaxToken
      Contents: InterpolatedStringContent array }
and InterpolatedStringContent =
    | InterpolatedStringText
    | Interpolation
and InterpolatedStringText =
    { TextToken: SyntaxToken }
and Interpolation =
    { OpenBraceToken: SyntaxToken
      Expression: ExpressionSyntax
      CloseBraceToken: SyntaxToken }
    
    
type InvocationExpressionSyntax =
    { LeftExpression: ExpressionSyntax  // typically a fn identifier or an inline function
      Arguments: ArgumentListSyntax }
    
    
type LiteralExpressionSyntax =
    { Kind: SyntaxKind
      Token: SyntaxToken }
    
    
type PrefixExpressionSyntax =
    { OperatorToken: SyntaxToken
      Operand: ExpressionSyntax }
    
type PostfixExpressionSyntax =
    { OperatorToken: SyntaxToken
      Operand: ExpressionSyntax }
    

    
    



(* #REGION Statements *)

type BlockSyntax =
    { OpenBraceToken: SyntaxToken
      Statements: StatementSyntax array
      CloseBraceToken: SyntaxToken }
    
type ExpressionStatementSyntax =
    { Expression: ExpressionSyntax
      SemicolonToken: SyntaxToken }
    
type IfStatementSyntax =
    { IfKeyword: SyntaxToken
      Condition: ExpressionSyntax
      Clause: BlockSyntax
      ElseIfClauses: ElseIfStatementSyntax array
      ElseClause: ElseClauseSyntax option }
and ElseIfStatementSyntax =
    { ElseKeyword: SyntaxToken
      IfKeyword: SyntaxToken
      Condition: ExpressionSyntax
      Clause: BlockSyntax }
and ElseClauseSyntax =
    { ElseKeyword: SyntaxToken
      ElseClause: BlockSyntax }
    
    
type VariableDeclarationStatementSyntax =
    { LetKeyword: SyntaxToken
      Name: SyntaxToken
      EqualsToken: SyntaxToken
      TypeAnnotation: VariableTypeAnnotation option }
and VariableTypeAnnotation =
    { ColonToken: SyntaxToken
      Type: TypeSyntax }