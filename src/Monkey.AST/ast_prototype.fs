﻿(*
Prototyping 'new' version of the AST. It still retains the same contents, however the structure of how they're represented
will most likely be different.

The primary goal is for each syntax type to share a common interface.

- If I wish to expand a syntax node that is represented by a union case, then i'd have to change it to a record type, 
  extract the union, then have it as a property/value of the new record type. Which is why all union cases below have 
  class/record types as their corresponding value, having union cases only represent 'type' of the specific syntax 
  node we're dealing with.

Inspired by:
- Roslyn C# AST representation: 
https://learn.microsoft.com/en-us/dotnet/csharp/roslyn-sdk/work-with-syntax
https://learn.microsoft.com/en-us/dotnet/csharp/roslyn-sdk/get-started/syntax-analysis

- clojure-clr-next AST representation:
https://github.com/dmiller/clojure-clr-next/blob/main/src/Clojure.Next/Clojure.Compiler/Expr.fs
*)


namespace Monkey.AST

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text


// TODO: Evaluate validity
type internal ParserContext =
    | Expression
    | Statement
type internal CompilationUnitEnv =
    { temp: obj }


// commented out to definition collision
(*
type MonkeyCompilationUnit =
    { temp: obj }
*)
    


[<Sealed>]
type SyntaxToken internal
    (
        kind: SyntaxKind,
        text: string,
        value: obj,
        textSpan: TextSpan,
        fullTextSpan: TextSpan,
        leadingTrivia: SyntaxTriviaList,
        trailingTrivia: SyntaxTriviaList
    ) =
    member val Kind: SyntaxKind
    member val Text: string
    member val Value: obj
    
    member val TextSpan: TextSpan  // excludes trivia
    member val FullTextSpan: TextSpan  // includes trivia
    
    member val LeadingTrivia: SyntaxTriviaList
    member val TrailingTrivia: SyntaxTriviaList
    
    
type IUnionBase =
    abstract member ToString: unit -> string
    
    
[<AbstractClass>]
type SyntaxNodeBase() =
    member val SourceText: SourceText = Operators.Unchecked.defaultof<SourceText> with get
    member val TextSpan: TextSpan = TextSpan() with get
    member val SyntaxNodeType: SyntaxNodeType = Operators.Unchecked.defaultof<SyntaxNodeType> with get
    
    
    
    

and SyntaxNodeType =
    | UsingDirective         of  UsingDirective
    | NamespaceDeclaration   of  NamespaceDeclaration
    | Expression             of  Expression
    | Statement              of  StatementType
with
    interface IUnionBase with
        member this.ToString() =
            match this with
            | UsingDirective usingDirective -> failwith "todo"
            | NamespaceDeclaration namespaceDeclaration -> failwith "todo"
            | Expression expression -> failwith "todo"
            | Statement statementType -> failwith "todo"
    
    
/// Represents a using directive in a monkey compilation unit.
and [<Sealed>] UsingDirective() =
    inherit SyntaxNodeBase()
with
    member val UsingToken: SyntaxToken
    member val Name: IdentifierNameExpr
    member val SemicolonToken: SyntaxToken
    
    
/// Represents a namespace declaration in a monkey compilation unit.
and [<Sealed>] NamespaceDeclaration() =
    inherit SyntaxNodeBase()
with
    member val NamespaceToken: SyntaxToken
    member val Name: IdentifierNameExpr
    member val SemicolonToken: SyntaxToken
    
    
/// Represents a list of arguments.
and [<Sealed>] ArgList() =
    inherit SyntaxNodeBase()
with
    member val OpenParenToken: SyntaxToken
    member val Arguments: Expression array
    member val Commas: SyntaxToken array
    member val CloseParenToken: SyntaxToken
    
    
/// Represents a list of parameters.
and [<Sealed>] ParamList() =
    inherit SyntaxNodeBase()
with
    member val OpenParenToken: SyntaxToken
    member val Parameters: Parameter array
    member val Commas: SyntaxToken array
    member val CloseParenToken: SyntaxToken
    
    
/// Represents a singular parameter.
and [<Sealed>] Parameter() =
    inherit SyntaxNodeBase()
with
    member val Type: TypeExpr
    member val Identifier: IdentifierNameExpr



and ExpressionType =
    | ParenthesizedExpr       of  ParenthesizedExpr
    | FunctionExpr            of  FunctionExpr
    | BinaryExpr              of  BinaryExpr
    | InterpolatedStringExpr  of  InterpolatedStringExpr
    | InvocationExpr          of  InvocationExpr
    | LiteralExpr             of  LiteralExpr
    | PostfixExpr             of  PostfixExpr
    | PrefixExpr              of  PrefixExpr
    | IdentifierNameExpr      of  IdentifierNameExpr
    | TypeExpr                of  TypeExpr
    | IfExpr                  of  IfExpr
    | ArrayExpr               of  ArrayExpr
  
  
and Expression internal () =
    inherit SyntaxNodeBase()
with
    member val ExprType: ExpressionType

  
/// Represents a parenthesized expression.
and [<Sealed>] ParenthesizedExpr internal () =
    inherit Expression()
with
    member val OpenParenToken: SyntaxToken
    member val Expr: Expression
    member val CloseParenToken: SyntaxToken
    
    
and [<Sealed>] FunctionExpr internal () =
    inherit Expression()
with
    member val FnKeywordToken: SyntaxToken
    member val ParamList: Expression
    member val ReturnType: FunctionReturnExpr option
    member val Body: BlockStatement
    
and [<Sealed>] FunctionReturnExpr internal () =
    member val ColonToken: SyntaxToken
    member val ReturnType: TypeExpr
    
    
and [<Sealed>] BinaryExpr internal () =
    inherit Expression()
with
    member val ExprKind: SyntaxKind
    member val LeftExpr: Expression
    member val OperatorToken: SyntaxToken
    member val RightExpr: Expression
    
    
and [<Sealed>] InterpolatedStringExpr internal () =
    inherit Expression()
with
    member val InterpolatedStringStartToken: SyntaxToken
    member val Contents: InterpolatedStringContents array
    member val InterpolatedStringEndToken: SyntaxToken
    
and InterpolatedStringContents =
    | InterpolatedStringText of InterpolatedStringText
    | Interpolation of Interpolation
    
and [<Sealed>] InterpolatedStringText internal () =
    inherit SyntaxNodeBase()
with
    member val Value: SyntaxToken
    
and [<Sealed>] Interpolation internal () =
    inherit SyntaxNodeBase()
with
    member val OpenBraceToken: SyntaxToken
    member val Expression: Expression
    member val CloseBraceToken: SyntaxToken
    
    
and [<Sealed>] InvocationExpr internal () =
    inherit Expression()
with
    member val OpenBraceToken: SyntaxToken
    member val Expression: InvocationLeftExpression
    member val CloseBraceToken: SyntaxToken
    
and InvocationLeftExpression =
    | InvocParenthesizedFunctionExpr of ParenthesizedExpr
    | InvocFunctionExpr of FunctionExpr
    | InvocIdentifierNameExpr of IdentifierNameExpr
    
    
and [<Sealed>] LiteralExpr internal () =
    inherit Expression()
with
    member val LiteralToken: SyntaxToken
    
    
and [<Sealed>] PostfixExpr internal () =
    inherit Expression()
with
    member val Expression: Expression
    member val PostfixOperator: PostfixOperator
    
/// Valid postfix operators.
and PostfixOperator =
    | PlusPlus of SyntaxToken
    | MinusMinus of SyntaxToken
    
    
and [<Sealed>] PrefixExpr internal () =
    inherit Expression()
with
    member val Expression: Expression
    member val PrefixOperator: PrefixOperator
    
and PrefixOperator =
    | PlusPlus of SyntaxToken
    | MinusMinus of SyntaxToken  // decrement
    | Minus of SyntaxToken  // arithmetic negation
    | Exclamation of SyntaxToken  // logical negation
    
    
and [<Sealed>] IdentifierNameExpr internal () =
    inherit Expression()
with
    member val IdentifierNameType: IdentifierNameType

and IdentifierNameType =
    | SimpleIdentifier of SimpleIdentifier
    | QualifiedIdentifier of QualifiedIdentifier
    
and [<Sealed>] SimpleIdentifier internal () =
    inherit SyntaxNodeBase()
with
    member val IdentifierToken: SyntaxToken
    
and [<Sealed>] QualifiedIdentifier internal () =
    inherit SyntaxNodeBase()
with
    member val Tokens: SyntaxToken
    member val Dots: SyntaxToken
    
    
and [<Sealed>] TypeExpr internal () =
    inherit Expression()
with
    member val Type: Type
and Type =
    | NameSyntax    of NameSyntax
    | BuiltinType   of BuiltinType
    | FunctionType  of FunctionType
    | ArrayType     of ArrayType
    | GenericType   of GenericType
    
and [<Sealed>] NameSyntax internal () =
    inherit SyntaxNodeBase()
with
    member val NameToken: IdentifierNameExpr 
    
and [<Sealed>] BuiltinType internal () =
    inherit SyntaxNodeBase()
with
    member val TypeToken: SyntaxToken
    
and [<Sealed>] FunctionType internal () =
    inherit SyntaxNodeBase()
with
    member val OpenBracketToken: SyntaxToken
    member val Types: TypeExpr array
    member val ArrowTokens: SyntaxToken array
    member val CloseBracketToken: SyntaxToken
    
and [<Sealed>] ArrayType internal () =
    inherit SyntaxNodeBase()
with
    member val Type: TypeExpr
    member val OpenBracketToken: SyntaxToken
    member val CloseBracketToken: SyntaxToken
    
and [<Sealed>] GenericType internal () =
    inherit SyntaxNodeBase()
with
    member val Type: TypeExpr
    member val LessThanToken: SyntaxToken
    member val GenericTypes: TypeExpr array
    member val Commas: SyntaxToken array
    member val GreaterThanToken: SyntaxToken
    
    
    
and [<Sealed>] IfExpr internal () =
    inherit Expression()
with
    member val IfKeyword: SyntaxToken
    member val OpenParenToken: SyntaxToken
    member val Condition: Expression
    member val CloseParenToken: SyntaxToken
    member val Clause: BlockStatement
    member val ElseIfClauses: ElseIfClause array
    member val ElseClause: ElseClause option
    
and [<Sealed>] ElseIfClause internal () =
    inherit SyntaxNodeBase()
with
    member val ElseKeyword: SyntaxToken
    member val IfKeyword: SyntaxToken
    member val Condition: Expression
    member val Clause: BlockStatement
    
and [<Sealed>] ElseClause internal () =
    inherit SyntaxNodeBase()
with
    member val ElseKeyword: SyntaxToken
    member val ElseClause: BlockStatement
    
    
and [<Sealed>] ArrayExpr internal () =
    inherit Expression()
    
    

    
and Statement internal () =
    inherit SyntaxNodeBase()
with
    member val StatementType: StatementType
    
and StatementType =
    | BlockStatement of BlockStatement 
    | ExpressionStatement of ExpressionStatement
    | VariableDeclarationStatement of VariableDeclarationStatement
    

and [<Sealed>] BlockStatement internal () =
    inherit Statement()
    
and [<Sealed>] ExpressionStatement internal () =
    inherit Statement()
    
and [<Sealed>] VariableDeclarationStatement internal() =
    inherit Statement()
    

