module Monkey.Frontend.CLR.Tests.Parser.ParserTests

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Monkey.Frontend.CLR.Tests.Parser.Helpers
open NUnit.Framework


(*
A reference of Monkey source code in "Writing a compiler in Go"

```Monkey
let name = "Monkey";
let age = 1;
let inspirations = ["Scheme", "Lisp", "JavaScript", "Clojure"];
let book = {
    "title": "Writing A Compiler In Go",
    "author": "Thorsten Ball",
    "prequel": "Writing An Interpreter In Go"
};

let printBookName = fn(book) {
    let title = book["title"];
    let author = book["author"];
    puts(author + "- " + title);
};

printBookName(book);
// => prints: "Thorsten Ball - Writing A Compiler In Go"

let fibonacci = fn(x) {
    if (x == 0) {
        0
    } else {
        if (x == 1) {
            return 1;
        } else {
            fibonacci(x- 1) + fibonacci(x- 2);
        }
    }
};

let map = fn(arr, f) {
    let iter = fn(arr, accumulated) {
        if (len(arr) == 0) {
            accumulated
        } else {
            iter(rest(arr), push(accumulated, f(first(arr))));
        }
    };
    
    iter(arr, []);
};

let numbers = [1, 1 + 1, 4- 1, 2 * 2, 2 + 3, 12 / 2];
map(numbers, fibonacci);
// => returns: [1, 1, 2, 3, 5, 8]
```
*)


[<TestFixture>]
type Runner() =
    [<Test>]
    member this.Runner() =
        Assert.Pass()
        
        
        
        
[<TestFixture>]
type ExpressionParsingTests() =
    [<Test>]
    member this.Runner() =
        Assert.Pass()
        
        
[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type NumericExpressionParsingTests() =
    // TODO: See if the declarations of the token are required (ex. the parentheses token in expression)
    let testCases: (string * SyntaxNode) list = [
        ("5", SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5)))
        
        ("-5", SyntaxFactory.PrefixUnaryExpression(
                   SyntaxKind.UnaryMinusExpression,
                   SyntaxFactory.Token(SyntaxKind.MinusToken),
                   SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5))))
        
        ("(5)", SyntaxFactory.ParenthesizedExpression(
                   SyntaxFactory.Token(SyntaxKind.OpenParenToken),
                   SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5)),
                   SyntaxFactory.Token(SyntaxKind.OpenParenToken)))
        
        ("(-5)", SyntaxFactory.ParenthesizedExpression(
                   SyntaxFactory.Token(SyntaxKind.OpenParenToken),
                   SyntaxFactory.PrefixUnaryExpression(
                       SyntaxKind.UnaryMinusExpression,
                       SyntaxFactory.Token(SyntaxKind.MinusToken),
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5))),
                   SyntaxFactory.Token(SyntaxKind.OpenParenToken)))
    ]
    
    [<Test>]
    member this.``something``() =
        Assert.Pass()
        
        
        
[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type BooleanExpressionParsingTests() =
    // TODO: See if the declarations of the token are required (ex. the parentheses token in expression)
    let testCases: (string * SyntaxNode) list = [
        ("true", SyntaxFactory.LiteralExpression(SyntaxKind.TrueKeyword))
        ("false", SyntaxFactory.LiteralExpression(SyntaxKind.FalseKeyword))
        ("!true", SyntaxFactory.PrefixUnaryExpression(
                   SyntaxKind.LogicalNotExpression,
                   SyntaxFactory.Token(SyntaxKind.ExclamationToken),
                   SyntaxFactory.LiteralExpression(SyntaxKind.TrueKeyword)))
        ("!false", SyntaxFactory.PrefixUnaryExpression(
                   SyntaxKind.LogicalNotExpression,
                   SyntaxFactory.Token(SyntaxKind.ExclamationToken),
                   SyntaxFactory.LiteralExpression(SyntaxKind.FalseKeyword)))
    ]
    
    [<Test>]
    member this.``something``() =
        Assert.Pass()
        
        
        
[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type BasicNumericExpressionParsingTests() =
    // TODO: See if the declarations of the token are required (ex. the parentheses token in expression)
    let testCases: (string * SyntaxNode) list = [
        ("5 + 5", SyntaxFactory.BinaryExpression(
                       SyntaxKind.AddExpression,
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5)),
                       SyntaxFactory.Token(SyntaxKind.PlusToken),
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5))))
        ("5 - 5", SyntaxFactory.BinaryExpression(
                       SyntaxKind.SubtractExpression,
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5)),
                       SyntaxFactory.Token(SyntaxKind.PlusToken),
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5))))
        ("5 * 5", SyntaxFactory.BinaryExpression(
                       SyntaxKind.MultiplyExpression,
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5)),
                       SyntaxFactory.Token(SyntaxKind.AsteriskToken),
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5))))
        ("5 / 5", SyntaxFactory.BinaryExpression(
                       SyntaxKind.DivideExpression,
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5)),
                       SyntaxFactory.Token(SyntaxKind.SlashToken),
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5))))
        ("5 > 5", SyntaxFactory.BinaryExpression(
                       SyntaxKind.GreaterThanExpression,
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5)),
                       SyntaxFactory.Token(SyntaxKind.GreaterThanToken),
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5))))
        ("5 < 5", SyntaxFactory.BinaryExpression(
                       SyntaxKind.LessThanExpression,
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5)),
                       SyntaxFactory.Token(SyntaxKind.LessThanToken),
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5))))
        ("5 == 5", SyntaxFactory.BinaryExpression(
                       SyntaxKind.EqualsExpression,
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5)),
                       SyntaxFactory.Token(SyntaxKind.EqualsEqualsToken),
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5))))
        ("5 != 5", SyntaxFactory.BinaryExpression(
                       SyntaxKind.EqualsExpression,
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5)),
                       SyntaxFactory.Token(SyntaxKind.NotEqualsExpression),
                       SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralToken, SyntaxFactory.Literal(5))))
    ]
    
    [<Test>]
    member this.``something``() =
        Assert.Pass()
        
        
        
[<TestFixture>]
[<ParserComponent(ParserComponentType.Statements)>]
[<ParserComponentDependsOn(ParserComponentType.Expressions)>]
type BasicVariableAssignmentParsingTests() =
    [<Test>]
    member this.``something``() =
        Assert.Pass()
