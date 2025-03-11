module Monkey.Frontend.CLR.Tests.Parser.ParserTests

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open type Microsoft.CodeAnalysis.CSharp.SyntaxFactory
open Monkey.Frontend.CLR.Lexer
open Monkey.Frontend.CLR.Parsers
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
[<ParserComponent(ParserComponentType.Expressions)>]
type NumericExpressionParsingTests() =
    member this.TestCases : (string * SyntaxNode) list = [
        (
            "5;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5))
            )
        )
        
        (
            "-5;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.PrefixUnaryExpression(
                    SyntaxKind.UnaryMinusExpression,
                    SyntaxFactory.Token(SyntaxKind.MinusToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5))))
        )
        
        (
            "(5);",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.ParenthesizedExpression(
                    SyntaxFactory.Token(SyntaxKind.OpenParenToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression,  SyntaxFactory.Literal(5)),
                    SyntaxFactory.Token(SyntaxKind.CloseParenToken)))
        )
        
        (
            "(-5);",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.ParenthesizedExpression(
                    SyntaxFactory.Token(SyntaxKind.OpenParenToken),
                    SyntaxFactory.PrefixUnaryExpression(
                        SyntaxKind.UnaryMinusExpression,
                        SyntaxFactory.Token(SyntaxKind.MinusToken),
                        SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5))),
                    SyntaxFactory.Token(SyntaxKind.CloseParenToken))))
            
    ]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    member this.``A: Test Basic Numeric Expression Parsing``(testCaseIndex: int) =
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = ModifiedRecursiveDescent.parseTokens tokens
        match List.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = List.toArray syntaxNodes |> Array.map (fun x -> x :> SyntaxNode)
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareSyntaxNodes input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
        
        
[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type BooleanExpressionParsingTests() =
    member this.TestCases : (string * SyntaxNode) list = [
        (
            "true;",
            SyntaxFactory.ExpressionStatement(SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression))
        )
        (
            "false;",
            SyntaxFactory.ExpressionStatement(SyntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression))
        )
        (
            "!true;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.PrefixUnaryExpression(
                    SyntaxKind.LogicalNotExpression,
                    SyntaxFactory.Token(SyntaxKind.ExclamationToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.TrueLiteralExpression)))
        )
        (
            "!false;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.PrefixUnaryExpression(
                    SyntaxKind.LogicalNotExpression,
                    SyntaxFactory.Token(SyntaxKind.ExclamationToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.FalseLiteralExpression)))
        )
    ]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    member this.``B. Test Basic Boolean Expression Parsing``(testCaseIndex: int) =
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = ModifiedRecursiveDescent.parseTokens tokens
        match List.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = List.toArray syntaxNodes |> Array.map (fun x -> x :> SyntaxNode)
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareSyntaxNodes input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
        
        
[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type BasicInfixExpressionParsingTests() =
    member this.TestCases : (string * SyntaxNode) list = [
        (
            "5 + 5;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.AddExpression,
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5)),
                    SyntaxFactory.Token(SyntaxKind.PlusToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5))))
        )
        (
            "5 - 5;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.SubtractExpression,
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5)),
                    SyntaxFactory.Token(SyntaxKind.MinusToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5))))
        )
        (
            "5 * 5;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.MultiplyExpression,
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5)),
                    SyntaxFactory.Token(SyntaxKind.AsteriskToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5))))
        )
        (
            "5 / 5;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.DivideExpression,
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5)),
                    SyntaxFactory.Token(SyntaxKind.SlashToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5))))
        )
        (
            "5 > 5;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.GreaterThanExpression,
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5)),
                    SyntaxFactory.Token(SyntaxKind.GreaterThanToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5))))
        )
        (
            "5 < 5;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.LessThanExpression,
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5)),
                    SyntaxFactory.Token(SyntaxKind.LessThanToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5))))
        )
        (
            "5 == 5;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.EqualsExpression,
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5)),
                    SyntaxFactory.Token(SyntaxKind.EqualsEqualsToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5))))
        )
        (
            "5 != 5;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.NotEqualsExpression,
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5)),
                    SyntaxFactory.Token(SyntaxKind.ExclamationEqualsToken),
                    SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5))))
        )
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5;",
            SyntaxFactory.ExpressionStatement(
                SyntaxFactory.BinaryExpression(
                    SyntaxKind.EqualsExpression,
                    SyntaxFactory.BinaryExpression(
                        SyntaxKind.AddExpression,
                        SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(3)),
                        SyntaxFactory.Token(SyntaxKind.PlusToken),
                        SyntaxFactory.BinaryExpression(
                            SyntaxKind.MultiplyExpression,
                            SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(4)),
                            SyntaxFactory.Token(SyntaxKind.AsteriskToken),
                            SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5)))),
                    SyntaxFactory.Token(SyntaxKind.EqualsEqualsToken),
                    SyntaxFactory.BinaryExpression(
                        SyntaxKind.AddExpression,
                        SyntaxFactory.BinaryExpression(
                            SyntaxKind.MultiplyExpression,
                            SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(3)),
                            SyntaxFactory.Token(SyntaxKind.AsteriskToken),
                            SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(1))),
                        SyntaxFactory.Token(SyntaxKind.PlusToken),
                        SyntaxFactory.BinaryExpression(
                            SyntaxKind.MultiplyExpression,
                            SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(4)),
                            SyntaxFactory.Token(SyntaxKind.AsteriskToken),
                            SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5)))))
                )
        )
    ]
    
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    [<TestCase(5)>]
    [<TestCase(6)>]
    [<TestCase(7)>]
    [<TestCase(8)>]  // more complicated starts from here
    member this.``C: Test Basic Infix Expression Parsing``(testCaseIndex: int) =
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = ModifiedRecursiveDescent.parseTokens tokens
        match List.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = List.toArray syntaxNodes |> Array.map (fun x -> x :> SyntaxNode)
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareSyntaxNodes input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
        
        
[<TestFixture>]
[<ParserComponent(ParserComponentType.Statements)>]
[<ParserComponentDependsOn(ParserComponentType.Expressions)>]
type BasicVariableAssignmentParsingTests() =
    // TODO: assumes proper identifier expression parsing, which isn't tested for - as of right now.
    member this.TestCases : (string * SyntaxNode) list = [
        (
            "let a = 5;",
            SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    SyntaxFactory.IdentifierName("var"),
                    SyntaxFactory.SeparatedList(
                        [|
                        SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier("a"))
                            .WithInitializer(
                                SyntaxFactory.EqualsValueClause(
                                    SyntaxFactory.LiteralExpression(
                                        SyntaxKind.NumericLiteralExpression,
                                        SyntaxFactory.Literal(5))
                                    ))
                        |]))
                )
        )
        (
            "let foobar = 3 + 4 * 5 == 3 * 1 + 4 * 5;",
            SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    SyntaxFactory.IdentifierName("var"),
                    SyntaxFactory.SeparatedList(
                        [|
                        SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier("foobar"))
                            .WithInitializer(
                                SyntaxFactory.EqualsValueClause(
                                    SyntaxFactory.BinaryExpression(
                                        SyntaxKind.EqualsExpression,
                                        SyntaxFactory.BinaryExpression(
                                            SyntaxKind.AddExpression,
                                            SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(3)),
                                            SyntaxFactory.Token(SyntaxKind.PlusToken),
                                            SyntaxFactory.BinaryExpression(
                                                SyntaxKind.MultiplyExpression,
                                                SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(4)),
                                                SyntaxFactory.Token(SyntaxKind.AsteriskToken),
                                                SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5)))),
                                        SyntaxFactory.Token(SyntaxKind.EqualsEqualsToken),
                                        SyntaxFactory.BinaryExpression(
                                            SyntaxKind.AddExpression,
                                            SyntaxFactory.BinaryExpression(
                                                SyntaxKind.MultiplyExpression,
                                                SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(3)),
                                                SyntaxFactory.Token(SyntaxKind.AsteriskToken),
                                                SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(1))),
                                            SyntaxFactory.Token(SyntaxKind.PlusToken),
                                            SyntaxFactory.BinaryExpression(
                                                SyntaxKind.MultiplyExpression,
                                                SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(4)),
                                                SyntaxFactory.Token(SyntaxKind.AsteriskToken),
                                                SyntaxFactory.LiteralExpression(SyntaxKind.NumericLiteralExpression, SyntaxFactory.Literal(5)))))
                                    ))
                        |]
                        )))
        )
        (
            "let a = foobar;",
            SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    SyntaxFactory.IdentifierName("var"),
                    SyntaxFactory.SeparatedList(
                        [|
                        SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier("a"))
                            .WithInitializer(
                                SyntaxFactory.EqualsValueClause(
                                    SyntaxFactory.IdentifierName("foobar")
                                    ))
                        |]
                        )))
        )
    ]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    member this.``D: Test Basic Let Statement Parsing``(testCaseIndex: int) =
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = ModifiedRecursiveDescent.parseTokens tokens
        match List.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = List.toArray syntaxNodes |> Array.map (fun x -> x :> SyntaxNode)
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareSyntaxNodes input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()


[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type IfStatementParsingTests() =
    member this.TestCases : (string * SyntaxNode) list = [
        (
            "if (5 > 2) { let foobar = 5; };",
            IfStatement(
                Token(SyntaxKind.IfKeyword),
                Token(SyntaxKind.OpenParenToken),
                BinaryExpression(
                    SyntaxKind.GreaterThanExpression,
                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(5)),
                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(2))),
                Token(SyntaxKind.CloseParenToken),
                Block(
                    [|
                        LocalDeclarationStatement(
                            VariableDeclaration(
                                IdentifierName("var"),
                                SeparatedList(
                                    [|
                                    VariableDeclarator(Identifier("foobar"))
                                        .WithInitializer(
                                            EqualsValueClause(
                                                LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(5))
                                                ))
                                    |]
                                    ))) :> StatementSyntax
                    |]),
                null)
        )
    ]
    
    [<TestCase(0)>]
    member this.``E: Test If Statement Parsing``(testCaseIndex: int) =
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = ModifiedRecursiveDescent.parseTokens tokens
        match List.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = List.toArray syntaxNodes |> Array.map (fun x -> x :> SyntaxNode)
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareSyntaxNodes input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
