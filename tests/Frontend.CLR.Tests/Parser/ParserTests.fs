module Monkey.Frontend.CLR.Tests.Parser.ParserTests

open System
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

(*
// Examples of updated statements that can be done with C#'s exposed functionality

```Monkey
let foobar: int = 5;  
// TODO: optional type annotations
// "int foobar = 5;"

let partial_app_demo = fn([int -> int -> int] multi_transform, int initialValue) { ... };
// TODO: taking functions as parameters, as well as being able to typedef them as above
// "Func<Func<int, int, int>, int, Func<int, int>> partial_app_demo = (Func<int, int, int> transform, int initialValue) => { ... }"

let full_transform: [int -> int -> int] = fn(int x, int y) { ... };
partial_app_demo(full_transform, foobar)
// TODO: passing functions as arguments
// "Func<int, int, int> full_transform = (int x, int y) => { ... };"
// "partial_app_demo(full_transform, foobar);"
 *)

[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type Runner() =
    
    member this.TestRunner() =
        failwith "todo"
    


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
    member this.TestCases : (string * SyntaxNode list) list = [
        (
            "if (5 > 2) { let foobar = 5; };",
            [
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
            ]
        )
        (
            "if (5 > 2) { let foo = 5; } else { let bar = 2; };",
            [
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
                                        VariableDeclarator(Identifier("foo"))
                                            .WithInitializer(
                                                EqualsValueClause(
                                                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(5))
                                                    ))
                                        |]
                                        ))) :> StatementSyntax
                        |]),
                    ElseClause(Block(
                        [|
                            LocalDeclarationStatement(
                                VariableDeclaration(
                                    IdentifierName("var"),
                                    SeparatedList(
                                        [|
                                        VariableDeclarator(Identifier("bar"))
                                            .WithInitializer(
                                                EqualsValueClause(
                                                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(2))
                                                    ))
                                        |]
                                        ))) :> StatementSyntax
                        |]))
                    )
            ]
        )
        (
            "let foobar = if (5 > 2) { 5; } else { 2; };",
            [
                LocalDeclarationStatement(
                    VariableDeclaration(
                        PredefinedType(Token(SyntaxKind.ObjectKeyword)),
                        SeparatedList(
                            [|
                            VariableDeclarator(Identifier("foobar"))
                            |]
                            )
                        )
                    )
                
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
                            ExpressionStatement(
                                AssignmentExpression(
                                    SyntaxKind.SimpleAssignmentExpression,
                                    IdentifierName("foobar"),
                                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(5))
                                    )
                                )
                            :> StatementSyntax
                        |]),
                    ElseClause(Block(
                        [|
                            ExpressionStatement(
                                AssignmentExpression(
                                    SyntaxKind.SimpleAssignmentExpression,
                                    IdentifierName("foobar"),
                                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(2))
                                    )
                                )
                            :> StatementSyntax
                        |]
                        )
                    ))
            ]
        )
    ]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]  // inline if statement assignment
    member this.``E: Test If Statement Parsing``(testCaseIndex: int) =
        let input, expectedSyntaxNodes = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = ModifiedRecursiveDescent.parseTokens tokens
        match List.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = List.toArray syntaxNodes |> Array.map (fun x -> x :> SyntaxNode)
            match compareSyntaxNodes input (List.toArray expectedSyntaxNodes) actualSyntaxNodes with
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
type FunctionParsingTests() =
    let castTypeSyntaxArr (arr: 'R array) =
        Array.map (fun o -> o :> TypeSyntax) arr
        
    let unitReturnStatement =
        ReturnStatement(
            ObjectCreationExpression(
                Token(SyntaxKind.NewKeyword),
                IdentifierName("unit"),
                ArgumentList(),
                null)
            )
    
    member this.TestCases : (string * SyntaxNode) list = [
        (
            """fn(int x, int y) : int {
    let z = 10;
    x + y + z;
}
""",
            ExpressionStatement(
                ParenthesizedLambdaExpression(
                    ParameterList(
                        SeparatedList<ParameterSyntax>(
                            [|
                                Parameter(Identifier("x")).WithType(PredefinedType(Token(SyntaxKind.IntKeyword)))
                                Parameter(Identifier("y")).WithType(PredefinedType(Token(SyntaxKind.IntKeyword)))
                            |])
                        ),
                    
                    Block(
                        [|
                            LocalDeclarationStatement(
                                VariableDeclaration(IdentifierName("var"))
                                    .WithVariables(
                                        SingletonSeparatedList(
                                            VariableDeclarator(Identifier("z"))
                                                .WithInitializer(
                                                    EqualsValueClause(
                                                        LiteralExpression(
                                                            SyntaxKind.NumericLiteralExpression,
                                                            Literal(10)
                                                        )
                                                    )
                                            )
                                        )
                                    )
                                ) :> StatementSyntax
                            
                            ReturnStatement(
                                BinaryExpression(
                                    SyntaxKind.AddExpression,
                                    BinaryExpression(
                                        SyntaxKind.AddExpression,
                                        IdentifierName("x"),
                                        IdentifierName("y")
                                        ),
                                    IdentifierName("z")
                                    )
                                ) :> StatementSyntax
                        |]
                        )
                    )
                )
        )
        (
            """fn() : int {
    let x = 10;
    x + 12;
}
""",
            ExpressionStatement(
                ParenthesizedLambdaExpression(
                    ParameterList(SeparatedList<ParameterSyntax>()),
                    Block(
                        [|
                            LocalDeclarationStatement(
                                VariableDeclaration(IdentifierName("var"))
                                    .WithVariables(
                                        SingletonSeparatedList(
                                            VariableDeclarator(Identifier("x"))
                                                .WithInitializer(
                                                    EqualsValueClause(
                                                        LiteralExpression(
                                                            SyntaxKind.NumericLiteralExpression,
                                                            Literal(10)
                                                        )
                                                    )
                                            )
                                        )
                                    )
                                ) :> StatementSyntax
                            
                            ReturnStatement(
                                BinaryExpression(
                                    SyntaxKind.AddExpression,
                                    IdentifierName("x"),
                                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(12))
                                    )
                                ) :> StatementSyntax
                        |]
                        )
                    )
                )
        )
        (
            """fn() : unit {
}
""",
            ExpressionStatement(
                ParenthesizedLambdaExpression(
                    ParameterList(SeparatedList<ParameterSyntax>()),
                    Block(
                        [|
                            unitReturnStatement :> StatementSyntax
                        |]
                        )
                    )
                )
        )
        
        (
            """let add = fn(int x, int y) : int {
    let z = 10;
    x + y + z;
}
""",
            LocalDeclarationStatement(
                VariableDeclaration(
                    GenericName(Identifier("Func"))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                SeparatedList<TypeSyntax>(
                                [|
                                    PredefinedType(Token(SyntaxKind.IntKeyword))
                                    PredefinedType(Token(SyntaxKind.IntKeyword))
                                    PredefinedType(Token(SyntaxKind.IntKeyword))
                                |] |> castTypeSyntaxArr,
                                [|
                                    Token(SyntaxKind.CommaToken)
                                    Token(SyntaxKind.CommaToken)
                                |]
                                ))
                            ),
                    SeparatedList(
                        [|
                            SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier("add"))
                                .WithInitializer(
                                    SyntaxFactory.EqualsValueClause(
                                        ParenthesizedLambdaExpression(
                                            ParameterList(
                                                SeparatedList<ParameterSyntax>(
                                                    [|
                                                        Parameter(Identifier("x")).WithType(PredefinedType(Token(SyntaxKind.IntKeyword)))
                                                        Parameter(Identifier("y")).WithType(PredefinedType(Token(SyntaxKind.IntKeyword)))
                                                    |])
                                                ),
                                            
                                            Block(
                                                [|
                                                    LocalDeclarationStatement(
                                                        VariableDeclaration(IdentifierName("var"))
                                                            .WithVariables(
                                                                SingletonSeparatedList(
                                                                    VariableDeclarator(Identifier("z"))
                                                                        .WithInitializer(
                                                                            EqualsValueClause(
                                                                                LiteralExpression(
                                                                                    SyntaxKind.NumericLiteralExpression,
                                                                                    Literal(10)
                                                                                )
                                                                            )
                                                                    )
                                                                )
                                                            )
                                                        ) :> StatementSyntax
                                                    
                                                    ReturnStatement(
                                                        BinaryExpression(
                                                            SyntaxKind.AddExpression,
                                                            BinaryExpression(
                                                                SyntaxKind.AddExpression,
                                                                IdentifierName("x"),
                                                                IdentifierName("y")
                                                                ),
                                                            IdentifierName("z")
                                                            )
                                                        ) :> StatementSyntax
                                                |]
                                                )
                                            )
                                        ))
                        |]
                        )
                    )
                )
        )
    ]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    // TODO: Find a way to implement unit typing when we implement 'class' implementations
    member this.``F: Test Function Expression Parsing``(testCaseIndex: int) =
        let input, expectedSyntaxNodes = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = ModifiedRecursiveDescent.parseTokens tokens
        match List.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = List.toArray syntaxNodes |> Array.map (fun x -> x :> SyntaxNode)
            match compareSyntaxNodes input [| expectedSyntaxNodes |] actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
