﻿namespace Monkey.Parser.Tests.Parser.CSharpAstParserTests

open System

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open type Microsoft.CodeAnalysis.CSharp.SyntaxFactory

open Monkey.Codegen.Dotnet.MonkeyToCSharpAstConverter
open Monkey.Parser.Tests.Parser.Helpers
open NUnit.Framework


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
#if LEGACY_PARSER
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = DirectCSharpAstParser.parseTokens tokens
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
#endif
        defaultComparison (List.item testCaseIndex this.TestCases)
        
        
        
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
#if LEGACY_PARSER
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = DirectCSharpAstParser.parseTokens tokens
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
#endif
        defaultComparison (List.item testCaseIndex this.TestCases)
        
        
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
#if LEGACY_PARSER
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = DirectCSharpAstParser.parseTokens tokens
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
#endif
        defaultComparison (List.item testCaseIndex this.TestCases)
        
        
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
        
        
        (
            "let a: int = 5;",
            SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    PredefinedType(Token(SyntaxKind.IntKeyword)),
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
            "let foobar: int = 3 + 4 * 5 == 3 * 1 + 4 * 5;",
            SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    PredefinedType(Token(SyntaxKind.IntKeyword)),
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
            "let a: SomeClass = foobar;",
            SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    IdentifierName("SomeClass"),
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
    
    // testing explicit type annotations
    [<TestCase(3)>]
    [<TestCase(4)>]
    [<TestCase(5)>]
    member this.``D: Test Basic Let Statement Parsing``(testCaseIndex: int) =
#if LEGACY_PARSER
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = DirectCSharpAstParser.parseTokens tokens
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
#endif
        defaultComparison (List.item testCaseIndex this.TestCases)


[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type IfStatementParsingTests() =
    member this.OldTestCases : (string * SyntaxNode list) list = [
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
    
    member this.TestCases : (string * SyntaxNode list) list = [
        (
            "if (5 > 2) { 5; } else { 10; };",
            [
                LocalDeclarationStatement(
                    VariableDeclaration(
                        PredefinedType(Token(SyntaxKind.ObjectKeyword)),
                        SeparatedList(
                            [|
                            VariableDeclarator(Identifier("LQNDkDVw"))
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
                                    IdentifierName("LQNDkDVw"),
                                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(5))
                                    )
                                )
                            :> StatementSyntax
                        |]),
                    ElseClause(
                        Token(SyntaxKind.ElseKeyword),
                        Block(
                            [|
                                ExpressionStatement(
                                    AssignmentExpression(
                                        SyntaxKind.SimpleAssignmentExpression,
                                        IdentifierName("LQNDkDVw"),
                                        LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(10))
                                        )
                                    )
                                :> StatementSyntax
                            |]))
                    )
                
                ExpressionStatement(IdentifierName("LQNDkDVw"))
            ]
        )
        (
            "let foobar = if (5 > 2) { 5; } else { 10; };",
            [
                LocalDeclarationStatement(
                    VariableDeclaration(
                        PredefinedType(Token(SyntaxKind.ObjectKeyword)),
                        SeparatedList(
                            [|
                            VariableDeclarator(Identifier("LQNDkDVw"))
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
                                    IdentifierName("LQNDkDVw"),
                                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(5))
                                    )
                                )
                            :> StatementSyntax
                        |]),
                    ElseClause(
                        Token(SyntaxKind.ElseKeyword),
                        Block(
                            [|
                                ExpressionStatement(
                                    AssignmentExpression(
                                        SyntaxKind.SimpleAssignmentExpression,
                                        IdentifierName("LQNDkDVw"),
                                        LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(10))
                                        )
                                    )
                                :> StatementSyntax
                            |]))
                    )
                
                LocalDeclarationStatement(
                    VariableDeclaration(
                        IdentifierName("var"),
                        SeparatedList(
                            [|
                            VariableDeclarator(Identifier("foobar"))
                                .WithInitializer(
                                    EqualsValueClause(
                                        IdentifierName("LQNDkDVw")
                                        ))
                            |]
                            )
                        )
                    )
            ]
        )
    ]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    member this.``E: Test If Statement Parsing``(testCaseIndex: int) =
#if LEGACY_PARSER
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = DirectCSharpAstParser.parseTokens tokens
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
#endif
        ConverterConfigSingleton.Instance.Seed <- Some 0
        multiStatementComparison (List.item testCaseIndex this.TestCases)
            
            
[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
[<ParserComponent(ParserComponentType.Statements)>]
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
        
    let createFuncSignatureType (types: TypeSyntax array) =
        let commas = Array.create ((Array.length types) - 1) (Token(SyntaxKind.CommaToken))
        GenericName(Identifier("Func"))
            .WithTypeArgumentList(
                TypeArgumentList(
                    SeparatedList<TypeSyntax>(types, commas)))
        
    
    member this.TestCases : (string * SyntaxNode) list = [
        (
            """fn(int x, int y) : int {
    let z = 10;
    x + y + z;
};
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
                    ).WithReturnType(PredefinedType(Token(SyntaxKind.IntKeyword)))
                )
        )
        (
            """fn() : int {
    let x = 10;
    x + 12;
};
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
                    ).WithReturnType(PredefinedType(Token(SyntaxKind.IntKeyword)))
                )
        )
        (
            """fn() : unit {
};
""",
            ExpressionStatement(
                ParenthesizedLambdaExpression(
                    ParameterList(SeparatedList<ParameterSyntax>()),
                    Block(
                        [|
                            unitReturnStatement :> StatementSyntax
                        |]
                        )
                    ).WithReturnType(IdentifierName("unit"))
                )
        )
        
        (
            """let add = fn(int x, int y) : int {
    let z = 10;
    x + y + z;
};
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
                                            ).WithReturnType(PredefinedType(Token(SyntaxKind.IntKeyword)))
                                        ))
                        |]
                        )
                    )
                )
        )
        (
            """fn([int -> int] transform, int init_value) : int {
    init_value;
};
""",
            ExpressionStatement(
                ParenthesizedLambdaExpression(
                    ParameterList(
                        SeparatedList<ParameterSyntax>(
                            [|
                                Parameter(Identifier("transform"))
                                    .WithType(createFuncSignatureType [|
                                        PredefinedType(Token(SyntaxKind.IntKeyword))
                                        PredefinedType(Token(SyntaxKind.IntKeyword))
                                    |])
                                Parameter(Identifier("init_value"))
                                    .WithType(PredefinedType(Token(SyntaxKind.IntKeyword)))
                            |])
                        ),
                    
                    Block(
                        [|
                            ReturnStatement(
                                IdentifierName("init_value")
                                ) :> StatementSyntax
                        |]
                        )
                    ).WithReturnType(PredefinedType(Token(SyntaxKind.IntKeyword)))
                )
        )
        (
            """fn([int -> int -> int] full_transform, int init_value) : [int -> int] {
    init_value;
};
""",
            ExpressionStatement(
                ParenthesizedLambdaExpression(
                    ParameterList(
                        SeparatedList<ParameterSyntax>(
                            [|
                                Parameter(Identifier("full_transform"))
                                    .WithType(createFuncSignatureType [|
                                        PredefinedType(Token(SyntaxKind.IntKeyword))
                                        PredefinedType(Token(SyntaxKind.IntKeyword))
                                        PredefinedType(Token(SyntaxKind.IntKeyword))
                                    |])
                                Parameter(Identifier("init_value"))
                                    .WithType(PredefinedType(Token(SyntaxKind.IntKeyword)))
                            |])
                        ),
                    
                    Block(
                        [|
                            ReturnStatement(
                                IdentifierName("init_value")
                                ) :> StatementSyntax
                        |]
                        )
                    ).WithReturnType(
                        createFuncSignatureType [|
                            PredefinedType(Token(SyntaxKind.IntKeyword))
                            PredefinedType(Token(SyntaxKind.IntKeyword))
                        |]
                    )
                )
        )
        (
            """let partial_application_example = fn([int -> int -> int] full_transform, int init_value) : [int -> int] {
    init_value;
};
""",
            LocalDeclarationStatement(
                VariableDeclaration(
                    GenericName(Identifier("Func"))
                        .WithTypeArgumentList(
                            TypeArgumentList(
                                SeparatedList<TypeSyntax>(
                                [|
                                    (createFuncSignatureType
                                        [|
                                            PredefinedType(Token(SyntaxKind.IntKeyword))
                                            PredefinedType(Token(SyntaxKind.IntKeyword))
                                            PredefinedType(Token(SyntaxKind.IntKeyword))
                                        |]) :> TypeSyntax
                                    
                                    PredefinedType(Token(SyntaxKind.IntKeyword)) :> TypeSyntax
                                    
                                    (createFuncSignatureType
                                        [|
                                            PredefinedType(Token(SyntaxKind.IntKeyword))
                                            PredefinedType(Token(SyntaxKind.IntKeyword))
                                        |]) :> TypeSyntax
                                |] |> castTypeSyntaxArr,
                                [|
                                    Token(SyntaxKind.CommaToken)
                                    Token(SyntaxKind.CommaToken)
                                |]
                                ))
                            ),
                    SeparatedList(
                        [|
                            SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier("partial_application_example"))
                                .WithInitializer(
                                    SyntaxFactory.EqualsValueClause(
                                        ParenthesizedLambdaExpression(
                                            ParameterList(
                                                SeparatedList<ParameterSyntax>(
                                                    [|
                                                        Parameter(Identifier("full_transform"))
                                                            .WithType(createFuncSignatureType [|
                                                                PredefinedType(Token(SyntaxKind.IntKeyword))
                                                                PredefinedType(Token(SyntaxKind.IntKeyword))
                                                                PredefinedType(Token(SyntaxKind.IntKeyword))
                                                            |])
                                                        Parameter(Identifier("init_value"))
                                                            .WithType(PredefinedType(Token(SyntaxKind.IntKeyword)))
                                                    |])
                                                ),
                                            
                                            Block(
                                                [|
                                                    ReturnStatement(
                                                        IdentifierName("init_value")
                                                        ) :> StatementSyntax
                                                |]
                                                )
                                            ).WithReturnType(
                                                createFuncSignatureType [|
                                                    PredefinedType(Token(SyntaxKind.IntKeyword))
                                                    PredefinedType(Token(SyntaxKind.IntKeyword))
                                                |]
                                            )
                                        )
                                    )
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
    [<TestCase(4)>]  // parsing function signature types as parameters
    [<TestCase(5)>]
    [<TestCase(6)>]  // asserting type in variable assignment
    // TODO: Find a way to implement unit typing when we implement 'class' implementations
    // TODO: For non assignment statements, the expression does not keep track of function return type, so we need to add annotation tests
    member this.``F: Test Function Expression Parsing``(testCaseIndex: int) =
#if LEGACY_PARSER
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = DirectCSharpAstParser.parseTokens tokens
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
#endif
        defaultComparison (List.item testCaseIndex this.TestCases)



// Examples:

// var array = new int[10];
// let array: int[10];

// var arrayInit = new int[] { 1, 2, 3, 4, 5 };
// let array: int[] = [1, 2, 3, 4, 5];
// [<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
[<ParserComponent(ParserComponentType.Statements)>]
type ArrayParsingTests() =
    
    member this.TestCases : (string * SyntaxNode) list = [
        (
            """[1, 2, 3, 4, 5];""",
            ExpressionStatement(
                ArrayCreationExpression(
                    Token(SyntaxKind.NewKeyword),
                    ArrayType(
                        PredefinedType(Token(SyntaxKind.IntKeyword)),
                        SyntaxFactory.List<ArrayRankSpecifierSyntax>([|
                            ArrayRankSpecifier(
                                Token(SyntaxKind.OpenBraceToken),
                                SeparatedList([|
                                    LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(5)) :> ExpressionSyntax
                                |]),
                                Token(SyntaxKind.CloseBraceToken)
                                )
                            |])
                        ),
                    InitializerExpression(
                        SyntaxKind.ArrayInitializerExpression,
                        Token(SyntaxKind.OpenBraceToken),
                        SeparatedList<ExpressionSyntax>([|
                            LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(1)) :> ExpressionSyntax
                            LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(2)) :> ExpressionSyntax
                            LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(3)) :> ExpressionSyntax
                            LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(4)) :> ExpressionSyntax
                            LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(5)) :> ExpressionSyntax
                        |]),
                        Token(SyntaxKind.CloseBraceToken))
                ))
        ) 
        (
            """let arr = [1, 2, 3, 4, 5];""",
            SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    IdentifierName("var"),
                    SyntaxFactory.SeparatedList(
                        [|
                        SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier("arr"))
                            .WithInitializer(
                                SyntaxFactory.EqualsValueClause(
                                    ArrayCreationExpression(
                                        Token(SyntaxKind.NewKeyword),
                                        ArrayType(
                                            PredefinedType(Token(SyntaxKind.IntKeyword)),
                                            SyntaxFactory.List<ArrayRankSpecifierSyntax>([|
                                                ArrayRankSpecifier(
                                                    Token(SyntaxKind.OpenBraceToken),
                                                    SeparatedList([|
                                                        LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(5)) :> ExpressionSyntax
                                                    |]),
                                                    Token(SyntaxKind.CloseBraceToken)
                                                    )
                                                |])
                                            ),
                                        InitializerExpression(
                                            SyntaxKind.ArrayInitializerExpression,
                                            Token(SyntaxKind.OpenBraceToken),
                                            SeparatedList<ExpressionSyntax>([|
                                                LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(1)) :> ExpressionSyntax
                                                LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(2)) :> ExpressionSyntax
                                                LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(3)) :> ExpressionSyntax
                                                LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(4)) :> ExpressionSyntax
                                                LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(5)) :> ExpressionSyntax
                                            |]),
                                            Token(SyntaxKind.CloseBraceToken)))
                                    ))
                        |]
                        )))
        ) 
        (
            """let arr: int[10];""",
            SyntaxFactory.LocalDeclarationStatement(
                SyntaxFactory.VariableDeclaration(
                    IdentifierName("var"),
                    SyntaxFactory.SeparatedList(
                        [|
                        SyntaxFactory.VariableDeclarator(SyntaxFactory.Identifier("arr"))
                            .WithInitializer(
                                SyntaxFactory.EqualsValueClause(
                                    ArrayCreationExpression(
                                        Token(SyntaxKind.NewKeyword),
                                        ArrayType(
                                            PredefinedType(Token(SyntaxKind.IntKeyword)),
                                            SyntaxFactory.List<ArrayRankSpecifierSyntax>([|
                                                ArrayRankSpecifier(
                                                    Token(SyntaxKind.OpenBraceToken),
                                                    SeparatedList([|
                                                        LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(5)) :> ExpressionSyntax
                                                    |]),
                                                    Token(SyntaxKind.CloseBraceToken)
                                                    )
                                                |])
                                            ),
                                        InitializerExpression(
                                            SyntaxKind.ArrayInitializerExpression,
                                            Token(SyntaxKind.OpenBraceToken),
                                            SeparatedList<ExpressionSyntax>([|
                                                LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(1)) :> ExpressionSyntax
                                                LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(2)) :> ExpressionSyntax
                                                LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(3)) :> ExpressionSyntax
                                                LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(4)) :> ExpressionSyntax
                                                LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(5)) :> ExpressionSyntax
                                            |]),
                                            Token(SyntaxKind.CloseBraceToken)))
                                    ))
                        |]
                        )))
        ) 
    ]
    
    member this.``G: Test Function Expression Parsing``(testCaseIndex: int) =
#if LEGACY_PARSER
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = DirectCSharpAstParser.parseTokens tokens
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
#endif
        defaultComparison (List.item testCaseIndex this.TestCases)


[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
[<ParserComponent(ParserComponentType.Statements)>]
type FunctionCallParsingTests () =
    
    member this.TestCases : (string * SyntaxNode) list = [
        (
            """foobar(1, 2);""",
            ExpressionStatement(
                InvocationExpression(
                        IdentifierName("foobar"),
                        ArgumentList(
                            Token(SyntaxKind.OpenParenToken),
                            SeparatedList<ArgumentSyntax>(
                                [|
                                    Argument(LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(1)))
                                    Argument(LiteralExpression(SyntaxKind.NumericLiteralExpression, Literal(2)))
                                |]
                                ),
                            Token(SyntaxKind.CloseParenToken)
                            )
                    )
                )
        )
        
        (
            """Console.WriteLine("Hello World");""",
            ExpressionStatement(
                InvocationExpression(
                        MemberAccessExpression(
                            SyntaxKind.SimpleMemberAccessExpression,
                            IdentifierName("Console"),
                            Token(SyntaxKind.DotToken),
                            IdentifierName("WriteLine")
                            ),
                        ArgumentList(
                            Token(SyntaxKind.OpenParenToken),
                            SeparatedList<ArgumentSyntax>(
                                [|
                                    Argument(LiteralExpression(SyntaxKind.StringLiteralExpression, Literal("Hello World")))
                                |]
                                ),
                            Token(SyntaxKind.CloseParenToken)
                            )
                    )
                )
        ) 
    ]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    member this.``H: Test Function Call Parsing``(testCaseIndex: int) =
#if LEGACY_PARSER
        let input, expectedSyntaxTree = List.item testCaseIndex this.TestCases
        let tokens = Lexer.parseIntoTokens input |> List.toArray
        
        let syntaxNodes, parseErrors = DirectCSharpAstParser.parseTokens tokens
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
#endif
        defaultComparison (List.item testCaseIndex this.TestCases)
