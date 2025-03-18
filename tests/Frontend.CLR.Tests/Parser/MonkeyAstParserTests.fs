namespace Monkey.Frontend.CLR.Tests.Parser.MonkeyAstParserTests


open Microsoft.CodeAnalysis.CSharp

open Monkey.Frontend.CLR.Syntax.AstTraverser
open NUnit.Framework

open Frontend.CLR.Syntax
open Monkey.Frontend.CLR.Syntax.Ast
open Monkey.Frontend.CLR.Tests.Parser.Helpers

open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeySyntaxTokenFactory
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeyExpressionSyntaxFactory
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeyStatementSyntaxFactory

[<AutoOpen>]
module private MonkeyAstParserTestsHelpers =
    let compareMonkeyStatements (monkeyInput: string) (expectedSyntaxNodes: MonkeySyntaxNode array) (actualSyntaxNodes: MonkeySyntaxNode array) : bool =
        let mutable pass = true
        for expected, actual in Array.zip expectedSyntaxNodes actualSyntaxNodes do
            let eq1 = MonkeySyntaxNode.AreEquivalent(actual, expected)
            let eq2 = MonkeySyntaxNode.AreEquivalent(expected, actual)
            if not eq1 || not eq2 then
                pass <- false
            
        printfn "Input/Output"
        printfn "---------------------------------------------------------"
        
        printfn "```monkey"
        printfn $"{monkeyInput}"
        printfn "```"
        printfn ""
        
        printfn "```monkey (expected)"
        for expected in expectedSyntaxNodes do
            printfn $"{expected}"
        printfn "```"
        printfn ""
        
        printfn "```monkey (actual)"
        for actual in actualSyntaxNodes do
            printfn $"{actual}"
        printfn "```"
        printfn "---------------------------------------------------------"
        
        printfn "\n\nSyntax Tree Visualization"
        printfn "---------------------------------------------------------"
        printfn "```csharp (expected)"
        for expected in expectedSyntaxNodes do
            printMonkeySyntaxNodeTree expected
        printfn "```"
        printfn ""
        
        printfn "```csharp (actual)"
        for actual in actualSyntaxNodes do
            printMonkeySyntaxNodeTree actual
        printfn "```"
        printfn "---------------------------------------------------------"
        
        pass 

[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type LiteralExpressionStatementParsing() =
    
    member this.TestCases : (string * ExpressionStatementSyntax) array = [|
        (
            "5;",
            ExpressionStatement(NumericLiteralExpression(5))
        )
        (
            "-5;",
            ExpressionStatement(MinusPrefixExpression(NumericLiteralExpression(5)))
        )
        (
            "(5);",
            ExpressionStatement(ParenthesizedExpression(NumericLiteralExpression(5)))
        )
        (
            "-(5);",
            ExpressionStatement(MinusPrefixExpression(ParenthesizedExpression(NumericLiteralExpression(5))))
        )
        (
            "(-5);",
            ExpressionStatement(ParenthesizedExpression(MinusPrefixExpression(NumericLiteralExpression(5))))
        )
        
        (
            "true;",
            ExpressionStatement(TrueLiteralExpression())
        )
        (
            "false;",
            ExpressionStatement(FalseLiteralExpression())
        )
        (
            "!true;",
            ExpressionStatement(LogicalNotPrefixExpression(TrueLiteralExpression()))
        )
        (
            "!false;",
            ExpressionStatement(LogicalNotPrefixExpression(FalseLiteralExpression()))
        )
        (
            "!(true);",
            ExpressionStatement(LogicalNotPrefixExpression(ParenthesizedExpression(TrueLiteralExpression())))
        )
        (
            "!(false);",
            ExpressionStatement(LogicalNotPrefixExpression(ParenthesizedExpression(FalseLiteralExpression())))
        )
    |]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    
    [<TestCase(5)>]
    [<TestCase(6)>]
    [<TestCase(7)>]
    [<TestCase(8)>]
    [<TestCase(9)>]
    [<TestCase(10)>]
    member this.``A: Test Basic Numeric Expression Parsing``(testCaseIndex: int) =
        // we keep the test case in 'ExpressionStatementSyntax' to avoid having to cast each during declaration
        let castedTestCases = this.TestCases |> Array.map (fun (input, expected) -> (input, expected |> StatementSyntax.ExpressionStatementSyntax |> MonkeySyntaxNode.StatementSyntax))
        let input, expectedSyntaxTree = castedTestCases[testCaseIndex]
        let tokens = Tokenizer.tokenize input
        
        let statements, parseErrors = Monkey.Frontend.CLR.Parsers.MonkeyAstParser.parseTokens tokens
        let asMonkeySyntaxNodes = statements |> Array.map MonkeySyntaxNode.StatementSyntax
        match Array.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = asMonkeySyntaxNodes
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareMonkeyStatements input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
