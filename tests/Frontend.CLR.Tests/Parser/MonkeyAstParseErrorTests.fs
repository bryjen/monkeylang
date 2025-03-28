namespace Monkey.Frontend.CLR.Tests.Parser.MonkeyAstParseErrorTests

open Frontend.CLR.Syntax
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text
open Monkey.Frontend.CLR.Parsers
open Monkey.Frontend.CLR.Parsers.ParsingErrors
open NUnit.Framework

open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeySyntaxTokenFactory
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeyExpressionSyntaxFactory
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeyStatementSyntaxFactory


[<AutoOpen>]
module private ErrorTestsHelpers =
    let printErrors (sourceText: SourceText) (errors: ParseError array) =
        let counts = [| 1 .. errors.Length |]
        let filePath = @"C:\Users\admin\Documents\SampleFile.mk"
        for count, error in Array.zip counts errors do
            printfn $"{count}."
            printfn $"{error.GetFormattedMessage(sourceText, Some filePath)}"


[<TestFixture>]
type GenericErrorTesting() =
    
    member this.TestCases : (string * ParseError array) array = [|
        (
            """let foo = 5;
let foobar = 10;
let bar = 20
""",
            [|
                ParsingErrors.AbsentSemicolonError(TextSpan(0, 0), AbsentSemicolonAt.LetStatement)
            |]
        )
        (
            """let if = 5;""",
            [|
                VariableAssignmentStatementErrors.InvalidVariableNameError(IfKeyword())
            |]
        )
        (
            """let 6pek = 5;""",
            [|
                VariableAssignmentStatementErrors.InvalidVariableNameError(IfKeyword())
            |]
        )
        (
            """let foobar  5;""",
            [|
                VariableAssignmentStatementErrors.AbsentEqualsError(TextSpan(0, 0))
            |]
        )
        (
            """5""",
            [|
                AbsentSemicolonError(TextSpan(0, 0), AbsentSemicolonAt.ExpressionStatement)
            |]
        )
        
        (
            """if 5 > 2) { 5; } else { 10; };""",
            [|
                AbsentOrInvalidTokenError(TextSpan(0, 0), [| SyntaxKind.OpenParenToken |], AbsentTokenAt.IfExpression)
            |]
        )
        (
            """if (5 > 2 { 5; } else { 10; };""",
            [|
                AbsentOrInvalidTokenError(TextSpan(0, 0), [| SyntaxKind.OpenParenToken |], AbsentTokenAt.IfExpression)
            |]
        )
        (
            """if (5 > 2)  5; } else { 10; };""",
            [|
                AbsentOrInvalidTokenError(TextSpan(0, 0), [| SyntaxKind.OpenParenToken |], AbsentTokenAt.IfExpression)
            |]
        )
        (
            """if (5 > 2) { 5;  else { 10; };""",
            [|
                AbsentOrInvalidTokenError(TextSpan(0, 0), [| SyntaxKind.OpenParenToken |], AbsentTokenAt.IfExpression)
            |]
        )
        (
            """let foobar = fn (Something<int, int} arg1) {};""",
            [|
                AbsentOrInvalidTokenError(TextSpan(0, 0), [| SyntaxKind.OpenParenToken |], AbsentTokenAt.IfExpression)
            |]
        )
        (
            """let foobar = 5;
            
if (5>2) {
    namespace Monkey;
    using System.Collections.Generic;
};
""",
            [|
                AbsentOrInvalidTokenError(TextSpan(0, 0), [| SyntaxKind.OpenParenToken |], AbsentTokenAt.IfExpression)
            |]
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
    member this.``Runner``(index: int) =
        let input, expectedErrors = this.TestCases[index]
        let sourceText = SourceText.From(input)
        let tokens = Tokenizer.tokenize input 
        
        printfn "Expected Errors:"
        printErrors sourceText expectedErrors
        
        let _, parseErrors = MonkeyAstParser.parseTokens tokens
        
        printfn ""
        printfn "Actual Errors:"
        printErrors sourceText parseErrors
        
        Assert.Pass()