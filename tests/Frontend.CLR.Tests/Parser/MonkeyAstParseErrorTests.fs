namespace Monkey.Frontend.CLR.Tests.Parser.MonkeyAstParseErrorTests

open Frontend.CLR.Syntax
open Microsoft.CodeAnalysis.Text
open Monkey.Frontend.CLR.Parsers.ParsingErrors
open NUnit.Framework

[<AutoOpen>]
module private ErrorTestsHelpers =
    let printErrors (sourceText: SourceText) (errors: ParseError array) =
        let counts = [| 1 .. errors.Length |]
        for count, error in Array.zip counts errors do
            printfn $"{count}."
            printfn $"{error.GetFormattedMessage(sourceText, None)}"


[<TestFixture>]
type GenericErrorTesting() =
    
    member this.TestCases : (string * ParseError array) array = [|
        (
            """let foo = 5;
let bar = 20
let foobar = 10;
""",
            [|
                VariableAssignmentStatementErrors.AbsentSemicolonError(TextSpan(10, 10))
            |]
        )
    |]
    
    [<TestCase(0)>]
    member this.``Runner``(index: int) =
        let input, expectedErrors = this.TestCases[index]
        let sourceText = SourceText.From(input)
        let tokens = Tokenizer.tokenize input 
        
        printfn "Expected Errors:"
        printErrors sourceText expectedErrors
        
        let _, parseErrors = Monkey.Frontend.CLR.Parsers.MonkeyAstParser.parseTokens tokens
        
        printfn ""
        printfn "Actual Errors:"
        printErrors sourceText parseErrors
        
        Assert.Pass()