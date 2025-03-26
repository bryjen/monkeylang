namespace Monkey.Frontend.CLR.Tests.Tokenizer

open Frontend.CLR.Syntax
open Microsoft.CodeAnalysis.CSharp
open Monkey.Frontend.CLR.Helpers
open Monkey.Frontend.CLR.Syntax.Ast
open NUnit.Framework
open FsToolkit.ErrorHandling


[<AutoOpen>]
module private TokenizerTestHelpers = 
    let internal assertTokenTypeIsExpectedTokenType (testCount: int) (expectedKind: SyntaxKind) (actualKind: SyntaxKind) =
        match expectedKind = actualKind with
        | true -> Ok ()
        | false ->  Error $"[test #{testCount}] Expected token type '{expectedKind}', but found '{actualKind}'"
            
    let private assertValueIsExpectedValue (testCount: int) (expected: obj) (actual: obj) =
        let expectedType = expected.GetType()
        let actualType = actual.GetType()
        match expectedType = actualType with
        | true ->
            if expected = actual then
                Ok ()
            else
                Error $"[test #{testCount}] Same types, but expected value \"{expected}\", but got \"{actual}\""
        | false ->
            Error $"[test #{testCount}] Expected type \"{expectedType}\", but got \"{actualType}\""
            
    let processTestCase (testCase: SyntaxToken * (int * SyntaxKind * obj option)) = 
        result {
            let actualToken, testInformation = testCase
            let testCount, expectedSyntaxKind, expectedValue = testInformation 
            
            // LexerLog.addTokenToLog actualToken
            
            do! assertTokenTypeIsExpectedTokenType testCount expectedSyntaxKind actualToken.Kind
            
            do! assertValueIsExpectedValue testCount expectedValue actualToken.Value
        }
        
    /// If any 'Result' DU is 'ERROR', return 'ERROR + error message', returns OK otherwise
    let private processResultsList (resultsList: Result<unit, string> array) : Result<unit, string> =
        result {
            for testResult in resultsList do
                do! testResult
            return ()
        }
            
    let testLexer (tokens: SyntaxToken array) (testCases: (SyntaxKind * obj option) array) =
        let testCasesWithCounts = TuplePrepender.AddCountsToTuples testCases
        
        let testCasesWithCountsAndTokens = Array.zip tokens testCasesWithCounts
        
        let testResult =
            testCasesWithCountsAndTokens
            |> Array.map processTestCase
            |> processResultsList
            
        // Clean-up, post logs, etc. before passing/failing
        // TestContext.WriteLine($"Log:\n{LexerLog.log}")
        
        testResult
        |> function
            | Ok _ ->Assert.Pass()
            | Error errorMsg -> Assert.Fail(errorMsg)
            
            
    let rec printTokens (tokens: SyntaxToken array) : unit =
        let counts = [1..tokens.Length] |> List.toArray
        for (count, token) in Array.zip counts tokens do
            printToken count token
            ()
        ()
    and private printToken count token =
        printfn $"[{count}] {token.Kind};\n\t\"{token.Text}\"\n\t{token.Value}"
        

    let rec printTestCases (testCases: (SyntaxKind * obj option) array) : unit =
        let testCasesWithCounts = TuplePrepender.AddCountsToTuples testCases
        for (testCase, expectedKind, expectedValue) in testCasesWithCounts do
            printTestCase testCase expectedKind expectedValue
            ()
        ()
    and private printTestCase testCase expectedKind expectedValue =
        printfn $"[{testCase}] {expectedKind};\n\t{expectedValue}"
        
        
    let printTokensAndTestCases (tokens: SyntaxToken array) (testCases: (SyntaxKind * obj option) array) : unit =
        if (Array.length tokens <> Array.length testCases) then
          printfn "Expected:"
          printTestCases testCases
          
          printfn "\n"
          printfn "Actual:"
          printTokens tokens
        else
          let zipped = Array.zip tokens testCases
          let zippedWithCounts = TuplePrepender.AddCountsToTuples zipped
          for testCase, token, (expectedKind, expectedValue) in zippedWithCounts do
              printfn $"[#{testCase}]"
              printfn $"[EXPECTED]\n\t{expectedKind};\n\t{expectedValue}\n"
              printfn $"[ACTUAL]\n\t{token.Kind};\n\t\"{token.Text}\"\n\t{token.Value}\n"
              ()
          ()
                
/// <summary>
/// Tests the core syntax/token processing without trivia (whitespaces, newlines, and "non-functional" parts of the source
/// text).
/// </summary>
[<TestFixture>]
type TokenizerTests() =
    [<Test>]
    member this.``Tokenizer Test 1``() =
        let testInput = "=+(){},;"
        
        let testCases: (SyntaxKind * obj) array = [|
            (SyntaxKind.EqualsToken,            "=")
            (SyntaxKind.PlusToken,              "+")
            (SyntaxKind.OpenParenToken,         "(")
            (SyntaxKind.CloseParenToken,        ")")
            (SyntaxKind.OpenBraceToken,         "{")
            (SyntaxKind.CloseBraceToken,        "}")
            (SyntaxKind.CommaToken,             ",")
            (SyntaxKind.SemicolonToken,         ";")
            (SyntaxKind.EndOfFileToken,         "")
        |]
        let testCases = Array.map (fun (syntaxKind, obj) -> (syntaxKind, Some obj)) testCases
        let tokens = Tokenizer.tokenize testInput
        printTokensAndTestCases tokens testCases
        testLexer tokens testCases

    
    [<Test>]
    member this.``Tokenizer Test 2``() =
        let testInput = """let five = 5;
let ten = 10;
            
let add = fn(int x, int y) : int {
    x + y;
};
            
let result = add(five, ten);"""

        let testCases: (SyntaxKind * obj) array = [|
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "five")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.NumericLiteralToken,
              5)
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "ten")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.NumericLiteralToken,
              10)
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "add")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.IdentifierToken,
              "fn")
            ( SyntaxKind.OpenParenToken,
              "(")
            ( SyntaxKind.IntKeyword,
              "int")
            ( SyntaxKind.IdentifierToken,
              "x")
            ( SyntaxKind.CommaToken,
              ",")
            ( SyntaxKind.IntKeyword,
              "int")
            ( SyntaxKind.IdentifierToken,
              "y")
            ( SyntaxKind.CloseParenToken,
              ")")
            ( SyntaxKind.ColonToken,
              ":")
            ( SyntaxKind.IntKeyword,
              "int")
            ( SyntaxKind.OpenBraceToken,
              "{")
            
            ( SyntaxKind.IdentifierToken,
              "x")
            ( SyntaxKind.PlusToken,
              "+")
            ( SyntaxKind.IdentifierToken,
              "y")
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.CloseBraceToken,
              "}")
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "result")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.IdentifierToken,
              "add")
            ( SyntaxKind.OpenParenToken,
              "(")
            ( SyntaxKind.IdentifierToken,
              "five")
            ( SyntaxKind.CommaToken,
              ",")
            ( SyntaxKind.IdentifierToken,
              "ten")
            ( SyntaxKind.CloseParenToken,
              ")")
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.EndOfFileToken,
              "")
        |]
        let testCases = Array.map (fun (syntaxKind, obj) -> (syntaxKind, Some obj)) testCases
        let tokens = Tokenizer.tokenize testInput
        printTokensAndTestCases tokens testCases
        testLexer tokens testCases

    
    [<Test>]
    member this.``Tokenizer Test 3``() =
        let testInput = """let five = 5;
let ten = 10;
            
let add = fn(int x, int y) : int {
    x + y;
};

let result = add(five, ten);

!-/*5;
5 < 10 > 5;

if (5 < 10) {
    return true;
} else {
    return false;
};

10 == 10;
10 != 9;
10 >= 9;
10 <= 9;
[int -> int];"""

        let testCases: (SyntaxKind * obj) array = [|
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "five")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.NumericLiteralToken,
              5)
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "ten")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.NumericLiteralToken,
              10)
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "add")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.IdentifierToken,
              "fn")
            ( SyntaxKind.OpenParenToken,
              "(")
            ( SyntaxKind.IntKeyword,
              "int")
            ( SyntaxKind.IdentifierToken,
              "x")
            ( SyntaxKind.CommaToken,
              ",")
            ( SyntaxKind.IntKeyword,
              "int")
            ( SyntaxKind.IdentifierToken,
              "y")
            ( SyntaxKind.CloseParenToken,
              ")")
            ( SyntaxKind.ColonToken,
              ":")
            ( SyntaxKind.IntKeyword,
              "int")
            ( SyntaxKind.OpenBraceToken,
              "{")
            
            ( SyntaxKind.IdentifierToken,
              "x")
            ( SyntaxKind.PlusToken,
              "+")
            ( SyntaxKind.IdentifierToken,
              "y")
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.CloseBraceToken,
              "}")
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "result")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.IdentifierToken,
              "add")
            ( SyntaxKind.OpenParenToken,
              "(")
            ( SyntaxKind.IdentifierToken,
              "five")
            ( SyntaxKind.CommaToken,
              ",")
            ( SyntaxKind.IdentifierToken,
              "ten")
            ( SyntaxKind.CloseParenToken,
              ")")
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.ExclamationToken,
              "!")
            ( SyntaxKind.MinusToken,
              "-")
            ( SyntaxKind.SlashToken,
              "/")
            ( SyntaxKind.AsteriskToken,
              "*")
            ( SyntaxKind.NumericLiteralToken,
              5)
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.NumericLiteralToken,
              5)
            ( SyntaxKind.LessThanToken,
              "<")
            ( SyntaxKind.NumericLiteralToken,
              10)
            ( SyntaxKind.GreaterThanToken,
              ">")
            ( SyntaxKind.NumericLiteralToken,
              5)
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.IfKeyword,
              "if")
            ( SyntaxKind.OpenParenToken,
              "(")
            ( SyntaxKind.NumericLiteralToken,
              5)
            ( SyntaxKind.LessThanToken,
              "<")
            ( SyntaxKind.NumericLiteralToken,
              10)
            ( SyntaxKind.CloseParenToken,
              ")")
            ( SyntaxKind.OpenBraceToken,
              "{")
            
            ( SyntaxKind.ReturnKeyword,
              "return")
            ( SyntaxKind.TrueKeyword,
              "true")
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.CloseBraceToken,
              "}")
            ( SyntaxKind.ElseKeyword,
              "else")
            ( SyntaxKind.OpenBraceToken,
              "{")
            
            ( SyntaxKind.ReturnKeyword,
              "return")
            ( SyntaxKind.FalseKeyword,
              "false")
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.CloseBraceToken,
              "}")
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.NumericLiteralToken,
              10)
            ( SyntaxKind.EqualsEqualsToken,
              "==")
            ( SyntaxKind.NumericLiteralToken,
              10)
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.NumericLiteralToken,
              10)
            ( SyntaxKind.ExclamationEqualsToken,
              "!=")
            ( SyntaxKind.NumericLiteralToken,
              9)
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.NumericLiteralToken,
              10)
            ( SyntaxKind.GreaterThanEqualsToken,
              ">=")
            ( SyntaxKind.NumericLiteralToken,
              9)
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.NumericLiteralToken,
              10)
            ( SyntaxKind.LessThanEqualsToken,
              "<=")
            ( SyntaxKind.NumericLiteralToken,
              9)
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.OpenBracketToken,
              "[")
            ( SyntaxKind.IntKeyword,
              "int")
            ( SyntaxKind.MinusGreaterThanToken,
              "->")
            ( SyntaxKind.IntKeyword,
              "int")
            ( SyntaxKind.CloseBracketToken,
              "]")
            ( SyntaxKind.SemicolonToken,
              ";")
            
            ( SyntaxKind.EndOfFileToken,
              "")
        |]
        let testCases = Array.map (fun (syntaxKind, obj) -> (syntaxKind, Some obj)) testCases
        let tokens = Tokenizer.tokenize testInput
        printTokensAndTestCases tokens testCases
        testLexer tokens testCases

    [<Test>]
    [<Description("Tests string literal tokenizing.")>]
    member this.``Tokenizer Test 4``() =
        let testInput = "let foo: string = \"bar\";"
        
        let testCases: (SyntaxKind * obj) array = [|
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "foo")
            ( SyntaxKind.ColonToken,
              ":")
            ( SyntaxKind.StringKeyword,
              "string")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.StringLiteralToken,
              "bar")
            ( SyntaxKind.SemicolonToken,
              ";")
            ( SyntaxKind.EndOfFileToken,
              "")
        |]
        let testCases = Array.map (fun (syntaxKind, obj) -> (syntaxKind, Some obj)) testCases
        let tokens = Tokenizer.tokenize testInput
        printTokensAndTestCases tokens testCases
        testLexer tokens testCases
        
    [<Test>]
    [<Description("Tests array tokenizing.")>]
    member this.``Tokenizer Test 5``() =
        let testInput = "let foo: string[] = [\"bar\", \"pluh\"];"
        
        let testCases: (SyntaxKind * obj) array = [|
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "foo")
            ( SyntaxKind.ColonToken,
              ":")
            ( SyntaxKind.StringKeyword,
              "string")
            ( SyntaxKind.OpenBracketToken,
              "[")
            ( SyntaxKind.CloseBracketToken,
              "]")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.OpenBracketToken,
              "[")
            ( SyntaxKind.StringLiteralToken,
              "bar")
            ( SyntaxKind.CommaToken,
              ",")
            ( SyntaxKind.StringLiteralToken,
              "pluh")
            ( SyntaxKind.CloseBracketToken,
              "]")
            ( SyntaxKind.SemicolonToken,
              ";")
            ( SyntaxKind.EndOfFileToken,
              "")
        |]
        let testCases = Array.map (fun (syntaxKind, obj) -> (syntaxKind, Some obj)) testCases
        let tokens = Tokenizer.tokenize testInput
        printTokensAndTestCases tokens testCases
        testLexer tokens testCases
        
    [<Test>]
    [<Description("Tests dictionary/map tokenizing.")>]
    member this.``Tokenizer Test 6``() =
        let testInput = "let foo = { \"bar\": 1, \"pluh\": 2 };"
        
        let testCases: (SyntaxKind * obj) array = [|
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "foo")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.OpenBraceToken,
              "{")
            ( SyntaxKind.StringLiteralToken,
              "bar")
            ( SyntaxKind.ColonToken,
              ":")
            ( SyntaxKind.NumericLiteralToken,
              1)
            ( SyntaxKind.CommaToken,
              ",")
            ( SyntaxKind.StringLiteralToken,
              "pluh")
            ( SyntaxKind.ColonToken,
              ":")
            ( SyntaxKind.NumericLiteralToken,
              2)
            ( SyntaxKind.CloseBraceToken,
              "}")
            ( SyntaxKind.SemicolonToken,
              ";")
            ( SyntaxKind.EndOfFileToken,
              "")
        |]
        let testCases = Array.map (fun (syntaxKind, obj) -> (syntaxKind, Some obj)) testCases
        let tokens = Tokenizer.tokenize testInput
        printTokensAndTestCases tokens testCases
        testLexer tokens testCases
