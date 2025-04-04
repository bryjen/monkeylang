namespace rec Monkey.Parser.Tests.Tokenizer.Tokenizer

open System

open Microsoft.CodeAnalysis.CSharp

open NUnit.Framework

open FsToolkit.ErrorHandling

open Monkey.AST
open Monkey.Parser.Tokenizer



// Lifted from 'Monkey.Frontend' (hella legacy)
(*
Notes:
Unlike what some LLMs may say, it is not possible to create a TYPE SAFE generic function that turns a "n" sized tuple 
into a "n+1" sized tuple (I tried). There are solutions in StackOverflow where we use Reflection. However, this 
"bypasses" the type system. For this, I don't want to do that.

The next best way is to manually specify the cases and use overloading as seen below.
Although not completely generic, it is rare in practice to use tuples 4 elements in size, much more to require prepending. 
*)

type TuplePrepender =
    
    static member Prepend(x: int, (a, b)) = (x, a, b)
    
    static member Prepend(x: int, (a, b, c)) = (x, a, b, c)
    
    static member Prepend(x: int, (a, b, c, d)) = (x, a, b, c, d)
    
    // Can add more cases as needed
    
    
    [<Obsolete>]
    static member AddCountsToTuples (tuples: ('a * 'b) list) : (int * 'a * 'b) list =
        let mutable testCount = 0
        
        let addTestCountToTuple (tuple: 'a * 'b) : int * 'a * 'b =
            testCount <- testCount + 1
            TuplePrepender.Prepend(testCount, tuple)
            
        tuples |> List.map addTestCountToTuple
    
    
    static member AddCountsToTuples (tuples: ('a * 'b) array) : (int * 'a * 'b) array =
        let mutable testCount = 0
        
        let addTestCountToTuple (tuple: 'a * 'b) : int * 'a * 'b =
            testCount <- testCount + 1
            TuplePrepender.Prepend(testCount, tuple)
            
        tuples |> Array.map addTestCountToTuple
    
    static member AddCountsToTuples (tuples: ('a * 'b * 'c) array) : (int * 'a * 'b * 'c) array =
        let mutable testCount = 0
        
        let addTestCountToTuple (tuple: 'a * 'b * 'c) : int * 'a * 'b * 'c =
            testCount <- testCount + 1
            TuplePrepender.Prepend(testCount, tuple)
            
        tuples |> Array.map addTestCountToTuple
        
    static member AddCountsToTuples (tuples: ('a * 'b * 'c * 'd) array) : (int * 'a * 'b * 'c * 'd) array =
        let mutable testCount = 0
        
        let addTestCountToTuple (tuple: 'a * 'b * 'c * 'd) : int * 'a * 'b * 'c * 'd =
            testCount <- testCount + 1
            TuplePrepender.Prepend(testCount, tuple)
            
        tuples |> Array.map addTestCountToTuple
        
        

[<AutoOpen>]
module private TokenizerTestHelpers =
    /// Returns the third element in a tuple.
    let trd (_, _, c) = c
    
    let addCountsToList (tuples: 'a array) : (int * 'a) array =
        let counts = Array.init tuples.Length (fun i -> i + 1)
        Array.zip counts tuples
        
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
                Error $"[test #{testCount}] Same types, but expected value `{expected}`, but got `{actual}`"
        | false ->
            Error $"[test #{testCount}] Expected type \"{expectedType}\", but got \"{actualType}\""
            
    let processTestCase (testCase: SyntaxToken * (int * SyntaxKind * obj)) = 
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
            
    let testLexer (tokens: SyntaxToken array) (testCases: (SyntaxKind * obj) array) =
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
        

    let rec printTestCases (testCases: (SyntaxKind * obj) array) : unit =
        let testCasesWithCounts = TuplePrepender.AddCountsToTuples testCases
        for (testCase, expectedKind, expectedValue) in testCasesWithCounts do
            printTestCase testCase expectedKind expectedValue
            ()
        ()
    and private printTestCase testCase expectedKind expectedValue =
        printfn $"[{testCase}] {expectedKind};\n\t{expectedValue}"
        
        
    let printTokensAndTestCases (tokens: SyntaxToken array) (testCases: (SyntaxKind * obj) array) : unit =
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
        let tokens = tokenize testInput
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
        let tokens = tokenize testInput
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
        let tokens = tokenize testInput
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
        let tokens = tokenize testInput
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
        let tokens = tokenize testInput
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
        let tokens = tokenize testInput
        printTokensAndTestCases tokens testCases
        testLexer tokens testCases
        
        
        
    [<Test>]
    [<Description("Tests interpolated string.")>]
    member this.``Tokenizer Test 7``() =
        let testInput = @"let foobar = $""Hello, {name}!"";"
        
        let testCases: (SyntaxKind * obj) array = [|
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "foobar")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.DollarToken,
              "$")
            ( SyntaxKind.StringLiteralToken,
              "Hello, {name}!")
            ( SyntaxKind.SemicolonToken,
              ";")
            ( SyntaxKind.EndOfFileToken,
              "")
        |]
        let tokens = tokenize testInput
        printTokensAndTestCases tokens testCases
        testLexer tokens testCases
        
        
    [<Test>]
    [<Description("Tests interpolated string & correct parsing of double quot tokens.")>]
    member this.``Tokenizer Test 8``() =
        let testInput = @"let foobar = $""Hello, \""{name}!\"""";"
        
        let testCases: (SyntaxKind * obj) array = [|
            ( SyntaxKind.LetKeyword,
              "let")
            ( SyntaxKind.IdentifierToken,
              "foobar")
            ( SyntaxKind.EqualsToken,
              "=")
            ( SyntaxKind.DollarToken,
              "$")
            ( SyntaxKind.StringLiteralToken,
              "Hello, \\\"{name}!\\\"")
            ( SyntaxKind.SemicolonToken,
              ";")
            ( SyntaxKind.EndOfFileToken,
              "")
        |]
        let tokens = tokenize testInput
        printTokensAndTestCases tokens testCases
        testLexer tokens testCases
