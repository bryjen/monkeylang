namespace Monkey.Parser.Tests.Tokenizer.RoundtrippableTests

open Microsoft.CodeAnalysis.Text
open Monkey.AST
open Monkey.Parser.Tokenizer
open NUnit.Framework

[<AutoOpen>]
module private RoundtrippableTestsHelpers =
    
    let internal escapeWhitespaceCharacters (s: string) =
        s
        |> Seq.map (function
            | '\n' -> "\\n"
            | '\r' -> "\\r"
            | '\t' -> "\\t"
            | ' '  -> "\\s"  // Represent space as "\s"
            | c    -> c.ToString())
        |> String.concat ""
    
    let rec internal printTokens (tokens: SyntaxToken array) : unit =
        let counts = [1..tokens.Length] |> List.toArray
        for (count, token) in Array.zip counts tokens do
            printToken count token
            ()
        ()
    and private printToken count token =
        printfn $"[{count}]"
        printfn $"\tKIND:\t\t{token.Kind}"
        printfn $"\tTEXT:\t\t{token.Kind}"
        printfn $"\tVALUE:\t\t{token.Value}"
        printfn $"\tSPAN:\t\t\"{token.TextSpan}\""
        printfn $"\tFULL_SPAN:\t\"{token.FullTextSpan}\""
        printfn $"\tLEAD_TRIV:\t\"{escapeWhitespaceCharacters (token.LeadingTrivia.ToFullString())}\""
        printfn $"\tTRAIL_TRIV:\t\"{escapeWhitespaceCharacters (token.TrailingTrivia.ToFullString())}\""
        
    let assertSpanEqualsText (sourceText: SourceText) (tokens: SyntaxToken array) =
        let tokenCounts = [1..tokens.Length] |> List.toArray
        let zipped = Array.zip tokenCounts tokens
        
        let errors = ResizeArray<string>()
        for (count, token) in zipped do
            try
                let textFromSource = sourceText.GetSubText(token.TextSpan).ToString()
                if textFromSource <> token.Text then
                    errors.Add($"[{count}] The textspan {token.TextSpan} has an incorrect snippet. Expected \"{token.Text}\" (token.Text), but got \"{textFromSource}\"")
            with
            | ex -> ()  // ignored
                
        if (errors.Count > 0) then
            let errorsList = System.String.Join("\n", errors)
            Assert.Fail($"Some textspans don't match the token's text:\n{errorsList}")
        
        
        
    let testCore (testInput: string) : unit =
        let originalSourceText = SourceText.From(testInput)
        
        let tokens = tokenize testInput
        let strings = Array.map _.ToString() tokens
        let reconstructed = System.String.Join("", strings)
        let reconstructedSourceTest = SourceText.From(reconstructed)
        
        printfn $"EXPECTED:\n```\n{testInput}\n```\n"
        printfn $"ACTUAL:\n```\n{reconstructed}\n```\n"
        
        
        let reconstructedTokens = tokenize reconstructed
        printfn "[EXPECTED]"
        printTokens tokens
        
        printfn "[ACTUAL]"
        printTokens reconstructedTokens
        
        match originalSourceText.ContentEquals(reconstructedSourceTest) && reconstructedSourceTest.ContentEquals(originalSourceText) with
        | true ->
            assertSpanEqualsText originalSourceText tokens
            Assert.Pass()
        | false -> Assert.Fail("The reconstructed text does not equal the original. Examine the logs to see the differences.")



/// <summary>
/// Tests the tokenizer can parse tokens while keeping trivia information. This includes "non-essential" source text
/// information such as the number of spaces, new-lines, etc.
/// <br/><br/>
/// The main goal is for the parsed tokens to be <a href="https://dev.to/cad97/lossless-syntax-trees-280c">roundtrippable</a>.
/// </summary>
[<TestFixture>]
type RoundtrippableTests() =
    [<Test>]
    member this.``Roundtrippable Test 1``() =
        let testInput = "let  foo:  string =  \"bar\";"
        testCore testInput
        
        
        
    [<Test>]
    member this.``Roundtrippable Test 2``() =
        let testInput = """let five = 5;
let ten = 10;
            
let add = fn(int x, int y) : int {
    x + y;
};
            
let result = add(five, ten);"""

        testCore testInput
        
        
        
    [<Test>]
    member this.``Roundtrippable Test 3``() =
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

        testCore testInput


    
    [<Test>]
    member this.``Roundtrippable Test 4``() =
        let testInput = "let foo: string[] = [\"bar\", \"pluh\"];"
        testCore testInput
        
        
        
    [<Test>]
    member this.``Roundtrippable Test 5``() =
        let testInput = "let foo = { \"bar\": 1, \"pluh\": 2 };"
        testCore testInput
