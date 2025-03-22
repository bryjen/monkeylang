namespace Monkey.Frontend.CLR.Tests.Parser.MonkeyAstParseErrorTests

open Frontend.CLR.Syntax
open Microsoft.CodeAnalysis.Text
open NUnit.Framework


[<TestFixture>]
type ArrayExpressionParsing() =
    
    [<Test>]
    member this.``Runner``() =
        let text = "fn int y) { }"
        let sourceText = SourceText.From(text)
        let tokens = Tokenizer.tokenize text
        
        let _, parseErrors = Monkey.Frontend.CLR.Parsers.MonkeyAstParser.parseTokens tokens
        parseErrors |> Array.map _.GetFormattedMessage(sourceText) |> Array.iter (fun str -> printfn $"{str}\n\n")
        
        Assert.Pass()