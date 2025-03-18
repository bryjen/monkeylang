module Frontend.CLR.Tests.Tokenizer.AstEquivalenceTests

open Monkey.Frontend.CLR.Syntax.Ast
open NUnit.Framework

open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeySyntaxTokenFactory
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeyExpressionSyntaxFactory
open type Monkey.Frontend.CLR.Syntax.SyntaxFactory.MonkeyStatementSyntaxFactory


[<AutoOpen>]
module private AstEquivalenceTests =
    let internal escapeWhitespaceCharacters (s: string) =
        s
        |> Seq.map (function
            | '\n' -> "\\n"
            | '\r' -> "\\r"
            | '\t' -> "\\t"
            | ' '  -> "\\s"  // Represent space as "\s"
            | c    -> c.ToString())
        |> String.concat ""
        
        

[<TestFixture>]
type AstEquivalenceTests() =
    [<Test>]
    member this.``Equivalence Test 1``() =
        let es1 = ExpressionStatement(NumericLiteralExpression(5))
        let es2 = ExpressionStatement(NumericLiteralExpression(5))
        
        printfn $"NODE 1:\n```\n{es1}\n```\n"
        printfn $"NODE 2:\n```\n{es1}\n```\n"
        
        if ExpressionStatementSyntax.AreEquivalent(es1, es2) then Assert.Pass() else Assert.Fail()
        
    [<Test>]
    member this.``Equivalence Test 2``() =
        let es1 = ExpressionStatement(ParenthesizedExpression(MinusPrefixExpression(NumericLiteralExpression(5))))
        let es2 = ExpressionStatement(ParenthesizedExpression(MinusPrefixExpression(NumericLiteralExpression(5))))
        
        printfn $"NODE 1:\n```\n{es1}\n```\n"
        printfn $"NODE 2:\n```\n{es1}\n```\n"
        
        if ExpressionStatementSyntax.AreEquivalent(es1, es2) then Assert.Pass() else Assert.Fail()
