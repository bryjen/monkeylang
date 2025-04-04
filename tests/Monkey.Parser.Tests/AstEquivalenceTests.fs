namespace Monkey.Parser.Tests.AstEquivalenceTests

open Monkey.AST
open NUnit.Framework

open type Monkey.AST.SyntaxFactory.MonkeySyntaxTokenFactory
open type Monkey.AST.SyntaxFactory.MonkeyExpressionSyntaxFactory
open type Monkey.AST.SyntaxFactory.MonkeyStatementSyntaxFactory


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
        let es1 = ExpressionStatementNoBox(NumericLiteralExpression(5))
        let es2 = ExpressionStatementNoBox(NumericLiteralExpression(5))
        
        printfn $"NODE 1:\n```\n{es1}\n```\n"
        printfn $"NODE 2:\n```\n{es1}\n```\n"
        
        if ExpressionStatementSyntax.AreEquivalent(es1, es2) then Assert.Pass() else Assert.Fail()
        
    [<Test>]
    member this.``Equivalence Test 2``() =
        let es1 = ExpressionStatementNoBox(ParenthesizedExpression(MinusPrefixExpression(NumericLiteralExpression(5))))
        let es2 = ExpressionStatementNoBox(ParenthesizedExpression(MinusPrefixExpression(NumericLiteralExpression(5))))
        
        printfn $"NODE 1:\n```\n{es1}\n```\n"
        printfn $"NODE 2:\n```\n{es1}\n```\n"
        
        if ExpressionStatementSyntax.AreEquivalent(es1, es2) then Assert.Pass() else Assert.Fail()
