module Monkey.Codegen.Dotnet.Tests.Codegen

open System.IO
open System.Reflection
open FsToolkit.ErrorHandling
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Monkey.Codegen.Dotnet.MonkeyToCSharpAstConverter
open Monkey.Codegen.Dotnet.Tests.CodegenTestsHelpers
open Monkey.Parser.Parser
open Monkey.Parser.Tokenizer
open NUnit.Framework


[<TestFixture>]
type CodegenTests() =
    member this.TestCases : (string * string) array =
        [|
            (
                """
using System;
Console.WriteLine("Hello, World!");
""",
                """
using System;
Console.WriteLine("Hello, World!");
"""
            )
        |]
        
    [<TestCase(0)>]
    member this.``Test Monkey and CSharp Equivalence``(testCaseIdx: int) =
        result {
            let monkeyInput, equivalentCs = this.TestCases[testCaseIdx]
            let tokens = tokenize monkeyInput
            let monkeyCompilationUnit, parseErrors = parseTokens tokens
            do! assertNoParseErrors monkeyInput parseErrors
            
            let! csharpCompilationUnit = (toCSharpCompilationUnit monkeyCompilationUnit.Statements |> Result.mapError _.ToString())
            let actualSyntaxTree = csharpCompilationUnit.NormalizeWhitespace().SyntaxTree
            let expectedSyntaxTree = CSharpSyntaxTree.ParseText(equivalentCs)
            
            printTestCases expectedSyntaxTree monkeyInput actualSyntaxTree
            
            return!
                match actualSyntaxTree.IsEquivalentTo(expectedSyntaxTree) && expectedSyntaxTree.IsEquivalentTo(actualSyntaxTree) with
                | true -> Ok ()
                | false -> Error "Syntax trees are not equivalent."
        }
        |> function
            | Ok _ -> Assert.Pass()
            | Error errorStr -> Assert.Fail(errorStr)
            
            
            
    [<TestCase(0, TestName="Equivalence Test for Case 0")>]
    member this.``Test Monkey and CSharp Equivalence (from sample files)``(sampleIdx: int) =
        result {
            let! input, expected = loadSample sampleIdx
            
            let assemblyExecutionDir = Assembly.GetExecutingAssembly().Location |> Path.GetDirectoryName |> Path.GetFullPath
            let outputDir = Path.Join(assemblyExecutionDir, "samples", $"{sampleIdx}", "generated")
            (*
            let scanResults =
                { MkProjFileInfo = input.ProjectFileInfo }
            *)
            return ()
        }
        |> function
            | Ok _ -> Assert.Pass()
            | Error errorStr -> Assert.Fail(errorStr)
