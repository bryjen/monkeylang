namespace Monkey.Backend.Tests.Compiler

open NUnit.Framework
open FsToolkit.ErrorHandling

open Monkey.Frontend.Parser

open Monkey.Backend.Tests.Compiler.Helpers
open Monkey.Backend.Compiler
open Monkey.Backend.Code

type CompilerTestCase =
    { Input: string
      ExpectedConstants: obj array
      ExpectedInstructions: Instructions array }
    
    

[<TestFixture>]
type CompilerTests() =
    
    static member ``A: Test Integer Arithmetic Case`` =
        { Input = "1 + 2"
          ExpectedConstants = [| 1; 2 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
          |] |> Array.map Instructions
        }
        
        
    static member TestCasesToExecute = [|
        ("\"A: Test Integer Arithmetic Case\"", CompilerTests.``A: Test Integer Arithmetic Case``)
    |]
    
    [<TestCaseSource("TestCasesToExecute")>]
    member this.``Run Compiler Tests``(testCase: string * CompilerTestCase) =
        let testCaseName, compilerTestCase = testCase
        TestContext.WriteLine($"{testCaseName}")
        TestContext.WriteLine($"Input: \"{compilerTestCase.Input}\"")
        
        result {
            let program = Parser.parseProgram compilerTestCase.Input
            let nodes = programToNodes program
            
            let compiler = Compiler.New
            let! updatedCompiler = compiler.Compile(nodes[0]) // TODO: Change to handle multiple statements
            let bytecode = updatedCompiler.Bytecode()
            
            do! CompilerHelpers.testInstructions compilerTestCase.ExpectedInstructions bytecode.Instructions
            
            do! CompilerHelpers.testConstants compilerTestCase.ExpectedConstants bytecode.Constants
        }
        |> function
           | Ok _ -> Assert.Pass("Passed")
           | Error errorMsg -> Assert.Fail(errorMsg)