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
              make Opcode.OpAdd [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions
        }
        
    static member ``B: Test OpPop generation`` = 
        { Input = "1; 2;"
          ExpectedConstants = [| 1; 2 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpPop [| |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions
        }
        
    static member TestCasesToExecute = [|
        CompilerTests.``A: Test Integer Arithmetic Case``
        CompilerTests.``B: Test OpPop generation``
    |]
    
    [<TestCaseSource("TestCasesToExecute")>]
    member this.``Run Compiler Tests``(compilerTestCase: CompilerTestCase) =
        // TestContext.WriteLine($"{testCaseName}")
        TestContext.WriteLine($"Input: \"{compilerTestCase.Input}\"")
        
        result {
            let program = Parser.parseProgram compilerTestCase.Input
            let nodes = programToNodes program
            
            let mutable compiler = Compiler.New
            for node in nodes do
                let! newCompiler = compiler.Compile(node)
                compiler <- newCompiler
                
            let bytecode = compiler.Bytecode()
            
            let expectedInstructions = compilerTestCase.ExpectedInstructions
                                       |> Array.map (_.GetBytes())
                                       |> Array.concat
                                       |> Instructions
            TestContext.WriteLine($"\nExpected:\n{expectedInstructions.ToString()}\n")
            TestContext.WriteLine($"Got:\n{bytecode.Instructions.ToString()}\n")
            
            do! CompilerHelpers.testInstructions compilerTestCase.ExpectedInstructions bytecode.Instructions
            
            do! CompilerHelpers.testConstants compilerTestCase.ExpectedConstants bytecode.Constants
        }
        |> function
           | Ok _ -> Assert.Pass("Passed")
           | Error errorMsg -> Assert.Fail(errorMsg)