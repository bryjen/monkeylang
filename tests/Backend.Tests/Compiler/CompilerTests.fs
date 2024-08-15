namespace Monkey.Backend.Tests.Compiler

open System.Net
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
    
    static member ``A: Test Integer Arithmetic Case`` = [|
        { Input = "1 + 2"
          ExpectedConstants = [| 1; 2 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpAdd [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "1; 2;"
          ExpectedConstants = [| 1; 2 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpPop [| |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "1 - 2"
          ExpectedConstants = [| 1; 2 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpSub [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "1 * 2"
          ExpectedConstants = [| 1; 2 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpMul [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "2 / 1"
          ExpectedConstants = [| 2; 1 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpDiv [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
    
    static member ``B: Test Boolean Expr codegen 1`` = [|
        { Input = "true"
          ExpectedConstants = [| |]
          ExpectedInstructions = [|
              make Opcode.OpTrue [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "false"
          ExpectedConstants = [| |]
          ExpectedInstructions = [|
              make Opcode.OpFalse [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
    
    static member ``C: Test Boolean Expr codegen 2`` = [|
        { Input = "1 > 2"
          ExpectedConstants = [| 1; 2 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpGreaterThan [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "1 < 2"
          ExpectedConstants = [| 2; 1 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpGreaterThan [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "1 == 2"
          ExpectedConstants = [| 1; 2 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpEqual [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "1 != 2"
          ExpectedConstants = [| 1; 2 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpNotEqual [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "true == false"
          ExpectedConstants = [| |]
          ExpectedInstructions = [|
              make Opcode.OpTrue [| |]
              make Opcode.OpFalse [| |]
              make Opcode.OpEqual [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "true != false"
          ExpectedConstants = [| |]
          ExpectedInstructions = [|
              make Opcode.OpTrue [| |]
              make Opcode.OpFalse [| |]
              make Opcode.OpNotEqual [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
        
    static member ``D: Test Prefix Expr codegen`` = [|
        { Input = "-1"
          ExpectedConstants = [| 1 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpMinus [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "!true"
          ExpectedConstants = [| |]
          ExpectedInstructions = [|
              make Opcode.OpTrue [| |]
              make Opcode.OpBang [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
    
    static member ``E: Test If Expr codegen`` = [|
        { Input = "if (true) { 10 }; 3333;"
          ExpectedConstants = [| 10; 3333 |]
          ExpectedInstructions = [|
              make Opcode.OpTrue [| |]
              make Opcode.OpJumpWhenFalse [| 10 |]
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpJump [| 11 |]
              make Opcode.OpNull [| |]
              make Opcode.OpPop [| |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "if (true) { 10 } else { 20 }; 3333;"
          ExpectedConstants = [| 10; 20; 3333 |]
          ExpectedInstructions = [|
              make Opcode.OpTrue [| |]
              make Opcode.OpJumpWhenFalse [| 10 |]
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpJump [| 13 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpPop [| |]
              make Opcode.OpConstant [| 2 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
    
    
    static member ``F: Test Let Statement codegen`` = [|
        { Input =
            "let one = 1;
            let two = 2;"
          ExpectedConstants = [| 1; 2 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpSetGlobal [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpSetGlobal [| 1 |]
          |] |> Array.map Instructions }
        
        { Input =
            "let one = 1;
            one;"
          ExpectedConstants = [| 1 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpSetGlobal [| 0 |]
              make Opcode.OpGetGlobal [| 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input =
            "let one = 1;
            let two = one;
            two;"
          ExpectedConstants = [| 1 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpSetGlobal [| 0 |]
              make Opcode.OpGetGlobal [| 0 |]
              make Opcode.OpSetGlobal [| 1 |]
              make Opcode.OpGetGlobal [| 1 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
    
    static member ``G: Test String literal codegen`` = [|
        { Input = "\"monkey\""
          ExpectedConstants = [| "monkey" |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "\"mon\" + \"key\""
          ExpectedConstants = [| "mon"; "key" |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpAdd [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
    
    static member ``H: Test Array literal codegen`` = [|
        { Input = "[1, 2, 3]"
          ExpectedConstants = [| 1; 2; 3 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpConstant [| 2 |]
              make Opcode.OpArray [| 3 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "[1 + 2, 3 - 4, 5 * 6]"
          ExpectedConstants = [| 1; 2; 3; 4; 5; 6 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpAdd [| |]
              
              make Opcode.OpConstant [| 2 |]
              make Opcode.OpConstant [| 3 |]
              make Opcode.OpSub [| |]
              
              make Opcode.OpConstant [| 4 |]
              make Opcode.OpConstant [| 5 |]
              make Opcode.OpMul [| |]
              
              make Opcode.OpArray [| 3 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
    
    // See if tests could fail due to the 'random' order that key value pairs could be returned when getting
    // the key value pairs of the map.
    static member ``I: Test Hash literal codegen`` = [|
        { Input = "{}"
          ExpectedConstants = [| |]
          ExpectedInstructions = [|
              make Opcode.OpHash [| 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "{1: 2, 3: 4, 5: 6}"
          ExpectedConstants = [| 1; 2; 3; 4; 5; 6 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpConstant [| 2 |]
              make Opcode.OpConstant [| 3 |]
              make Opcode.OpConstant [| 4 |]
              make Opcode.OpConstant [| 5 |]
              make Opcode.OpHash [| 6 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "{1: 2 + 3, 4: 5 * 6}"
          ExpectedConstants = [| 1; 2; 3; 4; 5; 6 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpConstant [| 2 |]
              make Opcode.OpAdd [| |]
              make Opcode.OpConstant [| 3 |]
              make Opcode.OpConstant [| 4 |]
              make Opcode.OpConstant [| 5 |]
              make Opcode.OpMul [| |]
              make Opcode.OpHash [| 4 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
        
    static member TestCasesToExecute = Array.concat [
        CompilerTests.``A: Test Integer Arithmetic Case``
        CompilerTests.``B: Test Boolean Expr codegen 1``
        CompilerTests.``C: Test Boolean Expr codegen 2``
        CompilerTests.``D: Test Prefix Expr codegen``
        CompilerTests.``E: Test If Expr codegen``
        CompilerTests.``F: Test Let Statement codegen``
        CompilerTests.``G: Test String literal codegen``
        CompilerTests.``H: Test Array literal codegen``
        CompilerTests.``I: Test Hash literal codegen``
    ]
    
    [<TestCaseSource("TestCasesToExecute")>]
    member this.``Run Compiler Tests``(compilerTestCase: CompilerTestCase) =
        // TestContext.WriteLine($"{testCaseName}")
        TestContext.WriteLine($"Input: \"{compilerTestCase.Input}\"")
        
        result {
            let program = Parser.parseProgram compilerTestCase.Input
            let nodes = programToNodes program
            
            let mutable compiler = Compiler.New
            let! newCompiler = compiler.CompileNodes(nodes) 
            let bytecode = newCompiler.Bytecode()
                
            
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