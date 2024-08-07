namespace Monkey.Backend.Tests.VirtualMachine

open FsToolkit.ErrorHandling
open Monkey.Backend.VirtualMachine
open Monkey.Backend.Compiler
open Monkey.Backend.Tests.Compiler
open Monkey.Frontend.Parser
open NUnit.Framework


type VMTestCase =
    { Input: string
      Expected: obj }


type VirtualMachineTests() =
    
    static member ``A: Test Integer Arithmetic Case`` =
        [|
            { Input = "1"
              Expected = 1 }
            { Input = "2"
              Expected = 2 }
            { Input = "1 + 2"
              Expected = 3 }
        |]
        
        
    static member TestCasesToExecute =
        [
            Array.map (fun testCase -> ("\"A: Test Integer Arithmetic Case\"", testCase)) VirtualMachineTests.``A: Test Integer Arithmetic Case``
        ]
        |> Array.concat
        
    [<TestCaseSource("TestCasesToExecute")>]
    member this.``Run VM Tests``(testCase: string * VMTestCase) =
        let testCaseName, vmTestCase = testCase
        
        result {
            let program = Parser.parseProgram vmTestCase.Input 
            let nodes = programToNodes program
            
            let compiler = Compiler.New
            let! newCompiler = compiler.Compile(nodes[0])
            let byteCode = newCompiler.Bytecode()
            
            TestContext.WriteLine($"Got:\n{byteCode.Instructions.ToString()}")
            
            let vm = VM.FromByteCode(byteCode)
            let! newVm = VM.Run(vm)
            
            let resultOption = newVm.LastPoppedStackElement()
            return! 
                match resultOption with
                | None -> Error "Stack top is empty" 
                | Some result -> VMHelpers.testExpectedObject vmTestCase.Expected result
        }
        |> function
           | Ok _ -> Assert.Pass("Passed\n")
           | Error errorMsg -> Assert.Fail(errorMsg)
