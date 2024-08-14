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
    
    static member ``A: Test Basic Integer Arithmetic Case`` = [|
            { Input = "1"; Expected = 1 }
            { Input = "2"; Expected = 2 }
            { Input = "1 + 2"; Expected = 3 }
    |]
        
    static member ``B: Test Complex Integer Arithmetic Case 1`` = [|
            { Input = "1 - 2"; Expected = -1 }
            { Input = "1 * 2"; Expected = 2 }
            { Input = "4 / 2"; Expected = 2 }
            { Input = "50 / 2 * 2 + 10 - 5"; Expected = 55 }
            { Input = "5 + 5 + 5 + 5 - 10"; Expected = 10 }
            { Input = "2 * 2 * 2 * 2 * 2"; Expected = 32 }
            { Input = "5 * 2 + 10"; Expected = 20 }
            { Input = "5 + 2 * 10"; Expected = 25 }
            { Input = "5 * (2 + 10)"; Expected = 60 }
    |]
        
    static member ``C: Test Complex Integer Arithmetic Case 2`` = [|
            // TODO: Prefix operations needs to be parsed for the following to pass
            
            { Input = "5 * 2 + 10"; Expected = 20 }
            { Input = "5 + 2 * 10"; Expected = 25 }
            { Input = "50 / 2 * 2 + 10"; Expected = 60 }
            { Input = "2 * (5 + 10)"; Expected = 30 }
            { Input = "3 * 3 * 3 + 10"; Expected = 37 }
            { Input = "3 * (3 * 3) + 10"; Expected = 37 }
    |]
        
    static member ``D: Test Basic Boolean Evaluation`` = [|
            { Input = "true"; Expected = true }
            { Input = "false"; Expected = false }
    |]
        
    static member ``E: Test Boolean Evaluation`` = [|
            { Input = "1 < 2"; Expected = true }
            { Input = "1 > 2"; Expected = false }
            { Input = "1 < 1"; Expected = false }
            { Input = "1 > 1"; Expected = false }
            { Input = "1 == 1"; Expected = true }
            { Input = "1 != 1"; Expected = false }
            { Input = "1 == 2"; Expected = false }
            { Input = "1 != 2"; Expected = true }
            
            { Input = "true == true"; Expected = true }
            { Input = "false == false"; Expected = true }
            { Input = "true == false"; Expected = false }
            { Input = "true != false"; Expected = true }
            { Input = "false != true"; Expected = true }
            
            { Input = "(1 < 2) == true"; Expected = true }
            { Input = "(1 < 2) == false"; Expected = false }
            { Input = "(1 > 2) == true"; Expected = false }
            { Input = "(1 > 2) == false"; Expected = true }
    |]
        
    static member ``F: Test Prefix Expression Evaluation`` = [|
            { Input = "-5"; Expected = -5 }
            { Input = "-10"; Expected = -10 }
            { Input = "-50 + 100 + -50"; Expected = 0 }
            { Input = "(5 + 10 * 2 + 15 / 3) * 2 + -10"; Expected = 50 }
    |]
        
    static member ``G: Test Boolean Expression Evaluation`` = [|
            { Input = "!true"; Expected = false }
            { Input = "!false"; Expected = true }
            { Input = "!!true"; Expected = true }
            { Input = "!!false"; Expected = false }
    |]
    
    static member ``H: Test Prefix and Infix Expression Evaluation`` = [|
            { Input = "-50 + 100 + -50"; Expected = 0 }
            { Input = "20 + 2 * -10"; Expected = 0 }
            { Input = "(5 + 10 * 2 + 15 / 3) * 2 + -10"; Expected = 50 }
    |]
    
    static member ``I: Test If Expression Evaluation`` = [|
            { Input = "if (true) { 10 }"; Expected = 10 }
            { Input = "if (true) { 10 } else { 20 }"; Expected = 10 }
            { Input = "if (false) { 10 } else { 20 }"; Expected = 20 }
            { Input = "if (1 < 2) { 10 }"; Expected = 10 }
            { Input = "if (1 < 2) { 10 } else { 20 }"; Expected = 10 }
            { Input = "if (1 > 2) { 10 } else { 20 }"; Expected = 20 }
            { Input = "if ((if (false) { false } else { true })) { 10 } else { 20 }"; Expected = 10 }
            
            { Input = "if (1 > 2) { 10 }"; Expected = null }
            { Input = "if (false) { 10 }"; Expected = null }
    |]
    
    static member ``J: Test Let Statement Evaluation`` = [|
            { Input = "let one = 1; one;"; Expected = 1 }
            { Input = "let one = 1; let two = 2; one + two;"; Expected = 3 }
            { Input = "let one = 1; let two = one + one; one + two;"; Expected = 3 }
    |]
    
    static member ``K: Test String Evaluation`` = [|
            { Input = "\"monkey\";"; Expected = "monkey" }
            { Input = "\"mon\" + \"key\";"; Expected = "monkey" }
            { Input = "\"mon\" + \"key\" + \"banana\";"; Expected = "monkeybanana" }
    |]
        
        
    static member TestCasesToExecute = Array.concat [
        VirtualMachineTests.``A: Test Basic Integer Arithmetic Case``
        VirtualMachineTests.``B: Test Complex Integer Arithmetic Case 1``
        VirtualMachineTests.``C: Test Complex Integer Arithmetic Case 2``
        VirtualMachineTests.``D: Test Basic Boolean Evaluation``
        VirtualMachineTests.``E: Test Boolean Evaluation``
        VirtualMachineTests.``F: Test Prefix Expression Evaluation``
        VirtualMachineTests.``G: Test Boolean Expression Evaluation``
        VirtualMachineTests.``H: Test Prefix and Infix Expression Evaluation``
        VirtualMachineTests.``I: Test If Expression Evaluation``
        VirtualMachineTests.``J: Test Let Statement Evaluation``
        VirtualMachineTests.``K: Test String Evaluation``
    ]
        
    [<TestCaseSource("TestCasesToExecute")>]
    member this.``Run VM Tests``(vmTestCase: VMTestCase) =
        result {
            let program = Parser.parseProgram vmTestCase.Input 
            let nodes = programToNodes program
            
            let mutable compiler = Compiler.New
            let! newCompiler = compiler.CompileNodes(nodes) 
            let bytecode = newCompiler.Bytecode()
            
            TestContext.WriteLine($"Got:\n{bytecode.Instructions.ToString()}")
            
            let vm = VM.FromByteCode(bytecode)
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
