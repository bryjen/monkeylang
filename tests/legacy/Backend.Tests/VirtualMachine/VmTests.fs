namespace Monkey.Backend.Tests.VirtualMachine

open FsToolkit.ErrorHandling
open Monkey.Backend.VM
open Monkey.Backend.Compiler
open Monkey.Backend.Tests.Compiler
open Monkey.Frontend.Parser
open NUnit.Framework


type VMTestCase =
    { Input: string
      Expected: obj }


type VmTests() =
    
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
        
    static member ``L: Test Array Literal Evaluation`` = [|
        { Input = "[];"; Expected = [| |] }
        { Input = "[1, 2, 3];"; Expected = ([| 1L; 2L; 3L |] : int64 array) }
        { Input = "[1 + 2, 3 * 4, 5 + 6];"; Expected = ([| 3L; 12L; 11L |] : int64 array) }
    |]
    
    static member ``M: Test Hash Literal Evaluation`` = [|
        { Input = "{};"; Expected = Map.empty }
        { Input = "{1: 2, 2: 3};"; Expected = (Map.ofList [ (1, 2); (2, 3) ] : Map<int64, int64>) }
        { Input = "{1 + 1: 2 * 2, 3 + 3: 4 * 4};"; Expected = (Map.ofList [ (2, 4); (6, 16) ] : Map<int64, int64>) }
    |]
    
    static member ``N: Test Array & Hash Indexing Evaluation`` = [|
        { Input = "[1, 2, 3][1];"; Expected = 2 }
        { Input = "[1, 2, 3][0 + 2];"; Expected = 3 }
        { Input = "[[1, 1, 1]][0][0];"; Expected = 1 }
        { Input = "[][0];"; Expected = null }
        { Input = "[1, 2, 3][99];"; Expected = null }
        { Input = "[1][-1];"; Expected = null }
        
        { Input = "{1: 1, 2: 2}[1];"; Expected = 1 }
        { Input = "{1: 1, 2: 2}[2];"; Expected = 2 }
        { Input = "{1: 1}[0];"; Expected = null }
        { Input = "{}[0];"; Expected = null }
    |]
    
    static member ``O: Test Function Call evaluation - without arguments - 1 `` = [|
        // Implicit returns
        { Input =
                "let fivePlusTen = fn() { 5 + 10; };
                fivePlusTen();"
          Expected = 15 }
        { Input =
                "let one = fn() { 1; };
                let two = fn() { 2; };
                one() + two();"
          Expected = 3 }
        { Input =
                "let a = fn () { 1; };
                let b = fn() { a() + 1 };
                let c = fn() { b() + 1};
                c();"
          Expected = 3 }
        
        // Explicit returns
        { Input =
                "let earlyExit = fn() { return 99; 100; };
                earlyExit();"
          Expected = 99 }
        { Input =
                "let earlyExit = fn() { return 99; return 100; };
                earlyExit();"
          Expected = 99 }
        
        // Empty functions
        { Input =
                "let noReturn = fn() { };
                noReturn();"
          Expected = null }
        { Input =
                "let noReturn = fn() { };
                let noReturnTwo = fn() { noReturn(); };
                noReturn();
                noReturnTwo();"
          Expected = null }
    |]
        
    static member ``P: Test Higher Function Call evaluation - without arguments`` = [|
        { Input =
                "let returnsOne = fn() { 1; };
                let returnsOneReturner = fn() { returnsOne; };
                returnsOneReturner()();"
          Expected = 1 }
    |]
    
    static member ``Q: Test Local Variable Binding evaluation - without arguments - 1`` = [|
        { Input =  // Asserts that local binding works at all
                "let one = fn() { let one = 1; one; };
                one();"
          Expected = 1 }
        { Input =  // Tests that multiple local bindings work within the same function
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                oneAndTwo();"
          Expected = 3 }
        { Input =  // Tests multiple local bindings within different functions
                "let oneAndTwo = fn() { let one = 1; let two = 2; one + two; };
                let threeAndFour = fn() { let three = 3; let four = 4; three + four; };
                oneAndTwo() + threeAndFour();"
          Expected = 10 }
        { Input =  // Asserts that local bindings with the same name in different functions do not cause problems
                "let firstFoobar = fn() { let foobar = 50; foobar; };
                let secondFoobar = fn() { let foobar = 100; foobar; };
                firstFoobar() + secondFoobar();"
          Expected = 150 }
        { Input =
                "let globalSeed = 50;
                let minusOne = fn() {
                    let num = 1;
                    globalSeed - num;
                };
                let minusTwo = fn() {
                    let num = 2;
                    globalSeed - num;
                };
                minusOne() + minusTwo();"
          Expected = 97 }
    |]
    
    static member ``R: Test first class function evaluation`` = [|
        { Input =
                "let returnsOneReturner = fn() {
                    let returnsOne = fn() { 1; };
                    returnsOne;
                };
                (returnsOneReturner())();"
          Expected = 1 }
    |]
    
    static member ``S: Test Function Call evaluation - with arguments - 1`` = [|
        { Input =
                "let identity = fn(a) { a; };
                identity(4);"
          Expected = 4 }
        { Input =
                "let sum = fn(a, b) { a + b; };
                sum(1, 2);"
          Expected = 3 }
        { Input =
                "let sum = fn(a, b) {
                    let c = a + b;
                    c;
                };
                sum(1, 2);"
          Expected = 3 }
        { Input =
                "let sum = fn(a, b) {
                    let c = a + b;
                    c;
                };
                sum(1, 2) + sum(3, 4);"
          Expected = 10 }
        { Input =
                "let sum = fn(a, b) {
                    let c = a + b;
                    c;
                };
                let outer = fn() {
                    sum(1, 2) + sum(3, 4);
                };
                outer();"
          Expected = 10 }
        { Input =
                "let globalNum = 10;
                let sum = fn(a, b) {
                    let c = a + b;
                    c + globalNum;
                };
                
                let outer = fn() {
                    sum(1, 2) + sum(3, 4) + globalNum;
                };
                
                outer() + globalNum;"
          Expected = 50 }
    |]
    
    static member ``T: Test builtin function evaluation 1`` = [|
        { Input = """len("");"""; Expected = 0 }
        { Input = """len("four");"""; Expected = 4 }
        { Input = """len("hello world");"""; Expected = 11 }
        { Input = "len([1, 2, 3]);"; Expected = 3 }
        { Input = "len([]);"; Expected = 0 }
        
        { Input = """println("Hello World");"""; Expected = null }
        
        { Input = "head([1, 2, 3]);"; Expected = 1 }
        { Input = "head([]);"; Expected = null }
        
        { Input = "last([1, 2, 3]);"; Expected = 3 }
        { Input = "last([]);"; Expected = null }
        
        { Input = "tail([1, 2, 3]);"; Expected = ([| 2; 3 |] : int64 array) }
        { Input = "tail([]);"; Expected = ([| |] : int64 array) }
        
        { Input = "push([], 1);"; Expected = ([| 1 |] : int64 array) }
    |]
    
        
    static member TestCasesToExecute = Array.concat [
        VmTests.``A: Test Basic Integer Arithmetic Case``
        VmTests.``B: Test Complex Integer Arithmetic Case 1``
        VmTests.``C: Test Complex Integer Arithmetic Case 2``
        VmTests.``D: Test Basic Boolean Evaluation``
        VmTests.``E: Test Boolean Evaluation``
        VmTests.``F: Test Prefix Expression Evaluation``
        VmTests.``G: Test Boolean Expression Evaluation``
        VmTests.``H: Test Prefix and Infix Expression Evaluation``
        VmTests.``I: Test If Expression Evaluation``
        VmTests.``J: Test Let Statement Evaluation``
        VmTests.``K: Test String Evaluation``
        VmTests.``L: Test Array Literal Evaluation``
        VmTests.``M: Test Hash Literal Evaluation``
        VmTests.``N: Test Array & Hash Indexing Evaluation``
        
        VmTests.``O: Test Function Call evaluation - without arguments - 1 ``
        // Ignored because parser does not parse 'stacked calls' in a single statement
        // VirtualMachineTests.``P: Test Higher Function Call evaluation - without arguments``
        // VirtualMachineTests.``R: Test first class function evaluation``
        VmTests.``Q: Test Local Variable Binding evaluation - without arguments - 1``
        VmTests.``S: Test Function Call evaluation - with arguments - 1``
        VmTests.``T: Test builtin function evaluation 1``
    ]
            
    [<TestCaseSource("TestCasesToExecute")>]
    member this.``Run VM Tests``(vmTestCase: VMTestCase) =
        result {
            let program = Parser.parseProgram vmTestCase.Input
            do! VMHelpers.assertProgramHasNoErrors program
            let nodes = programToNodes program
            
            let! newCompiler = Compiler.compileNodes nodes (Compiler.createNew ()) 
            let bytecode = Compiler.toByteCode newCompiler
            
            TestContext.WriteLine($"Got:\n{bytecode.Instructions.ToString()}")
            
            let vm = VM.fromByteCode bytecode
            let! newVm = VM.run vm
            
            let resultOption = VM.getLastPoppedStackElement newVm
            return! 
                match resultOption with
                | None -> Error "Stack top is empty" 
                | Some result -> VMHelpers.testExpectedObject vmTestCase.Expected result
        }
        |> function
           | Ok _ -> Assert.Pass("Passed\n")
           | Error errorMsg -> Assert.Fail(errorMsg)

    
    // Normal Functions
    [<TestCase("fn() { 1; }(1);")>]
    [<TestCase("fn(a) { a; }();")>]
    [<TestCase("fn(a, b) { a + b; }(1);")>]
    
    // Builtin Functions
    [<TestCase("len(1);")>]
    [<TestCase("""len("one", "two");""")>]
    [<TestCase("head(1);")>]    
    [<TestCase("last(1);")>]
    [<TestCase("push(1, 1);")>]
    member this.``Assert Errors with invalid number of arguments passed``(input: string) =
        result {
            let program = Parser.parseProgram input
            let nodes = programToNodes program
            
            let! newCompiler = Compiler.compileNodes nodes (Compiler.createNew ()) 
            let bytecode = Compiler.toByteCode newCompiler
            
            TestContext.WriteLine($"Got:\n{bytecode.Instructions.ToString()}")
            
            let vm = VM.fromByteCode bytecode
            return! (VM.run vm)
        }
        |> function
            | Ok _ -> Assert.Fail("Failed. Expected the program to error, but did not.\n") 
            | Error error -> Assert.Pass($"Passed. Program failed to evaluate with the message: \n\t\"{error}\"\n") 
        