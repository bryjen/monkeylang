namespace Monkey.Backend.Tests.Compiler

open System.Net
open NUnit.Framework
open FsToolkit.ErrorHandling

open Monkey.Frontend.Parser

open Monkey.Backend.Compiler
open Monkey.Backend.Code

open Monkey.Backend.Tests.Compiler.Helpers


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
    
    static member ``J: Test Index expression codegen`` = [|
        { Input = "[1, 2, 3][1 + 1];"
          ExpectedConstants = [| 1; 2; 3; 1; 1 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpConstant [| 2 |]
              make Opcode.OpArray [| 3 |]
              make Opcode.OpConstant [| 3 |]
              make Opcode.OpConstant [| 4 |]
              make Opcode.OpAdd [| |]
              make Opcode.OpIndex [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "{1: 2}[2 - 1]"
          ExpectedConstants = [| 1; 2; 2; 1 |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpHash [| 2 |]
              make Opcode.OpConstant [| 2 |]
              make Opcode.OpConstant [| 3 |]
              make Opcode.OpSub [| |]
              make Opcode.OpIndex [| |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
    
    static member ``K: Test Function codegen 1`` = [|
        { Input = "fn() { return 5 + 10; };"
          ExpectedConstants = [|
              5
              10
              [|
                  make Opcode.OpConstant [| 0 |]
                  make Opcode.OpConstant [| 1 |]
                  make Opcode.OpAdd [| |]
                  make Opcode.OpReturnValue [| |]
              |]
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 2; 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "fn() { 5 + 10; };"
          ExpectedConstants = [|
              5
              10
              [|
                  make Opcode.OpConstant [| 0 |]
                  make Opcode.OpConstant [| 1 |]
                  make Opcode.OpAdd [| |]
                  make Opcode.OpReturnValue [| |]
              |]
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 2; 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "fn() { 1; 2; };"
          ExpectedConstants = [|
              1
              2
              [|
                  make Opcode.OpConstant [| 0 |]
                  make Opcode.OpPop [| |]
                  make Opcode.OpConstant [| 1 |]
                  make Opcode.OpReturnValue [| |]
              |]
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 2; 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input = "fn() { if (true) { 10 } else { 20 }; };"
          ExpectedConstants = [|
              10
              20
              [|
                  make Opcode.OpTrue [| |]
                  make Opcode.OpJumpWhenFalse [| 10 |]
                  make Opcode.OpConstant [| 0 |]
                  make Opcode.OpJump [| 13 |]
                  make Opcode.OpConstant [| 1 |]
                  make Opcode.OpReturnValue [| |]
              |]
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 2; 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
    
    static member ``L: Test Function Call codegen 1`` = [|
        { Input = "fn() { 24; }();"
          ExpectedConstants = [|
              24
              [|
                  make Opcode.OpConstant [| 0 |]
                  make Opcode.OpReturnValue [| |]
              |]
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 1; 0 |]
              make Opcode.OpCall [| 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input =
            "let noArg = fn() { 24; };
            noArg();"
          ExpectedConstants = [|
              24
              [|
                  make Opcode.OpConstant [| 0 |]
                  make Opcode.OpReturnValue [| |]
              |]
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 1; 0 |]
              make Opcode.OpSetGlobal [| 0 |]
              make Opcode.OpGetGlobal [| 0 |]
              make Opcode.OpCall [| 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
    
    static member ``M: Test Local Statement scope codegen 1`` = [|
        // Case #1: "We assert that accessing a global binding from a function results in a 'OpGetGlobal' instruction."
        { Input =
            "let num = 55;
            fn() { num; };"
          ExpectedConstants = [|
              55
              [|
                  make Opcode.OpGetGlobal [| 0 |]
                  make Opcode.OpReturnValue [| |]
              |]
          |]
          ExpectedInstructions = [|
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpSetGlobal [| 0 |]
              make Opcode.OpClosure [| 1; 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        // Case #2: "We assert that creating and accessing a local binding results in a 'OpSetLocal' and 'OpGetLocal' insctructions."
        { Input =
            "fn() {
                let num = 55;
                num
            };"
          ExpectedConstants = [|
              55
              [|
                  make Opcode.OpConstant [| 0 |]
                  make Opcode.OpSetLocal [| 0 |]
                  make Opcode.OpGetLocal [| 0 |]
                  make Opcode.OpReturnValue [| |]
              |]
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 1; 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        // Case #3: "We assert that multiple local bindings in the same scope work."
        { Input =
            "fn() {
                let a = 55;
                let b = 77;
                a + b;
            };"
          ExpectedConstants = [|
              55
              77
              [|
                  make Opcode.OpConstant [| 0 |]
                  make Opcode.OpSetLocal [| 0 |]
                  make Opcode.OpConstant [| 1 |]
                  make Opcode.OpSetLocal [| 1 |]
                  make Opcode.OpGetLocal [| 0 |]
                  make Opcode.OpGetLocal [| 1 |]
                  make Opcode.OpAdd [| |]
                  make Opcode.OpReturnValue [| |]
              |]
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 2; 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
    
    static member ``M: Test Function with args codegen`` = [|
        { Input =
            "let oneArg = fn(a) { };
            oneArg(24);"
          ExpectedConstants = [|
              [|
                  make Opcode.OpReturn [| |]
              |]
              24
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 0; 0 |]
              make Opcode.OpSetGlobal [| 0 |]
              make Opcode.OpGetGlobal [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpCall [| 1 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input =
            "let manyArg = fn(a, b, c) { };
            manyArg(24, 25, 26);"
          ExpectedConstants = [|
              [|
                  make Opcode.OpReturn [| |]
              |]
              24
              25
              26
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 0; 0 |]
              make Opcode.OpSetGlobal [| 0 |]
              make Opcode.OpGetGlobal [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpConstant [| 2 |]
              make Opcode.OpConstant [| 3 |]
              make Opcode.OpCall [| 3 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input =
            "let oneArg = fn(a) { a; };
            oneArg(24);"
          ExpectedConstants = [|
              [|
                  make Opcode.OpGetLocal [| 0 |]
                  make Opcode.OpReturnValue [| |]
              |]
              24
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 0; 0 |]
              make Opcode.OpSetGlobal [| 0 |]
              make Opcode.OpGetGlobal [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpCall [| 1 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input =
            "let manyArg = fn(a, b, c) { a; b; c; };
            manyArg(24, 25, 26);"
          ExpectedConstants = [|
              [|
                  make Opcode.OpGetLocal [| 0 |]
                  make Opcode.OpPop [| |]
                  make Opcode.OpGetLocal [| 1 |]
                  make Opcode.OpPop [| |]
                  make Opcode.OpGetLocal [| 2 |]
                  make Opcode.OpReturnValue [| |]
              |]
              24
              25
              26
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 0; 0 |]
              make Opcode.OpSetGlobal [| 0 |]
              make Opcode.OpGetGlobal [| 0 |]
              make Opcode.OpConstant [| 1 |]
              make Opcode.OpConstant [| 2 |]
              make Opcode.OpConstant [| 3 |]
              make Opcode.OpCall [| 3 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
    
    // NOTE: Changing the order of items in 'Monkey.Frontend.Builtins.builtinsArray' changes the values of the opcodes
    // for 'OpGetBuiltin'
    static member ``N: Test Builtin Function codegen`` = [|
        { Input =
            "len([]);
            push([], 1);"
          ExpectedConstants = [| 1 |]
          ExpectedInstructions = [|
              make Opcode.OpGetBuiltin [| 0 |]
              make Opcode.OpArray [| 0 |]
              make Opcode.OpCall [| 1 |]
              make Opcode.OpPop [| |]
              make Opcode.OpGetBuiltin [| 5 |]
              make Opcode.OpArray [| 0 |]
              make Opcode.OpConstant [| 0 |]
              make Opcode.OpCall [| 2 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        { Input =
            "fn() { len([]); };"
          ExpectedConstants = [|
              [|
                  make Opcode.OpGetBuiltin [| 0 |]
                  make Opcode.OpArray [| 0 |]
                  make Opcode.OpCall [| 1 |]
                  make Opcode.OpReturnValue [| |]
              |]
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 0; 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
        
        // Asserts that we can override builtin functions 
        { Input =
            "let something = fn() {
                let len = 10;
                len;
            };
            something();"
            
          ExpectedConstants = [|
              10
              [|
                  make Opcode.OpConstant [| 0 |]
                  make Opcode.OpSetLocal [| |]
                  make Opcode.OpGetLocal [| |]
                  make Opcode.OpReturnValue [| |]
              |]
          |]
          ExpectedInstructions = [|
              make Opcode.OpClosure [| 1; 0 |]
              make Opcode.OpSetGlobal [| 0 |]
              make Opcode.OpGetGlobal [| 0 |]
              make Opcode.OpCall [| 0 |]
              make Opcode.OpPop [| |]
          |] |> Array.map Instructions }
    |]
        
    static member TestCasesToExecute1 = Array.concat [
        CompilerTests.``A: Test Integer Arithmetic Case``
        CompilerTests.``B: Test Boolean Expr codegen 1``
        CompilerTests.``C: Test Boolean Expr codegen 2``
        CompilerTests.``D: Test Prefix Expr codegen``
        CompilerTests.``E: Test If Expr codegen``
        CompilerTests.``F: Test Let Statement codegen``
        CompilerTests.``G: Test String literal codegen``
        CompilerTests.``H: Test Array literal codegen``
        CompilerTests.``I: Test Hash literal codegen``
        CompilerTests.``J: Test Index expression codegen``
    ]
    
    // Starting from 'K' and upwards, mainly more complex compilation patterns from functions 
    static member TestCasesToExecute2 = Array.concat [
        CompilerTests.``K: Test Function codegen 1``
        CompilerTests.``L: Test Function Call codegen 1``
        CompilerTests.``M: Test Function with args codegen``
        CompilerTests.``N: Test Builtin Function codegen``
    ]
     
     
    // [<TestCaseSource("TestCasesToExecute1")>]
    [<TestCaseSource("TestCasesToExecute2")>]
    member this.``Run Compiler Tests``(compilerTestCase: CompilerTestCase) =
        TestContext.WriteLine($"Input: \"{compilerTestCase.Input}\"")
        
        result {
            let program = Parser.parseProgram compilerTestCase.Input
            let nodes = programToNodes program
            
            let! newCompiler = Compiler.compileNodes nodes (Compiler.createNew ()) 
            let bytecode = Compiler.toByteCode newCompiler
            
            let expectedInstructions = CompilerHelpers.collapseInstructionsArray compilerTestCase.ExpectedInstructions
            TestContext.WriteLine($"\nExpected:\n{expectedInstructions.ToString()}\n")
            TestContext.WriteLine($"Got:\n{bytecode.Instructions.ToString()}\n")
            
            do! CompilerHelpers.testInstructions compilerTestCase.ExpectedInstructions bytecode.Instructions
                |> Result.mapError (fun errorMsg -> $"[Testing Instructions] {errorMsg}")
            
            do! CompilerHelpers.ConstantPool.testConstants compilerTestCase.ExpectedConstants bytecode.Constants
                |> Result.mapError (fun errorMsg -> $"[Testing Constants] {errorMsg}")
        }
        |> function
           | Ok _ -> Assert.Pass("Passed")
           | Error errorMsg -> Assert.Fail(errorMsg)
           
    [<Test>]
    member this.``Test Compiler Scope 1``() =
        result {
            let initCompiler = Compiler.createNew ()
            
            let compiler_scoped1 = Compiler.Scope.enterScope initCompiler
            let scopedBytes =
                [|
                      make Opcode.OpConstant [| 0 |]
                      make Opcode.OpConstant [| 1 |]
                      make Opcode.OpAdd [| |]
                      make Opcode.OpReturnValue [| |]
                |]
            let newCompiler_scoped1, _ = Compiler.Compilation.addInstruction compiler_scoped1 (Array.concat scopedBytes)
            let compiler_unscoped, returnedBytes = Compiler.Scope.leaveScope newCompiler_scoped1
            
            TestContext.WriteLine("\nScoped:")
            do! CompilerHelpers.testInstructions (Array.map Instructions scopedBytes) (Instructions returnedBytes)
            
            TestContext.WriteLine("\nUnscoped (Expected to be empty):")
            do! CompilerHelpers.testInstructions [| |] (compiler_unscoped |> Compiler.Scope.currentInstructions |> Instructions)
            return ()
        }
        |> function
           | Ok _ -> Assert.Pass("Passed")
           | Error errorMsg -> Assert.Fail($"Failed with: {errorMsg}")
