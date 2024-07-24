namespace MonkeyInterpreter.Test

open System
open MonkeyInterpreter.Eval.Evaluator
open MonkeyInterpreter.Eval.Object
open NUnit.Framework
open FsToolkit.ErrorHandling
open MonkeyInterpreter

[<AutoOpen>]
module private TestHelpers =
    let assertNoErrors program =
        match program.Errors.Length with
        | len when len > 0 ->
            let errorsAsString = program.Errors
                                 |> List.map (fun str -> "- " + str)
                                 |> String.concat "\n"
            Error $"Program has {len} errors:\n{errorsAsString}"
        | _ ->
            Ok ()
            
    let assertNumberOfStatements expectedNumStatements (program: Program) =
        match program.Statements.Length with
        | len when len <> expectedNumStatements ->
            Error $"Program was expected to have {expectedNumStatements} statements, got {len}"
        | _ ->
            Ok ()
            
    let assertIsNull (object: Object) =
        match object with
        | NullType -> Ok ()
        | _ -> Error $"[assertIsNull] Expected a Null type, got \"{object.Type()}\""
            
    let assertIsExpressionStatement statement =
        match statement with
        | ExpressionStatement expressionStatement -> Ok expressionStatement
        | _ -> Error $"Expected an \"ExpressionStatement\", got \"{statement.GetType()}\""
        
    let assertEqualIntegers (expectedValue: Int64) (object: Object) =
        match object with
        | IntegerType int64 when int64 = expectedValue -> Ok () 
        | IntegerType int64 -> Error $"[assertEqualIntegers] Expected {expectedValue}, got {int64}" 
        | _ -> Error $"[assertEqualIntegers] Expected an Integer type, got \"{object.Type()}\"" 
        
    let assertEqualStrings (expectedValue: string) (object: Object) =
        match object with
        | StringType string when string = expectedValue -> Ok () 
        | StringType string -> Error $"[assertEqualStrings] Expected {expectedValue}, got {string}" 
        | _ -> Error $"[assertEqualStrings] Expected a String type, got \"{object.Type()}\"" 
        
    let assertEqualBooleans (expectedValue: bool) (object: Object) =
        match object with
        | BooleanType boolean when boolean = expectedValue -> Ok () 
        | BooleanType boolean -> Error $"[assertEqualBooleans] Expected {expectedValue}, got {boolean}" 
        | _ -> Error $"[assertEqualBooleans] Expected a Boolean type, got \"{object.Type()}\""
        
    let assertEqualObj (expectedValue: obj) (object: Object) =
        match expectedValue with
        | null -> assertIsNull object
        | :? int as value -> assertEqualIntegers (int64 value) object
        | :? int64 as value -> assertEqualIntegers value object
        | :? string as value -> assertEqualStrings value object
        | :? bool as value -> assertEqualBooleans value object
        | _ -> Error $"Fatal error - testing: The type \"{expectedValue.GetType()}\" is not registered. See \"TestHelpers.assertEqualObj\"."

    let assertEqualObjsList (expectedValues: obj list) (object: Object) =
        match object with
        | ArrayType array when array.Length = expectedValues.Length ->
            let comparisonResults = List.zip expectedValues array |> List.map (fun (e, a) -> assertEqualObj e a) 
            match (List.tryFind Result.isError comparisonResults) with
            | Some errorResult -> errorResult 
            | None -> Ok () 
        | ArrayType array ->
            Error $"[assertEqualObjsList] Expected an \"ArrayType\" with length {expectedValues.Length}, got {array.Length}."
        | _ ->
            Error $"[assertEqualObjsList] 'object' was expected to be an \"ArrayType\", got \"{object.Type()}\"."


[<TestFixture>]
type EvaluatorTests() =
    
    [<Test>]
    member this.``Test integer evaluation 1``() =
        let testCases = [
            // test input, expected value
            ("5", 5)
            ("10", 10)
            ("-5", -5)
            ("-10", -10)
            ("5 + 5 + 5 + 5 - 10", 10)
            ("2 * 2 * 2 * 2 * 2", 32)
            ("-50 + 100 + -50", 0)
            ("5 * 2 + 10", 20)
            ("5 + 2 * 10", 25)
            ("20 + 2 * -10", 0)
            ("50 / 2 * 2 + 10", 60)
            ("2 * (5 + 10)", 30)
            ("3 * 3 * 3 + 10", 37)
            ("3 * (3 * 3) + 10", 37)
            ("(5 + 10 * 2 + 15 / 3) * 2 + -10", 50)
        ]
       
        let mutable currentTestCase = 0
        for testCase in testCases do
            currentTestCase <- currentTestCase + 1
            let testInput, expectedValue = testCase
            let program = Parser.parseProgram testInput
            
            result {
                do! assertNoErrors program
                do! assertNumberOfStatements 1 program
                
                let statement = program.Statements.Head
                let! _ = assertIsExpressionStatement program.Statements.Head
                let! _, evalResult = Evaluator.evalStatement Environment.Empty statement
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                return! assertEqualObj expectedValue evalResult
            }
            |> function
               | Ok actualValue ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", ex {expectedValue}, got {actualValue}")
               | Error errorMsg ->
                   Assert.Fail(errorMsg)
                   
    [<Test>]
    member this.``Test boolean evaluation 1``() =
        let testCases = [
            ("true", true)
            ("false", false)
            
            ("true == true", true)
            ("false == false", true)
            ("true == false", false)
            ("false == true", false)
            ("true != true", false)
            ("false != false", false)
            ("true != false", true)
            ("false != true", true)
            
            ("!true", false)
            ("!false", true)
            ("!!true", true)
            ("!!false", false)
            
            (* Test cases taken from the book, see if you should include their implementation or not, makes no sense
            ("!5", false)
            ("!!5", true)
            *)
            
            ("1 < 2", true)
            ("1 > 2", false)
            ("1 < 1", false)
            ("1 > 1", false)
            ("1 == 1", true)
            ("1 != 1", false)
            ("1 == 2", false)
            ("1 != 2", true)
        ]
       
        let mutable currentTestCase = 0
        for testCase in testCases do
            currentTestCase <- currentTestCase + 1
            let testInput, expectedValue = testCase
            let program = Parser.parseProgram testInput
            
            result {
                do! assertNoErrors program
                do! assertNumberOfStatements 1 program
                
                let statement = program.Statements.Head
                let! _ = assertIsExpressionStatement program.Statements.Head
                let! _, evalResult = Evaluator.evalStatement Environment.Empty statement
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                return! assertEqualObj expectedValue evalResult
            }
            |> function
               | Ok actualValue ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", ex {expectedValue}, got {actualValue}")
               | Error errorMsg ->
                   Assert.Fail(errorMsg)
                   
                   
    [<Test>]
    member this.``Test string evaluation 1``() =
        let testCases = [
            ("\"foo\" + \"bar\"", "foobar")
            ("\"Hello\" + \" world\" + \"!\"", "Hello world!")
            ("\"a\" + (\"b\" + \"c\" + \"d\") + \"e\"", "abcde")
        ]
       
        let mutable currentTestCase = 0
        for testCase in testCases do
            currentTestCase <- currentTestCase + 1
            let testInput, expectedValue = testCase
            let program = Parser.parseProgram testInput
            
            result {
                do! assertNoErrors program
                do! assertNumberOfStatements 1 program
                
                let statement = program.Statements.Head
                let! _ = assertIsExpressionStatement program.Statements.Head
                let! _, evalResult = Evaluator.evalStatement Environment.Empty statement
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                return! assertEqualObj expectedValue evalResult
            }
            |> function
               | Ok actualValue ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", ex {expectedValue}, got {actualValue}")
               | Error errorMsg ->
                   Assert.Fail(errorMsg)
                   
                   
    [<Test>]
    member this.``Test integer + identifier evaluation 1``() =
        let testCases = [
            ("
             let x = 10;
             let y = 20;
             x + y",
             30)
            
            ("
             let a = 12;
             let b = 7;
             let c = -a;
             let d = a + b - c;
             (a + b * c + d * 2) * 3 + -d",
             -61)
            
            ("let a = 5; a;", 5)
            
            ("let a = 5 * 5; a;", 25)
            
            ("let a = 5; let b = a; b;", 5)
            
            ("let a = 5; let b = a; let c = a + b + 5; c;", 15)
        ]
       
        let mutable currentTestCase = 0
        for testCase in testCases do
            currentTestCase <- currentTestCase + 1
            let testInput, expectedValue = testCase
            let program = Parser.parseProgram testInput
            
            result {
                do! assertNoErrors program
                let! _, evalResult = Evaluator.evalStatementsList Environment.Empty program.Statements 
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                return! assertEqualObj expectedValue evalResult
            }
            |> function
               | Ok actualValue ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", ex {expectedValue}, got {actualValue}")
               | Error errorMsg ->
                   Assert.Fail(errorMsg)
                   
                   
    [<Test>]
    member this.``Test if expression evaluation 1``() =
        let testCases: (string * obj) list = [
            ("
             let x = 2;
             let y = 1;
             let foo = if (x > y) { \"foo\"; } else { \"bar\"; };
             foo;",
             "foo")
            
            ("
             let statusCode = if (10 < 1) { 0; } else { 1; };
             statusCode;",
             1)
            
            ("if (true) { 10 }",10)
            
            ("if (1 < 2) { 10 }", 10)
            
            ("if (1 > 2) { 10 } else { 20 }", 20)
            
            ("if (1 < 2) { 10 } else { 20 }", 10)
            
            ("if (false) { 10 }", null)
            
            ("if (1 > 2) { 10 }", null)
        ]
       
        let mutable currentTestCase = 0
        for testCase in testCases do
            currentTestCase <- currentTestCase + 1
            let testInput, expectedValue = testCase
            let program = Parser.parseProgram testInput
            
            result {
                do! assertNoErrors program
                let! _, evalResult = Evaluator.evalStatementsList Environment.Empty program.Statements 
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                return! assertEqualObj expectedValue evalResult
            }
            |> function
               | Ok actualValue ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", ex {expectedValue}, got {actualValue}")
               | Error errorMsg ->
                   Assert.Fail(errorMsg)
                   
                   
    [<Test>]
    member this.``Test return statement evaluation 1``() =
        let testCases: (string * obj) list = [
            ("return 10;", 10)
            ("return 10; 9;", 10)
            ("return 2 * 5; 9;", 10)
            ("9; return 2 * 5; 9;", 10)
        ]
       
        let mutable currentTestCase = 0
        for testCase in testCases do
            currentTestCase <- currentTestCase + 1
            let testInput, expectedValue = testCase
            let program = Parser.parseProgram testInput
            
            result {
                do! assertNoErrors program
                let! _, evalResult = Evaluator.evalStatementsList Environment.Empty program.Statements 
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                return! assertEqualObj expectedValue evalResult
            }
            |> function
               | Ok actualValue ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", ex {expectedValue}, got {actualValue}")
               | Error errorMsg ->
                   Assert.Fail(errorMsg)
                   
                   
    [<Test>]
    member this.``Test function call evaluation 1``() =
        let testCases: (string * obj) list = [
            ("let identity = fn(x) { x; }; identity(5);", 5)
            ("let identity = fn(x) { return x; }; identity(5);", 5)
            ("let double = fn(x) { x * 2; }; double(5);", 10)
            ("let add = fn(x, y) { x + y; }; add(5, 5);", 10)
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(5, 5));", 20)
            
            ("let add = fn(x, y) { x + y; }; add(5 + 5, add(add(add(5, add(5, add(10, 10))), 5), 5));", 50)
            
            ("fn(x) { x; }(5)", 5)
            ("
             let message = \"Some message\";
             let addPrefix = fn(str) { \"[Prefix] \" + str; };
             addPrefix(message);",
             "[Prefix] Some message")
        ]
       
        let mutable currentTestCase = 0
        for testCase in testCases do
            currentTestCase <- currentTestCase + 1
            let testInput, expectedValue = testCase
            let program = Parser.parseProgram testInput
            
            result {
                do! assertNoErrors program
                let! _, evalResult = Evaluator.evalStatementsList Environment.Empty program.Statements 
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                do! assertEqualObj expectedValue evalResult
                return evalResult
            }
            |> function
               | Ok actualValue ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", ex {expectedValue}, got {actualValue}")
               | Error errorMsg ->
                   Assert.Fail($"[Test #{currentTestCase}] {errorMsg}")
                   
                   
    [<Test>]
    member this.``Test builtin function call evaluation 1``() =
        let testCases: (string * obj) list = [
            ("len(\"\")", 0)
            ("len(\"four\")", 4)
            ("len(\"hello world\")", 11)
        ]
       
        let mutable currentTestCase = 0
        for testCase in testCases do
            currentTestCase <- currentTestCase + 1
            let testInput, expectedValue = testCase
            let program = Parser.parseProgram testInput
            
            result {
                do! assertNoErrors program
                let! _, evalResult = Evaluator.evalStatementsList Environment.Empty program.Statements 
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                do! assertEqualObj expectedValue evalResult
                return evalResult
            }
            |> function
               | Ok actualValue ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", ex {expectedValue}, got {actualValue}")
               | Error errorMsg ->
                   Assert.Fail($"[Test #{currentTestCase}] {errorMsg}")
                   
                   
    [<Test>]
    member this.``Test array literal evaluation 1``() =
        let testCases: (string * obj list) list = [
            ("[1, 2, 3 + 4]", [1; 2; 7])
            ("[1, \"foo\", \"Hello\" + \" \" + \"World\"]", [1; "foo";"Hello World"])
        ]
       
        let mutable currentTestCase = 0
        for testCase in testCases do
            currentTestCase <- currentTestCase + 1
            let testInput, expectedValues = testCase
            let program = Parser.parseProgram testInput
            
            result {
                do! assertNoErrors program
                let! _, evalResult = Evaluator.evalStatementsList Environment.Empty program.Statements 
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                do! assertEqualObjsList expectedValues evalResult
                return evalResult
            }
            |> function
               | Ok actualValue ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", ex {expectedValues}, got {actualValue}")
               | Error errorMsg ->
                   Assert.Fail($"[Test #{currentTestCase}] {errorMsg}")
                   
                   
    [<Test>]
    member this.``Test index expression evaluation 1``() =
        let testCases: (string * obj) list = [
            ("[1, 2, 3 + 4][0] + 1", 2)
            
            ("let someStrings = [ \"hello\", \"world\", \"foo\", \"bar\" ];
             someStrings[-1 * 2 + 4]",
             "foo")
            
            ("let someStrings = [ \"hello\", \"world\", \"foo\", \"bar\" ];
             let generator = fn(x) { return \"index\" + x; };
             let arr = [ generator(0), generator(1), generator(2) ];
             arr[0] + arr[1] + arr[2];",
             "index0index1index2")
        ]
       
        let mutable currentTestCase = 0
        for testCase in testCases do
            currentTestCase <- currentTestCase + 1
            let testInput, expectedValue = testCase
            let program = Parser.parseProgram testInput
            
            result {
                do! assertNoErrors program
                let! _, evalResult = Evaluator.evalStatementsList Environment.Empty program.Statements 
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                do! assertEqualObj expectedValue evalResult 
                return evalResult
            }
            |> function
               | Ok actualValue ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", ex {expectedValue}, got {actualValue}")
               | Error errorMsg ->
                   Assert.Fail($"[Test #{currentTestCase}] {errorMsg}")
                   
                   
                   
[<TestFixture>]
type EvaluatorErrorHandlingTests() =
    
    [<Test>]
    member this.``Test return statement evaluation 1``() =
        let testCases = [
            "5 + true;"
            "5 + true; 5;"
            "-true"
            "true + false;"
            "5; true + false; 5"
            "if (10 > 1) { true + false; }"
            "if (10 > 1) { if (10 > 1) { return true + false; } return 1; }"
        ]
       
        let mutable currentTestCase = 0
        for testInput in testCases do
            currentTestCase <- currentTestCase + 1
            let program = Parser.parseProgram testInput
            
            result {
                let! _, _ = Evaluator.evalStatementsList Environment.Empty program.Statements 
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                return! assertEqualObj NullType NullType  // returns true/Ok
            }
            |> function
               | Error errorMsg ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", Expected error, got error with message:\n\t\"{errorMsg}\"\n")
               | Ok _ ->
                   Assert.Fail($"[Test #{currentTestCase}] Did not throw an error.")

    
    [<Test>]
    member this.``Test builtin function call evaluation 1``() =
        let testCases = [
            "len(1)"
            "len(\"one\", \"two\")"
        ]
       
        let mutable currentTestCase = 0
        for testInput in testCases do
            currentTestCase <- currentTestCase + 1
            let program = Parser.parseProgram testInput
            
            result {
                let! _, _ = Evaluator.evalStatementsList Environment.Empty program.Statements 
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                return! assertEqualObj NullType NullType  // returns true/Ok
            }
            |> function
               | Error errorMsg ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", Expected error, got error with message:\n\t\"{errorMsg}\"\n")
               | Ok _ ->
                   Assert.Fail($"[Test #{currentTestCase}] Did not throw an error.")
