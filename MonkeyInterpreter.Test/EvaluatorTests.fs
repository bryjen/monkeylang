namespace MonkeyInterpreter.Test

open MonkeyInterpreter.Evaluator
open MonkeyInterpreter.Object
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
            
    let assertIsExpressionStatement statement =
        match statement with
        | ExpressionStatement expressionStatement -> Ok expressionStatement
        | _ -> Error $"Expected an \"ExpressionStatement\", got \"{statement.GetType()}\""



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
                let! evalResult = Evaluator.evalStatement Environment.Empty statement
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                return! 
                    match evalResult with
                    | Integer int64, _ when int64 = expectedValue -> Ok int64
                    | Integer int64, _ -> Error $"[Evaluation Error] Expected {expectedValue}, got {int64}" 
                    | _ -> Error $"[Evaluation Error] Expected \"Integer\", got {evalResult.GetType()}" 
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
                let! evalResult = Evaluator.evalStatement Environment.Empty statement
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                return! 
                    match evalResult with
                    | Boolean boolean, _ when boolean = expectedValue -> Ok boolean 
                    | Boolean boolean, _ -> Error $"[Evaluation Error] Expected {expectedValue}, got {boolean}"  
                    | _ -> Error $"[Evaluation Error] Expected \"Boolean\", got {evalResult.GetType()}" 
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
                let! evalResult = Evaluator.evalStatement Environment.Empty statement
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                return! 
                    match evalResult with
                    | String str, _ when str = expectedValue -> Ok str 
                    | String str, _ -> Error $"[Evaluation Error] Expected {expectedValue}, got {str}"  
                    | _ -> Error $"[Evaluation Error] Expected \"String\", got {evalResult.GetType()}" 
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
        ]
       
        let mutable currentTestCase = 0
        for testCase in testCases do
            currentTestCase <- currentTestCase + 1
            let testInput, expectedValue = testCase
            let program = Parser.parseProgram testInput
            
            result {
                do! assertNoErrors program
                
                let! evalResult = Evaluator.evalStatementsList Environment.Empty program.Statements 
                                  |> Result.mapError (fun errorMsg -> "[Evaluation Error] " + errorMsg)
                return! 
                    match evalResult with
                    | Integer int64, _ when int64 = expectedValue -> Ok int64
                    | Integer int64, _ -> Error $"[Evaluation Error] Expected {expectedValue}, got {int64}" 
                    | _ -> Error $"[Evaluation Error] Expected \"Integer\", got {evalResult.GetType()}" 
            }
            |> function
               | Ok actualValue ->
                   TestContext.WriteLine($"[Test #{currentTestCase}] \"{testInput}\", ex {expectedValue}, got {actualValue}")
               | Error errorMsg ->
                   Assert.Fail(errorMsg)
