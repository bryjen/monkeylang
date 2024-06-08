namespace MonkeyInterpreter.Test

open MonkeyInterpreter.Helpers
open NUnit.Framework

open FsToolkit.ErrorHandling

open MonkeyInterpreter


[<AutoOpen>]
module private ParserHelpers =
    let printTestLetStatementResults (evaluatedTestCases: (Statement * (int * string) * Result<unit, string>) list) =
        for evaluatedTestCase in evaluatedTestCases do
            let statement, testCase, result = evaluatedTestCase
            let testCount, expectedName = testCase
            
            let evaluationMessage =
                match result with
                | Ok _ -> "OK" 
                | Error errorMessage -> errorMessage 
            printfn $"[Test #{testCount}] Raw statement: {statement}; Expected name: {expectedName}; Status: {evaluationMessage}"
            
    let assertNumberOfStatements numberOfExpectedStatements (program: Program) =
        match program.Statements.Length with
        | len when len = numberOfExpectedStatements -> ()
        | len -> Assert.Fail($"Error with 'program.Statements', expected {numberOfExpectedStatements} statements, got {len}")
        
    let assertNumberOfErrors numberOfExpectedErrors (program: Program) =
        match program.Errors.Length with
        | len when len = numberOfExpectedErrors -> ()
        | len -> Assert.Fail($"Error with 'program.Errors', expected {numberOfExpectedErrors} errors, got {len}")
        
        
    let testLetStatement (statement: Statement) (testCase: int * string) =
        result {
            let testCount = fst testCase 
            let expectedName = snd testCase 
            
            do! match statement.GetTokenLiteral() with
                | literal when literal = "let" -> Ok () 
                | literal -> Error $"[test #{testCount}] statement.TokenLiteral() returned \"{literal}\", expected \"let\""
                
            let! letStatement =
                match statement with
                | LetStatement letStatement -> Ok letStatement 
                | _ -> Error $"[test #{testCount}] Expected type \"LetStatement\", got \"{statement.GetType().FullName}\""
                
            do! match letStatement.Name.Value with
                | identName when identName = expectedName -> Ok () 
                | identName -> Error $"[test #{testCount}] statement.Name.Value returned \"{identName}\", expected \"{expectedName}\""
                
            do! match letStatement.Name.TokenLiteral() with
                | identName when identName = expectedName -> Ok () 
                | identName -> Error $"[test #{testCount}] statement.Name returned \"{identName}\", expected \"{expectedName}\""
        }
    
    let testExpectedIdentifiers (assertions: (Program -> unit) list) (testCases: string list) testInput = 
        result {
            let program = Parser.parseProgram testInput
            
            List.iter (fun assertion -> assertion program) assertions 
            
            let statementAndTestCasePairs =
                testCases
                |> addCountsToList
                |> List.zip program.Statements
                
            let statementTestCaseResultPairs =
                statementAndTestCasePairs
                |> List.map (fun statAndCasePair -> testLetStatement (fst statAndCasePair) (snd statAndCasePair))
                |> List.zip statementAndTestCasePairs
                |> List.map (fun tuple -> (fst (fst tuple), snd (fst tuple), snd tuple))
                
            printTestLetStatementResults statementTestCaseResultPairs
            
            return statementTestCaseResultPairs
                   |> List.map (fun (_, _, thd) -> thd)
                   |> processResultsList
        }
        |> function
            | Ok _ -> Assert.Pass()
            | Error errorMsg -> Assert.Fail(errorMsg)


[<TestFixture>]
type ParserTests() =
    [<Test>]
    member this.``Test let statements 1``() =
        let testInput = """let x = 5;
let y = 10;
let foobar = 838383;
"""
        let expectedIdentifiers = [ "x"; "y"; "foobar" ]
        
        let assertions = [
            assertNumberOfStatements 3
            assertNumberOfErrors 0
        ]
        
        testExpectedIdentifiers assertions expectedIdentifiers testInput
        
    [<Test>]
    member this.``Test errors 1``() =
        let testInput = """let x 5;
let = 10;
let 838383;
"""

        let assertions = [
            assertNumberOfErrors 3
        ]
        
        let program = Parser.parseProgram testInput
        List.iter (fun assertion -> assertion program) assertions 
