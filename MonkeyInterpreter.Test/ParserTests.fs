namespace MonkeyInterpreter.Test

open MonkeyInterpreter.Helpers
open NUnit.Framework

open FsToolkit.ErrorHandling

open MonkeyInterpreter


[<AutoOpen>]
module private ParserHelpers =
    
    let assertNumberOfStatements expectedStatements program =
        match program.Statements.Length with
        | len when len = expectedStatements -> Ok ()
        | len -> Error $"Error with 'program.Statements', expected {expectedStatements} statements, got {len}"

[<TestFixture>]
type ParserTests() =
    
    let testLetStatement (statement: Statement) (testCase: int * string) =
        result {
            let testCount = fst testCase 
            let name = snd testCase 
            
            do! match statement.TokenLiteral() with
                | literal when literal = "let" -> Ok () 
                | literal -> Error $"[test #{testCount}] statement.TokenLiteral() returned \"{literal}\", expected \"let\""
                
            let! letStatement =
                match statement with
                | LetStatement letStatement -> Ok letStatement 
                | _ -> Error $"[test #{testCount}] Expected type \"LetStatement\", got \"{statement.GetType().FullName}\""
                
            do! match letStatement.Name.Value with
                | identName when identName = name -> Ok () 
                | identName -> Error $"[test #{testCount}] statement.Name.Value returned \"{identName}\", expected \"{name}\""
                
            do! match letStatement.Name.TokenLiteral() with
                | identName when identName = name -> Ok () 
                | identName -> Error $"[test #{testCount}] statement.Name returned \"{identName}\", expected \"{name}\""
        }
    
    let testExpectedIdentifiers testInput (testCases: string list) = 
        let lexer = Lexer(testInput)
        let parser = Parser(lexer)
        
        let programResult = parser.ParseProgram()
        
        result {
            let! program = programResult 
            do! assertNumberOfStatements 3 program
            
            return testCases
                   |> addCountsToList
                   |> List.zip program.Statements
                   |> List.map (fun pair -> testLetStatement (fst pair) (snd pair))
                   |> processResultsList
        }
        

    [<Test>]
    member this.``Test let statements 1``() =
        let testInput = """let x = 5;
let y = 10;
let foobar = 838383
"""

        let expectedIdentifiers = [
            "x"
            "y"
            "foobar"
        ] 

        testExpectedIdentifiers testInput expectedIdentifiers
        |> function
            | Ok _ -> Assert.Pass()
            | Error errorMsg -> Assert.Fail(errorMsg)