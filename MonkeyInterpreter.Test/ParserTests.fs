namespace MonkeyInterpreter.Test

open System
open MonkeyInterpreter.Helpers
open MonkeyInterpreter.Token
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
                
            do! match letStatement.Name.GetTokenLiteral() with
                | identName when identName = expectedName -> Ok () 
                | identName -> Error $"[test #{testCount}] statement.Name returned \"{identName}\", expected \"{expectedName}\""
        }
    
    [<Obsolete("See if you can find a way to integrate this same functionality using 'testEachStatement'
               (more generic & composable)")>]
    let testExpectedIdentifiers (program: Program) (testCases: string list) = 
        result {
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
            
    let testEachStatement (program: Program) (predicates: (Statement -> Result<unit, string>) list) =
        result {
            let statementAndPredicateTuples = List.zip program.Statements predicates
            
            let counts = List.init program.Statements.Length (fun i -> i + 1)
            
            return statementAndPredicateTuples
                |> List.map (fun tuple -> (snd tuple) (fst tuple))
                |> List.zip counts
                |> List.map (fun tuple -> Result.mapError (fun errorMsg -> $"[Test #{fst tuple}] {errorMsg}") (snd tuple))
                |> processResultsList
        }
        |> function
            | Ok _ -> Assert.Pass()
            | Error errorMsg -> Assert.Fail(errorMsg)


[<TestFixture>]
type ParserTests() =
    
    let testIntegerLiteral (expr: Expression) (expectedValue: int64) =
        result {
            let! integerLiteral =
                match expr with
                | IntegerLiteral integerLiteral -> Ok integerLiteral
                | expr -> Error $"exp not \"IntegerLiteral\", got \"{expr.GetType()}\""
                
            do! if integerLiteral.Value = expectedValue
                then Ok ()
                else Error $"integerLiteral.Value not \"{expectedValue}\", got \"{integerLiteral.Value}\""
                
            let asStringLiteral = $"{expectedValue}"
            do! if integerLiteral.GetTokenLiteral() = asStringLiteral 
                then Ok ()
                else Error $"integerLiteral.Token.Literal not \"{asStringLiteral}\", got \"{integerLiteral.GetTokenLiteral()}\""
        }
        
        
    [<Test>]
    member this.``Test 'let' statements 1``() =
        let testInput = """let x = 5;
let y = 10;
let foobar = 838383;
"""
        let expectedIdentifiers = [ "x"; "y"; "foobar" ]
        
        let assertions = [
            assertNumberOfStatements 3
            assertNumberOfErrors 0
        ]
        
        let program = Parser.parseProgram testInput
        List.iter (fun assertion -> assertion program) assertions 
        testExpectedIdentifiers program expectedIdentifiers
        
        
    [<Test>]
    member this.``Test 'return' statements 1``() =
        let testInput = """return 5;
return 10;
return 993322;
"""

        let predicate (statement: Statement) =
            match statement with
            | ReturnStatement returnStatement ->
                result {
                    let tokenTypeAsStr = TokenType.ToCaseString returnStatement.Token.Type
                    let! _ = if returnStatement.Token.Type = TokenType.RETURN
                             then Ok ()
                             else Error $"Expected 'ReturnStatement' token type to be 'RETURN', got '{tokenTypeAsStr}'"
                             
                    let! _ = if statement.GetTokenLiteral() = "return" 
                             then Ok ()
                             else Error $"Expected literal to be 'return', got '{statement.GetTokenLiteral()}'"
                             
                    return! Ok ()
                }
            | _ ->
                Error $"'statement' was not a 'ReturnStatement' type, got {statement.GetType()}"
                
        let assertions = [
            assertNumberOfStatements 3
            assertNumberOfErrors 0
        ]
        
        let predicates = [ predicate; predicate; predicate ]
        
        let program = Parser.parseProgram testInput
        List.iter (fun assertion -> assertion program) assertions 
        testEachStatement program predicates 
        
        
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

    
    [<Test>]
    member this.``Test identifier expressions 1``() =
        result {
            let testInput = "foobar;"
            let program = Parser.parseProgram testInput
            
            let! statement =
                match program.Statements with
                | head :: _ -> Ok head
                | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"

            let! expressionStatement =
                match statement with
                | ExpressionStatement expStat -> Ok expStat
                | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement}\""
                
            let! identifier =
                match expressionStatement.Expression with
                | Identifier ident -> Ok ident
                | expr -> Error $"exp not \"Identifier\", got \"{expr}\""
                
            let expectedIdentValue = "foobar"
            do! if identifier.Value = expectedIdentValue
                then Ok ()
                else Error $"ident.Value not \"{expectedIdentValue}\", got \"{identifier.Value}\""
                
            do! if identifier.GetTokenLiteral() = expectedIdentValue
                then Ok ()
                else Error $"ident.GetTokenLiteral() not \"{expectedIdentValue}\", got \"{identifier.Value}\""
        }
        |> function
            | Ok _ -> Assert.Pass()
            | Error errorMsg -> Assert.Fail(errorMsg)
            
            
    [<Test>]
    member this.``Test integer literal expressions 1``() =
        result {
            let testInput = "5;"
            let program = Parser.parseProgram testInput
            
            let! statement =
                match program.Statements with
                | head :: _ -> Ok head
                | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"

            let! expressionStatement =
                match statement with
                | ExpressionStatement expStat -> Ok expStat
                | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement}\""
                
            let! integerLiteral =
                match expressionStatement.Expression with
                | IntegerLiteral ident -> Ok ident
                | expr -> Error $"exp not \"IntegerLiteral\", got \"{expr}\""
                
            let expectedValue = 5 
            do! if integerLiteral.Value = expectedValue
                then Ok ()
                else Error $"integerLiteral.Value not \"{expectedValue}\", got \"{integerLiteral.Value}\""
                
            let tokenLiteral = integerLiteral.Token.Literal
            do! if tokenLiteral = $"{expectedValue}" 
                then Ok ()
                else Error $"ident.TokenLiteral() not \"{expectedValue}\", got \"{tokenLiteral}\""
        }
        |> function
            | Ok _ -> Assert.Pass()
            | Error errorMsg -> Assert.Fail(errorMsg)
            
            
    [<Test>]
    member this.``Test parsing prefix expressions 1``() =
        let prefixTests = [
            ("!5", "!", 5)
            ("-15", "-", 15)
        ]
        
        for test in prefixTests do
            result {
                let testInput, expectedOperator, expectedValue = test
                let program = Parser.parseProgram testInput
                
                let! statement =
                    match program.Statements with
                    | head :: _ -> Ok head
                    | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"
                    
                let! expressionStatement =
                    match statement with
                    | ExpressionStatement expStat -> Ok expStat
                    | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement}\""
                    
                let! prefixExpression =
                    match expressionStatement.Expression with
                    | PrefixExpression prefixExpr -> Ok prefixExpr 
                    | expr -> Error $"exp not \"PrefixExpression\", got \"{expr}\""
                    
                do! if prefixExpression.Operator = expectedOperator
                    then Ok ()
                    else Error $"prefixExpression.Operator not \"{expectedOperator}\", got \"{prefixExpression.Operator}\""
                    
                do! testIntegerLiteral prefixExpression.Right expectedValue
            }
            |> function
                | Ok _ -> () 
                | Error errorMsg -> Assert.Fail(errorMsg)
            
        Assert.Pass()
            
            
    [<Test>]
    member this.``Test parsing infix expressions 1``() =
        let infixTests = [
            // (statement, left expr, operator, right expr)
            ("5 + 5;", 5, "+", 5)
            ("5 - 5;", 5, "-", 5)
            ("5 * 5;", 5, "*", 5)
            ("5 / 5;", 5, "/", 5)
            ("5 > 5;", 5, ">", 5)
            ("5 < 5;", 5, "<", 5)
            ("5 == 5;", 5, "==", 5)
            ("5 != 5;", 5, "!=", 5)
        ]
        
        for test in infixTests do
            let testInput, expectedLeftValue, expectedOperator, expectedRightValue = test 
            let program = Parser.parseProgram testInput
            
            result {
                let! statement =
                    match program.Statements with
                    | head :: _ -> Ok head
                    | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"
                    
                let! expressionStatement =
                    match statement with
                    | ExpressionStatement expStat -> Ok expStat
                    | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement}\""
                    
                let! infixExpression =
                    match expressionStatement.Expression with
                    | InfixExpression infixExpr -> Ok infixExpr 
                    | expr -> Error $"exp not \"InfixExpression\", got \"{expr.GetType()}\""
                    
                do! testIntegerLiteral infixExpression.Left expectedLeftValue 
                do! testIntegerLiteral infixExpression.Right expectedRightValue
                
                do! if infixExpression.Operator = expectedOperator
                    then Ok ()
                    else Error $"prefixExpression.Operator not \"{expectedOperator}\", got \"{infixExpression.Operator}\""
            }
            |> function
                | Ok _ -> () 
                | Error errorMsg -> Assert.Fail($"[For test \"{testInput}\"] {errorMsg}")


    [<Test>]
    member this.``Test operator precedence parsing``() =
        let testCases = [
            // input into parser, expected parser output
            ("-a * b", "((-a) * b)")
            ("!-a", "(!(-a))")
            ("a + b + c", "((a + b) + c)")
            ("a + b - c", "((a + b) - c)")
            ("a * b * c", "((a * b) * c)")
            ("a * b / c", "((a * b) / c)")
            ("a + b / c", "(a + (b / c))")
            ("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)")
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)")
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))")
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))")
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")
        ]
        
        for testCase in testCases do
            let inputString, expectedRepresentationString = testCase
            let program = Parser.parseProgram inputString
            
            let programAsStr = program.ToString()
            if programAsStr <> expectedRepresentationString then
                Assert.Fail($"Expected \"{expectedRepresentationString}\", but got \"{programAsStr}\"")