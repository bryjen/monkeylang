namespace MonkeyInterpreter.Test

open System
open MonkeyInterpreter.Helpers
open MonkeyInterpreter.Helpers.Queue
open NUnit.Framework

open FsToolkit.ErrorHandling

open MonkeyInterpreter


[<AutoOpen>]
module private ParserHelpers =
    
    [<Obsolete("Deprecated")>]
    let printTestLetStatementResults (evaluatedTestCases: (Statement * (int * string) * Result<unit, string>) list) =
        for evaluatedTestCase in evaluatedTestCases do
            let statement, testCase, result = evaluatedTestCase
            let testCount, expectedName = testCase
            
            let evaluationMessage =
                match result with
                | Ok _ -> "OK" 
                | Error errorMessage -> errorMessage 
            printfn $"[Test #{testCount}] Raw statement: {statement}; Expected name: {expectedName}; Status: {evaluationMessage}"
            
    [<Obsolete("Deprecated")>]
    let assertNumberOfStatements numberOfExpectedStatements (program: Program) =
        match program.Statements.Length with
        | len when len = numberOfExpectedStatements -> ()
        | len -> Assert.Fail($"Error with 'program.Statements', expected {numberOfExpectedStatements} statements, got {len}")
        
    [<Obsolete("Deprecated")>]
    let assertNumberOfErrors numberOfExpectedErrors (program: Program) =
        match program.Errors.Length with
        | len when len = numberOfExpectedErrors -> ()
        | len -> Assert.Fail($"Error with 'program.Errors', expected {numberOfExpectedErrors} errors, got {len}")
        
        
    [<Obsolete("Deprecated")>]
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
    
    [<Obsolete("Deprecated")>]
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
            
    [<Obsolete("Deprecated")>]
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
            

    let testIntegerLiteral (expr: Expression) (expectedValue: int64) =
        result {
            let! integerLiteral =
                match expr with
                | IntegerLiteral integerLiteral -> Ok integerLiteral
                | _ -> Error $"exp not \"IntegerLiteral\", got \"{expr.GetType()}\""
                
            do! if integerLiteral.Value = expectedValue
                then Ok ()
                else Error $"integerLiteral.Value not \"{expectedValue}\", got \"{integerLiteral.Value}\""
                
            let asStringLiteral = $"{expectedValue}"
            do! if integerLiteral.GetTokenLiteral() = asStringLiteral 
                then Ok ()
                else Error $"integerLiteral.Token.Literal not \"{asStringLiteral}\", got \"{integerLiteral.GetTokenLiteral()}\""
        }
        
    let testIdentifier (expr: Expression) (expectedValue: string) =
        result {
            let! identifier =
                match expr with
                | Expression.Identifier identifier -> Ok identifier
                | _ -> Error $"expr not \"Identifier\", got \"{expr.GetType()}\""
                
            do! if identifier.Value = expectedValue
                then Ok ()
                else Error $"identifier.Value not \"{expectedValue}\", got \"{identifier.Value}\""
                
            do! if identifier.GetTokenLiteral() = expectedValue 
                then Ok ()
                else Error $"identifier.GetTokenLiteral() not \"{expectedValue}\", got \"{identifier.GetTokenLiteral()}\""
        }
        
    let testBooleanLiteral (expr: Expression) (expectedValue: bool) =
        result {
            let! booleanLiteral =
                match expr with
                | BooleanLiteral boolLiteral -> Ok boolLiteral
                | _ -> Error $"expr not \"BooleanExpression\", got \"{expr.GetType()}\""
                
            do! if booleanLiteral.Value = expectedValue
                then Ok ()
                else Error $"booleanLiteral.Value not \"{expectedValue}\", got \"{booleanLiteral.Value}\""
                
            do! if booleanLiteral.GetTokenLiteral().ToLower() = $"{expectedValue}".ToLower() 
                then Ok ()
                else Error $"identifier.GetTokenLiteral() not \"{expectedValue}\", got \"{booleanLiteral.GetTokenLiteral()}\""
        }
        
    let testLiteralExpression (expr: Expression) (expectedType: obj) =
        match expectedType with
        | :? int as value -> testIntegerLiteral expr (int64 value) 
        | :? int64 as value -> testIntegerLiteral expr value
        | :? string as value -> testIdentifier expr value
        | :? bool as value -> testBooleanLiteral expr value
        | _ -> Error $"Type of expr not handled, got type \"{expectedType.GetType()}\""
        
    let testInfixExpression (expr: Expression) (expectedLeft: obj) (expectedOperator: string) (expectedRight: obj) =
        result {
            let! infixExpression =
                match expr with
                | InfixExpression infixExpr -> Ok infixExpr
                | _ -> Error $"expr not \"InfixExpression\", got \"{expr.GetType()}\""
                
            do! testLiteralExpression infixExpression.Left expectedLeft
            
            do! if infixExpression.Operator = expectedOperator 
                then Ok ()
                else Error $"identifier.Operator not \"{expectedOperator}\", got \"{infixExpression.Operator}\""
            
            do! testLiteralExpression infixExpression.Right expectedRight 
        }


[<TestFixture>]
type ParserTests() =
    
    [<Test>]
    [<Order(1)>]
    member this.``Test 'let' statement parsing 1``() =
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
    // Tests whether the assigned expression is correct
    member this.``Test 'let' statement parsing 2``() =
        let testCases: (string * string * obj) list = [
            ("let x = 5;", "x", 5)
            ("let y = true;", "x", true)
            ("let foobar = y;", "foobar", "y")
        ]
        
        for testCase in testCases do
            result {
                let testInput, expectedIdentifier, expectedValue = testCase
                let program = Parser.parseProgram testInput
                
                let! statement =
                    match program.Statements with
                    | head :: _ -> Ok head
                    | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"
                    
                let! letStatement =
                    match statement with
                    | LetStatement letStat -> Ok letStat
                    | _ -> Error $"program.Statements[0] is not a \"LetStatement\", got \"${statement.GetType()}\""
                    
                do! testLetStatement statement (-1, expectedIdentifier)
                do! testLiteralExpression letStatement.Value expectedValue
            }
            |> function
               | Ok _ -> Assert.Pass()
               | Error errorMsg -> Assert.Fail(errorMsg)
               
        
    [<Test>]
    [<Order(2)>]
    member this.``Test 'return' statement parsing 1``() =
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
    member this.``Test 'return' statement parsing 2``() =
        let testCases: (string * obj) list = [
            // test input, expected expression
            ("return 5;", 5)
            ("return true;", true)
            ("return y;", "y")
        ]
        
        for testCase in testCases do
            result {
                let testInput, expectedValue = testCase
                let program = Parser.parseProgram testInput
                
                let! statement =
                    match program.Statements with
                    | head :: _ -> Ok head
                    | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"
                    
                let! returnStatement =
                    match statement with
                    | ReturnStatement retStat -> Ok retStat
                    | _ -> Error $"program.Statements[0] is not a \"ReturnStatement\", got \"${statement.GetType()}\""
                    
                do! testLiteralExpression returnStatement.ReturnValue expectedValue
            }
            |> function
               | Ok _ -> Assert.Pass()
               | Error errorMsg -> Assert.Fail(errorMsg)
        
        
    [<Test>]
    [<Order(3)>]
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
    [<Order(4)>]
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
                | Expression.Identifier ident -> Ok ident
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
    [<Order(5)>]
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
    member this.``Test string literal expressions 1``() =
        result {
            let testInput = "\"foobar\";"
            let program = Parser.parseProgram testInput
            
            let! statement =
                match program.Statements with
                | head :: _ -> Ok head
                | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"

            let! expressionStatement =
                match statement with
                | ExpressionStatement expStat -> Ok expStat
                | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement}\""
                
            let! stringLiteral =
                match expressionStatement.Expression with
                | StringLiteral strLit -> Ok strLit
                | expr -> Error $"exp not \"StringLiteral\", got \"{expr}\""
                
            let expectedValue = "foobar" 
            do! if stringLiteral.Value = expectedValue
                then Ok ()
                else Error $"stringLiteral.Value not \"{expectedValue}\", got \"{stringLiteral.Value}\""
                
            let tokenLiteral = stringLiteral.Token.Literal
            do! if tokenLiteral = $"{expectedValue}" 
                then Ok ()
                else Error $"stringLiteral.TokenLiteral() not \"{expectedValue}\", got \"{tokenLiteral}\""
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
    // Tests expressions with booleans
    member this.``Test parsing prefix expressions 2``() =
        let prefixTests = [
            ("!true;", "!", true)
            ("!false;", "!", false)
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
                    
                do! testBooleanLiteral prefixExpression.Right expectedValue
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
                    
                let! _ =
                    match expressionStatement.Expression with
                    | InfixExpression infixExpr -> Ok infixExpr 
                    | expr -> Error $"exp not \"InfixExpression\", got \"{expr.GetType()}\""
                    
                do! testInfixExpression expressionStatement.Expression expectedLeftValue expectedOperator expectedRightValue
            }
            |> function
                | Ok _ -> () 
                | Error errorMsg -> Assert.Fail($"[For test \"{testInput}\"] {errorMsg}")
                
                
    [<Test>]
    // Tests expressions with booleans
    member this.``Test parsing infix expressions 2``() =
        let infixTests = [
            // (statement, left expr, operator, right expr)
            ("true == true", true, "==", true)
            ("true != false", true, "!=", false)
            ("false == false", false, "==", false)
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
                    
                let! _ =
                    match expressionStatement.Expression with
                    | InfixExpression infixExpr -> Ok infixExpr 
                    | expr -> Error $"exp not \"InfixExpression\", got \"{expr.GetType()}\""
                    
                do! testInfixExpression expressionStatement.Expression expectedLeftValue expectedOperator expectedRightValue
            }
            |> function
                | Ok _ -> () 
                | Error errorMsg -> Assert.Fail($"[For test \"{testInput}\"] {errorMsg}")


    [<Test>]
    member this.``Test operator precedence parsing 1``() =
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
            ("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))")
            ("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))")
            ("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")
            (*
            // ?
            ("3 + 4; -5 * 5", "(3 + 4)((-5) * 5)")
            *)
        ]
        
        for testCase in testCases do
            let inputString, expectedRepresentationString = testCase
            let program = Parser.parseProgram inputString
            
            let programAsStr = program.ToString()
            if programAsStr <> expectedRepresentationString then
                Assert.Fail($"[Comparing strings] Expected \"{expectedRepresentationString}\", but got \"{programAsStr}\"")
            else
                ()
                
                
    [<Test>]
    // Tests expressions with booleans
    member this.``Test operator precedence parsing 2``() =
        let testCases = [
            ("true", "true")
            ("false", "false")
            ("3 > 5 == false", "((3 > 5) == false)")
            ("3 < 5 == true", "((3 < 5) == true)")
        ]
        
        for testCase in testCases do
            let inputString, expectedRepresentationString = testCase
            let program = Parser.parseProgram inputString
            
            let programAsStr = program.ToString()
            if programAsStr <> expectedRepresentationString then
                Assert.Fail($"Expected \"{expectedRepresentationString}\", but got \"{programAsStr}\"")
                
                
    [<Test>]
    // Test expressions with parentheses
    member this.``Test operator precedence parsing 3``() =
        let testCases = [
            ("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)")
            ("(5 + 5) * 2", "((5 + 5) * 2)")
            ("2 / (5 + 5)", "(2 / (5 + 5))")
            ("-(5 + 5)", "(-(5 + 5))")
            ("!(true == true)", "(!(true == true))")
        ]
        
        for testCase in testCases do
            let inputString, expectedRepresentationString = testCase
            let program = Parser.parseProgram inputString
            
            let programAsStr = program.ToString()
            if programAsStr <> expectedRepresentationString then
                Assert.Fail($"Expected \"{expectedRepresentationString}\", but got \"{programAsStr}\"")
                
                
    [<Test>]
    // Test expressions with call expressions 
    member this.``Test operator precedence parsing 4``() =
        let testCases = [
            ("a + add(b * c) + d",
             "((a + add((b * c))) + d)")
            
            ("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))",
             "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))")
            
            ("add(a + b + c * d / f + g)",
             "add((((a + b) + ((c * d) / f)) + g))")
        ]
        
        for testCase in testCases do
            let inputString, expectedRepresentationString = testCase
            let program = Parser.parseProgram inputString
            
            let programAsStr = program.ToString()
            if programAsStr <> expectedRepresentationString then
                Assert.Fail($"Expected \"{expectedRepresentationString}\", but got \"{programAsStr}\"")
                
    [<Test>]
    member this.``Test block statement parsing 1``() =
        let testInput = """let x = 5;
let y = 10;
"""

        let tokens = testInput |> Lexer.parseIntoTokens |> List.rev 
        let tokensQueue = Queue.enqueueList Queue.empty tokens
        
        match (Parser.tryParseBlockStatement (fun _ -> false) tokensQueue) with
        | Ok (_, blockStatement) ->
            result {
                do! match blockStatement.Statements.Length with
                    | len when len = 2 -> Ok ()
                    | len -> Error $"ifExpression.Consequence.Statements.Length not 2, got {len}"
                    
                // For first statement
                let! firstLetStatement =
                    match blockStatement.Statements.Head with
                    | LetStatement letStatement -> Ok letStatement
                    | statement -> Error $"First statement not \"LetStatement\", got \"{statement.GetType()}\""
                    
                do! match firstLetStatement.Name.Value with
                    | name when name = "x" -> Ok ()
                    | name -> Error $"[First statement] Expected a name \"x\", got \"{name}\""
                    
                // For second statement
                let! secondLetStatement =
                    match (List.item 1 blockStatement.Statements) with
                    | LetStatement letStatement -> Ok letStatement
                    | statement -> Error $"Second statement not \"LetStatement\", got \"{statement.GetType()}\""
                    
                do! match secondLetStatement.Name.Value with
                    | name when name = "y" -> Ok ()
                    | name -> Error $"[Second statement] Expected a name \"y\", got \"{name}\""
            }
            |> function
               | Ok _ -> Assert.Pass() 
               | Error errorMsg -> Assert.Fail(errorMsg) 
        | Error (_, errors) ->
            let errorsString = String.concat "\n" errors
            Assert.Fail($"Parsing errors:\n\n{errorsString}")
                
                
    [<Test>]
    member this.``Test 'if' expression 1``() =
        result {
            let testInput = "if (x < y) { x }"
            let program = Parser.parseProgram testInput
            
            let! statement =
                match program.Statements with
                | head :: _ -> Ok head
                | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"
                
            let! expressionStatement =
                match statement with
                | ExpressionStatement expStat -> Ok expStat
                | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement.GetType()}\""
                
            let! ifExpression =
                match expressionStatement.Expression with
                | IfExpression ifExpr -> Ok ifExpr
                | _ -> Error $"program.Statements[0] is not a \"IfExpression\", got \"{expressionStatement.GetType()}\""
                
                
            do! testInfixExpression ifExpression.Condition "x" "<" "y"
            
            do! match ifExpression.Consequence.Statements.Length with
                | len when len = 1 -> Ok ()
                | len -> Error $"ifExpression.Consequence.Statements.Length not 1, got {len}"
                
            let! consequenceExpressionStatement =
                match ifExpression.Consequence.Statements.Head with
                | ExpressionStatement exprStatement ->
                    Ok exprStatement
                | consequenceStatement ->
                    Error $"ifExpression.Consequence.Statements.Head is not a \"ExpressionStatement\", got \"{consequenceStatement.GetType()}\""
                    
            do! testIdentifier consequenceExpressionStatement.Expression "x"
            
            do! match ifExpression.Alternative with
                | Some _ -> Error "ifStatement was found to have an 'alternative/else' block, when it was not supsoed to"
                | None -> Ok ()
        }
        |> function
           | Ok _ -> Assert.Pass()
           | Error errorMsg -> Assert.Fail(errorMsg)
        
        
    [<Test>]
    member this.``Test 'if' expression 2``() =
        result {
            let testInput = "if (x < y) { x } else { y }"
            let program = Parser.parseProgram testInput
            
            let! statement =
                match program.Statements with
                | head :: _ -> Ok head
                | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"
                
            let! expressionStatement =
                match statement with
                | ExpressionStatement expStat -> Ok expStat
                | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement.GetType()}\""
                
            let! ifExpression =
                match expressionStatement.Expression with
                | IfExpression ifExpr -> Ok ifExpr
                | _ -> Error $"program.Statements[0] is not a \"IfExpression\", got \"{expressionStatement.GetType()}\""
                
            do! testInfixExpression ifExpression.Condition "x" "<" "y"
            
            
            // Testing consequence 
            do! match ifExpression.Consequence.Statements.Length with
                | len when len = 1 -> Ok ()
                | len -> Error $"ifExpression.Consequence.Statements.Length not 1, got {len}"
                
            let! consequenceExpressionStatement =
                match ifExpression.Consequence.Statements.Head with
                | ExpressionStatement exprStatement ->
                    Ok exprStatement
                | consequenceStatement ->
                    Error $"ifExpression.Consequence.Statements.Head is not a \"ExpressionStatement\", got \"{consequenceStatement.GetType()}\""
                    
            do! testIdentifier consequenceExpressionStatement.Expression "x"
            
            
            // Testing alternative
            let! alternativeBlockStatement = 
                match ifExpression.Alternative with
                | Some altBlockStatement -> Ok altBlockStatement
                | None -> Error "ifStatement did not have an 'alternative/else' block, when it was supposed to"
                
            do! match alternativeBlockStatement.Statements.Length with
                | len when len = 1 -> Ok ()
                | len -> Error $"ifExpression.Alternative.Statements.Length not 1, got {len}"
                
            let! alternativeExpressionStatement =
                match alternativeBlockStatement.Statements.Head with
                | ExpressionStatement exprStatement ->
                    Ok exprStatement
                | alternativeStatement ->
                    Error $"ifExpression.Alternative.Statements.Head is not a \"ExpressionStatement\", got \"{alternativeStatement.GetType()}\""
                    
            do! testIdentifier alternativeExpressionStatement.Expression "y"
        }
        |> function
           | Ok _ -> Assert.Pass()
           | Error errorMsg -> Assert.Fail(errorMsg)
           
           
    [<Test>]
    member this.``Test function literal parsing 1``() =
        result {
            let testInput = "fn(x, y) { x + y; }"
            let program = Parser.parseProgram testInput
            
            let! statement =
                match program.Statements with
                | head :: _ -> Ok head
                | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"
                
            let! expressionStatement =
                match statement with
                | ExpressionStatement expStat -> Ok expStat
                | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement.GetType()}\""
                
            let! functionLiteral =
                match expressionStatement.Expression with
                | Expression.FunctionLiteral funcLit -> Ok funcLit
                | expr -> Error $"expressionStatement.Expression is not a \"FunctionLiteral\", got \"${expr.GetType()}\""
                
            do! match functionLiteral.Parameters.Length with
                | len when len = 2 -> Ok ()
                | len -> Error $"functionLiteral.Parameters.Length not 2, got {len}"
                
            do! testLiteralExpression (Expression.Identifier (List.item 0 functionLiteral.Parameters)) "x"
            do! testLiteralExpression (Expression.Identifier (List.item 1 functionLiteral.Parameters)) "y"
        
            do! match functionLiteral.Body.Statements.Length with
                | len when len = 1 -> Ok ()
                | len -> Error $"functionLiteral.Body.Statements.Length not 1, got {len}"
                
            let! expressionStatement =
                match functionLiteral.Body.Statements.Head with
                | ExpressionStatement exprStatement -> Ok exprStatement
                | statement -> Error $"functionLiteral.Body.Statements.Head is not an \"ExpressionStatement\", got \"{statement.GetType()}\""
                
            do! testInfixExpression expressionStatement.Expression "x" "+" "y"
        }
        |> function
           | Ok _ -> Assert.Pass()
           | Error errorMsg -> Assert.Fail(errorMsg)
           
           
    [<Test>]
    member this.``Test function parameters parsing 1``() =
        let testCases = [
            // input, expected params
            ("fn() {};", [])
            ("fn(x) {};", [ "x" ])
            ("fn(x, y, z) {};", [ "x"; "y"; "z" ])
        ]
        
        let mutable currentCase = 0  // provides a way for the debugger to stop at a specific test case
        for testCase in testCases do
            result {
                currentCase <- currentCase + 1  
                let testInput, expectedParameters = testCase
                let program = Parser.parseProgram testInput
                
                let! statement =
                    match program.Statements with
                    | head :: _ -> Ok head
                    | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"
                    
                let! expressionStatement =
                    match statement with
                    | ExpressionStatement expStat -> Ok expStat
                    | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement.GetType()}\""
                    
                let! functionLiteral =
                    match expressionStatement.Expression with
                    | Expression.FunctionLiteral funcLit -> Ok funcLit
                    | expr -> Error $"expressionStatement.Expression is not a \"FunctionLiteral\", got \"${expr.GetType()}\""
                    
                let parameters = functionLiteral.Parameters |> List.map (_.Value)
                
                do! if List.forall2 (=) parameters expectedParameters
                    then Ok ()
                    else
                        let expectedParamsStr = String.concat ", " expectedParameters
                        let actualParamsStr = String.concat ", " parameters 
                        Error $"Expected parameters list: [{expectedParamsStr}], got [{actualParamsStr}]"
            } 
            |> function
               | Ok _ -> Assert.Pass()
               | Error errorMsg -> Assert.Fail(errorMsg)
               
               
    [<Test>]
    member this.``Test call expression parsing 1``() =
        result {
            let testInput = "add(1, 2 * 3, 4 + 5);"
            let program = Parser.parseProgram testInput
            
            let! statement =
                match program.Statements with
                | head :: _ -> Ok head
                | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"
                
            let! expressionStatement =
                match statement with
                | ExpressionStatement expStat -> Ok expStat
                | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement.GetType()}\""
                
            let! callExpression =
                match expressionStatement.Expression with
                | Expression.CallExpression callExpr -> Ok callExpr
                | expr -> Error $"expressionStatement.Expression not \"CallExpression\", got \"{expr.GetType()}\""
                
            do! testIdentifier (CallExpr.ToExpression callExpression.Function) "add"
            
            do! if callExpression.Arguments.Length = 3
                then Ok ()
                else Error $"callExpression.Arguments.Length expected to be 3, got \"{callExpression.Arguments.Length}\""
                
            do! testLiteralExpression (List.item 0 callExpression.Arguments) 1
            do! testInfixExpression (List.item 1 callExpression.Arguments) 2 "*" 3
            do! testInfixExpression (List.item 2 callExpression.Arguments) 4 "+" 5
        }
        |> function
           | Ok _ -> Assert.Pass()
           | Error errorMsg -> Assert.Fail(errorMsg)
           
           
    [<Test>]
    member this.``Test array literal parsing 1``() =
        result {
            let testInput = "[1, 2 * 2, 3 + 3];"
            let program = Parser.parseProgram testInput
            
            let! statement =
                match program.Statements with
                | head :: _ -> Ok head
                | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"
                
            let! expressionStatement =
                match statement with
                | ExpressionStatement expStat -> Ok expStat
                | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement.GetType()}\""
                
            let! arrayLiteral =
                match expressionStatement.Expression with
                | ArrayLiteral arrayLiteral -> Ok arrayLiteral
                | expr -> Error $"expressionStatement.Expression not \"ArrayLiteral\", got \"{expr.GetType()}\""
            
            do! if arrayLiteral.Elements.Length = 3
                then Ok ()
                else Error $"callExpression.Elements.Length expected to be 3, got \"{arrayLiteral.Elements.Length}\""
                
            do! testLiteralExpression (List.item 0 arrayLiteral.Elements) 1
            do! testInfixExpression (List.item 1 arrayLiteral.Elements) 2 "*" 2
            do! testInfixExpression (List.item 2 arrayLiteral.Elements) 3 "+" 3
        }
        |> function
           | Ok _ -> Assert.Pass()
           | Error errorMsg -> Assert.Fail(errorMsg)


    [<Test>]
    member this.``Test index expression parsing 1``() =
        result {
            let testInput = "[1, 2 * 2, 3 + 3][1 + 2];"
            let program = Parser.parseProgram testInput
            
            let! statement =
                match program.Statements with
                | head :: _ -> Ok head
                | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"
                
            let! expressionStatement =
                match statement with
                | ExpressionStatement expStat -> Ok expStat
                | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement.GetType()}\""
                
            let! indexExpression =
                match expressionStatement.Expression with
                | IndexExpression indexExpr -> Ok indexExpr
                | expr -> Error $"expressionStatement.Expression not \"IndexExpression\", got \"{expr.GetType()}\""
                
            // Testing 'left' for array literal
            let! arrayLiteral = 
                match indexExpression.Left with
                | ArrayLiteral arrLiteral -> Ok arrLiteral
                | expr -> Error $"indexExpression.Left not \"ArrayLiteral\", got \"{expr.GetType()}\""
            
            do! if arrayLiteral.Elements.Length = 3
                then Ok ()
                else Error $"arrayLiteral.Elements.Length expected to be 3, got \"{arrayLiteral.Elements.Length}\""
                
            do! testLiteralExpression (List.item 0 arrayLiteral.Elements) 1
            do! testInfixExpression (List.item 1 arrayLiteral.Elements) 2 "*" 2
            do! testInfixExpression (List.item 2 arrayLiteral.Elements) 3 "+" 3
            
            // Testing 'index' for infix expr
            let! _ = 
                match indexExpression.Index with
                | InfixExpression infixExpr -> Ok infixExpr
                | expr -> Error $"indexExpression.Index not \"InfixExpression\", got \"{expr.GetType()}\""
                
            do! testInfixExpression indexExpression.Index 1 "+" 2
        }
        |> function
           | Ok _ -> Assert.Pass()
           | Error errorMsg -> Assert.Fail(errorMsg)
