namespace Monkey.Frontend.Tests.Parser

open FrontendTests.Attributes
open Monkey.Frontend.Helpers.Queue
open Monkey.Frontend.Lexer
open NUnit.Framework

open FsToolkit.ErrorHandling

open Monkey.Frontend.Ast
open Monkey.Frontend.Parser
open Monkey.Frontend.Tests.Parser.Helpers


[<TestFixture>]
type ParsingTests() =
    
    [<TestCase("let x = 5;", "x", 5, "IntegerLiteral")>]
    [<TestCase("let y = true;", "y", true, "BooleanLiteral")>]
    [<TestCase("let foobar = y;", "foobar", "y", "Identifier")>]
    member this.``A: Test let statement parsing``(testInput: string,
                                                  expectedIdentifier: string,
                                                  expectedValue: obj,
                                                  expectedTypeStr: string) =
        
        let program = Parser.parseProgram testInput
        let expectedType = Expression.getTypeFromStr expectedTypeStr
        
        result {
            do! Program.assertNumberOfStatements 1 program
            do! Program.assertZeroErrors program
            
            let statement = program.Statements.Head
            let! letStatement = Statement.assertIsLetStatement statement
            
            do! testIdentifierOrStringLiteral (letStatement.Name |> Expression.Identifier) expectedIdentifier
            do! testLiteralExpression letStatement.Value expectedValue
            do! assertEqualTypes expectedType (letStatement.Value.GetNonUnionCaseType())
            return letStatement.Name.Value, letStatement.Value
        }
        |> function
           | Ok (actualIdentifierValue, exprAsStr) ->
               Assert.Pass($"Input:\t\"{testInput}\"\n
                           Identifier: expected \"{expectedIdentifier}\", got \"{actualIdentifierValue}\"\n
                           Value: expected \"{expectedValue}\" ({expectedValue.GetType()}), got \"{exprAsStr}\" ({exprAsStr.GetType()})\n
                           Type: expected \"{expectedType}\", got {exprAsStr.GetNonUnionCaseType()}")
           | Error errorMsg ->
               Assert.Fail($"Input:\t\"{testInput}\"\n{errorMsg}\n")
               
               
    [<TestCase("return 5;", 5, "IntegerLiteral")>]
    [<TestCase("return true;", true, "BooleanLiteral")>]
    [<TestCase("return y;", "y", "Identifier")>]
    member this.``B: Test return statement parsing``(testInput: string, expectedValue: obj, expectedTypeStr: string) =
        let program = Parser.parseProgram testInput
        let expectedType = Expression.getTypeFromStr expectedTypeStr
        
        result {
            do! Program.assertNumberOfStatements 1 program
            do! Program.assertZeroErrors program
            
            let statement = program.Statements.Head
            let! returnStatement = Statement.assertIsReturnStatement statement
            
            do! testLiteralExpression returnStatement.ReturnValue expectedValue
            do! assertEqualTypes expectedType (returnStatement.ReturnValue.GetNonUnionCaseType())
            return returnStatement.ReturnValue
        }
        |> function
           | Ok exprAsStr ->
               Assert.Pass($"Input:\t\"{testInput}\"\n
                           Value: expected \"{expectedValue}\" ({expectedValue.GetType()}), got \"{exprAsStr}\" ({exprAsStr.GetType()})\n
                           Type: expected \"{expectedType}\", got {exprAsStr.GetNonUnionCaseType()}")
           | Error errorMsg ->
               Assert.Fail($"Input:\t\"{testInput}\"\n{errorMsg}\n")
               
               
    [<TestCase("5;", 5, "IntegerLiteral")>]
    [<TestCase("true;", true, "BooleanLiteral")>]
    [<TestCase("y;", "y", "Identifier")>]
    [<TestCase("\"foobar\";", "foobar", "StringLiteral")>]
    member this.``C: Test expression statement parsing 1 - literals & identifier``(testInput: string,
                                                                                   expectedValue: obj,
                                                                                   expectedTypeStr: string) =
        let program = Parser.parseProgram testInput
        let expectedType = Expression.getTypeFromStr expectedTypeStr
        
        result {
            do! Program.assertNumberOfStatements 1 program
            do! Program.assertZeroErrors program
            
            let statement = program.Statements.Head
            let! expressionStatement = Statement.assertIsExpressionStatement statement
            
            do! assertEqualTypes expectedType (expressionStatement.Expression.GetNonUnionCaseType())
            return expressionStatement.Expression
        }
        |> function
           | Ok exprAsStr ->
               let checkValueStr = $"Value: expected \"{expectedValue}\" ({expectedValue.GetType()}), got \"{exprAsStr}\" ({exprAsStr.GetType()})\n"
               Assert.Pass($"Input:\t\"{testInput}\"\n
                           {checkValueStr}
                           Type: expected \"{expectedType}\", got {exprAsStr.GetNonUnionCaseType()}")
           | Error errorMsg ->
               Assert.Fail($"Input:\t\"{testInput}\"\n{errorMsg}\n")
               
               
    [<TestCase("!5", "!", 5)>]
    [<TestCase("-15", "-", 15)>]
    [<TestCase("-(12)", "-", 12)>]
    [<TestCase("!true;", "!", true)>]
    [<TestCase("!false;", "!", false)>]
    member this.``D: Test expression statement parsing 2 - prefix expression``(testInput: string,
                                                                               expectedOperator: string,
                                                                               expectedRightValue: obj) =
        let program = Parser.parseProgram testInput
        
        result {
            do! Program.assertNumberOfStatements 1 program
            do! Program.assertZeroErrors program
            
            let statement = program.Statements.Head
            let! expressionStatement = Statement.assertIsExpressionStatement statement
            let! prefixExpression = Expression.assertIsPrefixExpression expressionStatement.Expression
            
            do! if prefixExpression.Operator = expectedOperator
                then Ok ()
                else Error $"prefixExpression.Operator not \"{expectedOperator}\", got \"{prefixExpression.Operator}\""
            
            do! testLiteralExpression prefixExpression.Right expectedRightValue
            return prefixExpression
        }
        |> function
           | Ok prefixExpression ->
               Assert.Pass($"Input:\t\"{testInput}\"\n
                           Type: expected \"-.PrefixExpression\", got \"{prefixExpression.GetType()}\"\n
                           Operator: expected \"{expectedOperator}\", got \"{prefixExpression.Operator}\"\n
                           Value: expected \"{expectedRightValue}\", got \"{prefixExpression.Right.ToString()}\"")
           | Error errorMsg ->
               Assert.Fail($"Input:\t\"{testInput}\"\n{errorMsg}\n")
               
               
    [<TestCase("5 + 5;", 5, "+", 5)>]
    [<TestCase("5 - 5;", 5, "-", 5)>]
    [<TestCase("5 * 5;", 5, "*", 5)>]
    [<TestCase("5 / 5;", 5, "/", 5)>]
    [<TestCase("5 > 5;", 5, ">", 5)>]
    [<TestCase("5 < 5;", 5, "<", 5)>]
    [<TestCase("5 == 5;", 5, "==", 5)>]
    [<TestCase("5 != 5;", 5, "!=", 5)>]
    [<TestCase("true == true", true, "==", true)>]
    [<TestCase("true != false", true, "!=", false)>]
    [<TestCase("false == false", false, "==", false)>]
    member this.``E: Test expression statement parsing 3 - infix expression``
        (testInput: string, expectedLeftValue: obj, expectedOperator: string, expectedRightValue: obj) =
        
        let program = Parser.parseProgram testInput
        
        result {
            do! Program.assertNumberOfStatements 1 program
            do! Program.assertZeroErrors program
            
            let statement = program.Statements.Head
            let! expressionStatement = Statement.assertIsExpressionStatement statement
            let! infixExpression = Expression.assertIsInfixExpression expressionStatement.Expression
            
            do! if infixExpression.Operator = expectedOperator
                then Ok ()
                else Error $"prefixExpression.Operator not \"{expectedOperator}\", got \"{infixExpression.Operator}\""
            
            do! testLiteralExpression infixExpression.Left expectedLeftValue 
            do! testLiteralExpression infixExpression.Right expectedRightValue 
            return infixExpression 
        }
        |> function
           | Ok infixExpression ->
               Assert.Pass($"Input:\t\"{testInput}\"\n
                           Type: expected \"-.PrefixExpression\", got \"{infixExpression.GetType()}\"\n
                           Operator: expected \"{expectedOperator}\", got \"{infixExpression.Operator}\"\n
                           Left Value: expected \"{expectedLeftValue}\", got \"{infixExpression.Left.ToString()}\"\n
                           Right Value: expected \"{expectedRightValue}\", got \"{infixExpression.Right.ToString()}\"")
           | Error errorMsg ->
               Assert.Fail($"Input:\t\"{testInput}\"\n{errorMsg}\n")
               
               
    // Batch 1
    [<TestCase("-a * b", "((-a) * b)")>]
    [<TestCase("!-a", "(!(-a))")>]
    [<TestCase("a + b + c", "((a + b) + c)")>]
    [<TestCase("a + b - c", "((a + b) - c)")>]
    [<TestCase("a * b * c", "((a * b) * c)")>]
    [<TestCase("a * b / c", "((a * b) / c)")>]
    [<TestCase("a + b / c", "(a + (b / c))")>]
    [<TestCase("a + b * c + d / e - f", "(((a + (b * c)) + (d / e)) - f)")>]
    [<TestCase("5 > 4 == 3 < 4", "((5 > 4) == (3 < 4))")>]
    [<TestCase("5 < 4 != 3 > 4", "((5 < 4) != (3 > 4))")>]
    [<TestCase("3 + 4 * 5 == 3 * 1 + 4 * 5", "((3 + (4 * 5)) == ((3 * 1) + (4 * 5)))")>]
    
    // Batch 2 - tests expression with booleans
    [<TestCase("true", "true")>]
    [<TestCase("false", "false")>]
    [<TestCase("3 > 5 == false", "((3 > 5) == false)")>]
    [<TestCase("3 < 5 == true", "((3 < 5) == true)")>]
    
    // Batch 3 - tests expression with parentheses
    [<TestCase("1 + (2 + 3) + 4", "((1 + (2 + 3)) + 4)")>]
    [<TestCase("(5 + 5) * 2", "((5 + 5) * 2)")>]
    [<TestCase("2 / (5 + 5)", "(2 / (5 + 5))")>]
    [<TestCase("-(5 + 5)", "(-(5 + 5))")>]
    [<TestCase("!(true == true)", "(!(true == true))")>]
    
    // Batch 4 - tests expression with call expressions
    [<TestCase("a + add(b * c) + d", "((a + add((b * c))) + d)")>]
    [<TestCase("add(a, b, 1, 2 * 3, 4 + 5, add(6, 7 * 8))", "add(a, b, 1, (2 * 3), (4 + 5), add(6, (7 * 8)))")>]
    [<TestCase("add(a + b + c * d / f + g)", "add((((a + b) + ((c * d) / f)) + g))")>]
    member this.``F: Test expression statement parsing 4 - nested infix expr parsing``
        (testInput: string, expectedInfixExprRepresentationStr: string) =
        
        let program = Parser.parseProgram testInput
        
        result {
            do! Program.assertNumberOfStatements 1 program
            do! Program.assertZeroErrors program
            
            let statement = program.Statements.Head
            let! _ = Statement.assertIsExpressionStatement statement
            // test cases contain both prefix & infix expressions, cant test for one specifically
            
            let parseAsStr = program.ToString()
            do! if parseAsStr = expectedInfixExprRepresentationStr 
                then Ok ()
                else Error $"[Comparing strings] Expected \"{expectedInfixExprRepresentationStr}\", but got \"{parseAsStr}\""
        }
        |> function
           | Ok _ ->
               Assert.Pass($"Input:\t\"{testInput}\"\n
                           Got: \"{program.ToString()}\"\n
                           Expected: \"{expectedInfixExprRepresentationStr}\"")
           | Error errorMsg ->
               Assert.Fail($"Input:\t\"{testInput}\"\n{errorMsg}\n")
               
               
    // have as separate member variable cause we can't pass lists to 'TestCase' attributes 
    member private this.``G - test cases`` = [
        ("let x = 5;
         let y = 10;",
         ["let x = 5;"
          "let y = 10;"])
    ] 

    [<TestCase(0)>]
    member this.``G: Test block statement parsing``
        (testCaseInput: int) =
        
        // We have to use internal parser methods to manually parse the code block as a 'BlockStatement'
        // Otherwise passing the input to the parser would result in individual statements.
        
        let testInput, expectedStringRepresentations = List.item testCaseInput this.``G - test cases``
        let tokens = testInput |> Lexer.parseIntoTokens |> List.rev 
        let tokensQueue = Queue.enqueueList Queue.empty tokens
        
        match (Parser.tryParseBlockStatement (fun _ -> false) tokensQueue) with
        | Ok (_, blockStatement) ->
            result {
                do! if blockStatement.Statements.Length = expectedStringRepresentations.Length
                    then Ok ()
                    else Error $"blockStatement.Statements.Length expected to be {expectedStringRepresentations.Length}, got {blockStatement.Statements.Length}"
                
                let actualAndExpectedStrPairs = blockStatement.Statements
                                                |> List.map (_.ToString())
                                                |> List.zip expectedStringRepresentations
                                                
                return! 
                    match (List.tryFind (fun (actualStr, expectedStr) -> actualStr <> expectedStr) actualAndExpectedStrPairs) with
                    | Some (actualStr, expectedStr) -> Error $"Statements not equal, expected {expectedStr}, got {actualStr}" 
                    | None -> Ok actualAndExpectedStrPairs
            }
            |> function
               | Ok actualAndExpectedStrPairs ->
                   let formattedPairs = actualAndExpectedStrPairs
                                        |> List.map (fun (actualStr, expectedStr) -> $"expected \"{expectedStr}\", got \"{actualStr}\"")
                                        |> String.concat "\n"
                   Assert.Pass($"Input:\t\"{testInput}\"\n{formattedPairs}\n")
               | Error errorMsg ->
                   Assert.Fail($"Input:\t\"{testInput}\"\n{errorMsg}\n")
        | Error (_, errors) ->
            let errorsString = String.concat "\n" errors
            Assert.Fail($"Parsing errors:\n\n{errorsString}")
            
            
    member private this.``H - test cases`` = [
        ("if (x < y) { return x; }",
         (fun expr -> testInfixExpression expr "x" "<" "y"),
         [ "return x;" ],
         None)
        
        ("if (x < y) { return x; } else { return y; }",
         (fun expr -> testInfixExpression expr "x" "<" "y"),
         [ "return x;" ],
         Some [ "return y;" ])
    ]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    member this.``H: Test if expression parsing``
        (testCaseInput: int) =
        
        let testInput, testConditionFunc, expectedConsequenceStrList, expectedAltStrOptionList = List.item testCaseInput this.``H - test cases``
        let program = Parser.parseProgram testInput
        
        result {
            do! Program.assertNumberOfStatements 1 program
            do! Program.assertZeroErrors program
            
            let statement = program.Statements.Head
            let! expressionStatement = Statement.assertIsExpressionStatement statement
            let! ifExpression = Expression.assertIsIfExpression expressionStatement.Expression
            
            // Testing conditional
            do! testConditionFunc ifExpression.Condition
            
            // Testing consequence
            let consequenceEqualityResults = List.zip ifExpression.Consequence.Statements expectedConsequenceStrList
                                             |> List.map (fun (statement, expected) -> assertStatementsHaveEqualStrRepresentation statement expected)
            do! assertListHasNoErrors consequenceEqualityResults
            
            // Testing alternative
            let! altEqualityResultsOption =
                match ifExpression.Alternative, expectedAltStrOptionList with
                | None, None ->
                    Ok None 
                | None, Some strList ->
                    let listAsStr = String.concat ", " strList
                    Error $"The if expression was expected to have the following statements: [ {listAsStr} ], got none."
                | Some blockStat, None ->
                    Error $"The if expression was expected to have no alternative block, got: \"{blockStat.ToString()}\""
                | Some blockStat, Some strList ->
                    List.zip blockStat.Statements strList
                    |> List.map (fun (statement, expected) -> assertStatementsHaveEqualStrRepresentation statement expected)
                    |> Some |> Ok
                    
            do! 
                match altEqualityResultsOption with
                | None -> Ok ()
                | Some value -> assertListHasNoErrors value
                
            return (ifExpression.Condition.ToString(),
                    List.map forceGetOkCase consequenceEqualityResults,
                    altEqualityResultsOption |> Option.map (List.map forceGetOkCase))
        }
        |> function
           | Ok (actualConditionalStr, consequencePairs, altPairsOption) ->
               let format strs = strs |> List.map (fun str -> $"\"{str}\"") |> String.concat ", "
               let actualConseqStrs, expectedConseqStrs = List.unzip consequencePairs
               let altMsgStr =
                   match altPairsOption with
                   | None -> "" 
                   | Some value ->
                       let actualAltStrs, expectedAltStrs = List.unzip value
                       $"\nAlternative: Expected [ {format expectedAltStrs} ], got [ {format actualAltStrs} ]"
               
               Assert.Pass($"Input:\t\"{testInput}\"\n
                           Condition: \"{actualConditionalStr}\"\n
                           Consequence: Expected [ {format expectedConseqStrs} ], got [ {format actualConseqStrs} ]
                           {altMsgStr}")
           | Error errorMsg ->
               Assert.Fail($"Input:\t\"{testInput}\"\n{errorMsg}\n")
               
               
    [<TestMessage("Ported from \"Frontend.Tests.ParserTests\"")>]
    [<Test>]
    member this.``I: Test function literal parsing 1``() =
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
           
           
    member private this.``J - test cases`` = [
        ("fn() {};", [])
        ("fn(x) {};", [ "x" ])
        ("fn(x, y, z) {};", [ "x"; "y"; "z" ])
    ]
           
           
    [<TestMessage("Ported from \"Frontend.Tests.ParserTests\"")>]
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    member this.``J: Test function parameters parsing 1``(testCaseIndex: int) =
        let testInput, expectedParameters = List.item testCaseIndex this.``J - test cases`` 
        result {
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
               
               
    [<TestMessage("Ported from \"Frontend.Tests.ParserTests\"")>]
    [<Test>]
    member this.``K: Test call expression parsing 1``() =
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
                
            do! testLiteralExpression (CallExpr.ToExpression callExpression.Function) "add"
            
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
           
           
    [<TestMessage("Ported from \"Frontend.Tests.ParserTests\"")>]
    [<Test>]
    member this.``L: Test array literal parsing 1``() =
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
                
            do! testLiteralExpression arrayLiteral.Elements[0] 1
            do! testInfixExpression arrayLiteral.Elements[1] 2 "*" 2
            do! testInfixExpression arrayLiteral.Elements[2] 3 "+" 3
        }
        |> function
           | Ok _ -> Assert.Pass()
           | Error errorMsg -> Assert.Fail(errorMsg)


    [<TestMessage("Ported from \"Frontend.Tests.ParserTests\"")>]
    [<Test>]
    member this.``M: Test index expression parsing 1``() =
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
                
            do! testLiteralExpression arrayLiteral.Elements[0] 1
            do! testInfixExpression arrayLiteral.Elements[1] 2 "*" 2
            do! testInfixExpression arrayLiteral.Elements[2] 3 "+" 3
            
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
           
           
    [<TestMessage("Ported from \"Frontend.Tests.ParserTests\"")>]
    [<Test>]
    member this.``N: Test hash literal parsing 1``() =
        result {
            let testInput = "{\"one\": 1, \"two\": 2, \"three\": 3};"
            let program = Parser.parseProgram testInput
            
            let! statement =
                match program.Statements with
                | head :: _ -> Ok head
                | _ -> Error $"Program has not enough statements. Expected 1, got {program.Statements.Length}"
                
            let! expressionStatement =
                match statement with
                | ExpressionStatement expStat -> Ok expStat
                | _ -> Error $"program.Statements[0] is not a \"ExpressionStatement\", got \"${statement.GetType()}\""
                
            let! hashLiteral =
                match expressionStatement.Expression with
                | HashLiteral hashLiteral -> Ok hashLiteral
                | expr -> Error $"expressionStatement.Expression not \"HashLiteral\", got \"{expr.GetType()}\""
                
            do! if hashLiteral.Pairs.Count = 3
                then Ok ()
                else Error $"The dictionary was expected to have a count of 3, got {hashLiteral.Pairs.Count}"
                
            let sortHeuristic (_, expr) =
                match expr with
                | IntegerLiteral integerLiteral -> integerLiteral.Value
                | _ -> failwith "testing failure"
                
            let mapAsList = Map.toList hashLiteral.Pairs |> List.sortBy sortHeuristic
            
            let key1, value1 = List.item 0 mapAsList
            do! match key1 with
                | StringLiteral stringLiteral when stringLiteral.Value = "one" -> Ok ()
                | StringLiteral stringLiteral -> Error $"StringLiteral.Value expected to be \"one\", got \"{stringLiteral.Value}\"" 
                | _ -> Error $"Key1: Expected a \"StringLiteral\", got \"{key1.GetType()}\"" 
            do! match value1 with
                | IntegerLiteral integerLiteral when integerLiteral.Value = 1 -> Ok ()
                | IntegerLiteral integerLiteral -> Error $"IntegerLiteral.Value expected to be \"1\", got \"{integerLiteral.Value}\"" 
                | _ -> Error $"Value1: Expected a \"IntegerLiteral\", got \"{value1.GetType()}\"" 
            
            let key2, value2 = List.item 1 mapAsList
            do! match key2 with
                | StringLiteral stringLiteral when stringLiteral.Value = "two" -> Ok ()
                | StringLiteral stringLiteral -> Error $"StringLiteral.Value expected to be \"two\", got \"{stringLiteral.Value}\"" 
                | _ -> Error $"Key2: Expected a \"StringLiteral\", got \"{key2.GetType()}\"" 
            do! match value2 with
                | IntegerLiteral integerLiteral when integerLiteral.Value = 2 -> Ok ()
                | IntegerLiteral integerLiteral -> Error $"IntegerLiteral.Value expected to be \"2\", got \"{integerLiteral.Value}\"" 
                | _ -> Error $"Value2: Expected a \"IntegerLiteral\", got \"{value1.GetType()}\"" 
            
            let key3, value3 = List.item 2 mapAsList
            do! match key3 with
                | StringLiteral stringLiteral when stringLiteral.Value = "three" -> Ok ()
                | StringLiteral stringLiteral -> Error $"StringLiteral.Value expected to be \"three\", got \"{stringLiteral.Value}\"" 
                | _ -> Error $"Key3: Expected a \"StringLiteral\", got \"{key3.GetType()}\"" 
            do! match value3 with
                | IntegerLiteral integerLiteral when integerLiteral.Value = 3 -> Ok ()
                | IntegerLiteral integerLiteral -> Error $"IntegerLiteral.Value expected to be \"3\", got \"{integerLiteral.Value}\"" 
                | _ -> Error $"Value3: Expected a \"IntegerLiteral\", got \"{value1.GetType()}\"" 
        }
        |> function
           | Ok _ -> Assert.Pass()
           | Error errorMsg -> Assert.Fail(errorMsg)
