namespace Monkey.Frontend.Tests.Parser

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
