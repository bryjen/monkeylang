[<AutoOpen>]
module Monkey.Frontend.Tests.Parser.Helpers

open System
open System.Diagnostics
open FsToolkit.ErrorHandling
open Monkey.Frontend.Ast


let private getNameOfFunction () =
    let stackTrace = StackTrace()
    let frame = stackTrace.GetFrame(1)  // this func is called by other functions in this module, so we go two 'levels' up
    $"[{frame.GetMethod().Name}]"



[<RequireQualifiedAccess>]
module Program =
    let assertNumberOfStatements (expectedStatements: int) (program: Program) =
        if program.Statements.Length = expectedStatements
        then Ok ()
        else Error $"{getNameOfFunction ()} \"Program\" was expected to have {expectedStatements} statements, got {program.Statements.Length}."
        
    let assertZeroErrors (program: Program) =
        if program.Errors.Length = 0
        then Ok ()
        else
            let errorsListAsStr = program.Errors |> List.map (fun str -> "- " + str) |> String.concat "\n"
            Error $"{getNameOfFunction ()} \"Program\" was expected to have no errors, got {program.Errors.Length} errors: \n\n{errorsListAsStr}"
        
       
        
[<RequireQualifiedAccess>]
module Statement =
    let private getIncorrectTypeMsg (statement: Statement) (expectedTypeAsStr: string) =
        $"{getNameOfFunction ()} The statement \"{statement.ToString()}\" was expected to be a \"{expectedTypeAsStr}\", got \"{statement.GetType().ToString()}\"."
    
    let assertIsLetStatement (statement: Statement) =
        match statement with
        | LetStatement letStatement -> Ok letStatement
        | _ -> getIncorrectTypeMsg statement "Statement.LetStatement" |> Error

    let assertIsReturnStatement (statement: Statement) =
        match statement with
        | ReturnStatement returnStatement -> Ok returnStatement 
        | _ -> getIncorrectTypeMsg statement "Statement.ReturnStatement" |> Error
        
    let assertIsExpressionStatement (statement: Statement) =
        match statement with
        | ExpressionStatement expressionStatement -> Ok expressionStatement 
        | _ -> getIncorrectTypeMsg statement "Statement.ExpressionStatement" |> Error

    let assetIsBlockStatement (statement: Statement) =
        match statement with
        | BlockStatement blockStatement -> Ok blockStatement 
        | _ -> getIncorrectTypeMsg statement "Statement.BlockStatement" |> Error


module Expression =
    let private getIncorrectTypeMsg (expression: Expression) (expectedTypeAsStr: string) =
        $"{getNameOfFunction ()} The expression \"{expression.ToString()}\" was expected to be a \"{expectedTypeAsStr}\", got \"{expression.GetType().ToString()}\"."
        
    let assertIsPrefixExpression (expression: Expression) =
        match expression with
        | PrefixExpression value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.PrefixExpression" |> Error
        
    let assertIsInfixExpression (expression: Expression) =
        match expression with
        | InfixExpression value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.InfixExpression" |> Error

    let assertIsIfExpression (expression: Expression) =
        match expression with
        | IfExpression value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.IfExpression" |> Error
        
    let assertIsCallExpression (expression: Expression) =
        match expression with
        | CallExpression value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.CallExpression" |> Error

    let assertIsIndexExpression (expression: Expression) =
        match expression with
        | IndexExpression value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.IndexExpression" |> Error
        
    let assertIsIntegerLiteral (expression: Expression) =
        match expression with
        | IntegerLiteral value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.IntegerLiteral" |> Error

    let assertIsFunctionLiteral (expression: Expression) =
        match expression with
        | Expression.FunctionLiteral value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.FunctionLiteral" |> Error

    let assertIsStringLiteral (expression: Expression) =
        match expression with
        | StringLiteral value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.StringLiteral" |> Error

    let assertIsArrayLiteral (expression: Expression) =
        match expression with
        | ArrayLiteral value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.ArrayLiteral" |> Error
        
    let assertIsHashLiteral (expression: Expression) =
        match expression with
        | HashLiteral value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.HashLiteral" |> Error

    let assertIsMacroLiteral (expression: Expression) =
        match expression with
        | MacroLiteral value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.MacroLiteral" |> Error
        
    let assertIsIdentifier (expression: Expression) =
        match expression with
        | Expression.Identifier value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.Identifier" |> Error
        
    let assertBooleanLiteral (expression: Expression) =
        match expression with
        | BooleanLiteral value -> Ok value 
        | _ -> getIncorrectTypeMsg expression "Expression.BooleanLiteral" |> Error
        
    let getTypeFromStr (typeAsStr: string) =
        match typeAsStr with
        | "PrefixExpression" -> PrefixExpression.Default.GetType()
        | "InfixExpression" -> InfixExpression.Default.GetType()
        | "IfExpression" -> IfExpression.Default.GetType()
        | "CallExpression" -> CallExpression.Default.GetType()
        | "IndexExpression" -> IndexExpression.Default.GetType()
        | "IntegerLiteral" -> IntegerLiteral.Default.GetType()
        | "FunctionLiteral" -> FunctionLiteral.Default.GetType()
        | "StringLiteral" -> StringLiteral.Default.GetType()
        | "ArrayLiteral" -> ArrayLiteral.Default.GetType()
        | "HashLiteral" -> HashLiteral.Default.GetType()
        | "Identifier" -> Identifier.Default.GetType()
        | "BooleanLiteral" -> BooleanLiteral.Default.GetType()
        | "MacroLiteral" -> failwith "todo" 
        | _ -> failwith "todo"



let assertEqualTypes (expectedType: Type) (actualType: Type)  =
    match expectedType = actualType with
    | true -> Ok() 
    | false -> Error $"{getNameOfFunction ()} Expected \"{expectedType.ToString()}\", got \"{actualType.ToString()}\"" 
    

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
    
let rec testIdentifierOrStringLiteral (expr: Expression) (expectedValue: string) =
    match expr with
    | Expression.Identifier identifier -> testIdentifier identifier expectedValue
    | StringLiteral stringLiteral ->  testStringLiteral stringLiteral expectedValue
    | _ -> Error $"Expected an \"Identifier\" or \"StringLiteral\", but got \"{expr.GetType()}\""
    
and testIdentifier identifier expectedValue =
    result {
        do! if identifier.Value = expectedValue
            then Ok ()
            else Error $"identifier.Value not \"{expectedValue}\", got \"{identifier.Value}\""
            
        do! if identifier.GetTokenLiteral() = expectedValue 
            then Ok ()
            else Error $"identifier.GetTokenLiteral() not \"{expectedValue}\", got \"{identifier.GetTokenLiteral()}\""
    }
    
and testStringLiteral stringLiteral expectedValue =
    result {
        do! if stringLiteral.Value = expectedValue
            then Ok ()
            else Error $"identifier.Value not \"{expectedValue}\", got \"{stringLiteral.Value}\""
            
        do! if stringLiteral.GetTokenLiteral() = expectedValue 
            then Ok ()
            else Error $"identifier.GetTokenLiteral() not \"{expectedValue}\", got \"{stringLiteral.GetTokenLiteral()}\""
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
    | :? string as value -> testIdentifierOrStringLiteral expr value
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

let assertStatementsHaveEqualStrRepresentation (statement: Statement) (expectedStrRepresentation: string) =
    let statementStr = statement.ToString()
    match statementStr = expectedStrRepresentation with
    | true -> Ok (statementStr, expectedStrRepresentation)
    | false -> Error $"Expected statement to be \"{expectedStrRepresentation}\", got \"{statementStr}\""
    
let assertListHasNoErrors (resultList: Result<'a, string> list) =
    match (List.tryFind Result.isError resultList) with
    | Some value -> value |> Result.map (fun _ -> ())
    | None -> Ok ()
    
let forceGetOkCase result =
    match result with
    | Ok value -> value
    | Error _ -> failwith "fatal error, tried to forcibly access the 'Ok' value." 