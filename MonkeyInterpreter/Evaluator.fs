module rec MonkeyInterpreter.Evaluator

open MonkeyInterpreter.Object
open FsToolkit.ErrorHandling



type Environment = internal Environment of Map<string, Object>
with
    static member Empty = [ ] |> Map.ofList |> Environment
    
    member this.Map =
        let (Environment e) = this
        e



[<RequireQualifiedAccess>]
module Evaluator =
    let evalStatementsList (environment: Environment) statements =
        match statements with
        | [] ->
            Ok (environment, Null)
            
        | [ head ] ->
            evalStatement environment head
            
        | head :: remaining ->
            match head with
            | ReturnStatement _ -> // stop if return statement
                evalStatement environment head
            | _ -> 
                match evalStatement environment head with
                | Ok (newEnv, _) -> evalStatementsList newEnv remaining
                | Error error -> Error error
    
    let rec evalStatement environment statement : Result<Environment * Object, string> =
        match statement with
        | LetStatement letStatement ->
            evalLetStatement environment letStatement
        | ReturnStatement returnStatement -> 
            evalExpression environment returnStatement.ReturnValue
        | ExpressionStatement expressionStatement ->
            evalExpression environment expressionStatement.Expression
        | BlockStatement blockStatement ->
            evalStatementsList environment blockStatement.Statements
            
    and private evalLetStatement environment letStatement : Result<Environment * Object, string> =
        match evalExpression environment letStatement.Value with
        | Ok (newEnv, object) ->
            let newEnv = newEnv.Map.Add (letStatement.Name.Value, object) |> Environment
            Ok (newEnv, Null)  // binding does not return any value
        | Error errorMsg ->
            Error errorMsg

    let rec evalExpression (environment: Environment) expression : Result<Environment * Object, string> =
        let attachCurrentEnvironment value = (environment, value) 
        
        match expression with
        | IntegerLiteral integerLiteral ->
            Ok (Integer(integerLiteral.Value))
            |> Result.map attachCurrentEnvironment
        | StringLiteral stringLiteral ->
            Ok (String(stringLiteral.Value))
            |> Result.map attachCurrentEnvironment
        | BooleanLiteral booleanLiteral ->
            Ok (Boolean(booleanLiteral.Value))
            |> Result.map attachCurrentEnvironment
        | Expression.Identifier identifier ->
            evalIdentifier environment identifier
            |> Result.map attachCurrentEnvironment
        | PrefixExpression prefixExpression ->
            evalPrefixExpression environment prefixExpression
            |> Result.map attachCurrentEnvironment
        | InfixExpression infixExpression ->
            evalInfixExpression environment infixExpression
            |> Result.map attachCurrentEnvironment
        | IfExpression ifExpression ->
            evalIfExpression environment ifExpression
        | CallExpression callExpression ->
            failwith "todo"
        | IndexExpression indexExpression ->
            failwith "todo"
        | Expression.FunctionLiteral functionLiteral ->
            failwith "todo"
        | ArrayLiteral arrayLiteral ->
            failwith "todo"
        | HashLiteral hashLiteral ->
            failwith "todo"
        | MacroLiteral macroLiteral ->
            failwith "todo"

    and private evalIdentifier environment identifier =
        match Map.tryFind identifier.Value environment.Map with
        | Some value -> Ok value
        | None -> Error $"The variable \"{identifier.Value}\" is not defined." 

    and private evalPrefixExpression environment prefixExpression =
        let doEval operator right =
            match operator, right with
            | "!", Boolean bool -> bool |> not |> Boolean |> Ok
            | "-", Integer integer -> (-integer) |> Integer |> Ok
            | c, r -> Error $"The prefix operator \"{c}\" is not compatible with the type \"{r.Type()}\"."
        
        result {
            // Impossible for variable binding to occur during prefix evaluation
            let! _, rightObj = evalExpression environment prefixExpression.Right
            let operator = prefixExpression.Operator
            return! doEval operator rightObj
        }

    and private evalInfixExpression environment infixExpression =
        let doEval operator left right =
            match operator, left, right with
            | "+", Integer l, Integer r -> (l + r) |> Integer |> Ok 
            | "+", String l, String r -> (l + r) |> String |> Ok 
            | "-", Integer l, Integer r -> (l - r) |> Integer |> Ok 
            | "/", Integer l, Integer r -> (l / r) |> Integer |> Ok 
            | "*", Integer l, Integer r -> (l * r) |> Integer |> Ok 
            | "==", Integer l, Integer r -> (l = r) |> Boolean |> Ok 
            | "==", Boolean l, Boolean r -> (l = r) |> Boolean |> Ok 
            | "!=", Integer l, Integer r -> (l <> r) |> Boolean |> Ok 
            | "!=", Boolean l, Boolean r -> (l <> r) |> Boolean |> Ok 
            | ">", Integer l, Integer r -> (l > r) |> Boolean |> Ok 
            | "<", Integer l, Integer r -> (l < r) |> Boolean |> Ok 
            | _ -> Error $"The operation \"{left.Type()}\" \"{operator}\" \"{right.Type()}\" is not valid." 
        
        result {
            // Impossible for variable binding to occur during infix evaluation
            let! _, leftObj = evalExpression environment infixExpression.Left
            let! _, rightObj = evalExpression environment infixExpression.Right
            let operator = infixExpression.Operator
            return! doEval operator leftObj rightObj
        }
        
    and private evalIfExpression environment ifExpression =
        result {
            // Variable binding cannot occur during evaluation of the conditional
            let! _, conditionObj = evalExpression environment ifExpression.Condition
            
            let! asBool = match conditionObj with
                          | Boolean value -> Ok value
                          | object -> Error $"Condition expression does not evaluate into a boolean, got \"{object.Type()}\"."
                          
            if asBool then
                return! evalStatementsList environment ifExpression.Consequence.Statements
            elif not asBool && ifExpression.Alternative.IsSome then
                let alternativeBlockStatement = (Option.get ifExpression.Alternative)
                return! evalStatementsList environment alternativeBlockStatement.Statements
            else
                return (environment, Null)
        } 