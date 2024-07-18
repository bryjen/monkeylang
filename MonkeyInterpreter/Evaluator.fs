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
    let evalStatementsList environment statements =
        match statements with
        | [ head ] ->
            evalStatement environment head            
        | head :: remaining ->
            match evalStatement environment head with
            | Ok (_, newEnv) -> evalStatementsList newEnv remaining
            | Error error -> Error error
        | [] ->
            Ok (Null, environment)
    
    let rec evalStatement environment statement : Result<Object * Environment, string> =
        match statement with
        | LetStatement letStatement ->
            evalLetStatement environment letStatement
        | ReturnStatement returnStatement -> 
            failwith "todo"
        | ExpressionStatement expressionStatement ->
            evalExpression environment expressionStatement.Expression
            |> Result.map (fun obj -> (obj, environment))
        | BlockStatement blockStatement -> 
            failwith "todo"
            
    and evalLetStatement environment letStatement =
        match evalExpression environment letStatement.Value with
        | Ok object ->
            let newEnv = environment.Map.Add (letStatement.Name.Value, object) |> Environment
            Ok (object, newEnv)
        | Error errorMsg ->
            Error errorMsg

    let rec evalExpression (environment: Environment) expression : Result<Object, string> =
        match expression with
        | IntegerLiteral integerLiteral ->
            Ok (Integer(integerLiteral.Value))
        | StringLiteral stringLiteral ->
            Ok (String(stringLiteral.Value))
        | BooleanLiteral booleanLiteral ->
            Ok (Boolean(booleanLiteral.Value))
        | Expression.Identifier identifier ->
            evalIdentifier environment identifier
        | PrefixExpression prefixExpression ->
            evalPrefixExpression environment prefixExpression
        | InfixExpression infixExpression ->
            evalInfixExpression environment infixExpression
        | IfExpression ifExpression ->
            failwith "todo"
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
            let! rightObj = evalExpression environment prefixExpression.Right
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
            let! leftObj = evalExpression environment infixExpression.Left
            let! rightObj = evalExpression environment infixExpression.Right
            let operator = infixExpression.Operator
            return! doEval operator leftObj rightObj
        }