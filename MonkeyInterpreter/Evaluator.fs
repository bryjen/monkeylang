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
    let evalStatement (environment: Environment) statement : Result<Object, string> =
        match statement with
        | LetStatement letStatement ->
            failwith "todo"
        | ReturnStatement returnStatement -> 
            failwith "todo"
        | ExpressionStatement expressionStatement ->
            evalExpression environment expressionStatement.Expression
        | BlockStatement blockStatement -> 
            failwith "todo"

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
        result {
            let! returnObj = evalExpression environment prefixExpression.Right
            match prefixExpression.Token.Type with
            | BANG ->
                return! match returnObj with
                        | Boolean boolean -> Ok (boolean |> not |> Boolean)
                        | _ -> Error $"The type \"{returnObj.Type()}\" is not compatible with the \"!\" operator."
            | MINUS -> 
                return! match returnObj with
                        | Integer integer -> Ok (Integer (-integer))
                        | _ -> Error $"The type \"{returnObj.Type()}\" is not compatible with the \"-\" operator."
            | tokType ->
                return! Error $"\"{TokenType.ToCaseString tokType}\" is not a valid prefix operator."
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