module rec MonkeyInterpreter.Eval.Evaluator

open FsToolkit.ErrorHandling
open MonkeyInterpreter
open MonkeyInterpreter.Eval.Object



[<RequireQualifiedAccess>]
module Evaluator =
    let evalStatementsList (environment: Environment) statements =
        match statements with
        | [] ->
            Ok (environment, NullType)
            
        | [ head ] ->
            evalStatement environment head
            
        | head :: remaining ->
            match head with
            | ReturnStatement _ -> // stop if return statement
                evalStatement environment head
            | _ -> 
                match evalStatement environment head with
                | Ok (newEnv, _) -> evalStatementsList newEnv remaining
                | Result.Error error -> Result.Error error
    
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
            Ok (newEnv.Set letStatement.Name.Value object, NullType)  // binding does not return any value
        | Result.Error errorMsg ->
            Result.Error errorMsg

    let rec evalExpression (environment: Environment) expression : Result<Environment * Object, string> =
        let attachCurrentEnvironment value = (environment, value) 
        
        match expression with
        | IntegerLiteral integerLiteral ->
            Ok (IntegerType(integerLiteral.Value))
            |> Result.map attachCurrentEnvironment
        | StringLiteral stringLiteral ->
            Ok (StringType(stringLiteral.Value))
            |> Result.map attachCurrentEnvironment
        | BooleanLiteral booleanLiteral ->
            Ok (BooleanType(booleanLiteral.Value))
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
        | Expression.FunctionLiteral functionLiteral ->
            evalFunctionLiteral environment functionLiteral
            |> Result.map (FunctionType >> attachCurrentEnvironment)
        | CallExpression callExpression ->
            evalCallExpression environment callExpression
        | IndexExpression indexExpression ->
            evalIndexExpression environment indexExpression
            |> Result.map attachCurrentEnvironment
        | ArrayLiteral arrayLiteral ->
            evalArrayLiteral environment arrayLiteral
            |> Result.map attachCurrentEnvironment
        | HashLiteral hashLiteral ->
            failwith "todo"
        | MacroLiteral macroLiteral ->
            failwith "todo"

    and private evalIdentifier environment identifier =
        // TODO: Make it so that the user cannot override builtin values so we don't do this anymore 
        match environment.Get identifier.Value with
        | Some value -> Ok value
        | None ->
            match Map.tryFind identifier.Value Builtins.builtins with
            | Some value -> Ok value
            | None -> Result.Error $"The variable \"{identifier.Value}\" is not defined."
        
    and private evalPrefixExpression environment prefixExpression =
        let doEval operator right =
            match operator, right with
            | "!", BooleanType bool -> bool |> not |> BooleanType |> Ok
            | "-", IntegerType integer -> (-integer) |> IntegerType |> Ok
            | c, r -> Result.Error $"The prefix operator \"{c}\" is not compatible with the type \"{r.Type()}\"."
        
        result {
            // Impossible for variable binding to occur during prefix evaluation
            let! _, rightObj = evalExpression environment prefixExpression.Right
            let operator = prefixExpression.Operator
            return! doEval operator rightObj
        }

    and private evalInfixExpression environment infixExpression =
        let doEval operator left right =
            match operator, left, right with
            | "+", IntegerType l, IntegerType r -> (l + r) |> IntegerType |> Ok 
            | "+", StringType l, StringType r -> (l + r) |> StringType |> Ok 
            | "+", IntegerType l, StringType r -> ($"{l}" + r) |> StringType |> Ok 
            | "+", StringType l, IntegerType r -> (l + $"{r}") |> StringType |> Ok 
            | "-", IntegerType l, IntegerType r -> (l - r) |> IntegerType |> Ok 
            | "/", IntegerType l, IntegerType r -> (l / r) |> IntegerType |> Ok 
            | "*", IntegerType l, IntegerType r -> (l * r) |> IntegerType |> Ok 
            | "==", IntegerType l, IntegerType r -> (l = r) |> BooleanType |> Ok 
            | "==", BooleanType l, BooleanType r -> (l = r) |> BooleanType |> Ok 
            | "!=", IntegerType l, IntegerType r -> (l <> r) |> BooleanType |> Ok 
            | "!=", BooleanType l, BooleanType r -> (l <> r) |> BooleanType |> Ok 
            | ">", IntegerType l, IntegerType r -> (l > r) |> BooleanType |> Ok 
            | "<", IntegerType l, IntegerType r -> (l < r) |> BooleanType |> Ok
            | _ -> Result.Error $"The operation \"{left.Type()}\" \"{operator}\" \"{right.Type()}\" is not valid." 
        
        result {
            // Impossible for variable binding to occur during infix evaluation
            let! _, leftObj = evalExpression environment infixExpression.Left
            let! _, rightObj = evalExpression environment infixExpression.Right
            let operator = infixExpression.Operator
            return! doEval operator leftObj rightObj
        }
        
    and private evalFunctionLiteral environment functionLiteral : Result<Function, string> =
        let enclosedEnv = Environment.CreateEnclosedEnv environment
        UserFunction.FromFunctionLiteral enclosedEnv functionLiteral |> Function.UserFunction |> Ok
        
    and private evalIfExpression environment ifExpression =
        result {
            // Variable binding cannot occur during evaluation of the conditional
            let! _, conditionObj = evalExpression environment ifExpression.Condition
            
            let! asBool = match conditionObj with
                          | BooleanType value -> Ok value
                          | object -> Result.Error $"Condition expression does not evaluate into a boolean, got \"{object.Type()}\"."
                          
            if asBool then
                return! evalStatementsList environment ifExpression.Consequence.Statements
            elif not asBool && ifExpression.Alternative.IsSome then
                let alternativeBlockStatement = (Option.get ifExpression.Alternative)
                return! evalStatementsList environment alternativeBlockStatement.Statements
            else
                return (environment, NullType)
        }
        
    // REGION evalCallExpression
    and private evalCallExpression (environment: Environment) (callExpression: CallExpression) =
        let rec evalArguments env expressions evalObjs =
            match expressions with
            | [] -> Ok (List.rev evalObjs)
            | head :: tail ->
                match evalExpression env head with
                | Ok (newEnv, object) -> evalArguments newEnv tail (object :: evalObjs)
                | Result.Error error -> Result.Error error
        
        result {
            let! applicationFunction = getApplicationFunction environment callExpression
            let! requiredArgsLength = getNumberOfParameters environment callExpression
            
            do! if callExpression.Arguments.Length = requiredArgsLength
                then Ok ()
                else Result.Error $"Expected {requiredArgsLength} arguments, got {callExpression.Arguments.Length}"
                
            let! evaluatedArgs = evalArguments environment callExpression.Arguments []
            return! applicationFunction evaluatedArgs
        }
        
    and private getFunction environment callExpression =
        match callExpression.Function with
        | Identifier identifier -> tryGetFunctionFromIdentifier environment identifier
        | FunctionLiteral functionLiteral -> evalFunctionLiteral environment functionLiteral
        
    and private getApplicationFunction environment callExpression =
        match getFunction environment callExpression with
        | Ok func -> 
            match func with
            | UserFunction userFunction -> applyUserFunction userFunction |> Ok 
            | BuiltinFunction builtinFunction -> applyBuiltinFunction environment builtinFunction |> Ok
        | Error error ->
            Error error 
        
    and private getNumberOfParameters environment callExpression = 
        match getFunction environment callExpression with
        | Ok func -> 
            match func with
            | UserFunction userFunction -> userFunction.Parameters.Length |> Ok 
            | BuiltinFunction builtinFunction -> builtinFunction.ParametersLength |> Ok
        | Error error ->
            Error error
        
    and private tryGetFunctionFromIdentifier env identifier =
        match evalIdentifier env identifier with
        | Ok object ->
            match object with
            | FunctionType func -> Ok func
            | _ -> Result.Error $"The value assigned to \"{identifier.Value}\" is not a function, got \"{object.Type()}\""
        | Result.Error error ->
            Result.Error error
            
    and private applyUserFunction userFunc evaluatedArguments =
        let rec populateEnv (env: Environment) pairs =
            match pairs with
            | [] ->
                env
            | (varName, object) :: tail ->
                let newEnv = env.Set varName object
                populateEnv newEnv tail
                
        let identifierNames = userFunc.Parameters |> List.map (_.Value)
        let nameAndValPairs = List.zip identifierNames evaluatedArguments
        let updatedFunc = { userFunc with Env = populateEnv userFunc.Env nameAndValPairs }
        evalStatement updatedFunc.Env (Statement.BlockStatement updatedFunc.Body)
        
    and private applyBuiltinFunction environment builtinFunc evaluatedArguments =
        match builtinFunc.Fn evaluatedArguments with
        | Object.ErrorType error -> Result.Error error.GetMsg
        | returnObject -> Ok (environment, returnObject)
    // END REGION evalCallExpression

    and private evalArrayLiteral environment arrayLiteral =
        let rec processResults currentObjResults objs errors =
            match currentObjResults with
            | head :: tail ->
                match head with
                | Ok obj -> processResults tail (obj :: objs) errors
                | Error error -> processResults tail objs (error :: errors)
            | _ ->
                if errors.Length = 0
                then Ok (List.rev objs)
                else (Error errors.Head)
        
        let evaluatedExpressions = arrayLiteral.Elements |> List.map (evalExpression environment)
        match (processResults evaluatedExpressions [] []) with
        | Ok evaluatedResults -> List.map snd evaluatedResults |> ArrayType |> Ok
        | Error error -> Error error
        
    and private evalIndexExpression environment indexExpression =
        result {
            let! evalObj =
                match indexExpression.Left with
                | ArrayLiteral arrayLiteral -> evalArrayLiteral environment arrayLiteral
                | Expression.Identifier identifier -> evalIdentifier environment identifier
                | expr -> Error $"[evalIndexExpression] Expected either an \"ArrayLiteral\" or \"Identifier\", got \"{expr.GetType()}\""
                
            let! asArrayType =
                match evalObj with
                | ArrayType arr -> Ok arr
                | _ -> Error $"[evalIndexExpression] Expected an \"ArrayType\", got {evalObj.Type()}."
                
            // variable binding shouldn't be allowed, so we ignore new env - envs should be the same
            let! _, evaluatedIndex = evalExpression environment indexExpression.Index
            let! indexAsInt =
                match evaluatedIndex with
                | IntegerType int -> Ok int
                | _ -> Error $"[evalIndexExpression] Index was expected to be an \"IntegerType\", got {evaluatedIndex.Type()}"
                
            do! if indexAsInt >= 0 && indexAsInt < asArrayType.Length
                then Ok ()
                else Error $"[evalIndexExpression] Index {indexAsInt} out of bounds."
                
            return (List.item (int indexAsInt) asArrayType)
        } 
        
        