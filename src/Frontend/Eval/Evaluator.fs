module rec Monkey.Frontend.Eval.Evaluator

open FsToolkit.ErrorHandling
open Monkey.Frontend.Ast
open Monkey.Frontend.Eval.Object



[<AutoOpen>]
module private EvaluatorHelpers = 
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
            
    let rec filterOkResults objList resultsList =
        match resultsList with
        | head :: tail ->
            match head with
            | Ok obj -> filterOkResults (obj :: objList) tail
            | Error err -> Error err
        | [] ->
            Ok (List.rev objList)
            
    let rec filterSomeResults objList optionsList =
        match optionsList with
        | head :: tail ->
            match head with
            | Some obj ->
                filterSomeResults (obj :: objList) tail
            | None ->
                // TODO: Find a way to make this error msg more descriptive
                Error "This aint hashable." 
        | [] ->
            Ok (List.rev objList)



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
        let variableName = letStatement.Name.Value
        
        match evalExpression environment letStatement.Value with
        | Ok _ when isBuiltinFunctionName variableName ->
            Error $"[evalLetStatement] Cannot bind to variable, \"{variableName}\" is a builtin function."
        | Ok (newEnv, object) ->
            Ok (newEnv.Set variableName object, NullType)  // binding does not return any value
        | Error errorMsg ->
            Error errorMsg
            
    and private isBuiltinFunctionName identName = (Map.tryFind identName Builtins.builtins) |> Option.isSome

    let rec evalExpression (environment: Environment) expression : Result<Environment * Object, string> =
        let attachCurrentEnvironment value = (environment, value) 
        
        match expression with
        | IntegerLiteral integerLiteral ->
            integerLiteral.Value |> Object.IntegerType |> attachCurrentEnvironment |> Ok
        | StringLiteral stringLiteral ->
            stringLiteral.Value |> Object.StringType |> attachCurrentEnvironment |> Ok
        | BooleanLiteral booleanLiteral ->
            booleanLiteral.Value |> Object.BooleanType |> attachCurrentEnvironment |> Ok
        | Expression.Identifier identifier ->
            evalIdentifier environment identifier |> Result.map attachCurrentEnvironment
            
        | PrefixExpression prefixExpression ->
            evalPrefixExpression environment prefixExpression |> Result.map attachCurrentEnvironment
        | InfixExpression infixExpression ->
            evalInfixExpression environment infixExpression |> Result.map attachCurrentEnvironment
        | IfExpression ifExpression ->
            evalIfExpression environment ifExpression
            
        | ArrayLiteral arrayLiteral ->
            evalArrayLiteral environment arrayLiteral |> Result.map attachCurrentEnvironment
        | HashLiteral hashLiteral ->
            evalHashLiteral environment hashLiteral |> Result.map attachCurrentEnvironment
        | IndexExpression indexExpression ->
            evalIndexExpression environment indexExpression |> Result.map attachCurrentEnvironment
            
        | Expression.FunctionLiteral functionLiteral ->
            evalFunctionLiteral environment functionLiteral |> Result.map (FunctionType >> attachCurrentEnvironment)
        | CallExpression callExpression ->
            evalCallExpression environment callExpression
            
        | MacroLiteral macroLiteral ->
            failwith "todo"

    and internal evalIdentifier environment identifier =
        // TODO: Make it so that the user cannot override builtin values so we don't do this anymore 
        match environment.Get identifier.Value with
        | Some value -> Ok value
        | None ->
            match Map.tryFind identifier.Value Builtins.builtins with
            | Some value -> Ok value
            | None -> Result.Error $"The variable \"{identifier.Value}\" is not defined."
        
    and internal evalPrefixExpression environment prefixExpression =
        let doEval operator right =
            match operator, right with
            | "!", Object.BooleanType bool -> bool |> not |> Object.BooleanType |> Ok
            | "-", Object.IntegerType integer -> (-integer) |> Object.IntegerType |> Ok
            | c, r -> Result.Error $"The prefix operator \"{c}\" is not compatible with the type \"{r.Type()}\"."
        
        result {
            // Impossible for variable binding to occur during prefix evaluation
            let! _, rightObj = evalExpression environment prefixExpression.Right
            let operator = prefixExpression.Operator
            return! doEval operator rightObj
        }

    and internal evalInfixExpression environment infixExpression =
        let doEval operator left right =
            match operator, left, right with
            | "+", Object.IntegerType l, Object.IntegerType r -> (l + r) |> Object.IntegerType |> Ok 
            | "+", Object.StringType l, Object.StringType r -> (l + r) |> Object.StringType |> Ok 
            | "+", Object.IntegerType l, Object.StringType r -> ($"{l}" + r) |> Object.StringType |> Ok 
            | "+", Object.StringType l, Object.IntegerType r -> (l + $"{r}") |> Object.StringType |> Ok 
            | "-", Object.IntegerType l, Object.IntegerType r -> (l - r) |> Object.IntegerType |> Ok 
            | "/", Object.IntegerType l, Object.IntegerType r -> (l / r) |> Object.IntegerType |> Ok 
            | "*", Object.IntegerType l, Object.IntegerType r -> (l * r) |> Object.IntegerType |> Ok 
            | "==", Object.IntegerType l, Object.IntegerType r -> (l = r) |> Object.BooleanType |> Ok 
            | "==", Object.BooleanType l, Object.BooleanType r -> (l = r) |> Object.BooleanType |> Ok 
            | "!=", Object.IntegerType l, Object.IntegerType r -> (l <> r) |> Object.BooleanType |> Ok 
            | "!=", Object.BooleanType l, Object.BooleanType r -> (l <> r) |> Object.BooleanType |> Ok 
            | ">", Object.IntegerType l, Object.IntegerType r -> (l > r) |> Object.BooleanType |> Ok 
            | "<", Object.IntegerType l, Object.IntegerType r -> (l < r) |> Object.BooleanType |> Ok
            | _ -> Result.Error $"The operation \"{left.Type()}\" \"{operator}\" \"{right.Type()}\" is not valid." 
        
        result {
            // Impossible for variable binding to occur during infix evaluation
            let! _, leftObj = evalExpression environment infixExpression.Left
            let! _, rightObj = evalExpression environment infixExpression.Right
            let operator = infixExpression.Operator
            return! doEval operator leftObj rightObj
        }
        
    and internal evalFunctionLiteral environment functionLiteral : Result<Function, string> =
        let enclosedEnv = Environment.CreateEnclosedEnv environment
        UserFunction.FromFunctionLiteral enclosedEnv functionLiteral |> Function.UserFunction |> Ok
        
    and internal evalIfExpression environment ifExpression =
        result {
            // Variable binding cannot occur during evaluation of the conditional
            let! _, conditionObj = evalExpression environment ifExpression.Condition
            
            let! asBool = match conditionObj with
                          | Object.BooleanType value -> Ok value
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
    and internal evalCallExpression (environment: Environment) (callExpression: CallExpression) =
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
    
    and internal evalArrayLiteral environment arrayLiteral =
        let evaluatedExpressions = arrayLiteral.Elements |> Array.toList |> List.map (evalExpression environment)
        match (processResults evaluatedExpressions [] []) with
        | Ok evaluatedResults -> List.map snd evaluatedResults |> List.toArray |> ArrayType |> Ok
        | Error error -> Error error
        
    and internal evalIndexExpression environment indexExpression =
        result {
            let! evalObj =
                match indexExpression.Left with
                | ArrayLiteral arrayLiteral -> evalArrayLiteral environment arrayLiteral
                | HashLiteral hashLiteral -> evalHashLiteral environment hashLiteral 
                | Expression.Identifier identifier -> evalIdentifier environment identifier
                | expr -> Error $"[evalIndexExpression] Expected either an \"ArrayLiteral\" or \"Identifier\", got \"{expr.GetType()}\""
                
            return! 
                match evalObj with
                | ArrayType arr -> indexArray environment indexExpression.Index arr
                | HashType hash -> indexHash environment indexExpression.Index hash 
                | _ -> Error $"[evalIndexExpression] Expected an \"ArrayType\", got {evalObj.Type()}."
        }
        
    and private indexArray environment index arr =
        result {
            // variable binding shouldn't be allowed, so we ignore new env - envs should be the same
            let! _, evaluatedIndex = evalExpression environment index
            let! indexAsInt =
                match evaluatedIndex with
                | Object.IntegerType int -> Ok int
                | _ -> Error $"[evalIndexExpression] Index was expected to be an \"IntegerType\", got {evaluatedIndex.Type()}"
                
            do! if indexAsInt >= 0 && indexAsInt < arr.Length
                then Ok ()
                else Error $"[evalIndexExpression] Index {indexAsInt} out of bounds."
                
            return arr[int indexAsInt]
            // return (List.item (int indexAsInt) arr)
        }
        
    and private indexHash environment index hash =
        result {
            let! _, evaluatedIndex = evalExpression environment index
            let! asHashableObject =
                match HashableObject.FromObject evaluatedIndex with
                | Some value -> Ok value
                | None -> Error $"The index is not a hashable type, got {evaluatedIndex.Type()}"
                
            match Map.tryFind asHashableObject hash with
            | Some value -> return value 
            | None -> return NullType 
        }
        
    and internal evalHashLiteral environment hashLiteral =
        result {
            let keyExpressions, valueExpressions = hashLiteral.Pairs |> Map.toList |> List.unzip
            let evaluatedKeyExprs = List.map (evalExpression environment >> Result.map snd) keyExpressions
            let evaluatedValueExprs = List.map (evalExpression environment >> Result.map snd) valueExpressions
            
            // Filter for any errors in evaluation
            let! filteredKeyExprs = evaluatedKeyExprs |> filterOkResults []
            let! filteredValueExprs = evaluatedValueExprs |> filterOkResults []
            
            // Filter only hashable keys
            let! typeFilteredKeyExprs = filteredKeyExprs |> List.map HashableObject.FromObject |> filterSomeResults []
            
            // Zip can't error, component lists are guaranteed to be equal in length
            return List.zip typeFilteredKeyExprs filteredValueExprs |> Map.ofList |> HashType
        }
        