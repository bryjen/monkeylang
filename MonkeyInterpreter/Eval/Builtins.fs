module MonkeyInterpreter.Eval.Builtins

open MonkeyInterpreter.Eval.Object


[<AutoOpen>]
module private Functions = 
    let len object =
        match object with
        | Object.StringType str -> str.Length |> int64 |> Object.IntegerType
        | _ -> ErrorType $"Argument to 'len' not supported, got {object.Inspect()}" |> Object.ErrorType 
    
let private wrap fn =
    let wrappedFn args =
        match args with
        | [ value ] -> fn value
        | _ -> ErrorType $"Wrong number of arguments, expected 1, got {args.Length}" |> Object.ErrorType 
        
    { Fn = wrappedFn; ParametersLength = 1 } |> Function.BuiltinFunction |> FunctionType
    
let builtins = Map.ofList [
    ("len", wrap len)
]