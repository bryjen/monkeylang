module Monkey.Frontend.Eval.Builtins

open Microsoft.FSharp.Core
open Monkey.Frontend.Eval.Object


[<AutoOpen>]
module private Functions =
    let inline private wrapToIntegerType int = int |> int64 |> Object.IntegerType
        
    let len object =
        match object with
        | Object.StringType str -> str.Length |> wrapToIntegerType 
        | ArrayType array -> array.Length |> wrapToIntegerType 
        | HashType hash -> hash.Count |> wrapToIntegerType
        | _ -> ErrorType $"Argument to 'len' not supported, got {object.Type()}" |> Object.ErrorType
        
    let println (object: Object) =
        printf $"{object.Inspect()}\n"
        NullType
    
    let head object =
        match object with
        | ArrayType array ->
            match array with
            | head :: _ -> head
            | [] -> NullType 
        | _ ->
            ErrorType $"Argument to 'head' not supported, got {object.Type()}" |> Object.ErrorType
            
    let tail object =
        match object with
        | ArrayType array ->
            match array with
            | _ :: tail -> ArrayType tail
            | [] -> ArrayType []
        | _ ->
            ErrorType $"Argument to 'tail' not supported, got {object.Type()}" |> Object.ErrorType
            
    // Remark: '@' operator has log(n) complexity
    let push object toPush =
        match object with
        | ArrayType array -> [toPush] @ array |> ArrayType
        | _ -> ErrorType $"Argument to 'push' not supported, got {object.Type()}" |> Object.ErrorType
    

/// Wrapper functions that assert the correct number of parameters have been passed.
[<AutoOpen>]
module private Wrappers =
    let wrap fn =
        let wrappedFn args =
            match args with
            | [ value ] -> fn value
            | _ -> ErrorType $"Wrong number of arguments, expected 1, got {args.Length}" |> Object.ErrorType 
            
        { Fn = wrappedFn; ParametersLength = 1 } |> Function.BuiltinFunction |> FunctionType
        
    let wrap2 fn =
        let wrappedFn args =
            match args with
            | [ arg1; arg2 ] -> fn arg1 arg2 
            | _ -> ErrorType $"Wrong number of arguments, expected 2, got {args.Length}" |> Object.ErrorType 
            
        { Fn = wrappedFn; ParametersLength = 2 } |> Function.BuiltinFunction |> FunctionType
    
    
let builtins = Map.ofList [
    ("len", wrap len)
    ("head", wrap head)
    ("tail", wrap tail)
    ("println", wrap println)
    ("push", wrap2 push)
]