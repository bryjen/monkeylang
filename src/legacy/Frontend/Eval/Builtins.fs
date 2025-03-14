module Monkey.Frontend.Eval.Builtins

open FsToolkit.ErrorHandling
open Microsoft.FSharp.Core
open Monkey.Frontend.Eval.Object

[<AutoOpen>]
module internal Helpers =
    let error errorMsg = errorMsg |> Error 
    
    let someOk a = a |> Some |> Ok
    

[<AutoOpen>]
module private Functions =
    let inline private wrapToIntegerType int = int |> int64 |> Object.IntegerType
    
    (* Result<Object option, string>
        - Ok Some -> Builtin function returns a non-null type object.
        - Ok None -> Builtin function returns a 'null' type. Uses option so that the caller can interpret it how they like.
        - Error -> Builtin function errors with an error message. Uses result so that the caller can interpret it how they like.
     *)
        
    let len object : Result<Object option, string> =
        match object with
        | Object.StringType str -> str.Length |> wrapToIntegerType |> someOk
        | ArrayType array -> array.Length |> wrapToIntegerType |> someOk
        | HashType hash -> hash.Count |> wrapToIntegerType |> someOk
        | _ -> error $"Argument to 'len' not supported, got {object.Type()}"
        
    let println (object: Object) : Result<Object option, string> =
        printf $"{object.Inspect()}\n"
        Ok None
    
    let head object : Result<Object option, string> =
        match object with
        | ArrayType array ->
            match array with
            | [| |] -> Ok None
            | _ -> array[0] |> someOk
        | _ ->
            error $"Argument to 'head' not supported, got {object.Type()}"
            
    let tail object : Result<Object option, string> =
        match object with
        | ArrayType array ->
            match array with
            | [| |] -> object  // return object, since its already an empty array type
            | _ -> array[1..] |> ArrayType
            |> someOk
        | _ ->
            error $"Argument to 'tail' not supported, got {object.Type()}"
            
    let last object : Result<Object option, string> =
        match object with
        | ArrayType array ->
            match array with
            | [| |] -> Ok None
            | _ -> array[array.Length - 1] |> someOk
        | _ ->
            error $"Argument to 'tail' not supported, got {object.Type()}"
            
    let push object toPush : Result<Object option, string> =
        match object with
        | ArrayType array -> (Array.append array [| toPush |]) |> ArrayType |> someOk
        | _ -> error $"Argument to 'push' not supported, got {object.Type()}"
    


/// Wrapper functions that assert the correct number of parameters have been passed.
[<AutoOpen>]
module private Wrappers =
    let wrap fn =
        let wrappedFn args =
            match args with
            | [ value ] -> fn value
            | _ -> error $"Wrong number of arguments, expected 1, got {args.Length}"
            
        { Fn = wrappedFn; NumParameters = 1 } |> Function.BuiltinFunction |> FunctionType
        
    let wrap2 fn =
        let wrappedFn args =
            match args with
            | [ arg1; arg2 ] -> fn arg1 arg2 
            | _ -> error $"Wrong number of arguments, expected 2, got {args.Length}"
            
        { Fn = wrappedFn; NumParameters = 2 } |> Function.BuiltinFunction |> FunctionType
    
let builtinsArray = [|
    ("len", wrap len)
    ("println", wrap println)
    ("head", wrap head)
    ("last", wrap last)
    ("tail", wrap tail)
    ("push", wrap2 push)
|]

let builtins = Map.ofArray builtinsArray
