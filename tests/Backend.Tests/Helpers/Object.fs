namespace Monkey.Backend.Tests.Helpers

open Monkey.Frontend.Eval.Object


[<RequireQualifiedAccess>]
module CastEvalObj =
    let toIntegerType object =
        match object with
        | Object.IntegerType intType -> Ok intType
        | _ -> Error $"'object' is not an 'Object.IntegerType', got '{object.Type()}'."
        
    let toBooleanType object =
        match object with
        | Object.BooleanType boolType -> Ok boolType 
        | _ -> Error $"'object' is not an 'Object.BooleanType', got '{object.Type()}'."
        
    let toStringType object =
        match object with
        | Object.StringType stringType -> Ok stringType 
        | _ -> Error $"'object' is not an 'Object.StringType', got '{object.Type()}'."
        
    let toArrayType object =
        match object with
        | Object.ArrayType arrayType -> Ok arrayType 
        | _ -> Error $"'object' is not an 'Object.ArrayType', got '{object.Type()}'."
        
    let toHashType object =
        match object with
        | Object.HashType hashType -> Ok hashType 
        | _ -> Error $"'object' is not an 'Object.HashType', got '{object.Type()}'."
        
    let toCompiledFunctionType object =
        match object with
        | Object.CompiledFunctionType compiledFunction -> Ok compiledFunction 
        | _ -> Error $"'object' is not an 'Object.CompiledFunctionType', got '{object.Type()}'."
        
