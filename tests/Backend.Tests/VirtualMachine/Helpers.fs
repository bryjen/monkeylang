[<AutoOpen>]
module Monkey.Backend.Tests.VirtualMachine.Helpers

open System
open FsToolkit.ErrorHandling
open Monkey.Frontend.Eval.Object

[<RequireQualifiedAccess>]
module VMHelpers =
    
    let private assertObjectIsNullType (actual: Object) =
        match actual with
        | NullType -> Ok ()
        | _ -> Error $"'actual' is not a 'NullType', got '{actual.Type()}'"
    
    let private castEvalObjIntoIntegerType object =
        match object with
        | Object.IntegerType intType -> Ok intType
        | _ -> Error $"'object' is not an 'Object.IntegerType', got '{object.Type()}'."
        
    let private castEvalObjIntoBoolType object =
        match object with
        | Object.BooleanType boolType -> Ok boolType 
        | _ -> Error $"'object' is not an 'Object.BooleanType', got '{object.Type()}'."
        
    let private castEvalObjIntoStringType object =
        match object with
        | Object.StringType stringType -> Ok stringType 
        | _ -> Error $"'object' is not an 'Object.StringType', got '{object.Type()}'."
        
    let private castEvalObjIntoArrayType object =
        match object with
        | Object.ArrayType arrayType -> Ok arrayType 
        | _ -> Error $"'object' is not an 'Object.ArrayType', got '{object.Type()}'."
        
    let private castArrayTypeIntoNativeArray (cast: Object -> Result<'a, string>) (objectArray: Object array) =
        let nativeArray = Array.zeroCreate<'a> objectArray.Length
        
        let rec helper (currentIndex: int) =
            match currentIndex with
            | i when i >= 0 && i < objectArray.Length ->
                match cast objectArray[currentIndex] with
                | Ok value ->
                    nativeArray[i] <- value
                    helper (currentIndex + 1)
                | Error error -> Error error 
            | _ ->
                Ok nativeArray
                
        helper 0
        
    let private assertValuesAreEqual expected actual =
        if actual = expected
        then Ok ()
        else Error $"'actual' has wrong value, expected {expected}, but got {actual}."
        
    let private assertArraysAreEqual expectedArray actualArray =
        if Array.length expectedArray = Array.length actualArray && Array.forall2 (=) expectedArray actualArray
        then Ok ()
        else Error $"'actualArray' has a wrong value, expected \"{expectedArray}\", got \"{actualArray}\""
        
        
        
        
    let rec testExpectedObject (expected: obj) (actual: Object) =
        match expected with
        | null -> assertObjectIsNullType actual
        | :? int64 as value ->
            castEvalObjIntoIntegerType actual
            |> Result.bind (assertValuesAreEqual value)
        | :? int as value -> 
            castEvalObjIntoIntegerType actual
            |> Result.bind (assertValuesAreEqual (int64 value))
        | :? bool as value -> 
            castEvalObjIntoBoolType actual
            |> Result.bind (assertValuesAreEqual value)
        | :? string as value -> 
            castEvalObjIntoStringType actual
            |> Result.bind (assertValuesAreEqual value)
        
        | :? (System.Object array) as _ ->
            Ok () // Todo: fix sm with this
        | :? (int64 array) as expectedArray ->
            castEvalObjIntoArrayType actual
            |> Result.map Array.ofList
            |> Result.bind (castArrayTypeIntoNativeArray castEvalObjIntoIntegerType)
            |> Result.bind (assertArraysAreEqual expectedArray)
        | _ -> failwith $"Test method does not handle expected type \"{expected.GetType()}\""