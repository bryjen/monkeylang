[<AutoOpen>]
module Monkey.Backend.Tests.VirtualMachine.Helpers

open System
open FsToolkit.ErrorHandling
open Monkey.Backend.Tests.Helpers
open Monkey.Frontend.Ast
open Monkey.Frontend.Eval.Object

[<RequireQualifiedAccess>]
module VMHelpers =
    
    [<AutoOpen>]
    module Helpers =
        let hashObjectCast (cast: Object -> Result<'a, string>) (hashableObject: HashableObject) =
            match hashableObject with
            | HashableObject.IntegerType integerType -> Object.IntegerType integerType 
            | HashableObject.BooleanType booleanType -> Object.BooleanType booleanType
            | HashableObject.StringType stringType -> Object.StringType stringType
            |> cast
        
    let assertProgramHasNoErrors (program: Program) =
        if program.Errors.Length = 0 then
            Ok ()
        else
            let errorsListStr = program.Errors |> List.map (fun str -> $"\n- {str}") |> String.concat ""
            Error $"Program couldn't parse with errors:\n{errorsListStr}"
    
    let private assertObjectIsNullType (actual: Object) =
        match actual with
        | NullType -> Ok ()
        | _ -> Error $"'actual' is not a 'NullType', got '{actual.Type()}'"
    
        
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
        
    let private castMapKeys (cast: Object -> Result<'a, string>) (map: Map<HashableObject, 'b>) =
        let updatedMap = map
                         |> Map.toSeq
                         |> Seq.map (fun (key, value) -> (hashObjectCast cast key, value))
                         |> Map.ofSeq
                         
        match updatedMap |> Map.keys |> Seq.tryFind Result.isError with
        | None ->
            let forceGetOk result = match result with | Ok value -> value | Error _ -> failwith "FATAL: Found an error type."
            updatedMap 
            |> Map.toSeq
            |> Seq.map (fun (key, value) -> (forceGetOk key, value))
            |> Map.ofSeq
            |> Ok
        | Some error ->
            error |> Result.map (fun _ -> Map.empty)
        
    let private castMapValues (cast: Object -> Result<'a, string>) (map: Map<'b, Object>) =
        let updatedMap = map |> Map.map (fun _ -> cast)
        
        match updatedMap |> Map.values |> Seq.tryFind Result.isError with
        | None ->
            let forceGetOk result = match result with | Ok value -> value | Error _ -> failwith "FATAL: Found an error type."
            updatedMap |> Map.map (fun _ -> forceGetOk) |> Ok
        | Some error ->
            error |> Result.map (fun _ -> Map.empty)
        
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
            CastEvalObj.toIntegerType actual
            |> Result.bind (assertValuesAreEqual value)
        | :? int as value -> 
            CastEvalObj.toIntegerType actual
            |> Result.bind (assertValuesAreEqual (int64 value))
        | :? bool as value -> 
            CastEvalObj.toBooleanType actual
            |> Result.bind (assertValuesAreEqual value)
        | :? string as value -> 
            CastEvalObj.toStringType actual
            |> Result.bind (assertValuesAreEqual value)
        
        | :? (System.Object array) as _ ->
            Ok () // Todo: fix sm with this
        | :? (int64 array) as expectedArray ->
            CastEvalObj.toArrayType actual
            |> Result.bind (castArrayTypeIntoNativeArray CastEvalObj.toIntegerType)
            |> Result.bind (assertArraysAreEqual expectedArray)
            
        | :? Map<int64, int64> as expectedMap ->
            CastEvalObj.toHashType actual
            |> Result.bind (castMapKeys CastEvalObj.toIntegerType)
            |> Result.bind (castMapValues CastEvalObj.toIntegerType)
            |> Result.bind (assertValuesAreEqual expectedMap)
            
        | :? Map<IComparable, obj> as _ ->
            Ok () // Todo: fix sm with this
        | _ -> failwith $"Test method does not handle expected type \"{expected.GetType()}\""
