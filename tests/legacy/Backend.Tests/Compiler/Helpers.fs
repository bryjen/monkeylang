[<AutoOpen>]
module Monkey.Backend.Tests.Compiler.Helpers

open System.Diagnostics
open NUnit.Framework
open FsToolkit.ErrorHandling

open Monkey.Backend.Code
open Monkey.Backend.Operators
open Monkey.Frontend.Ast

open Monkey.Frontend.Eval.Object
open Monkey.Backend.Tests.Helpers
open Monkey.Backend.Tests.Code.Helpers



let private getNameOfFunction () =
    let stackTrace = StackTrace()
    let frame = stackTrace.GetFrame(1)  // this func is called by other functions in this module, so we go two 'levels' up
    $"[{frame.GetMethod().Name}]"
    
let programToNodes (program: Program) : Node list =
    let rec helper statements nodes =
        match statements with
        | head :: tail -> helper tail ((Node.Statement head) :: nodes)
        | [ ] -> nodes |> List.rev
        
    helper program.Statements []
    
    

[<RequireQualifiedAccess>]
module Program =
    let assertNumberOfStatements (expectedStatements: int) (program: Program) =
        if program.Statements.Length = expectedStatements
        then Ok ()
        else Error $"{getNameOfFunction ()} \"Program\" was expected to have {expectedStatements} statements, got {program.Statements.Length}."
        
    let assertZeroErrors (program: Program) =
        if program.Errors.Length = 0
        then Ok ()
        else
            let errorsListAsStr = program.Errors |> List.map (fun str -> "- " + str) |> String.concat "\n"
            Error $"{getNameOfFunction ()} \"Program\" was expected to have no errors, got {program.Errors.Length} errors: \n\n{errorsListAsStr}"
            
            
            
[<RequireQualifiedAccess>]
module CompilerHelpers =
    let inline collapseInstructionsArray (instructionsArray: Instructions array) =
        instructionsArray |> Array.map (_.GetBytes()) |> Array.concat |> Instructions
    
    let rec testInstructions (expected: Instructions array) (actual: Instructions) =
        result {
            let expectedBytes = expected |> Array.map (_.GetBytes()) |> Array.concat
            let actualBytes = actual.GetBytes()
            
            let expectedVsActualStr = $"\nExpected:\n{expected |> collapseInstructionsArray |> (_.ToString())}\n\nGot:\n{actual.ToString()}\n"
            
            do! if expectedBytes.Length = actualBytes.Length
                then Ok ()
                else
                    Error $"[Error] Wrong instructions length, expected {expectedBytes.Length}, got {actualBytes.Length}.\n{expectedVsActualStr}."
                
            do! processBytes expectedVsActualStr 0 (Array.zip expectedBytes actualBytes |> Array.toList)
        }
        
    and private processBytes errorMsg currentIndex expectedAndActualPairs =
        match expectedAndActualPairs with
        | (expected, actual) :: tail ->
            let msg = $"Expected {formatByteWithInt expected}, got {formatByteWithInt actual}"
            match expected = actual with
            | true ->
                TestContext.WriteLine($"[Ok] {msg}")
                processBytes errorMsg (currentIndex + 1) tail 
            | false ->
                Error $"[Error] Wrong instruction at index {currentIndex}: {msg}\n{errorMsg}"
        | [] ->
            Ok ()
            
            
    [<AutoOpen>]
    module ConstantPool =
        let private testIntegerObject (currentIndex: int) (expected: int64) (object: Object) =
            match object with
            | Object.IntegerType value ->
                match value = expected with
                | true -> Ok () 
                | false -> Error $"[Error] Object has wrong value at index {currentIndex}. Expected {expected}, got {value}." 
            | _ ->
                Error $"[Error] Object at {currentIndex} is not an \"integer\" type, was {object.Type()}"
                
        let private testStringObject (currentIndex: int) (expected: string) (object: Object) =
            match object with
            | Object.StringType value ->
                match value = expected with
                | true -> Ok () 
                | false -> Error $"[Error] Object has wrong value at index {currentIndex}. Expected {expected}, got {value}." 
            | _ ->
                Error $"[Error] Object at {currentIndex} is not an \"string\" type, was {object.Type()}"
                
                
                
        let rec private processExpectedVsActual currentIndex (expectedAndActualPairs: (obj * Object) list) =
            match expectedAndActualPairs with
            | (expected, actual) :: tail ->
                result {
                    do! 
                        match expected with
                        | :? int as value -> testIntegerObject currentIndex (int64 value) actual
                        | :? int64 as value -> testIntegerObject currentIndex value actual
                        | :? string as value -> testStringObject currentIndex value actual
                        
                        | :? (byte array array) as value ->
                            CastEvalObj.toCompiledFunctionType actual
                            >> (_.InstructionBytes)
                            >> Instructions
                            >>= (testInstructions (Array.map Instructions value))
                        | :? (Instructions array) as value ->
                            CastEvalObj.toCompiledFunctionType actual
                            >> (_.InstructionBytes)
                            >> Instructions
                            >>= (testInstructions value)
                            
                        | _ -> failwith "todo"
                    
                    return! processExpectedVsActual (currentIndex + 1) tail
                }
            | [] ->
                Ok ()
                
        let rec testConstants (expected: obj array) (actual: Object array) =
            result {
                do! if expected.Length = actual.Length
                    then Ok ()
                    else Error $"[Error] Wrong number of constants, expected {expected.Length}, got {actual.Length}."
                    
                do! processExpectedVsActual 0 (Array.zip expected actual |> Array.toList)
            }
