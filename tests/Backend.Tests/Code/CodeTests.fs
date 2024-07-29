namespace Monkey.Backend.Tests.Code

open System
open NUnit.Framework
open FsToolkit.ErrorHandling

open Monkey.Backend.Code
open Monkey.Backend.Tests.Code.Helpers


[<TestFixture>]
type OpcodeTests() =
    
    [<TestCase(0x00uy, Opcode.OpConstant)>]
    [<TestCase(0x01uy, Opcode.OpAdd)>]
    member this.``A: Test byte to opcode cast``(byteValue: byte, expectedOpcode: Opcode) =
        match tryCastByteToOpcode byteValue with
        | Some opcode ->
            let msg = $"Expected \"{getEnumCaseName expectedOpcode}\"({expectedOpcode |> byte |> formatByte}), got \"{getEnumCaseName opcode}\"({opcode |> byte |> formatByte})."
            if opcode = expectedOpcode then Assert.Pass($"[Pass] {msg}") else Assert.Fail($"[Fail] {msg}")
        | None ->
            Assert.Fail($"[Fail] Expected \"{getEnumCaseName expectedOpcode}\"({byte expectedOpcode}), got nothing.") 
            
            
    member private this.``B: Test cases`` = [
        (Opcode.OpConstant, [| 65534 |], [| byte Opcode.OpConstant; 255uy; 254uy |])
        (Opcode.OpAdd, [| |], [| byte Opcode.OpAdd |])
    ]
            
    [<TestCase(0)>]
    [<TestCase(1)>]
    member this.``B: Test 'make'``(testCaseIndex: int) =
        let opcode, operands, expectedBytes = List.item testCaseIndex this.``B: Test cases``
        let expectedItemsStr = expectedBytes |> Array.map formatByteWithInt |> String.concat ", "
        TestContext.WriteLine($"Expected:\n\t[| {expectedItemsStr} |]")
        
        result {
            let instruction = make opcode operands
            let actualItemsStr = instruction |> Array.map formatByteWithInt |> String.concat ", "
            TestContext.WriteLine($"Expected:\n\t[| {actualItemsStr} |]\n")
            
            do! if instruction.Length <> expectedBytes.Length
                then Error $"Instruction has wrong length, expected {expectedBytes.Length}, got {instruction.Length}"
                else Ok ()
                
            let expectedActualPairsWithNumber = Array.zip3 [| 0..(instruction.Length - 1) |] expectedBytes instruction 
            for (index, expected, actual) in expectedActualPairsWithNumber do
                do! if (expected <> actual)
                    then
                        let errorMsg = $"[Error] Error at index {index}, expected {expected |> formatByteWithInt}, got {actual |> formatByteWithInt}."
                        TestContext.WriteLine(errorMsg)
                        Error errorMsg
                    else
                        TestContext.WriteLine($"[Ok] Index {index}: expected {expected |> formatByteWithInt}, got {actual |> formatByteWithInt}.") |> Ok
        }
        |> function
           | Ok _ -> Assert.Pass("Test passed.\n\n")
           | Error errorMsg -> Assert.Fail(errorMsg)