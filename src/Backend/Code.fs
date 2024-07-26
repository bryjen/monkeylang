module Monkey.Backend.Code

open Microsoft.FSharp.Reflection


type Instructions = Instructions of byte array

type Opcode =
    | OpConstant = 0x00uy
    | OpAdd      = 0x01uy

type Definition =
    { Name: string
      OperandWidths: int array }

    
let private numberOfCases = FSharpType.GetUnionCases typeof<Opcode> |> (_.Length) |> byte

let private opcodeDefinitions = Map.ofList [
    (Opcode.OpConstant,     { Name = "OpConstant"; OperandWidths = [| 2 |] })
    (Opcode.OpAdd,          { Name = "OpAdd"; OperandWidths = [| |] })
]


let tryCastByteToOpcode (byteValue: byte) : Opcode option =
    if byteValue >= 0x00uy && byteValue < numberOfCases then
        LanguagePrimitives.EnumOfValue byteValue |> Some
    else
        None
    
let lookup byteValue =
    match tryCastByteToOpcode byteValue with
    | Some opcode -> Map.tryFind opcode opcodeDefinitions 
    | None -> None 
