module Monkey.Backend.Code

open System
open Microsoft.FSharp.Reflection


type Instructions = Instructions of byte array

type Opcode =
    | OpConstant = 0x00uy
    | OpAdd      = 0x01uy

type Definition =
    { Name: string
      OperandWidths: int array }

    
let private numberOfCases = Enum.GetValues(typeof<Opcode>).Length |> byte

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

let rec make (opcode: Opcode) (operands: int array) =
    match Map.tryFind opcode opcodeDefinitions with
    | Some definition ->
        let instructionLen = 1 + (Array.sum definition.OperandWidths)
        let instruction = Array.zeroCreate<byte> instructionLen
        instruction[0] <- byte opcode
        
        let mutable offset = 1
        for i in 0 .. (operands.Length - 1) do
            let width = definition.OperandWidths[i]
            let operand = operands[i]
            
            // for possible int32 values, only the first two indices in the converted byte array will have values
            let operandAsBytes = BitConverter.GetBytes(operand)
            
            // 'BitConverter.GetBytes' returns bytes in little endian, we need bytes in big endian notation
            // TODO: See if this differs based on architecture
            let operandAsBytesBE = [| operandAsBytes[1]; operandAsBytes[0] |]
            
            match width with
            | w when w = 2 -> Array.blit operandAsBytesBE 0 instruction offset operandAsBytesBE.Length
            | _ -> failwith "todo"
            offset <- offset + width
        
        instruction
        
    | None ->
        failwith "todo"
    
    
    
and toBytes (value: int) =
    let bytes = BitConverter.GetBytes(value)
    match BitConverter.IsLittleEndian with
    | true -> Array.rev 
    | false -> failwith "todo"