module rec Monkey.Backend.Code

open System
open FsToolkit.ErrorHandling


type Instructions = Instructions of byte array
with
    member this.GetBytes() =
        match this with
        | Instructions bytes -> bytes
        
    override this.ToString() =
        match unmake this with
        | Ok tupleList ->
            let formatTuple (offset, definition, operands) =
                let operandsAsString = operands |> Array.map string |> String.concat " "
                $"%04d{offset} {definition.Name} {operandsAsString}".Trim()
                
            tupleList |> List.map formatTuple |> String.concat "\n"
        | Error errorValue ->
            $"Error: Could not represent the instructions as a str: \"{errorValue}\""


type Opcode =
    | OpConstant = 0x00uy
    | OpAdd      = 0x01uy
    | OpPop      = 0x02uy

type Definition =
    { Name: string
      OperandWidths: int array }

    
    
let private numberOfCases = Enum.GetValues(typeof<Opcode>).Length |> byte

let private opcodeDefinitions = Map.ofList [
    (Opcode.OpConstant,     { Name = "OpConstant"; OperandWidths = [| 2 |] })
    (Opcode.OpAdd,          { Name = "OpAdd"; OperandWidths = [| |] })
    (Opcode.OpPop,          { Name = "OpPop"; OperandWidths = [| |] })
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
            | w when w = 2 ->
                Array.blit operandAsBytesBE 0 instruction offset operandAsBytesBE.Length
            | _ -> failwith "todo"
            offset <- offset + width
        
        instruction
        
    | None ->
        failwith "todo"
    
    
let rec unmake (instruction: Instructions) : Result<(int * Definition * int array) list, string> =
    result {
        let byteArray = instruction.GetBytes()
        let! offsetAndDefinitionPairs = getOffsetAndDefinitionPairs 0 byteArray []
        let operands = List.map (fun (offset, def) -> readOperands byteArray def (offset + 1)) offsetAndDefinitionPairs
        
        let offsets, definitions = List.unzip offsetAndDefinitionPairs
        return List.zip3 offsets definitions operands
    }
    
and private getOffsetAndDefinitionPairs (currentIndex: int) (byteArray: byte array) (pairs: (int * Definition) list) =
    match currentIndex with
    | i when i >= 0 && i < byteArray.Length ->
        match lookup byteArray[i] with
        | Some definition ->
            let totalWidth = Array.sum definition.OperandWidths
            let newPairs = (currentIndex, definition) :: pairs
            getOffsetAndDefinitionPairs (currentIndex + totalWidth + 1) byteArray newPairs 
        | None ->
            Error $"Error: Could not find an opcode that corresponds with the byte: \"{byteArray[0]}\""
    
    | i ->
        if i = byteArray.Length then
            pairs |> List.rev |> Ok
        else
            failwith "todo"
    
and readOperands byteArray definition initialOffset : int array =
    let operands = Array.zeroCreate<int> definition.OperandWidths.Length
    let mutable offset = initialOffset 
    
    for i in 0 .. (operands.Length - 1) do
        let width = definition.OperandWidths[i]
        match width with
        | w when w = 0 ->
            ()
        | w when w = 2 ->
            operands[i] <- byteArray[offset..(offset + width - 1)] |> readUInt16 |> int
        | _ ->
            failwith "todo"
            
        offset <- offset + width

    operands
    
and readUInt16 byteArr =
    let reversedByteArr = Array.rev byteArr
    BitConverter.ToUInt16(reversedByteArr, 0)
