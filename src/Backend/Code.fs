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
    | OpConstant       = 0x00uy
    | OpAdd            = 0x01uy
    | OpPop            = 0x02uy
    
    | OpSub            = 0x03uy
    | OpMul            = 0x04uy
    | OpDiv            = 0x05uy
    
    | OpTrue           = 0x06uy
    | OpFalse          = 0x07uy
    
    | OpEqual          = 0x08uy
    | OpNotEqual       = 0x09uy
    | OpGreaterThan    = 0x0Auy
    
    | OpMinus          = 0x0Buy
    | OpBang           = 0x0Cuy
    
    | OpJumpWhenFalse  = 0x0Duy
    | OpJump           = 0x0Euy
    
    | OpNull           = 0x0Fuy
    
    | OpSetGlobal      = 0x10uy
    | OpGetGlobal      = 0x11uy
    
    | OpArray          = 0x12uy
    | OpHash           = 0x13uy
    | OpIndex          = 0x14uy
    
    | OpCall           = 0x15uy
    | OpReturnValue    = 0x16uy
    | OpReturn         = 0x17uy
    
    | OpSetLocal       = 0x18uy
    | OpGetLocal       = 0x19uy
    
    | OpGetBuiltin     = 0x1Auy
    
    | OpClosure        = 0x1Buy

type Definition =
    { Name: string
      OperandWidths: int array }

    
    
let private numberOfCases = Enum.GetValues(typeof<Opcode>).Length |> byte

let private opcodeDefinitions = Map.ofList [
    (Opcode.OpConstant,      { Name = "OpConstant"; OperandWidths = [| 2 |] })
    (Opcode.OpAdd,           { Name = "OpAdd"; OperandWidths = [| |] })
    (Opcode.OpPop,           { Name = "OpPop"; OperandWidths = [| |] })
    
    (Opcode.OpSub,           { Name = "OpSub"; OperandWidths = [| |] })
    (Opcode.OpMul,           { Name = "OpMul"; OperandWidths = [| |] })
    (Opcode.OpDiv,           { Name = "OpDiv"; OperandWidths = [| |] })
    
    (Opcode.OpTrue,          { Name = "OpTrue"; OperandWidths = [| |] })
    (Opcode.OpFalse,         { Name = "OpFalse"; OperandWidths = [| |] })
    
    (Opcode.OpEqual,         { Name = "OpEqual"; OperandWidths = [| |] })
    (Opcode.OpNotEqual,      { Name = "OpNotEqual"; OperandWidths = [| |] })
    (Opcode.OpGreaterThan,   { Name = "OpGreaterThan"; OperandWidths = [| |] })
    
    (Opcode.OpMinus,         { Name = "OpMinus"; OperandWidths = [| |] })
    (Opcode.OpBang,          { Name = "OpBang"; OperandWidths = [| |] })
    
    (Opcode.OpJumpWhenFalse, { Name = "OpJumpWhenFalse"; OperandWidths = [| 2 |] })
    (Opcode.OpJump,          { Name = "OpJump"; OperandWidths = [| 2 |] })
    
    (Opcode.OpNull,          { Name = "OpNull"; OperandWidths = [| |] })
    
    (Opcode.OpSetGlobal,     { Name = "OpSetGlobal"; OperandWidths = [| 2 |] })
    (Opcode.OpGetGlobal,     { Name = "OpGetGlobal"; OperandWidths = [| 2 |] })
    
    (Opcode.OpArray,         { Name = "OpArray"; OperandWidths = [| 2 |] })
    (Opcode.OpHash,          { Name = "OpHash"; OperandWidths = [| 2 |] })
    (Opcode.OpIndex,         { Name = "OpIndex"; OperandWidths = [| |] })
    
    (Opcode.OpCall,          { Name = "OpCall"; OperandWidths = [| 1 |] })
    (Opcode.OpReturnValue,   { Name = "OpReturnValue"; OperandWidths = [| |] })
    (Opcode.OpReturn,        { Name = "OpReturn"; OperandWidths = [| |] })
    
    (Opcode.OpSetLocal,      { Name = "OpSetLocal"; OperandWidths = [| 1 |] })
    (Opcode.OpGetLocal,      { Name = "OpGetLocal"; OperandWidths = [| 1 |] })
    
    (Opcode.OpGetBuiltin,    { Name = "OpGetBuiltin"; OperandWidths = [| 1 |] })
    
    (Opcode.OpClosure,    { Name = "OpClosure"; OperandWidths = [| 2; 1 |] })
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
            | w when w = 1 ->
                instruction[offset] <- byte operand 
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
        | w when w = 1 ->
            operands[i] <- byteArray[offset] |> readUInt8 |> int
        | w when w = 2 ->
            operands[i] <- byteArray[offset..(offset + width - 1)] |> readUInt16 |> int
        | _ ->
            failwith "todo"
            
        offset <- offset + width

    operands
    
and readUInt16 byteArr =
    let reversedByteArr = Array.rev byteArr
    BitConverter.ToUInt16(reversedByteArr, 0)
    
and readUInt8 byteArr = int byteArr
    