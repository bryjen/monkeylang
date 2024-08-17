module Monkey.Backend.VirtualMachine


open System
open FsToolkit.ErrorHandling

open Microsoft.FSharp.Core
open Monkey.Frontend.Eval
open Monkey.Frontend.Eval.Object

open Monkey.Backend.Code
open Monkey.Backend.Helpers
open Monkey.Backend.Compiler
open Monkey.Backend.Operators

let private stackSize = 2048
let private globalsSize = UInt16.MaxValue |> int

let private trueObj = Object.BooleanType true
let private falseObj = Object.BooleanType false
let private nullObj = Object.NullType

let private getBoolObj boolValue = match boolValue with | true -> trueObj | false -> falseObj



type VM =
    { Constants: Object array
      Instructions: Instructions
      
      Globals: Object array
      Stack: ObjectWrapper array
      StackPointer: int }
    
        
[<RequireQualifiedAccess>]
module VM =
    [<AutoOpen>]
    module internal Utils = 
        let failedPopMsg = "Could not pop stack. Stack is empty."
        
        let fromOptionToResult opt = if Option.isSome opt then Ok opt.Value else Error failedPopMsg
        
        let toTuple2 a b = (b, a)
        
    
    module internal Stack =
        
        let push vm object =
            if vm.StackPointer >= stackSize then
                Error "Stack Overflow"
            else
                vm.Stack[vm.StackPointer] <- ObjectWrapper object
                Ok { vm with StackPointer = vm.StackPointer + 1 }
                
        let peek vm =
            if vm.StackPointer >= stackSize then
                None
            else
                let onStack = vm.Stack[vm.StackPointer - 1]
                if not (isNull onStack) then Some onStack.Value else None
                     
        let pop vm =
            match peek vm with
            | Some value -> Some ({ vm with StackPointer = vm.StackPointer - 1 }, value)  
            | None -> None
            
            
    [<AutoOpen>]
    module Opcode =
        let handleOpConstant (vm: VM) (i: int) (byteArr: byte array) =
            let constPoolIndex = readUInt16 byteArr[i + 1 .. i + 2]
            Stack.push vm vm.Constants[int constPoolIndex]
            >> toTuple2 (i + 2)
            
            
        let handlePrefixOperation (vm: VM) (operatorOpcode: Opcode) =
            result {
                let! newVm, expr = vm |> Stack.pop |> fromOptionToResult
                
                let! result =
                    match operatorOpcode, expr with
                    | Opcode.OpMinus, Object.IntegerType i -> -i |> Object.IntegerType |> Ok
                    | Opcode.OpBang, Object.BooleanType b -> b |> not |> getBoolObj |> Ok
                    | _ -> Error $"The operation [opcode: {operatorOpcode}, value: {expr}] is not valid."
                    
                return! Stack.push newVm result
            }
            
            
        let handleInfixOperation (vm: VM) (operatorOpcode: Opcode) =
            result {
                let! newVm, right = Stack.pop vm |> fromOptionToResult
                let! newVm, left = Stack.pop newVm |> fromOptionToResult
                
                let! result =
                    match operatorOpcode, left, right with
                    | Opcode.OpAdd, Object.IntegerType l, Object.IntegerType r -> (l + r) |> Object.IntegerType |> Ok 
                    | Opcode.OpAdd, Object.StringType l, Object.StringType r -> $"{l}{r}" |> Object.StringType |> Ok
                    | Opcode.OpAdd, Object.IntegerType l, Object.StringType r -> $"{l}{r}" |> Object.StringType |> Ok
                    | Opcode.OpAdd, Object.StringType l, Object.IntegerType r -> $"{l}{r}" |> Object.StringType |> Ok
                    | Opcode.OpSub, Object.IntegerType l, Object.IntegerType r -> (l - r) |> Object.IntegerType |> Ok
                    | Opcode.OpMul, Object.IntegerType l, Object.IntegerType r -> (l * r) |> Object.IntegerType |> Ok
                    | Opcode.OpDiv, Object.IntegerType l, Object.IntegerType r -> (l / r) |> Object.IntegerType |> Ok
                    | Opcode.OpEqual, Object.IntegerType l, Object.IntegerType r -> l = r |> getBoolObj |> Ok 
                    | Opcode.OpEqual, Object.BooleanType l, Object.BooleanType r -> l = r |> getBoolObj |> Ok
                    | Opcode.OpNotEqual, Object.IntegerType l, Object.IntegerType r -> l <> r |> getBoolObj |> Ok 
                    | Opcode.OpNotEqual, Object.BooleanType l, Object.BooleanType r -> l <> r |> getBoolObj |> Ok
                    | Opcode.OpGreaterThan, Object.IntegerType l, Object.IntegerType r -> l > r |> getBoolObj |> Ok 
                    | _ -> Error $"The operation left: \"{left.Type()}\" right: \"{right.Type()}\" opcode: {operatorOpcode} is not valid."
                
                return! Stack.push newVm result
            }
            
            
        let handleBooleanOpcode (vm: VM) (opcode: Opcode) =
            match opcode with
            | Opcode.OpTrue -> Stack.push vm trueObj
            | Opcode.OpFalse -> Stack.push vm falseObj 
            | _ -> Error $"Fatal: The \"{opcode}\" does not represent a boolean."
            
            
        let handleSetGlobalOpcode (vm: VM) (i: int) (byteArr: byte array) =
            let arraySlice = byteArr[i + 1 .. i + 2]
            let globalIndex = arraySlice |> readUInt16 |> int
            
            Stack.pop vm
            |> Option.map (fun (newVm, object) -> vm.Globals[globalIndex] <- object; newVm)
            |> function
               | Some newVm -> Ok (newVm, i + 2)
               | None -> failwith "todo"
               
               
        let handleGetGlobalOpcode (vm: VM) (i: int) (byteArr: byte array) =
            let arraySlice = byteArr[i + 1 .. i + 2]
            let globalIndex = arraySlice |> readUInt16 |> int
            
            Stack.push vm vm.Globals[globalIndex]
            >> toTuple2 (i + 2)
            
            
        let rec handleOpArray (vm: VM) (i: int) (byteArr: byte array) =
            let arraySlice = byteArr[i + 1 .. i + 2]
            let numElements = arraySlice |> readUInt16 |> int
            
            let array = buildArray vm (vm.StackPointer - numElements) (vm.StackPointer)
            let newVm = { vm with StackPointer = vm.StackPointer - numElements }
            Stack.push newVm array
            >> toTuple2 (i + 2)
            
        and buildArray (vm: VM) (startIndex: int) (endIndex: int) =
            let elements = Array.zeroCreate<Object> (endIndex - startIndex)
            for i in startIndex .. (endIndex - 1) do
                elements[i - startIndex] <- vm.Stack[i].Value  // TODO: Straight dereferencing, see if this is a bad idea
            elements |> ArrayType
            
            
        let rec handleOpHash (vm: VM) (i: int) (byteArr: byte array) =
            let arraySlice = byteArr[i + 1 .. i + 2]
            let numElements = arraySlice |> readUInt16 |> int
            
            let hash = buildHash vm (vm.StackPointer - numElements) (numElements / 2)
            let newVm = { vm with StackPointer = vm.StackPointer - numElements }
            Stack.push newVm hash >> toTuple2 (i + 2)
            
        and buildHash (vm: VM) (startIndex: int) (numPairs: int) =
            let pairs = Array.zeroCreate<HashableObject * Object> numPairs
            
            let mutable currentPair = 0
            while currentPair < numPairs do
                let i = startIndex + (2 * currentPair)
                let key = vm.Stack[i].Value |> HashableObject.FromObject |> Option.get  // TODO: See if this is safe
                let value = vm.Stack[i + 1].Value
                
                pairs[currentPair] <- (key, value)
                currentPair <- currentPair + 1
                
            Map.ofArray pairs |> HashType
            
            
        let rec handleOpIndex (vm: VM) =
            option {
                let! newVm, index = Stack.pop vm
                let! newVm, left = Stack.pop newVm 
                return newVm, left, index 
            }
            |> function
               | Some (vm, left, index) ->
                   match left with
                   | ArrayType objects -> indexArrayType objects index
                   | HashType hash -> indexHashType hash index
                   | _ -> Error "Attempting to index a non-indexable type."
                   >>= (Stack.push vm) 
               | None ->
                   Error "Stack top was empty while trying to get 'index' or 'left'."
        
        and indexArrayType (objects: Object array) (index: Object) =
            match index with
            | Object.IntegerType i when i >= 0 && i < objects.Length ->
                objects[int i] |> Ok
            | Object.IntegerType _ ->
                NullType |> Ok
            | _ ->
                Error $"Expected index to be an \"IntegerType\", got \"{index.Type()}\""
            
        and indexHashType (hash: Map<HashableObject, Object>) (index: Object) =
            match HashableObject.FromObject index with
            | Some key ->
                match Map.tryFind key hash with
                | None -> Ok NullType 
                | Some value -> Ok value 
            | None ->
                Error $"Index \"{index}\" is not hashable"
                
                
        let handleOpJump (vm: VM) (i: int) (byteArr: byte array) =
            let arraySlice = byteArr[i + 1 .. i + 2]
            let indexToJumpTo = arraySlice |> readUInt16 |> int
            Ok (vm, indexToJumpTo - 1)  // -1 to step back from callback
            
        let handleOpJumpWhenFalse (vm: VM) (i: int) (byteArr: byte array) =
            let arraySlice = byteArr[i + 1 .. i + 2]
            let indexToJumpTo = arraySlice |> readUInt16 |> int
            
            match Stack.peek vm with
            | None -> Error "'OpJumpWhenFalse' invalid, stack is empty." 
            | Some obj ->
                match obj with
                | Object.BooleanType bool when bool = false ->
                    Ok (vm, indexToJumpTo - 1)  // -1 to step back from callback
                | Object.BooleanType _ ->
                    Ok (vm, i + 2) 
                | _ ->
                    Error $"Expected a boolean object at the stack top, got {obj.Type()}."
            
            
    let fromByteCode (byteCode: Bytecode) =
        { Constants = byteCode.Constants
          Instructions = byteCode.Instructions
          
          Globals = Array.zeroCreate<Object> globalsSize    // TODO: Check if obj type needs to be wrapped
          Stack = Array.zeroCreate<ObjectWrapper> stackSize
          StackPointer = 0 }
            
    let getLastPoppedStackElement vm =
        match vm.StackPointer with
        | sp when sp >= 0 && sp < stackSize ->
            let peek = vm.Stack[sp]
            match System.Object.ReferenceEquals(peek, null) with
            | true -> None 
            | false -> Some peek.Value
        | _ -> None 
            
    let rec run vm =
        let bytes = vm.Instructions.GetBytes()
        runLoop bytes vm 0
        >> fst
        
    and private runLoop (bytes: byte array) vm currentIndex : Result<VM * int, string> =
        let callback i = i + 1 
        
        if currentIndex < bytes.Length then
            let opcode = LanguagePrimitives.EnumOfValue<byte, Opcode> bytes[currentIndex]
            match opcode with
            // opcodes that push to stack
            | Opcode.OpNull ->
                Stack.push vm nullObj
                >> toTuple2 currentIndex
            | Opcode.OpConstant ->
                handleOpConstant vm currentIndex bytes
            | Opcode.OpGetGlobal ->
                handleGetGlobalOpcode vm currentIndex bytes
            | Opcode.OpTrue | Opcode.OpFalse ->
                let something = handleBooleanOpcode vm opcode
                something >> toTuple2 currentIndex
            | Opcode.OpArray ->
                handleOpArray vm currentIndex bytes
            | Opcode.OpHash ->
                handleOpHash vm currentIndex bytes
                
            // opcodes that operate
            | Opcode.OpMinus | Opcode.OpBang ->
                handlePrefixOperation vm opcode
                >> toTuple2 currentIndex
            | Opcode.OpAdd | Opcode.OpSub | Opcode.OpMul | Opcode.OpDiv | Opcode.OpEqual | Opcode.OpNotEqual | Opcode.OpGreaterThan ->
                handleInfixOperation vm opcode
                >> toTuple2 currentIndex
            | Opcode.OpIndex ->
                handleOpIndex vm
                >> toTuple2 currentIndex
                
            // jump opcodes 
            | Opcode.OpJump ->
                handleOpJump vm currentIndex bytes
            | Opcode.OpJumpWhenFalse ->
                handleOpJumpWhenFalse vm currentIndex bytes
                
            // other
            | Opcode.OpPop ->
                match Stack.pop vm with
                | None -> Ok vm 
                | Some (newVm, _) -> Ok newVm 
                >> toTuple2 currentIndex
            | Opcode.OpSetGlobal ->
                handleSetGlobalOpcode vm currentIndex bytes
                
            | _ ->
                Error "unrecognized opcode"
            
            |> function
                | Ok (newVm, newIndex) -> runLoop bytes newVm (callback newIndex)
                | Error error -> Error error
        else
            Ok (vm, currentIndex) 
