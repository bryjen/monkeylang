module Monkey.Backend.VirtualMachine


open System
open FsToolkit.ErrorHandling

open Microsoft.FSharp.Core
open Monkey.Backend.Frame
open Monkey.Frontend.Eval
open Monkey.Frontend.Eval.Object

open Monkey.Backend.Code
open Monkey.Backend.Helpers
open Monkey.Backend.Compiler
open Monkey.Backend.Operators

let private stackSize = 2048
let private maxFrames = 1024
let private globalsSize = UInt16.MaxValue |> int

let private trueObj = Object.BooleanType true
let private falseObj = Object.BooleanType false
let private nullObj = Object.NullType

let private getBoolObj boolValue = match boolValue with | true -> trueObj | false -> falseObj



type VM =
    { Constants: Object array
      Globals: Object array
      
      Stack: ObjectWrapper array
      StackPointer: int
      
      Frames: Frame array
      FramePointer: int }
    
        
[<RequireQualifiedAccess>]
module VM =
    [<AutoOpen>]
    module internal Utils = 
        let failedPopMsg = "Could not pop stack. Stack is empty."
        
        let inline fromOptionToResult opt = if Option.isSome opt then Ok opt.Value else Error failedPopMsg
        
        let inline toTuple2 a b = (b, a)
        
        [<Obsolete>]
        let getCompiledFunctionFromOption objectOption =
            match objectOption with
            | Some object ->
                match object with
                | Object.CompiledFunctionType compiledFunction -> Ok compiledFunction
                | _ -> Error $"Expected type of object to be \"CompiledFunctionType\", got \"{object.Type()}\"."
            | None -> Error "Stack is empty."
            
        let tryGetCompiledFunction (objectWrapper: ObjectWrapper) =
            if not (isNull objectWrapper) then
                match objectWrapper.Value with
                | Object.CompiledFunctionType compiledFunction -> Ok compiledFunction
                | object -> Error $"Expected type of object to be \"CompiledFunctionType\", got \"{object.Type()}\"."
            else
                Error "Stack is empty."
                
            
        
    
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
            
        let unsafeIndex vm index =
            let objWrapper = vm.Stack[index]
            if not (isNull objWrapper) then
                objWrapper.Value
            else
                failwith "FATAL: 'OpGetLocal' getting a value that doesn't exist."
            
            
    module internal FrameControl =
        let inline currentFrame vm = vm.Frames[vm.FramePointer - 1]
        
        let pushFrame vm newFrame =
            vm.Frames[vm.FramePointer] <- newFrame
            { vm with FramePointer = vm.FramePointer + 1 }
            
        let inline popFrame vm = { vm with FramePointer = vm.FramePointer - 1 }, vm.Frames[vm.FramePointer - 1]
        
        let attachCurrentFrameInsPointer vm =
            let insPointer = vm |> currentFrame |> (_.InsPointer)
            (vm, insPointer)
            
            
            
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
                    
        let rec handleOpCall vm i (byteArr: byte array) = 
            result {
                let noArgs = readUInt8 byteArr[i + 1]
                let! compiledFunction = vm.Stack[vm.StackPointer - 1 - noArgs] |> tryGetCompiledFunction
                do! assertCorrectNumberOfArguments compiledFunction.NumParameters noArgs
                
                // Creating new frame
                let basePtr = vm.StackPointer - noArgs
                let newFrame = Frame.createNewFrame compiledFunction basePtr
                let newVm = FrameControl.pushFrame vm newFrame
                
                // Allocating space on stack for arguments and locals
                let newVm = { newVm with StackPointer = newVm.StackPointer + compiledFunction.NumLocals }
                
                
                return (newVm, -1)  // -1 to step back from ins pointer increment
            }
            
        and assertCorrectNumberOfArguments expectedNoArgs actualNoArgs =
            if expectedNoArgs <> actualNoArgs then
                Error $"Incorrect number of arguments: expected {expectedNoArgs}, got {actualNoArgs}."
            else
                Ok ()
            
        let private attachIncrementedInsPointer vm =
            let currentFrameInsPtr = vm |> FrameControl.currentFrame |> (_.InsPointer)
            (vm, currentFrameInsPtr + 1)
            
        let handleOpReturnValue vm =
            option {
                let! newVm, returnValue = Stack.pop vm
                let newVm, poppedFrame = FrameControl.popFrame newVm
                let cleanedVm = { newVm with StackPointer = poppedFrame.BasePointer - 1 }
                return Stack.push cleanedVm returnValue
            }
            |> function
                | Some newVmResult -> newVmResult >> attachIncrementedInsPointer
                | None -> Error "Stack is empty."
                
        let handleOpReturn vm =
            option {
                let newVm, poppedFrame = FrameControl.popFrame vm 
                let cleanedVm = { newVm with StackPointer = poppedFrame.BasePointer - 1 }
                return Stack.push cleanedVm nullObj 
            }
            |> function
                | Some newVmResult -> newVmResult >> attachIncrementedInsPointer 
                | None -> Error "Stack is empty."
            
            
    let fromByteCode (byteCode: Bytecode) =
        let mainFn: CompiledFunction = { InstructionBytes = byteCode.Instructions.GetBytes()
                                         NumLocals = 0
                                         NumParameters = 0 }
        let mainFrame = Frame.createNewFrame mainFn 0
        
        let frames = Array.zeroCreate<Frame> maxFrames
        frames[0] <- mainFrame
        
        { Constants = byteCode.Constants
          Globals = Array.zeroCreate<Object> globalsSize    // TODO: Check if obj type needs to be wrapped
          
          Stack = Array.zeroCreate<ObjectWrapper> stackSize
          StackPointer = 0
          
          Frames = frames 
          FramePointer = 1 }
            
    let getLastPoppedStackElement vm =
        match vm.StackPointer with
        | sp when sp >= 0 && sp < stackSize ->
            let peek = vm.Stack[sp]
            match System.Object.ReferenceEquals(peek, null) with
            | true -> None 
            | false -> Some peek.Value
        | _ -> None 
            
    let rec run (vm: VM) =
        let bytes = vm |> FrameControl.currentFrame |> (_.Function) |> (_.InstructionBytes)
        runLoop vm
        
    and private runLoop vm : Result<VM, string> =
        let currentFrame = FrameControl.currentFrame vm
        let bytes = currentFrame.Function.InstructionBytes
        let insPointer = currentFrame.InsPointer 
        
        if insPointer < bytes.Length then
            let opcode = LanguagePrimitives.EnumOfValue<byte, Opcode> bytes[insPointer]
            match opcode with
            // opcodes that push to stack
            | Opcode.OpNull ->
                Stack.push vm nullObj
                >> toTuple2 insPointer 
            | Opcode.OpConstant ->
                handleOpConstant vm insPointer bytes
            | Opcode.OpTrue | Opcode.OpFalse ->
                let something = handleBooleanOpcode vm opcode
                something >> toTuple2 insPointer
            | Opcode.OpArray ->
                handleOpArray vm insPointer bytes
            | Opcode.OpHash ->
                handleOpHash vm insPointer bytes
                
            // opcodes that operate
            | Opcode.OpMinus | Opcode.OpBang ->
                handlePrefixOperation vm opcode
                >> toTuple2 insPointer
            | Opcode.OpAdd | Opcode.OpSub | Opcode.OpMul | Opcode.OpDiv | Opcode.OpEqual | Opcode.OpNotEqual | Opcode.OpGreaterThan ->
                handleInfixOperation vm opcode
                >> toTuple2 insPointer
            | Opcode.OpIndex ->
                handleOpIndex vm
                >> toTuple2 insPointer
                
            // jump opcodes 
            | Opcode.OpJump ->
                handleOpJump vm insPointer bytes
            | Opcode.OpJumpWhenFalse ->
                handleOpJumpWhenFalse vm insPointer bytes
                
            // opcodes for functions
            | Opcode.OpCall ->
                handleOpCall vm insPointer bytes
            | Opcode.OpReturnValue ->
                handleOpReturnValue vm
            | Opcode.OpReturn ->
                handleOpReturn vm
                
            // other
            | Opcode.OpPop ->
                match Stack.pop vm with
                | None -> Ok vm 
                | Some (newVm, _) -> Ok newVm 
                >> toTuple2 insPointer
                
            // variables
            | Opcode.OpSetGlobal ->
                handleSetGlobalOpcode vm insPointer bytes
            | Opcode.OpGetGlobal ->
                handleGetGlobalOpcode vm insPointer bytes
                
            | Opcode.OpSetLocal ->
                match Stack.pop vm with
                | Some (newVm, obj) ->
                    let localIndex = bytes[insPointer + 1] |> readUInt8
                    let basePtr = newVm |> FrameControl.currentFrame |> (_.BasePointer)
                    vm.Stack[basePtr + localIndex] <- ObjectWrapper obj
                    Ok (newVm, insPointer + 1)
                | None -> failwith "todo"
                
            | Opcode.OpGetLocal ->
                result {
                    let localIndex = bytes[insPointer + 1] |> readUInt8
                    
                    let basePtr = vm |> FrameControl.currentFrame |> (_.BasePointer)
                    let obj = Stack.unsafeIndex vm (basePtr + localIndex)
                    let! newVm = Stack.push vm obj
                    return (newVm, insPointer + 1)
                }
                
            | _ ->
                Error "unrecognized opcode"
            
            |> function
                | Ok (newVm, newIndex) ->
                    let currentFrame = FrameControl.currentFrame newVm
                    currentFrame.InsPointer <- newIndex + 1
                    runLoop newVm 
                | Error error -> Error error
        else
            Ok vm
