module Monkey.Backend.VirtualMachine


open System
open FsToolkit.ErrorHandling
open Monkey.Backend.Code
open Monkey.Backend.Compiler
open Monkey.Backend.Helpers
open Monkey.Frontend.Eval
open Monkey.Frontend.Eval.Object

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
      mutable StackPointer: int }
with
    static member FromByteCode(byteCode: Bytecode) =
        { Constants = byteCode.Constants
          Instructions = byteCode.Instructions
          
          Globals = Array.zeroCreate<Object> globalsSize    // TODO: Check if obj type needs to be wrapped
          Stack = Array.zeroCreate<ObjectWrapper> stackSize
          StackPointer = 0 }
        
    static member private FailedPopMessage = "Could not pop stack. Stack is empty."
        

    member internal this.LastPoppedStackElement() =
        match this.StackPointer with
        | sp when sp >= 0 && sp < stackSize ->
            let peek = this.Stack[sp]
            match System.Object.ReferenceEquals(peek, null) with
            | true -> None 
            | false -> Some peek.Value
        | _ -> None
            
    static member Run(vm: VM) : Result<VM, string> =
        let asByteArr = vm.Instructions.GetBytes()
        let mutable i = 0
        
        // Callback func called whenever a successful instruction slice has been processed
        let callback () = i <- i + 1
        
        let rec runHelper (_vm: VM) : Result<VM, string> =
            if i < asByteArr.Length then
                let opcode = LanguagePrimitives.EnumOfValue<byte, Opcode> asByteArr[i] 
                match opcode with
                | Opcode.OpNull ->
                    _vm.Push(nullObj)
                | Opcode.OpConstant ->
                    _vm.HandleOpConstant(&i, asByteArr)
                | Opcode.OpMinus | Opcode.OpBang ->
                    _vm.HandlePrefixOperation(&i, opcode)
                | Opcode.OpAdd | Opcode.OpSub | Opcode.OpMul | Opcode.OpDiv | Opcode.OpEqual | Opcode.OpNotEqual | Opcode.OpGreaterThan ->
                    _vm.HandleInfixOperation(&i, opcode)
                | Opcode.OpTrue | Opcode.OpFalse ->
                    _vm.HandleBooleanOpcode(&i, opcode)
                | Opcode.OpPop ->
                    match _vm.Pop() with
                    | None -> Ok _vm 
                    | Some (newVm, _) -> Ok newVm
                | Opcode.OpSetGlobal ->
                    _vm.HandleSetGlobalOpcode(&i, asByteArr)
                | Opcode.OpGetGlobal ->
                    _vm.HandleGetGlobalOpcode(&i, asByteArr)
                | Opcode.OpJumpWhenFalse ->
                    let arraySlice = asByteArr[i + 1 .. i + 2]
                    let indexToJumpTo = arraySlice |> readUInt16 |> int
                    
                    match _vm.Peek() with
                    | None -> failwith "todo"
                    | Some obj ->
                        match obj with
                        | Object.BooleanType bool when bool = false ->
                            i <- indexToJumpTo - 1  // -1 to step back from callback
                            Ok _vm
                        | Object.BooleanType _ ->
                            i <- i + 2 
                            Ok _vm
                        | _ ->
                            Error $"Expected a boolean object at the stack top, got {obj.Type()}."
                | Opcode.OpJump -> 
                    let arraySlice = asByteArr[i + 1 .. i + 2]
                    let indexToJumpTo = arraySlice |> readUInt16 |> int
                    i <- indexToJumpTo - 1  // -1 to step back from callback
                    Ok _vm
                | _ ->
                    failwith "unrecognized opcode"
                |> Result.map (fun newVm -> callback (); newVm) // Call the callback
                |> Result.bind runHelper
            else
                Ok _vm
            
        runHelper vm
        
        
    member private this.HandleOpConstant(i: byref<int>, byteArr: byte array) : Result<VM, string> =
        let arraySlice = byteArr[i + 1 .. i + 2]
        let constIndex = readUInt16 arraySlice
        i <- i + 2
        this.Push(this.Constants[int constIndex])
        
    member private this.HandlePrefixOperation(i: byref<int>, operatorOpcode: Opcode) : Result<VM, string> =
        result {
            let fromOptionToResult opt = if Option.isSome opt then Ok opt.Value else Error VM.FailedPopMessage
            let! newVm, expr = this.Pop() |> fromOptionToResult
            
            let! result =
                match operatorOpcode, expr with
                | Opcode.OpMinus, Object.IntegerType i -> -i |> Object.IntegerType |> Ok
                | Opcode.OpBang, Object.BooleanType b -> b |> not |> getBoolObj |> Ok
                | _ -> Error $"The operation [opcode: {operatorOpcode}, value: {expr}] is not valid."
                
            return! newVm.Push(result)
        }
        
    member private this.HandleInfixOperation(i: byref<int>, operatorOpcode: Opcode) : Result<VM, string> =
        result {
            let fromOptionToResult opt = if Option.isSome opt then Ok opt.Value else Error VM.FailedPopMessage
            let! newVm, right = this.Pop() |> fromOptionToResult
            let! newVm, left = newVm.Pop() |> fromOptionToResult
            
            let! result =
                match operatorOpcode, left, right with
                | Opcode.OpAdd, Object.IntegerType l, Object.IntegerType r -> (l + r) |> Object.IntegerType |> Ok 
                | Opcode.OpAdd, Object.StringType l, Object.StringType r -> failwith "todo"
                | Opcode.OpAdd, Object.IntegerType l, Object.StringType r -> failwith "todo"
                | Opcode.OpAdd, Object.StringType l, Object.IntegerType r -> failwith "todo"
                | Opcode.OpSub, Object.IntegerType l, Object.IntegerType r -> (l - r) |> Object.IntegerType |> Ok
                | Opcode.OpMul, Object.IntegerType l, Object.IntegerType r -> (l * r) |> Object.IntegerType |> Ok
                | Opcode.OpDiv, Object.IntegerType l, Object.IntegerType r -> (l / r) |> Object.IntegerType |> Ok
                | Opcode.OpEqual, Object.IntegerType l, Object.IntegerType r -> l = r |> getBoolObj |> Ok 
                | Opcode.OpEqual, Object.BooleanType l, Object.BooleanType r -> l = r |> getBoolObj |> Ok
                | Opcode.OpNotEqual, Object.IntegerType l, Object.IntegerType r -> l <> r |> getBoolObj |> Ok 
                | Opcode.OpNotEqual, Object.BooleanType l, Object.BooleanType r -> l <> r |> getBoolObj |> Ok
                | Opcode.OpGreaterThan, Object.IntegerType l, Object.IntegerType r -> l > r |> getBoolObj |> Ok 
                | _ -> Error $"The operation left: \"{left.Type()}\" right: \"{right.Type()}\" opcode: {operatorOpcode} is not valid."
            
            return! newVm.Push(result)
        }
        
    member private this.HandleBooleanOpcode(i: byref<int>, booleanOpcode: Opcode) : Result<VM, string> =
        match booleanOpcode with
        | Opcode.OpTrue -> this.Push(trueObj) 
        | Opcode.OpFalse -> this.Push(falseObj) 
        | _ -> Error $"Fatal: The \"{booleanOpcode}\" does not represent a boolean."
        
    member private this.HandleSetGlobalOpcode(i: byref<int>, byteArr: byte array) : Result<VM, string> =
        let arraySlice = byteArr[i + 1 .. i + 2]
        let globalIndex = arraySlice |> readUInt16 |> int
        i <- i + 2
        
        this.Pop()
        |> Option.map (fun (newVm, object) -> this.Globals[globalIndex] <- object; newVm)
        |> function
           | Some newVm -> Ok newVm
           | None -> failwith "todo" 
        
    member private this.HandleGetGlobalOpcode(i: byref<int>, byteArr: byte array) : Result<VM, string> =
        let arraySlice = byteArr[i + 1 .. i + 2]
        let globalIndex = arraySlice |> readUInt16 |> int
        i <- i + 2
        this.Push(this.Globals[globalIndex])
        
    member private this.Push(object: Object) : Result<VM, string> =
        match this.StackPointer >= stackSize with
        | true ->
            Error "Stack Overflow"
        | false ->
            this.Stack[this.StackPointer] <- ObjectWrapper object
            this.StackPointer <- this.StackPointer + 1
            Ok this
            
    member private this.Peek() : Object option =
        match this.StackPointer >= stackSize with
        | true -> None
        | false ->
            let i = this.StackPointer - 1
            let objectWrapper = this.Stack[i]
            
            match isNull objectWrapper with
            | true -> None 
            | false ->
                Some objectWrapper.Value
            
    member private this.Pop() : (VM * Object) option =
        match this.StackPointer >= stackSize with
        | true -> None
        | false ->
            let i = this.StackPointer - 1
            let objectWrapper = this.Stack[i]
            // this.Stack[i] <- null
            
            match isNull objectWrapper with
            | true -> None 
            | false ->
                this.StackPointer <- this.StackPointer - 1
                Some (this, objectWrapper.Value) 
