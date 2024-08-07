module Monkey.Backend.VirtualMachine


open FsToolkit.ErrorHandling
open Monkey.Backend.Code
open Monkey.Backend.Compiler
open Monkey.Backend.Helpers
open Monkey.Frontend.Eval.Object

let private stackSize = 2048

type VM =
    { Constants: Object array
      Instructions: Instructions
      
      Stack: ObjectWrapper array
      mutable StackPointer: int }
with
    static member FromByteCode(byteCode: Bytecode) =
        { Constants = byteCode.Constants
          Instructions = byteCode.Instructions
          
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
                | Opcode.OpConstant ->
                    _vm.HandleOpConstant(&i, asByteArr)
                | Opcode.OpAdd ->
                    _vm.HandleOpAdd(&i)
                | Opcode.OpPop ->
                    match _vm.Pop() with
                    | None -> Ok _vm 
                    | Some (newVm, _) -> Ok newVm 
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
        
    member private this.HandleOpAdd(i: byref<int>) : Result<VM, string> =
        result {
            let fromOptionToResult opt = if Option.isSome opt then Ok opt.Value else Error VM.FailedPopMessage
            let! newVm, right = this.Pop() |> fromOptionToResult
            let! newVm, left = newVm.Pop() |> fromOptionToResult
            let! result =
                match left, right with
                | Object.IntegerType l, Object.IntegerType r -> (l + r) |> Object.IntegerType |> Ok 
                | Object.StringType l, Object.StringType r -> failwith "todo"
                | Object.IntegerType l, Object.StringType r -> failwith "todo"
                | Object.StringType l, Object.IntegerType r -> failwith "todo"
                | _ -> Error $"The operation \"{left.Type()}\" \"+\" \"{right.Type()}\" is not valid."
            return! newVm.Push(result)
        }
        
    member private this.Push(object: Object) : Result<VM, string> =
        match this.StackPointer >= stackSize with
        | true ->
            Error "Stack Overflow"
        | false ->
            this.Stack[this.StackPointer] <- ObjectWrapper object
            this.StackPointer <- this.StackPointer + 1
            Ok this
            
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
