module Monkey.Backend.VirtualMachine

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
        

    member this.StackTop() =
        match this.StackPointer with
        | sp when sp = 0 -> None
        | sp ->
            let peek = this.Stack[sp - 1]
            match System.Object.ReferenceEquals(peek, null) with
            | true -> None 
            | false -> Some peek.Value
        
    static member Run(vm: VM) : Result<VM, string> =
        let asByteArr = vm.Instructions.GetBytes()
        let mutable i = 0
        
        // Callback func called whenever a successful instruction slice has been processed
        let callback () = i <- i + 1
        
        let rec runHelper (_vm: VM) : Result<VM, string> =
            if i < asByteArr.Length then
                let opcode = LanguagePrimitives.EnumOfValue<byte, Opcode> asByteArr[i] 
                match opcode with
                | Opcode.OpConstant -> vm.HandleOpConstant(&i, asByteArr)
                | Opcode.OpAdd -> failwith "todo"
                | _ -> failwith "todo"
                |> Result.map (fun newVm -> callback (); newVm) // Call the callback
                |> Result.bind runHelper
            else
                Ok _vm
            
        runHelper vm
        
        
    member private this.HandleOpConstant(i: byref<int>, byteArr: byte array) : Result<VM, string> =
        let constIndex = readUInt16 (byteArr[i + 1 .. i + 3])
        i <- i + 2
        this.Push(this.Constants[int constIndex])
        
    member private this.Push(object: Object) : Result<VM, string> =
        match this.StackPointer >= stackSize with
        | true ->
            Error "Stack Overflow"
        | false ->
            this.Stack[this.StackPointer] <- ObjectWrapper object
            this.StackPointer <- this.StackPointer + 1
            Ok this