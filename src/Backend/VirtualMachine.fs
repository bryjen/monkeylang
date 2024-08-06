module Monkey.Backend.VirtualMachine

open Monkey.Backend.Code
open Monkey.Backend.Compiler
open Monkey.Frontend.Eval.Object

let private stackSize = 2048

type VM =
    { Constants: Object array
      Instructions: Instructions
      
      Stack: Object array
      mutable StackPointer: int }
with
    static member FromByteCode(byteCode: Bytecode) =
        { Constants = byteCode.Constants
          Instructions = byteCode.Instructions
          
          Stack = Array.zeroCreate<Object> stackSize
          StackPointer = 0 }
        

    member this.StackTop() =
        match this.StackPointer with
        | sp when sp = 0 -> None
        | sp ->
            let peek = this.Stack[sp - 1]
            if System.Object.ReferenceEquals(peek, null) then None else Some peek
        
        
    member this.Run() : Result<unit, string> =
        let asByteArr = this.Instructions.GetBytes()
        
        let mutable i = 0
        while i < asByteArr.Length do
            let opcode = LanguagePrimitives.EnumOfValue<byte, Opcode> asByteArr[i] 
            match opcode with
            | Opcode.OpConstant -> this.HandleOpConstant(&i, asByteArr)
            | Opcode.OpAdd -> failwith "todo"
            | _ -> System.ArgumentOutOfRangeException() |> raise
            
            i <- i + 1
            
        Ok ()
        
    member private this.HandleOpConstant(i: byref<int>, byteArr: byte array) =
        let constIndex = readUInt16 (byteArr[i + 1 .. i + 3])
        i <- i + 2
        
        match this.Push(this.Constants[int constIndex]) with
        | Ok _ -> () 
        | Error error -> failwith "todo"
        
    member private this.Push(object: Object) =
        match this.StackPointer >= stackSize with
        | true ->
            Error "Stack Overflow"
        | false ->
            this.Stack[this.StackPointer] <- object
            this.StackPointer <- this.StackPointer + 1
            Ok ()