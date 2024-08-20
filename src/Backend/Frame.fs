module Monkey.Backend.Frame

open Monkey.Frontend.Eval.Object


type Frame =
    { Function: CompiledFunction
      mutable InsPointer: int  // pointer for executing byte instructions 
      BasePointer: int  // pointer for temporarily storing a stack pointer value
      }
    
module Frame =
    let inline createNewFrame compiledFunction basePointerVal =
        { Function = compiledFunction
          InsPointer = 0
          BasePointer = basePointerVal }
        
    let inline getInstructions frame = frame.Function.InstructionBytes