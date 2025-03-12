module Monkey.Backend.Frame

open Monkey.Frontend.Eval.Object


type Frame =
    { ClosureFn: ClosureFunction
      mutable InsPointer: int  // pointer for executing byte instructions 
      BasePointer: int  // pointer for temporarily storing a stack pointer value
      }
    
module Frame =
    let inline createNewFrame closureFunction basePointerVal =
        { ClosureFn = closureFunction
          InsPointer = 0
          BasePointer = basePointerVal }
        
    let inline getInstructions frame = frame.ClosureFn.Fn.InstructionBytes