module Monkey.Backend.Frame

open Monkey.Frontend.Eval.Object


type Frame =
    { Function: CompiledFunction
      InsPointer: int  // instruction pointer
       }
    
module Frame =
    let inline createNewFrame compiledFunction =
        { Function = compiledFunction
          InsPointer = 0 }
        
    let inline getInstructions frame = frame.Function.InstructionBytes