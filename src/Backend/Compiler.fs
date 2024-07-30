module Monkey.Backend.Compiler

open Monkey.Frontend.Ast
open Monkey.Frontend.Eval.Object
open Monkey.Backend.Code

type Compiler =
    { Instructions: Instructions
      Constants: Object list }
with
    static member New : Compiler =
        { Instructions = Instructions [|  |]
          Constants = [] }
        
    member this.Compile(node: Node) : Result<Compiler, string> =
        Error ""
        
    member this.CompileMultiple(nodes: Node array) : Result<Compiler, string> =
        Error ""
        
    member this.Bytecode() : Bytecode =
        { Instructions = Instructions [|  |]
          Constants = [|  |] }
    
    
and Bytecode =
    { Instructions: Instructions
      Constants: Object array }
      
