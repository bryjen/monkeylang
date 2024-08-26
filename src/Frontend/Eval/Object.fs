module Monkey.Frontend.Eval.Object

open System
open Monkey.Frontend.Ast


type Environment =
    { Map: Map<string, Object>
      Outer: Environment Option } 
with
    static member Empty =
        { Map = [ ] |> Map.ofList
          Outer = None }
    
    static member CreateEnclosedEnv outerEnv =
        { Map = [ ] |> Map.ofList
          Outer = Some outerEnv }
    
    member this.Set name object = { this with Map = this.Map.Add (name, object) }
    
    member this.Get name =
        match Map.tryFind name this.Map with
        | Some value ->
            Some value
        | None when this.Outer.IsSome ->
            let outerEnv = (Option.get this.Outer)
            outerEnv.Get name
        | _ ->
            None
          
          
          
///
and Object =
    | IntegerType of Int64
    | BooleanType of bool
    | NullType
    | StringType of string
    | FunctionType of Function
    | ErrorType of ErrorType
    | ArrayType of Object array
    | HashType of Map<HashableObject, Object>
    
with
    member this.Type() =
        match this with
        | IntegerType _ -> "INTEGER"
        | BooleanType _ -> "BOOLEAN"
        | NullType -> "NULL"
        | StringType _ -> "STRING"
        | FunctionType funcType -> funcType.Type() 
        | ErrorType _ -> "ERROR" 
        | ArrayType _ -> "ARRAY" 
        | HashType _ -> "HASH"
        
    member this.Inspect() = this.ToString()
        
    override this.ToString() =
        match this with
        | IntegerType integer -> $"{integer}"
        | BooleanType boolean -> $"{boolean}"
        | NullType -> "null"
        | StringType string -> string
        | FunctionType _ -> failwith "todo" 
        | ErrorType _ -> failwith "todo" 
        | ArrayType arr ->
            let elementsString = String.concat ", " (arr |> Array.map (_.ToString()))
            $"[{elementsString}]"
        | HashType _ -> failwith "todo"






///
and Function =
    | UserFunction of UserFunction
    | CompiledFunction of CompiledFunction
    | BuiltinFunction of BuiltinFunction
    | ClosureFunction of ClosureFunction
with
    member this.Type() =
        match this with
        | UserFunction _ -> "USER_FUNCTION" 
        | CompiledFunction _ -> "COMPILED_FUNCTION"
        | BuiltinFunction _ -> "BUILTIN_FUNCTION"
        | ClosureFunction _ -> "CLOSURE_FUNCTION"
    
    
    
///
and UserFunction =
    { Parameters: Identifier list
      Body: BlockStatement
      Env: Environment }
with
    static member FromFunctionLiteral environment (functionLiteral: FunctionLiteral) =
        { Parameters = functionLiteral.Parameters; Body = functionLiteral.Body; Env = environment }
        
    override this.ToString() =
        let commaSeparatedParameters = String.concat ", " (this.Parameters |> List.map (_.Value)) 
        $"fn ({commaSeparatedParameters}) {{ {this.Body.ToString()} }}" 


///
and CompiledFunction =
    { InstructionBytes: byte array
      NumLocals: int
      NumParameters: int }


and ClosureFunction = 
    { Fn: CompiledFunction
      FreeVariables: Object array }


///
and BuiltinFunction =
    { Fn: Object list -> Result<Object option, string>
      NumParameters: int } 

and ErrorType = ErrorType of string
with
    member this.GetMsg =
        let (ErrorType e) = this
        e
        
        
        
///
and HashableObject =
    | IntegerType of Int64
    | BooleanType of bool
    | StringType of string
with
    static member FromObject (object: Object) =
        match object with
        | Object.IntegerType integerType -> integerType |> HashableObject.IntegerType |> Some 
        | Object.BooleanType booleanType -> booleanType |> HashableObject.BooleanType |> Some 
        | Object.StringType stringType -> stringType  |> HashableObject.StringType |> Some 
        | _ -> None
        
    static member Hash (hashableObject: HashableObject) =
        match hashableObject with
        | HashableObject.IntegerType integerType -> hash integerType
        | HashableObject.BooleanType booleanType -> hash booleanType 
        | HashableObject.StringType stringType -> hash stringType 
