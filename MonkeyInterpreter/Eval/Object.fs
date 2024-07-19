module MonkeyInterpreter.Eval.Object

open System
open FsToolkit.ErrorHandling
open MonkeyInterpreter


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
          
          

and Object =
    | Integer of Int64
    | Boolean of bool
    | Null
    | String of string
    | Function of Function
    | ErrorType of ErrorType 
with
    member this.Type() =
        match this with
        | Integer _ -> "INTEGER"
        | Boolean _ -> "BOOLEAN"
        | Null -> "NULL"
        | String _ -> "STRING"
        | Function _ -> "FUNCTION" 
        | ErrorType _ -> "ERROR" 
        
    member this.Inspect() = this.ToString()
        
    override this.ToString() =
        match this with
        | Integer integer -> $"{integer}"
        | Boolean boolean -> $"{boolean}"
        | Null -> "null"
        | String string -> string
        | Function _ -> failwith "todo" 
        | ErrorType _ -> failwith "todo" 



and Function =
    | UserFunction of UserFunction
    | BuiltinFunction of BuiltinFunction
    
    
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

and BuiltinFunction =
    { Fn: Object list -> Object
      ParametersLength: int }


and ErrorType = ErrorType of string
with
    member this.GetMsg =
        let (ErrorType e) = this
        e