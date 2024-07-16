namespace MonkeyInterpreter

open System

type ObjectType =
    | INTEGER_OBJ
    | BOOLEAN_OBJ 
    | NULL_OBJ 
with
    member this.ToString() =
        match this with
        | INTEGER_OBJ -> "INTEGER"
        | BOOLEAN_OBJ -> "BOOLEAN"
        | NULL_OBJ -> "NULL"
    

type IObject =
    abstract member Type: unit -> ObjectType
    abstract member Inspect: unit -> string
    
    
type Integer(value: Int64) =
    interface IObject with
        member this.Type() = INTEGER_OBJ
        member this.Inspect() = $"{value}"


type Boolean(value: bool) =
    interface IObject with
        member this.Type() = BOOLEAN_OBJ 
        member this.Inspect() = $"{value}"

type Null =
    interface IObject with
        member this.Type() = NULL_OBJ 
        member this.Inspect() = "null"
