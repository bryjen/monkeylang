module MonkeyInterpreter.Object

open System


type Object =
    | Integer of Int64
    | Boolean of bool
    | Null
    | String of string
with
    member this.Type() =
        match this with
        | Integer _ -> "INTEGER"
        | Boolean _ -> "BOOLEAN"
        | Null -> "NULL"
        | String _ -> "STRING"
        
    member this.Inspect() = this.ToString()
        
    override this.ToString() =
        match this with
        | Integer integer -> $"{integer}"
        | Boolean boolean -> $"{boolean}"
        | Null -> "null"
        | String string -> string
