module Monkey.Semantics.Types

/// Specifies the ids of special runtime types.
/// Similar to Roslyn's <a href="https://learn.microsoft.com/en-us/dotnet/api/microsoft.codeanalysis.specialtype?view=roslyn-dotnet-4.13.0" target="_blank">SpecialType</a> enum.
type BuiltinType =
    | Boolean
    | String
    | Array
    | Int32
with
    override this.ToString() =
        match this with
        | Boolean -> "System.Boolean"
        | String -> "System.String"
        | Array -> "System.Array"
        | Int32 -> "System.Int32"


type UserDefinedType =
    { Name: string
      Namespace: string }
    
    
type Type =
    | BuiltinType of BuiltinType
    | UserDefinedType of UserDefinedType
