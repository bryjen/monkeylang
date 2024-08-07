module Monkey.Backend.Helpers

open Monkey.Frontend.Eval.Object

/// Wrapper type for 'Object' that allows for null values. Null values for performance considerations.
[<AllowNullLiteral>]
type ObjectWrapper(object: Object) =
    member val Value = object with get, set
    