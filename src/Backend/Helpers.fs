module Monkey.Backend.Helpers

open Monkey.Frontend.Ast

/// <summary>
/// Asserts that the given program does not contain any parsing errors. Returns a result object where the error value
/// is an error message aggregating all errors that occurred while parsing the program.
/// </summary>
let assertProgramHasNoErrors (program: Program) =
    if program.Errors.Length = 0 then
        Ok ()
    else
        let errorsListStr = program.Errors |> List.map (fun str -> $"\n- {str}") |> String.concat ""
        Error $"Program couldn't parse with errors:\n{errorsListStr}"

/// <summary>
/// Transforms a program into a set of nodes (statements & expression tree) that can then be passed into a compiler.
/// </summary>
let programToNodes (program: Program) : Node list =
    let rec helper statements nodes =
        match statements with
        | head :: tail -> helper tail ((Node.Statement head) :: nodes)
        | [ ] -> nodes |> List.rev

    helper program.Statements []
