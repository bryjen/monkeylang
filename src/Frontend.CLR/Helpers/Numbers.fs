// Lifted from 'Monkey.Frontend'
[<AutoOpen>]
module internal Monkey.Frontend.CLR.Helpers.Numbers

let increment n =
    n + 1
    
let tryParseInt (input: string) : int option =
    match System.Int32.TryParse(input) with
    | (true, value) -> Some value
    | (false, _)   -> None