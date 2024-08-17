module Monkey.Backend.Operators


let inline (>>=) result func = Result.bind func result  // Alias for Result.bind

let inline (<!>) result func = Result.map func result  // Alias for Result.map
