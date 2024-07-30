[<AutoOpen>]
module internal Monkey.Backend.Tests.Code.Helpers

open System
open Monkey.Backend.Code

let getEnumCaseName (value: 'T) = Enum.GetName(typeof<'T>, value)
    
let formatByte (byteValue: byte) = $"0x{byteValue:X2}uy"

let formatByteWithInt (byteValue: byte) = $"0x{byteValue:X2}uy ({byteValue})"

