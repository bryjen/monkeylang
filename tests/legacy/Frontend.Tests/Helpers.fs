[<AutoOpen>]
module Monkey.Frontend.Tests.Helpers

open FsToolkit.ErrorHandling

(*
let addNumbersToTestCase testCases =
    let mutable testCount = 0
    
    let addTestCountToTuple (tokenType, literal) =
        testCount <- testCount + 1
        (testCount, tokenType, literal)
        
    testCases
    |> List.map addTestCountToTuple
*)
    
/// If any 'Result' DU is 'ERROR', return 'ERROR + error message', returns OK otherwise
let processResultsList (resultsList: Result<unit, string> list) : Result<unit, string> =
    result {
        for testResult in resultsList do
            do! testResult
        return ()
    }
