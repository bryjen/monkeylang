namespace Monkey.Frontend.Tests

open NUnit.Framework
open FsToolkit.ErrorHandling
open Monkey.Frontend.Helpers.Queue


[<TestFixture>]
type QueueTests() =
    
    let testQueueCount queue expectedCount =
        let actualCount = Queue.count queue
        let msg = $"[Queue.count] Expected: 5, Actual: {actualCount}."
        match actualCount with
        | count when count = expectedCount ->
            TestContext.Progress.WriteLine(msg)
            Ok ()
        | _ ->
            Error msg 
    
    let testForEmptyQueue queue =
        let msg = "[Queue.isEmpty] Queue is not empty when it is supposed to be."
        match Queue.isEmpty queue with
        | true ->
            TestContext.Progress.WriteLine(msg)
            Ok ()
        | false ->
            Error msg
    
    let testDequeueReturnValue expectedValue valueOption =
        match valueOption with
        | Some value ->
            let msg = $"[Queue.deque] Expected: {expectedValue}, Actual: {value}."
            TestContext.Progress.WriteLine(msg)
            if value = expectedValue then Ok () else  Error msg 
        | None ->
            Error "[Queue.deque] Queue empty when it shouldn't be."
            
            
    [<Test>]
    member this.``General Queue Test 1``() =
        result {
            let queue = Queue.empty
            let queue = Queue.enqueue queue 1
            let queue = Queue.enqueue queue 2
            let queue = Queue.enqueue queue 3
            let queue = Queue.enqueue queue 4
            let queue = Queue.enqueue queue 5
            
            do! testQueueCount queue 5
                
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 1 itemOption
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 2 itemOption
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 3 itemOption
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 4 itemOption
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 5 itemOption
                
            do! testForEmptyQueue queue
        }
        |> function
           | Ok _ -> Assert.Pass()
           | Error errorMsg -> Assert.Fail(errorMsg)
           
           
    [<Test>]
    member this.``General Queue Test 2``() =
        result {
            let queue = Queue.empty
            
            let queue = Queue.enqueue queue 1
            let queue = Queue.enqueue queue 2
            do! testQueueCount queue 2
            
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 1 itemOption
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 2 itemOption
            do! testForEmptyQueue queue
            
            let queue = Queue.enqueue queue 3
            let queue = Queue.enqueue queue 4
            let queue = Queue.enqueue queue 5
            do! testQueueCount queue 3
            
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 3 itemOption
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 4 itemOption
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 5 itemOption
            
            let queue = Queue.enqueue queue 6
            let queue = Queue.enqueue queue 7
            let queue = Queue.enqueue queue 8
            
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 6 itemOption
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 7 itemOption
            let queue, itemOption = Queue.dequeue queue
            do! testDequeueReturnValue 8 itemOption
            do! testForEmptyQueue queue
                
        }
        |> function
           | Ok _ -> Assert.Pass()
           | Error errorMsg -> Assert.Fail(errorMsg)
