// Lifted from 'Monkey.Frontend'
namespace Monkey.Frontend.CLR.Helpers.Queue

open System
open System.Diagnostics


[<Serializable>]
[<DebuggerDisplay("Count = {Count}")>]
type 'a Queue =
    { EnqueueList: 'a list
      DequeueList: 'a list }
with
    member this.Count : int = (List.length this.EnqueueList) + (List.length this.DequeueList)
    
    
and QueueDebugView<'a>(queue: Queue<'a>) =
    member this.Count = queue
    
    member this.EnqueueList = List.toArray queue.EnqueueList
    
    member this.DequeueList = List.toArray queue.DequeueList
    

module Queue =
    let empty = { EnqueueList = []
                  DequeueList = [] }
    
    let isEmpty queue : bool = List.isEmpty queue.EnqueueList && List.isEmpty queue.DequeueList
    
    let count (queue: 'a Queue) : int = queue.Count 
    
    /// Adds 'item' to the tail of the queue.
    let enqueue queue item : 'a Queue =
        { queue with EnqueueList = item :: queue.EnqueueList }
        
    /// Adds a list of 'item' objects to the tail of the queue.
    let enqueueList queue itemList : 'a Queue =
        { queue with EnqueueList = queue.EnqueueList @ itemList }
        
    let rec dequeue queue : 'a Queue * 'a option =
        match queue.DequeueList with
        | head :: tail ->
            let newQueue = { EnqueueList = queue.EnqueueList; DequeueList = tail }
            newQueue, Some head
            
        | [] ->
            match List.rev queue.EnqueueList with
            | [] -> queue, None 
            | head :: tail ->
                let newQueue = { EnqueueList = []; DequeueList = tail }
                newQueue, Some head
                
    let resultDequeue (defaultValue: 'b) queue : 'a Queue * Result<'a, 'b> =
        match (dequeue queue) with
        | newQueue, Some value -> newQueue, Ok value
        | newQueue, None -> newQueue, Error defaultValue
            
    let peek queue : 'a option =
        match queue.DequeueList with
        | head :: _ ->
            Some head
            
        | [] ->
            match List.rev queue.EnqueueList with
            | [] -> None
            | head :: _ -> Some head
            
    let removeTop queue: 'a Queue =
        let newQueue, _ = dequeue queue
        newQueue
        