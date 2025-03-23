// Lifted from 'Monkey.Frontend'
[<AutoOpen>]
module internal Monkey.Frontend.CLR.Helpers.Tuple

open System


/// Returns the third element in a tuple.
let trd (_, _, c) = c


(*
Notes:
Unlike what some LLMs may say, it is not possible to create a TYPE SAFE generic function that turns a "n" sized tuple 
into a "n+1" sized tuple (I tried). There are solutions in StackOverflow where we use Reflection. However, this 
"bypasses" the type system. For this, I don't want to do that.

The next best way is to manually specify the cases and use overloading as seen below.
Although not completely generic, it is rare in practice to use tuples 4 elements in size, much more to require prepending. 
*)

type TuplePrepender =
    static member Prepend(x: int, (a, b)) = (x, a, b)
    
    static member Prepend(x: int, (a, b, c)) = (x, a, b, c)
    
    static member Prepend(x: int, (a, b, c, d)) = (x, a, b, c, d)
    
    // Can add more cases as needed
    
    
    [<Obsolete>]
    static member AddCountsToTuples (tuples: ('a * 'b) list) : (int * 'a * 'b) list =
        let mutable testCount = 0
        
        let addTestCountToTuple (tuple: 'a * 'b) : int * 'a * 'b =
            testCount <- testCount + 1
            TuplePrepender.Prepend(testCount, tuple)
            
        tuples |> List.map addTestCountToTuple
    
    
    static member AddCountsToTuples (tuples: ('a * 'b) array) : (int * 'a * 'b) array =
        let mutable testCount = 0
        
        let addTestCountToTuple (tuple: 'a * 'b) : int * 'a * 'b =
            testCount <- testCount + 1
            TuplePrepender.Prepend(testCount, tuple)
            
        tuples |> Array.map addTestCountToTuple
    
    static member AddCountsToTuples (tuples: ('a * 'b * 'c) array) : (int * 'a * 'b * 'c) array =
        let mutable testCount = 0
        
        let addTestCountToTuple (tuple: 'a * 'b * 'c) : int * 'a * 'b * 'c =
            testCount <- testCount + 1
            TuplePrepender.Prepend(testCount, tuple)
            
        tuples |> Array.map addTestCountToTuple
        
    static member AddCountsToTuples (tuples: ('a * 'b * 'c * 'd) array) : (int * 'a * 'b * 'c * 'd) array =
        let mutable testCount = 0
        
        let addTestCountToTuple (tuple: 'a * 'b * 'c * 'd) : int * 'a * 'b * 'c * 'd =
            testCount <- testCount + 1
            TuplePrepender.Prepend(testCount, tuple)
            
        tuples |> Array.map addTestCountToTuple
        
        
let addCountsToList (tuples: 'a array) : (int * 'a) array =
    let counts = Array.init tuples.Length (fun i -> i + 1)
    Array.zip counts tuples 