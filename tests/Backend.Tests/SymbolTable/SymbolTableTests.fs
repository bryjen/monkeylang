namespace Monkey.Backend.Tests.SymbolTable

open NUnit.Framework
open FsToolkit.ErrorHandling 
open Monkey.Backend.SymbolTable 


[<TestFixture>]
type SymbolTableTests() =
    
    [<Test>]
    member this.``A: Test 'Define'``() =
        let expected = Map.ofList [
            ("a", { Name = "a"; Scope = GlobalScope; Index = 0 })
            ("b", { Name = "b"; Scope = GlobalScope; Index = 1 })
        ]
        
        result {
            let globalST = SymbolTable.New
            
            let newGlobalST, symbolA = globalST.Define "a"
            let expectedSymbolA = (Map.find "a" expected)
            TestContext.WriteLine($"\nExpected:\n{expectedSymbolA}\nGot:\n{symbolA}")
            do! if symbolA = expectedSymbolA
                then Ok ()
                else Error () 
                
            let _, symbolB = newGlobalST.Define "b"
            let expectedSymbolB = (Map.find "b" expected)
            TestContext.WriteLine($"\nExpected:\n{expectedSymbolB}\nGot:\n{symbolB}")
            do! if symbolB = expectedSymbolB
                then Ok ()
                else Error () 
        }
        |> function
           | Ok _ -> Assert.Pass("Passed\n")
           | Error _ -> Assert.Fail("Failed\n")
        
    [<Test>]
    member this.``B: Test 'Resolve'``() =
        let expected = Map.ofList [
            ("a", { Name = "a"; Scope = GlobalScope; Index = 0 })
            ("b", { Name = "b"; Scope = GlobalScope; Index = 1 })
        ]
        
        let globalST = SymbolTable.New
        let newGlobalST, _ = globalST.Define "a"
        let newGlobalST, _ = newGlobalST.Define "b"
        
        for KeyValue(name, expectedSymbol) in expected do
            match newGlobalST.Resolve name with
            | Some actualSymbol ->
                let msg = $"\nExpected:\n{expectedSymbol}\nGot:\n{actualSymbol}"
                TestContext.WriteLine(msg)
                if actualSymbol = expectedSymbol
                then ()
                else Assert.Fail("Failed\n")
            | None ->
                Assert.Fail($"Failed\nExpected the symbol \"{name}\", was not present.") 
       
        Assert.Pass("Passed\n")
