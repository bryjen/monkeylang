namespace Monkey.Backend.Tests.SymbolTable

open NUnit.Framework
open FsToolkit.ErrorHandling 
open Monkey.Backend.SymbolTable 


[<TestFixture>]
type SymbolTableTests() =
    
    [<Test>]
    member this.``A: Test 'Define' 1``() =
        let expected = Map.ofList [
            ("a", { Name = "a"; Scope = GlobalScope; Index = 0 })
            ("b", { Name = "b"; Scope = GlobalScope; Index = 1 })
        ]
        
        result {
            let globalST = SymbolTable.createNew ()
            
            let newGlobalST, symbolA = SymbolTable.define globalST "a"
            let expectedSymbolA = (Map.find "a" expected)
            TestContext.WriteLine($"\nExpected:\n{expectedSymbolA}\nGot:\n{symbolA}")
            do! if symbolA = expectedSymbolA
                then Ok ()
                else Error () 
                
            let _, symbolB = SymbolTable.define newGlobalST "b"
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
    member this.``A: Test 'Define' 2``() =
        let expected = Map.ofList [
            ("a", { Name = "a"; Scope = GlobalScope; Index = 0 })
            ("b", { Name = "b"; Scope = GlobalScope; Index = 1 })
            ("c", { Name = "c"; Scope = LocalScope; Index = 0 })
            ("d", { Name = "d"; Scope = LocalScope; Index = 1 })
            ("e", { Name = "e"; Scope = LocalScope; Index = 0 })
            ("f", { Name = "f"; Scope = LocalScope; Index = 1 })
        ]
        
        let printExpectedVsGot expectedSymbol symbol = TestContext.WriteLine($"\nExpected:\n{expectedSymbol}\nGot:\n{symbol}")
        
        result {
            let globalST = SymbolTable.createNew ()
            
            let globalST, symbolA = SymbolTable.define globalST "a"
            let expectedSymbolA = (Map.find "a" expected)
            printExpectedVsGot expectedSymbolA symbolA
            do! if symbolA = expectedSymbolA then Ok () else Error () 
                
            let globalST, symbolB = SymbolTable.define globalST "b"
            let expectedSymbolB = (Map.find "b" expected)
            printExpectedVsGot expectedSymbolB symbolB
            do! if symbolB = expectedSymbolB then Ok () else Error ()
            
            
            let firstLocalST = SymbolTable.createNewEnclosed globalST
            
            let firstLocalST, symbolC = SymbolTable.define firstLocalST "c"
            let expectedSymbolC = (Map.find "c" expected)
            printExpectedVsGot expectedSymbolC symbolC
            do! if symbolC = expectedSymbolC then Ok () else Error ()
            
            let firstLocalST, symbolD = SymbolTable.define firstLocalST "d"
            let expectedSymbolD = (Map.find "d" expected)
            printExpectedVsGot expectedSymbolD symbolD
            do! if symbolD = expectedSymbolD then Ok () else Error ()
            
            
            let secondLocalST = SymbolTable.createNewEnclosed firstLocalST
            
            let secondLocalST, symbolE = SymbolTable.define secondLocalST "e"
            let expectedSymbolE = (Map.find "e" expected)
            printExpectedVsGot expectedSymbolE symbolE
            do! if symbolE = expectedSymbolE then Ok () else Error ()
            
            let _, symbolF = SymbolTable.define secondLocalST "f"
            let expectedSymbolF = (Map.find "f" expected)
            printExpectedVsGot expectedSymbolF symbolF
            do! if symbolF = expectedSymbolF then Ok () else Error ()
        }
        |> function
           | Ok _ -> Assert.Pass("Passed\n")
           | Error _ -> Assert.Fail("Failed\n")
           
           
    [<Test>]
    member this.``Test Resolve 1``() =
        let expected = Map.ofList [
            ("a", { Name = "a"; Scope = GlobalScope; Index = 0 })
            ("b", { Name = "b"; Scope = GlobalScope; Index = 1 })
        ]
        
        let globalST = SymbolTable.createNew ()
        let newGlobalST, _ = SymbolTable.define globalST "a"
        let newGlobalST, _ = SymbolTable.define newGlobalST "b"
        
        for KeyValue(name, expectedSymbol) in expected do
            match SymbolTable.resolve newGlobalST name with
            | Some (_, actualSymbol) ->
                let msg = $"\nExpected:\n{expectedSymbol}\nGot:\n{actualSymbol}"
                TestContext.WriteLine(msg)
                if actualSymbol = expectedSymbol
                then ()
                else Assert.Fail("Failed\n")
            | None ->
                Assert.Fail($"Failed\nExpected the symbol \"{name}\", was not present.") 
       
        Assert.Pass("Passed\n")

    [<Test>]
    member this.``Test Resolve 2``() =
        let expected = [|
            { Name = "a"; Scope = GlobalScope; Index = 0 }
            { Name = "b"; Scope = GlobalScope; Index = 1 }
            { Name = "c"; Scope = LocalScope; Index = 0 }
            { Name = "d"; Scope = LocalScope; Index = 1 }
        |]
        
        let globalST = SymbolTable.createNew ()
        let newGlobalST, _ = SymbolTable.define globalST "a"
        let newGlobalST, _ = SymbolTable.define newGlobalST "b"
        
        let localST = SymbolTable.createNewEnclosed newGlobalST
        let newLocalST, _ = SymbolTable.define localST "c"
        let newLocalST, _ = SymbolTable.define newLocalST "d"
        
        for expectedSymbol in expected do
            match SymbolTable.resolve newLocalST expectedSymbol.Name with
            | Some (_, actualSymbol) ->
                let msg = $"\nExpected:\n{expectedSymbol}\nGot:\n{actualSymbol}"
                TestContext.WriteLine(msg)
                if actualSymbol = expectedSymbol
                then ()
                else Assert.Fail("Failed\n")
            | None ->
                Assert.Fail($"Failed\nExpected the symbol \"{expectedSymbol.Name}\", was not present.") 
       
        Assert.Pass("Passed\n")
        
    [<Test>]
    member this.``Test Resolve 3``() =
        let globalST = SymbolTable.createNew ()
        let globalST, _ = SymbolTable.define globalST "a"
        let globalST, _ = SymbolTable.define globalST "b"
        
        let firstLocalST = SymbolTable.createNewEnclosed globalST 
        let firstLocalST, _ = SymbolTable.define firstLocalST "c"
        let firstLocalST, _ = SymbolTable.define firstLocalST "d"
        
        let secondLocalST = SymbolTable.createNewEnclosed firstLocalST 
        let secondLocalST, _ = SymbolTable.define secondLocalST "e"
        let secondLocalST, _ = SymbolTable.define secondLocalST "f"
        
        // Symbol table * expected symbols
        let expected = [|
            (globalST,    
            [|
                { Name = "a"; Scope = GlobalScope; Index = 0 }
                { Name = "b"; Scope = GlobalScope; Index = 1 }
            |])
            
            (firstLocalST,    
            [|
                { Name = "a"; Scope = GlobalScope; Index = 0 }
                { Name = "b"; Scope = GlobalScope; Index = 1 }
                { Name = "c"; Scope = LocalScope; Index = 0 }
                { Name = "d"; Scope = LocalScope; Index = 1 }
            |])
            
            (secondLocalST,    
            [|
                { Name = "a"; Scope = GlobalScope; Index = 0 }
                { Name = "b"; Scope = GlobalScope; Index = 1 }
                (*  // Are 'free' variables, check the tests below
                { Name = "c"; Scope = LocalScope; Index = 0 }
                { Name = "d"; Scope = LocalScope; Index = 1 }
                *)
                { Name = "e"; Scope = LocalScope; Index = 0 }
                { Name = "f"; Scope = LocalScope; Index = 1 }
            |])
        |]
        
        let mutable currentTestCase = 0
        for symbolTable, expectedSymbols in expected do
            currentTestCase <- currentTestCase + 1
            for expectedSymbol in expectedSymbols do
                match SymbolTable.resolve symbolTable expectedSymbol.Name with
                | Some (_, actualSymbol) ->
                    let msg = $"\n[Test Case #{currentTestCase}] Expected:\n{expectedSymbol}\nGot:\n{actualSymbol}"
                    TestContext.WriteLine(msg)
                    if actualSymbol = expectedSymbol
                    then ()
                    else Assert.Fail($"[Test Case #{currentTestCase}] Failed - Wrong symbols, got {actualSymbol}\nexpected {expectedSymbol}\n")
                | None ->
                    Assert.Fail($"[Test Case #{currentTestCase}] Failed\nExpected the symbol \"{expectedSymbol.Name}\", was not present.") 
       
        Assert.Pass("Passed\n")
        
        
    [<Test>]
    member this.``Test Resolve Free``() =
        let globalST = SymbolTable.createNew ()
        let globalST, _ = SymbolTable.define globalST "a"
        let globalST, _ = SymbolTable.define globalST "b"
        
        let firstLocalST = SymbolTable.createNewEnclosed globalST 
        let firstLocalST, _ = SymbolTable.define firstLocalST "c"
        let firstLocalST, _ = SymbolTable.define firstLocalST "d"
        
        let secondLocalST = SymbolTable.createNewEnclosed firstLocalST 
        let secondLocalST, _ = SymbolTable.define secondLocalST "e"
        let secondLocalST, _ = SymbolTable.define secondLocalST "f"
        
        // Symbol table * expected symbols * expected free symbols
        let expected = [|
            (firstLocalST,    
            [|
                { Name = "a"; Scope = GlobalScope; Index = 0 }
                { Name = "b"; Scope = GlobalScope; Index = 1 }
                { Name = "c"; Scope = LocalScope; Index = 0 }
                { Name = "d"; Scope = LocalScope; Index = 1 }
            |],
            [| |]
            )
            
            (secondLocalST,    
            [|
                { Name = "a"; Scope = GlobalScope; Index = 0 }
                { Name = "b"; Scope = GlobalScope; Index = 1 }
                { Name = "c"; Scope = FreeScope; Index = 0 }
                { Name = "d"; Scope = FreeScope; Index = 1 }
                { Name = "e"; Scope = LocalScope; Index = 0 }
                { Name = "f"; Scope = LocalScope; Index = 1 }
            |],
            [|
                { Name = "c"; Scope = LocalScope; Index = 0 }
                { Name = "d"; Scope = LocalScope; Index = 1 }
            |]
            )
        |]
        
        let mutable currentTestCase = 0
        for symbolTable, expectedSymbols, expectedFreeSymbols in expected do
            currentTestCase <- currentTestCase + 1
            
            let mutable updatedSymbolTable = symbolTable
            for expectedSymbol in expectedSymbols do
                match SymbolTable.resolve updatedSymbolTable expectedSymbol.Name with
                | Some (updatedST, actualSymbol) ->
                    updatedSymbolTable <- updatedST
                    
                    let msg = $"\n[Test Case #{currentTestCase}] Expected:\n{expectedSymbol}\nGot:\n{actualSymbol}"
                    TestContext.WriteLine(msg)
                    if actualSymbol = expectedSymbol
                    then ()
                    else Assert.Fail($"[Test Case #{currentTestCase}] Failed - Wrong symbols, got {actualSymbol}\nexpected {expectedSymbol}\n")
                | None ->
                    Assert.Fail($"[Test Case #{currentTestCase}] Failed\nExpected the symbol \"{expectedSymbol.Name}\", was not present.")
                    
            for expectedFreeSymbol in expectedFreeSymbols do
                match List.tryFind (fun symbol -> symbol.Name = expectedFreeSymbol.Name) updatedSymbolTable.FreeSymbols with
                | None -> Assert.Fail($"Could not find the free symbol for '{expectedFreeSymbol.Name}'")
                | Some actualFreeSymbol ->
                    if actualFreeSymbol = expectedFreeSymbol
                    then ()
                    else Assert.Fail($"[Test Case #{currentTestCase}] Failed\nExpected:\n\"{expectedFreeSymbol}\"\nGot:\n{actualFreeSymbol}")
                
            ()
       
        Assert.Pass("Passed\n")
        
