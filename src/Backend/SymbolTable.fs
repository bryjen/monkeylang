module Monkey.Backend.SymbolTable

type SymbolScope =
    | GlobalScope
with
    override this.ToString() =
        match this with
        | GlobalScope -> "GlobalScope"
        
type Symbol =
    { Name: string
      Scope: SymbolScope
      Index: int }
    
type SymbolTable =
    { Store: Map<string, Symbol>
      Count: int }
with
    static member New =
        { Store = Map.empty<string, Symbol>
          Count = 0 }
        
    member this.Define(name: string) =
        let symbol = { Name = name; Scope = GlobalScope; Index = this.Count }
        let newSymbolTable = { this with Store = this.Store.Add (name, symbol); Count = this.Count + 1 }
        newSymbolTable, symbol
        
    member this.Resolve(name: string) = this.Store.TryFind name