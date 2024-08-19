module Monkey.Backend.SymbolTable

type SymbolScope =
    | LocalScope
    | GlobalScope
with
    override this.ToString() =
        match this with
        | LocalScope -> "LOCAL"
        | GlobalScope -> "GLOBAL"
        
type Symbol =
    { Name: string
      Scope: SymbolScope
      Index: int }
    
    
type SymbolTable =
    { Outer: SymbolTable option
      
      Store: Map<string, Symbol>
      Count: int }
    
    
[<RequireQualifiedAccess>]
module SymbolTable =
    let createNew () =
        { Outer = None
          
          Store = Map.empty<string, Symbol>
          Count = 0 }
        
    let createNewEnclosed symbolTable =
        { Outer = Some symbolTable 
          
          Store = Map.empty<string, Symbol>
          Count = 0 }
        
    let isGlobalScope symbolTable = Option.isNone symbolTable.Outer

    let define symbolTable name =
        let scope =
            match symbolTable.Outer with
            | Some _ -> SymbolScope.LocalScope 
            | None -> SymbolScope.GlobalScope 
        
        let symbol = { Name = name; Scope = scope; Index = symbolTable.Count }
        let newSymbolTable = { symbolTable with Store = symbolTable.Store.Add (name, symbol); Count = symbolTable.Count + 1 }
        newSymbolTable, symbol

    let rec resolve symbolTable name =
        match symbolTable.Store.TryFind name with
        | Some symbol -> Some symbol
        | None ->
            match symbolTable.Outer with
            | Some outer -> resolve outer name 
            | None -> None 