module Monkey.Backend.SymbolTable

open Monkey.Frontend.Eval

type SymbolScope =
    | FreeScope 
    | LocalScope
    | GlobalScope
    | BuiltinScope 
with
    override this.ToString() =
        match this with
        | FreeScope -> "FREE"
        | LocalScope -> "LOCAL"
        | GlobalScope -> "GLOBAL"
        | BuiltinScope -> "BUILTIN"
        
type Symbol =
    { Name: string
      Scope: SymbolScope
      Index: int }
    
    
type SymbolTable =
    { Outer: SymbolTable option
      
      FreeSymbols: Symbol list
      FreeSymbolsCount: int
      
      Store: Map<string, Symbol>
      Count: int }
    
    
[<RequireQualifiedAccess>]
module SymbolTable =
    let rec createNew () =
        { Outer = None
          
          FreeSymbols = [ ]
          FreeSymbolsCount = 0 
          
          Store = storeWithBuiltins 
          Count = 0 }
        
    and private storeWithBuiltins =
        let getNameSymbolPair index =
            let name, _ = Builtins.builtinsArray[index]
            (name, { Name = name; Scope = BuiltinScope; Index = index })
        
        [| 0 .. (Builtins.builtinsArray.Length - 1) |]
        |> Array.map getNameSymbolPair
        |> Map.ofArray
        
    let createNewEnclosed symbolTable =
        { Outer = Some symbolTable 
          
          FreeSymbols = [ ]  // TODO: Temporary value
          FreeSymbolsCount = 0 
          
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
        
        
    let private defineFree symbolTable originalSymbol =
        let name = originalSymbol.Name
        let newSymbol = { Name = name 
                          Scope = FreeScope
                          Index = symbolTable.FreeSymbolsCount }
        let newSymbolTable = { symbolTable with
                                FreeSymbols = originalSymbol :: symbolTable.FreeSymbols
                                FreeSymbolsCount = symbolTable.FreeSymbolsCount + 1
                                Store = symbolTable.Store.Add (name, newSymbol) }
        (newSymbolTable, newSymbol)
        

    let rec resolve symbolTable name =
        match (symbolTable.Store.TryFind name), symbolTable.Outer with
        | Some symbol, _ -> Some (symbolTable, symbol)
        | None, Some outerSymbolTable -> tryResolveInOuterScope symbolTable outerSymbolTable name
        | None, None -> None 
            
    and tryResolveInOuterScope innerSymbolTable outerSymbolTable name =
        match resolve outerSymbolTable name with
        | Some (_, symbol) ->
            match symbol.Scope with  // Resolved symbol is impossible to be 'LocalScope' since we are searching in outer scopes
            | GlobalScope | BuiltinScope ->
                Some (innerSymbolTable, symbol)
            | _ ->
                let newInnerSymbolTable, freeSymbol = defineFree innerSymbolTable symbol
                Some (newInnerSymbolTable, freeSymbol)
        | None -> None 