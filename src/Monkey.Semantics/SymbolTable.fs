module Monkey.Semantics.SymbolTable

open Monkey.Semantics.Symbols


let private replaceFirst newVal lst =
    match lst with
    | [] -> []
    | _::tail -> newVal :: tail
    

type Scope = Map<string, Symbol list>

type SymbolTable() =
    let mutable scopeStack: Scope list = [ Map.empty ]
    let mutable scopeLevel: int = 0
with
    member this.EnterScope() : Scope =
        scopeLevel <- scopeLevel + 1
        let newScope: Scope = Map.empty
        scopeStack <- newScope :: scopeStack
        newScope
        
    member this.ExitScope() : Scope =
        scopeLevel <- scopeLevel - 1
        let poppedScope = scopeStack.Head
        scopeStack <- List.tail scopeStack
        poppedScope
        
    member this.SearchForSymbol(symbolName: string) =
        let rec searchCore (_scopeStack: Scope list) =
            match _scopeStack with
            | currentScope :: rest ->
                match (Map.tryFind symbolName currentScope) with
                | None -> searchCore rest
                | Some symbolInfos -> Some symbolInfos
            | [] ->
                None
                
        searchCore scopeStack
        
    member this.AddSymbol(name: string, symbol: Symbol) =
        let currentScope = List.head scopeStack
        
        let symbolList =
            match Map.tryFind name currentScope with
            | Some value -> value
            | None -> []
        let updatedSymbolList = symbol :: symbolList
        let updatedScope = Map.add name updatedSymbolList currentScope
        scopeStack <- replaceFirst updatedScope scopeStack
        ()