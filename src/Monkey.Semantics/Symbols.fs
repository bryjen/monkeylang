module Monkey.Semantics.Symbols

open Microsoft.CodeAnalysis
open Monkey.Semantics.Types

type SymbolBase() =
    member val Name: string
    

type Symbol =
    | LocalSymbol     of LocalSymbol
    | FunctionSymbol  of FunctionSymbol
    | NamespaceSymbol of NamespaceSymbol
    | ParameterSymbol of ParameterSymbol
    | TypeSymbol      of TypeSymbol
    
and LocalSymbol() =
    inherit SymbolBase()
with
    member val Type: TypeSymbol
    
and FunctionSymbol() =
    inherit SymbolBase()
with
    member val Type: TypeSymbol
    
and NamespaceSymbol() =
    inherit SymbolBase()
    
and ParameterSymbol() =
    inherit SymbolBase()
with
    member val Type: TypeSymbol
    

and TypeSymbol =
    | NamedTypeSymbol of NamedTypeSymbol
    | ArrayTypeSymbol of ArrayTypeSymbol
    | FunctionTypeSymbol of FunctionTypeSymbol
    | TypeParameterSymbol of TypeParameterSymbol  // ex. the 'T' in Func<T, T>
    
and NamedTypeSymbol
    (
        _type: Type
    ) =
    inherit SymbolBase()
with
    member val Type: Type = _type
    
    
and FunctionTypeSymbol() =
    inherit SymbolBase()
with
    member val Name: string
    
    
and ArrayTypeSymbol() =
    inherit SymbolBase()
    
    
and TypeParameterSymbol() =
    inherit SymbolBase()
