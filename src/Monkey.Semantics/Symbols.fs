﻿module Monkey.Semantics.Symbols

open Microsoft.CodeAnalysis
open Monkey.Semantics.Types

type SymbolBase() =
    member val Name: string
    

type Symbol =
    | LocalSymbol     of LocalSymbol
    | NamespaceSymbol of NamespaceSymbol
    | ParameterSymbol of ParameterSymbol
    | TypeSymbol      of TypeSymbol
    
and LocalSymbol() =
    inherit SymbolBase()
with
    member val Type: TypeSymbol
    
and NamespaceSymbol() =
    inherit SymbolBase()
    
and ParameterSymbol() =
    inherit SymbolBase()
with
    member val Type: TypeSymbol
    


and [<CustomEquality; CustomComparison>] TypeSymbol =
    | NamedTypeSymbol of NamedTypeSymbol
    | ArrayTypeSymbol of ArrayTypeSymbol
    | FunctionTypeSymbol of FunctionTypeSymbol
    | GenericTypeSymbol of GenericTypeSymbol
    
    interface System.IComparable with
        member this.CompareTo(o: obj) =
            failwith "todo"
            
    override this.Equals(obj: obj) =
        match obj with
        | :? TypeSymbol as typeSymbol ->
            match typeSymbol, this with
            | NamedTypeSymbol nts1, NamedTypeSymbol nts2 -> nts1.Equals(nts2)
            | ArrayTypeSymbol ats1, ArrayTypeSymbol ats2 -> ats1.Equals(ats2)
            | FunctionTypeSymbol fts1, FunctionTypeSymbol fts2 -> fts1.Equals(fts2)
            | GenericTypeSymbol gts1, GenericTypeSymbol gts2 -> gts1.Equals(gts2)
            | _ -> false
        | _ -> false
    
    override this.GetHashCode() =
        match this with
        | NamedTypeSymbol namedTypeSymbol -> namedTypeSymbol.GetHashCode() 
        | ArrayTypeSymbol arrayTypeSymbol -> arrayTypeSymbol.GetHashCode() 
        | FunctionTypeSymbol functionTypeSymbol -> functionTypeSymbol.GetHashCode() 
        | GenericTypeSymbol genericTypeSymbol -> genericTypeSymbol.GetHashCode() 
    
    
and NamedTypeSymbol
    (
        _type: Type
    ) =
    inherit SymbolBase()
with
    member val Type: Type = _type
    
    override this.Equals(obj) = base.Equals(obj)
    
    override this.GetHashCode() = base.GetHashCode()
    
    
    
and FunctionTypeSymbol
    (
        parameterTypes: TypeSymbol array,
        returnType: TypeSymbol
    ) =
    inherit SymbolBase()
with
    member val ParameterTypes: TypeSymbol array = parameterTypes
    member val ReturnType: TypeSymbol = returnType
    
    override this.Equals(obj) = base.Equals(obj)
    
    override this.GetHashCode() = base.GetHashCode()
    
    
and ArrayTypeSymbol
    (
        _type: TypeSymbol
    ) =
    inherit SymbolBase()
with
    member val Type: TypeSymbol = _type
    
    
    override this.Equals(obj) = base.Equals(obj)
    
    override this.GetHashCode() = base.GetHashCode()
    
    
and GenericTypeSymbol
    (
        _type: TypeSymbol,
        genericTypes: TypeSymbol array
    ) =
    inherit SymbolBase()
with
    member val Type: TypeSymbol = _type
    member val GenericTypes: TypeSymbol array = genericTypes
    
    
    override this.Equals(obj) = base.Equals(obj)
    
    override this.GetHashCode() = base.GetHashCode()
