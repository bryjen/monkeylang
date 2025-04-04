module rec Monkey.Semantics.SymbolsExtensions

open Monkey.AST.AstToString
open Monkey.Semantics.Symbols


let identStr indentation = 
    let indentationStr = "    "
    String.replicate indentation indentationStr
    

type NamedTypeSymbol with
    member internal this.Repr(indentation: int) =
        let strings =
            [|
                $"{identStr indentation}[{nameof(NamedTypeSymbol)}]"
                $"{identStr (indentation + 1)}[{this.Type.GetType().Name}] {this.Type.ToString()}"
            |]
        System.String.Join("\n", strings)
        
    
type FunctionTypeSymbol with
    member internal this.Repr(indentation: int) =
        let strs = ResizeArray<string>()
        strs.Add($"{identStr indentation}[{nameof(FunctionTypeSymbol)}]")
        
        let parameterStrings: string array = this.ParameterTypes |> Array.map _.Repr(indentation + 1)
        if this.ParameterTypes |> Array.isEmpty |> not then
            strs.Add(System.String.Join("\n", parameterStrings))
        
        strs.Add($"{this.ReturnType.Repr(indentation + 1)}")
        System.String.Join("\n", strs.ToArray())
        
    
type ArrayTypeSymbol with
    member internal this.Repr(indentation: int) =
        failwith "todo"
        
    
type GenericTypeSymbol with
    member internal this.Repr(indentation: int) =
        failwith "todo"


type TypeSymbol with
    member internal this.Repr(indentation: int) =
        match this with
        | NamedTypeSymbol namedTypeSymbol -> namedTypeSymbol.Repr(indentation)
        | ArrayTypeSymbol arrayTypeSymbol -> arrayTypeSymbol.Repr(indentation)
        | FunctionTypeSymbol functionTypeSymbol -> functionTypeSymbol.Repr(indentation)
        | GenericTypeSymbol genericTypeSymbol -> genericTypeSymbol.Repr(indentation)
        