module Monkey.Frontend.CLR.Tests.Parser.Helpers

open System
open System.ComponentModel
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax

type ParserComponentType =
    | Expressions = 1
    | Statements  = 2


/// <summary>
/// Attribute defining what type of 'nodes' a specific test would parse. 
/// </summary>
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = true)>]
type ParserComponentAttribute(parserComponent: ParserComponentType) =
    inherit Attribute()
    member _.ParserComponent = parserComponent
    
    
/// <summary>
/// Attribute defining what other types of 'nodes' that the test assumes are well-tested and functional. (ex. some
/// 'statements' like variable assignment relies on expressions being properly parsed).
/// </summary>
[<AttributeUsage(AttributeTargets.Class, AllowMultiple = true)>]
type ParserComponentDependsOnAttribute(dependsOn: ParserComponentType) =
    inherit Attribute()
    member _.DependsOn = dependsOn
    

type SyntaxNode with
    member this.Print(?indent: int) =
        let indent = defaultArg indent 0
        printfn "%s%s : %s" (String.replicate indent " ") (this.Kind().ToString()) (this.ToString().Trim())
        for child in this.ChildNodesAndTokens() do
            if child.IsNode then
                child.AsNode().Print(indent = indent + 2)
            else
                printfn "%sToken: %s - %s" (String.replicate (indent + 2) " ") (child.Kind().ToString()) (child.ToString())


let compareSyntaxNodes (monkeyInput: string) (expectedSyntaxNodes: SyntaxNode array) (actualSyntaxNodes: SyntaxNode array) : bool =
    let mutable pass = true
    for expected, actual in Array.zip expectedSyntaxNodes actualSyntaxNodes do
        if (not (expected.IsEquivalentTo(actual)) || not (actual.IsEquivalentTo(expected))) then
            pass <- false
        
    printfn "Input/Output"
    printfn "---------------------------------------------------------"
    
    printfn "```monkey"
    printfn $"{monkeyInput}"
    printfn "```"
    printfn ""
    
    printfn "```csharp (expected)"
    for expected in expectedSyntaxNodes do
        printfn $"{expected}"
    printfn "```"
    printfn ""
    
    printfn "```csharp (actual)"
    for actual in actualSyntaxNodes do
        printfn $"{actual}"
    printfn "```"
    printfn "---------------------------------------------------------"
    
    printfn "\n\nSyntax Tree Visualization"
    printfn "---------------------------------------------------------"
    printfn "```csharp (expected)"
    for expected in expectedSyntaxNodes do
        printfn $"{expected.Print()}"
    printfn "```"
    printfn ""
    
    printfn "```csharp (actual)"
    for actual in actualSyntaxNodes do
        printfn $"{actual.Print()}"
    printfn "```"
    printfn ""
    
    pass 