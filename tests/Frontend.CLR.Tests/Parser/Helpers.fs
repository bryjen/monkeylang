module Monkey.Frontend.CLR.Tests.Parser.Helpers

open System
open Microsoft.CodeAnalysis.CSharp

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
