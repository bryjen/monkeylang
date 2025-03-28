module Monkey.Frontend.CLR.Tests.Parser.Helpers

open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open Microsoft.CodeAnalysis.Text
open Monkey.Codegen.Dotnet.MonkeyToCSharpAstConverter
open Monkey.Parser.Parser
open Monkey.Parser.Tokenizer
open NUnit.Framework

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
                
    
let filterGlobalStatementsAsStatementSyntaxes (arr: MemberDeclarationSyntax array) : Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax array =
    let statements = ResizeArray<Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax>()
    for memberDeclaration in arr do
        match memberDeclaration with
        | :? GlobalStatementSyntax as globalStatement ->
            statements.Add(globalStatement.Statement)
        | _ ->
            ()
            
    statements.ToArray()
                

let compareSyntaxNodes (monkeyInput: string) (expectedSyntaxNodes: SyntaxNode array) (actualSyntaxNodes: SyntaxNode array) : bool =
    
    printfn "Input/Output"
    printfn "---------------------------------------------------------"
    
    printfn "```monkey"
    printfn $"{monkeyInput}"
    printfn "```"
    printfn ""
    
    printfn "```csharp (expected)"
    for expected in expectedSyntaxNodes do
        printfn $"{expected.NormalizeWhitespace()}"
    printfn "```"
    printfn ""
    
    printfn "```csharp (actual)"
    for actual in actualSyntaxNodes do
        printfn $"{actual.NormalizeWhitespace()}"
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
    
    let mutable pass = true
    for expected, actual in Array.zip expectedSyntaxNodes actualSyntaxNodes do
        let expected = expected.NormalizeWhitespace()
        let actual = actual.NormalizeWhitespace()
        if (not (expected.IsEquivalentTo(actual)) || not (actual.IsEquivalentTo(expected))) then
            pass <- false
        
    pass
    
    
    
let private comparisonCore (expectedSyntaxNodes: SyntaxNode array) (input: string) =
    let sourceText = SourceText.From(input)
    let tokens = tokenize input
    let monkeyCompilationUnit, parseErrors = parseTokens tokens
    
    if parseErrors.Length > 0 then
        printfn "Parsing Errors:"
        let mutable count = 1
        for parseError in parseErrors do
            let demoFilePath = @"C:\Users\Public\Documents\repos\MonkeyProject\Program.mk"
            printfn $"{count}. {parseError.GetFormattedMessage(sourceText, Some demoFilePath)}"
            count <- count + 1
        Assert.Fail()
    
    let conversionResult = toCSharpCompilationUnit monkeyCompilationUnit.Statements  // TODO: Support MonkeyCompilationUnit
    match conversionResult with
    | Ok compilationUnitSyntax ->
        let statements = filterGlobalStatementsAsStatementSyntaxes (Seq.toArray compilationUnitSyntax.Members)
        let actualSyntaxNodes = statements |> Array.map (fun s -> s :> SyntaxNode)
        match compareSyntaxNodes input expectedSyntaxNodes actualSyntaxNodes with
        | true -> Assert.Pass()
        | false -> Assert.Fail()
    | Error conversionErrors -> 
        printfn "Conversion Errors:"
        let mutable count = 1
        for error in conversionErrors do
            printfn $"{count}. {error}\n{error.Message}"
            count <- count + 1
        Assert.Fail()
    
let defaultComparison (testCase: string * SyntaxNode) =
    let input, expectedSyntaxNode = testCase
    comparisonCore (List.toArray [ expectedSyntaxNode ]) input

let multiStatementComparison (testCase: string * SyntaxNode list) =
    let input, expectedSyntaxNodesList = testCase
    comparisonCore (List.toArray expectedSyntaxNodesList) input
    
        
