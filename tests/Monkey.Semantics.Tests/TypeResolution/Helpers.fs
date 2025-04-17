module Monkey.Semantics.Tests.TypeResolution.Helpers

open FsToolkit.ErrorHandling
open Microsoft.CodeAnalysis.Text
open Monkey.AST
open Monkey.Parser.Errors
open Monkey.Parser.Parser
open Monkey.Parser.Tokenizer
open Monkey.Semantics.Interface
open Monkey.Semantics.Symbols
open Monkey.Semantics.SymbolsExtensions
open NUnit.Framework

    
let filePathToDisplay = @"C:\Users\admin\Documents\SampleFile.mk"


let assertNoParseErrors sourceText filePath (parseErrors: ParseError array) =
    match parseErrors with
    | [|  |] ->
        Ok ()
    | _ ->
        System.String.Join("\n\n", parseErrors |> Array.map _.GetFormattedMessage(sourceText, filePath)) |> Result.Error


let rec inferTypeFromExpressionStatement (input: string) (expectedTypeSymbol: TypeSymbol) =
    (result {
        let sourceText = SourceText.From(input)
        let tokens = tokenize input
        let monkeyCompilationUnit, parseErrors = parseTokens tokens
        
        do! assertNoParseErrors sourceText (Some filePathToDisplay) parseErrors
        
        let semanticModel = createSymbolTable monkeyCompilationUnit
        let analyzerState = semanticModel.AnalyzerState
        
        let! lastStatement =
            match monkeyCompilationUnit.Statements with
            | [|  |] -> Result.Error "Parsing resulted in 0 statements."
            | arr -> arr |> Array.last |> Ok
            
        let! lastStatementExpr =
            match lastStatement with
            | ExpressionStatementSyntax expressionStatementSyntax -> Ok expressionStatementSyntax
            | stat -> Result.Error $"Expected an expression statement, but got {stat.GetType()}."
            
        let! typeSymbol =
            match tryInferExpressionType analyzerState semanticModel lastStatementExpr.Expression with
            | None ->
                let errors =
                    semanticModel.Diagnostics.ToArray()
                    |> Array.choose (function | Diagnostic.Error err -> Some err | _ -> None)
                    |> Array.map _.GetFormattedMessage(sourceText, Some filePathToDisplay)
                Result.Error ("Couldn't resolve type with errors:\n" + System.String.Join("\n", errors))
            | Some value ->
                Ok value
                
        printTypeSyntaxes input expectedTypeSymbol typeSymbol
        
        return!
            match typeSymbol.Equals(expectedTypeSymbol) with
            | false ->
                Result.Error $"Expected {expectedTypeSymbol.GetType().ToString()}, but received {typeSymbol.GetType().ToString()}."
            | true ->
                Ok ()
    }: Result<unit, string>)
    |> function
        | Ok _ -> Assert.Pass()
        | Result.Error err -> Assert.Fail(err)
        
        
and private printTypeSyntaxes (input: string) (expectedTypeSymbol: TypeSymbol) (actualTypeSymbol: TypeSymbol) : unit =
    
    printfn "Input/Output"
    printfn "---------------------------------------------------------"
    
    printfn "```monkey"
    printfn $"{input}"
    printfn "```"
    printfn ""
    
    printfn "```expected type symbol"
    printfn $"{expectedTypeSymbol.Repr(0)}"
    printfn "```"
    printfn ""
    
    printfn "```actual type symbol"
    printfn $"{actualTypeSymbol.Repr(0)}"
    printfn "```"
    printfn "---------------------------------------------------------"
    ()