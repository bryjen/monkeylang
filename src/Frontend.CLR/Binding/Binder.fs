module Monkey.Frontend.CLR.Binding.Binder

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open type Microsoft.CodeAnalysis.CSharp.SyntaxFactory


let semanticAnalysis (compilation: CSharpCompilation) (tree: SyntaxTree) =
    let sometjing = compilation.GetSemanticModel(tree, true)
    failwith "todo"