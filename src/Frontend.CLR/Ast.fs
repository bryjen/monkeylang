module Monkey.Frontend.CLR.Ast

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax


let tryParseProgram (input: string) : SyntaxTree =
    // The following is equivalent to the following C# and Monkey source code:
    // C#:
    // var result = a + b
    //
    // Monkey:
    // let result = a + b
    let variableDeclarationSyntax =
        SyntaxFactory
            .VariableDeclaration(
                SyntaxFactory.PredefinedType(SyntaxFactory.Token(SyntaxKind.IntKeyword)))
            .AddVariables(
                SyntaxFactory
                    .VariableDeclarator("result")
                    .WithInitializer(
                        SyntaxFactory.EqualsValueClause(
                            SyntaxFactory.BinaryExpression(SyntaxKind.AddExpression, SyntaxFactory.IdentifierName("a"), SyntaxFactory.IdentifierName("b")))))
    failwith "todo"
