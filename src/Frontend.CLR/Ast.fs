module Monkey.Frontend.CLR.Ast

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax


let tryParseProgram (input: string) : SyntaxTree =
    let something =
        SyntaxFactory
            .VariableDeclaration(
                SyntaxFactory.PredefinedType(
                    SyntaxFactory.Token(SyntaxKind.IntKeyword)))
            .AddVariables(
                SyntaxFactory
                    .VariableDeclarator("result")
                    .WithInitializer(
                        SyntaxFactory.EqualsValueClause(
                            SyntaxFactory.BinaryExpression(SyntaxKind.AddExpression,
                                                           SyntaxFactory.IdentifierName("a"),
                                                           SyntaxFactory.IdentifierName("b")))))
    failwith "todo"
