namespace Monkey.Parser.Tests.Parser.MonkeyAstParserTests


open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.CSharp

open Monkey.AST.AstPrinter
open Monkey.AST.AstToString
open Monkey.Parser.Errors
open Monkey.Parser.Tests.Parser.Helpers
open Monkey.Parser.Tokenizer
open NUnit.Framework

open Monkey.AST
open Monkey.Parser.Parser

open type Monkey.AST.SyntaxFactory.MonkeySyntaxTokenFactory
open type Monkey.AST.SyntaxFactory.MonkeyExpressionSyntaxFactory
open type Monkey.AST.SyntaxFactory.MonkeyStatementSyntaxFactory
open type Monkey.AST.SyntaxFactory.MonkeyOtherSyntaxFactory

[<AutoOpen>]
module private MonkeyAstParserTestsHelpers =
    let compareMonkeyStatements (monkeyInput: string) (expectedSyntaxNodes: MonkeySyntaxNode array) (actualSyntaxNodes: MonkeySyntaxNode array) : bool =
        
        printfn "Input/Output"
        printfn "---------------------------------------------------------"
        
        printfn "```monkey"
        printfn $"{monkeyInput}"
        printfn "```"
        printfn ""
        
        printfn "```monkey (expected)"
        for expected in expectedSyntaxNodes do
            printfn $"{expected}"
        printfn "```"
        printfn ""
        
        printfn "```monkey (actual)"
        for actual in actualSyntaxNodes do
            printfn $"{actual}"
        printfn "```"
        printfn "---------------------------------------------------------"
        
        printfn "\n\nSyntax Tree Visualization"
        printfn "---------------------------------------------------------"
        printfn "```csharp (expected)"
(*
        for expected in expectedSyntaxNodes do
            printMonkeySyntaxNodeTree expected
*)
        for expected in expectedSyntaxNodes do
            printfn $"{nodeToString expected}"
        printfn "```"
        printfn ""
        
        printfn "```csharp (actual)"
(*
        for actual in actualSyntaxNodes do
            printMonkeySyntaxNodeTree actual
*)
        for actual in actualSyntaxNodes do
            printfn $"{nodeToString actual}"
        printfn "```"
        printfn "---------------------------------------------------------"
        
        
        let mutable pass = true
        for expected, actual in Array.zip expectedSyntaxNodes actualSyntaxNodes do
            try
                let eq1 = MonkeySyntaxNode.AreEquivalent(actual, expected)
                let eq2 = MonkeySyntaxNode.AreEquivalent(expected, actual)
                if not eq1 || not eq2 then
                    pass <- false
            with
            | _ ->
                pass <- false
            
        
        pass
        
        
    let emptyBlock () =
        { OpenBraceToken = OpenBraceToken(); Statements = [||]; CloseBraceToken = CloseBraceToken() }

[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type LiteralExpressionStatementParsing() =
    member this.TestCases : (string * ExpressionStatementSyntax) array = [|
        (
            "5;",
            ExpressionStatementNoBox(NumericLiteralExpression(5))
        )
        (
            "-5;",
            ExpressionStatementNoBox(MinusPrefixExpression(NumericLiteralExpression(5)))
        )
        (
            "(5);",
            ExpressionStatementNoBox(ParenthesizedExpression(NumericLiteralExpression(5)))
        )
        (
            "-(5);",
            ExpressionStatementNoBox(MinusPrefixExpression(ParenthesizedExpression(NumericLiteralExpression(5))))
        )
        (
            "(-5);",
            ExpressionStatementNoBox(ParenthesizedExpression(MinusPrefixExpression(NumericLiteralExpression(5))))
        )
        
        (
            "true;",
            ExpressionStatementNoBox(TrueLiteralExpression())
        )
        (
            "false;",
            ExpressionStatementNoBox(FalseLiteralExpression())
        )
        (
            "!true;",
            ExpressionStatementNoBox(LogicalNotPrefixExpression(TrueLiteralExpression()))
        )
        (
            "!false;",
            ExpressionStatementNoBox(LogicalNotPrefixExpression(FalseLiteralExpression()))
        )
        (
            "!(true);",
            ExpressionStatementNoBox(LogicalNotPrefixExpression(ParenthesizedExpression(TrueLiteralExpression())))
        )
        (
            "!(false);",
            ExpressionStatementNoBox(LogicalNotPrefixExpression(ParenthesizedExpression(FalseLiteralExpression())))
        )
    |]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    
    [<TestCase(5)>]
    [<TestCase(6)>]
    [<TestCase(7)>]
    [<TestCase(8)>]
    [<TestCase(9)>]
    [<TestCase(10)>]
    member this.``A: Test Basic Numeric Expression Parsing``(testCaseIndex: int) =
        let castedTestCases = this.TestCases |> Array.map (fun (input, expected) -> (input, expected |> StatementSyntax.ExpressionStatementSyntax |> MonkeySyntaxNode.StatementSyntax))
        let input, expectedSyntaxTree = castedTestCases[testCaseIndex]
        let tokens = tokenize input
        
        let monkeyCompilationUnit, parseErrors = parseTokens tokens
        let asMonkeySyntaxNodes = monkeyCompilationUnit.Statements |> Array.map MonkeySyntaxNode.StatementSyntax
        match Array.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = asMonkeySyntaxNodes
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareMonkeyStatements input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
            
            
            
[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type BinaryExpressionStatementParsing() =
    
    member this.TestCases : (string * ExpressionStatementSyntax) array = [|
        (
            "5;",
            ExpressionStatementNoBox(NumericLiteralExpression(5))
        )
        (
            "5 + 5;",
            ExpressionStatementNoBox(AddExpression(NumericLiteralExpression(5), NumericLiteralExpression(5)))
        )
        (
            "5 - 5;",
            ExpressionStatementNoBox(SubtractExpression(NumericLiteralExpression(5), NumericLiteralExpression(5)))
        )
        (
            "5 * 5;",
            ExpressionStatementNoBox(MultiplicationExpression(NumericLiteralExpression(5), NumericLiteralExpression(5)))
        )
        (
            "5 / 5;",
            ExpressionStatementNoBox(DivideExpression(NumericLiteralExpression(5), NumericLiteralExpression(5)))
        )
        (
            "5 > 5;",
            ExpressionStatementNoBox(GreaterThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(5)))
        )
        (
            "5 < 5;",
            ExpressionStatementNoBox(LessThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(5)))
        )
        (
            "5 >= 5;",
            ExpressionStatementNoBox(GreaterThanOrEqExpression(NumericLiteralExpression(5), NumericLiteralExpression(5)))
        )
        (
            "5 <= 5;",
            ExpressionStatementNoBox(LessThanOrEqExpression(NumericLiteralExpression(5), NumericLiteralExpression(5)))
        )
        (
            "5 == 5;",
            ExpressionStatementNoBox(EqualsExpression(NumericLiteralExpression(5), NumericLiteralExpression(5)))
        )
        (
            "5 != 5;",
            ExpressionStatementNoBox(NotEqualsExpression(NumericLiteralExpression(5), NumericLiteralExpression(5)))
        )
        (
            "3 + 4 * 5 == 3 * 1 + 4 * 5;",
            ExpressionStatementNoBox(
                EqualsExpression(
                    AddExpression(
                        NumericLiteralExpression(3),
                        MultiplicationExpression(
                            NumericLiteralExpression(4),
                            NumericLiteralExpression(5))),
                    AddExpression(
                        MultiplicationExpression(
                            NumericLiteralExpression(3),
                            NumericLiteralExpression(1)
                            ),
                        MultiplicationExpression(
                            NumericLiteralExpression(4),
                            NumericLiteralExpression(5)
                            )
                        )
                    )
                )
        )
        (
            "1 + (2 + 3) + 4;",
            ExpressionStatementNoBox(
                AddExpression(
                    NumericLiteralExpression(1),
                    AddExpression(
                        ParenthesizedExpression(
                            AddExpression(
                                NumericLiteralExpression(2),
                                NumericLiteralExpression(3)
                                )
                            ),
                        NumericLiteralExpression(4)
                        )
                    )
                )
        )
        (
            "(5 + 5) * 2;",
            ExpressionStatementNoBox(
                MultiplicationExpression(
                    ParenthesizedExpression(
                        AddExpression(
                            NumericLiteralExpression(5),
                            NumericLiteralExpression(5)
                            )
                        ),
                    NumericLiteralExpression(2)
                    )
                )
        )
        (
            "2/(5+5);",
            ExpressionStatementNoBox(
                DivideExpression(
                    NumericLiteralExpression(2),
                    ParenthesizedExpression(
                        AddExpression(
                            NumericLiteralExpression(5),
                            NumericLiteralExpression(5)
                            )
                        )
                    )
                )
        )
        (
            "-(5 + 5);",
            ExpressionStatementNoBox(
                MinusPrefixExpression(
                    ParenthesizedExpression(
                        AddExpression(
                            NumericLiteralExpression(5),
                            NumericLiteralExpression(5)
                            )
                        )
                    )
                )
        )
        (
            "!(true == true);",
            ExpressionStatementNoBox(
                LogicalNotPrefixExpression(
                    ParenthesizedExpression(
                        EqualsExpression(
                            TrueLiteralExpression(),
                            TrueLiteralExpression()
                            )
                        )
                    )
                )
        )
    |]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    [<TestCase(5)>]
    [<TestCase(6)>]
    [<TestCase(7)>]
    [<TestCase(8)>]
    [<TestCase(9)>]
    [<TestCase(10)>]
    [<TestCase(11)>]
    // [<TestCase(12)>]  TODO: Find a way to figure out comparison for semantically equivalent expressions, ex "1 + (2 + 3)" must be equivalent to "1 + (2 + 3)" even though they aren't considered as of right now
    [<TestCase(13)>]
    [<TestCase(14)>]
    [<TestCase(15)>]
    [<TestCase(16)>]
    member this.``B: Test basic infix expression parsing``(testCaseIndex: int) =
        let castedTestCases = this.TestCases |> Array.map (fun (input, expected) -> (input, expected |> StatementSyntax.ExpressionStatementSyntax |> MonkeySyntaxNode.StatementSyntax))
        let input, expectedSyntaxTree = castedTestCases[testCaseIndex]
        let tokens = tokenize input
        
        let monkeyCompilationUnit, parseErrors = parseTokens tokens
        let asMonkeySyntaxNodes = monkeyCompilationUnit.Statements |> Array.map MonkeySyntaxNode.StatementSyntax
        match Array.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = asMonkeySyntaxNodes
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareMonkeyStatements input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
            
            
            
[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type IfExpressionParsingTests() =
    member this.TestCases : (string * ExpressionStatementSyntax) array = [|
        (
            "if (5 > 2) { 5; };",
            ExpressionStatementNoBox(
                IfExpression(
                    GreaterThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(2)),
                    BlockStatementNoBox(
                        [|
                            ExpressionStatementNoBox(NumericLiteralExpression(5)) |> StatementSyntax.ExpressionStatementSyntax
                        |])
                    )
                )
        )
        (
            "if (5 > 2) { 5; } else { 10; };",
            ExpressionStatementNoBox(
                IfExpression(
                    GreaterThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(2)),
                    BlockStatementNoBox(
                        [|
                            ExpressionStatementNoBox(NumericLiteralExpression(5)) |> StatementSyntax.ExpressionStatementSyntax
                        |]),
                    ElseClause(
                        BlockStatementNoBox(
                            [|
                                ExpressionStatementNoBox(NumericLiteralExpression(10)) |> StatementSyntax.ExpressionStatementSyntax
                            |]
                            )
                        )
                    )
                )
        )
    |]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    member this.``C: If expression parsing tests``(testCaseIndex: int) =
        let castedTestCases = this.TestCases |> Array.map (fun (input, expected) -> (input, expected |> StatementSyntax.ExpressionStatementSyntax |> MonkeySyntaxNode.StatementSyntax))
        let input, expectedSyntaxTree = castedTestCases[testCaseIndex]
        let tokens = tokenize input
        
        let monkeyCompilationUnit, parseErrors = parseTokens tokens
        let asMonkeySyntaxNodes = monkeyCompilationUnit.Statements |> Array.map MonkeySyntaxNode.StatementSyntax
        match Array.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = asMonkeySyntaxNodes
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareMonkeyStatements input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
            
            
[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type FunctionParsingTests() =
    member this.TestCases : (string * ExpressionStatementSyntax) array = [|
        (
            """fn(int x, int y) : int {
    x + y;
};
""",
            ExpressionStatementNoBox(
                FunctionExpression(
                    ParameterList(
                        [|
                            Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("x")))
                            Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("y")))
                        |]),
                    BuiltinType(IntKeyword()),
                    BlockStatementNoBox(
                        [|
                            ExpressionStatement(
                                AddExpression(
                                    IdentifierName(Identifier("x")),
                                    IdentifierName(Identifier("y"))
                                    )
                                )
                        |])
                    )
                )
        )
        (
            """fn() : int {
    x + 12;
};
""",
            ExpressionStatementNoBox(
                FunctionExpression(
                    ParameterList(
                        [|
                        |]),
                    BuiltinType(IntKeyword()),
                    BlockStatementNoBox(
                        [|
                            ExpressionStatement(
                                AddExpression(
                                    IdentifierName(Identifier("x")),
                                    NumericLiteralExpression(12)
                                    )
                                )
                        |])
                    )
                )
        )
        (
            """fn() : unit {
};
""",
            ExpressionStatementNoBox(
                FunctionExpression(
                    ParameterList(
                        [|
                        |]),
                    NameType(IdentifierNameNoBox(Identifier("unit"))),
                    BlockStatementNoBox(
                        [|
                        |])
                    )
                )
        )
        (
            """fn([int -> int] transform, int init_value) : int {
    init_value;
};
""",
            ExpressionStatementNoBox(
                FunctionExpression(
                    ParameterList(
                        [|
                            Parameter(
                                FunctionType(
                                    [|
                                        BuiltinType(IntKeyword())
                                        BuiltinType(IntKeyword())
                                    |]),
                                SimpleIdentifierNoBox(Identifier("transform")))
                            Parameter(
                                BuiltinType(IntKeyword()),
                                SimpleIdentifierNoBox(Identifier("init_value")))
                        |]),
                    BuiltinType(IntKeyword()),
                    BlockStatementNoBox(
                        [|
                            ExpressionStatement(IdentifierName(Identifier("init_value")))
                        |])
                    )
                )
        )
        (
            """fn([int -> int -> int] full_transform, int init_value) : [int -> int] {
    init_value;
};
""",
            ExpressionStatementNoBox(
                FunctionExpression(
                    ParameterList(
                        [|
                            Parameter(
                                FunctionType(
                                    [|
                                        BuiltinType(IntKeyword())
                                        BuiltinType(IntKeyword())
                                        BuiltinType(IntKeyword())
                                    |]),
                                SimpleIdentifierNoBox(Identifier("full_transform")))
                            Parameter(
                                BuiltinType(IntKeyword()),
                                SimpleIdentifierNoBox(Identifier("init_value")))
                        |]),
                    FunctionType(
                        [|
                            BuiltinType(IntKeyword())
                            BuiltinType(IntKeyword())
                        |]),
                    BlockStatementNoBox(
                        [|
                            ExpressionStatement(IdentifierName(Identifier("init_value")))
                        |])
                    )
                )
        )
    |]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    member this.``D: Function expression parsing tests``(testCaseIndex: int) =
        let castedTestCases = this.TestCases |> Array.map (fun (input, expected) -> (input, expected |> StatementSyntax.ExpressionStatementSyntax |> MonkeySyntaxNode.StatementSyntax))
        let input, expectedSyntaxTree = castedTestCases[testCaseIndex]
        let tokens = tokenize input
        
        let monkeyCompilationUnit, parseErrors = parseTokens tokens
        let asMonkeySyntaxNodes = monkeyCompilationUnit.Statements |> Array.map MonkeySyntaxNode.StatementSyntax
        match Array.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = asMonkeySyntaxNodes
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareMonkeyStatements input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
            
            
[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type InvocationExpressionParsingTests() =
    member this.TestCases : (string * ExpressionStatementSyntax) array = [|
        (
            "some_function(5, 5);",
            ExpressionStatementNoBox(
                InvocationExpression(
                    InvocationExpressionIdentifierName(Identifier("some_function")),
                    ArgumentList(
                        [|
                            NumericLiteralExpression(5)
                            NumericLiteralExpression(5)
                        |])
                    )
                )
        )
        (
            "some_function(some_function(5, 5, 5), 5);",
            ExpressionStatementNoBox(
                InvocationExpression(
                    InvocationExpressionIdentifierName(Identifier("some_function")),
                    ArgumentList(
                        [|
                            InvocationExpression(
                                InvocationExpressionIdentifierName(Identifier("some_function")),
                                ArgumentList(
                                    [|
                                        NumericLiteralExpression(5)
                                        NumericLiteralExpression(5)
                                        NumericLiteralExpression(5)
                                    |])
                                )
                            NumericLiteralExpression(5)
                        |])
                    )
                )
        )
        (
            "some_function(if (5 > 2) { 5; } else { 10; }, 5);",
            ExpressionStatementNoBox(
                InvocationExpression(
                    InvocationExpressionIdentifierName(Identifier("some_function")),
                    ArgumentList(
                        [|
                            IfExpression(
                                GreaterThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(2)),
                                BlockStatementNoBox(
                                    [|
                                        ExpressionStatementNoBox(NumericLiteralExpression(5)) |> StatementSyntax.ExpressionStatementSyntax
                                    |]),
                                ElseClause(
                                    BlockStatementNoBox(
                                        [|
                                            ExpressionStatementNoBox(NumericLiteralExpression(10)) |> StatementSyntax.ExpressionStatementSyntax
                                        |]
                                        )
                                    )
                                )
                            NumericLiteralExpression(5)
                        |])
                    )
                )
        )
        (
            "some_function(if (5 > 2) { 5; } else { 10; }, if (5 > 2) { 5; } else { 10; });",
            ExpressionStatementNoBox(
                InvocationExpression(
                    InvocationExpressionIdentifierName(Identifier("some_function")),
                    ArgumentList(
                        [|
                            IfExpression(
                                GreaterThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(2)),
                                BlockStatementNoBox(
                                    [|
                                        ExpressionStatementNoBox(NumericLiteralExpression(5)) |> StatementSyntax.ExpressionStatementSyntax
                                    |]),
                                ElseClause(
                                    BlockStatementNoBox(
                                        [|
                                            ExpressionStatementNoBox(NumericLiteralExpression(10)) |> StatementSyntax.ExpressionStatementSyntax
                                        |]
                                        )
                                    )
                                )
                            IfExpression(
                                GreaterThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(2)),
                                BlockStatementNoBox(
                                    [|
                                        ExpressionStatementNoBox(NumericLiteralExpression(5)) |> StatementSyntax.ExpressionStatementSyntax
                                    |]),
                                ElseClause(
                                    BlockStatementNoBox(
                                        [|
                                            ExpressionStatementNoBox(NumericLiteralExpression(10)) |> StatementSyntax.ExpressionStatementSyntax
                                        |]
                                        )
                                    )
                                )
                        |])
                    )
                )
        )
        (
            "perform_hook(fn(string str) : unit { log(str); }, \"SOME STATE\");",
            ExpressionStatementNoBox(
                InvocationExpression(
                    InvocationExpressionIdentifierName(Identifier("perform_hook")),
                    ArgumentList(
                        [|
                            FunctionExpression(
                                ParameterList(
                                    [|
                                        Parameter(
                                            BuiltinType(StringKeyword()),
                                            SimpleIdentifierNoBox(Identifier("str"))
                                            )
                                    |]),
                                NameType(IdentifierNameNoBox(Identifier("unit"))),
                                BlockStatementNoBox(
                                    [|
                                        ExpressionStatement(
                                            InvocationExpression(
                                                InvocationExpressionIdentifierName(Identifier("log")),
                                                ArgumentList(
                                                    [|
                                                        IdentifierName(Identifier("str"))
                                                    |])
                                                )
                                            )
                                    |])
                                )
                            StringLiteralExpression("SOME STATE")
                        |])
                    )
                )
        )
        (
            "perform_hook(fn(string str) : unit { log(str); }, if (5 > 2) { 5; } else { 10; }, 3 + 4 * 5 == 3 * 1 + 4 * 5);",
            ExpressionStatementNoBox(
                InvocationExpression(
                    InvocationExpressionIdentifierName(Identifier("perform_hook")),
                    ArgumentList(
                        [|
                            FunctionExpression(
                                ParameterList(
                                    [|
                                        Parameter(
                                            BuiltinType(StringKeyword()),
                                            SimpleIdentifierNoBox(Identifier("str"))
                                            )
                                    |]),
                                NameType(IdentifierNameNoBox(Identifier("unit"))),
                                BlockStatementNoBox(
                                    [|
                                        ExpressionStatement(
                                            InvocationExpression(
                                                InvocationExpressionIdentifierName(Identifier("log")),
                                                ArgumentList(
                                                    [|
                                                        IdentifierName(Identifier("str"))
                                                    |])
                                                )
                                            )
                                    |])
                                )
                            IfExpression(
                                GreaterThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(2)),
                                BlockStatementNoBox(
                                    [|
                                        ExpressionStatementNoBox(NumericLiteralExpression(5)) |> StatementSyntax.ExpressionStatementSyntax
                                    |]),
                                ElseClause(
                                    BlockStatementNoBox(
                                        [|
                                            ExpressionStatementNoBox(NumericLiteralExpression(10)) |> StatementSyntax.ExpressionStatementSyntax
                                        |]
                                        )
                                    )
                                )
                            EqualsExpression(
                                AddExpression(
                                    NumericLiteralExpression(3),
                                    MultiplicationExpression(
                                        NumericLiteralExpression(4),
                                        NumericLiteralExpression(5))),
                                AddExpression(
                                    MultiplicationExpression(
                                        NumericLiteralExpression(3),
                                        NumericLiteralExpression(1)
                                        ),
                                    MultiplicationExpression(
                                        NumericLiteralExpression(4),
                                        NumericLiteralExpression(5)
                                        )
                                    )
                                )
                        |])
                    )
                )
        )
        (
            "fn(int x, int y) : int { x + y; }(5, 5);",
            ExpressionStatementNoBox(
                InvocationExpression(
                    InvocationExpressionFunctionExpression(
                        ParameterList(
                            [|
                                Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("x")))
                                Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("y")))
                            |]),
                        BuiltinType(IntKeyword()),
                        BlockStatementNoBox(
                            [|
                                ExpressionStatement(
                                    AddExpression(
                                        IdentifierName(Identifier("x")),
                                        IdentifierName(Identifier("y"))
                                        )
                                    )
                            |])
                        ),
                    ArgumentList(
                        [|
                            NumericLiteralExpression(5)
                            NumericLiteralExpression(5)
                        |])
                    )
                )
        )
        (
            "(fn(int x, int y) : int { x + y; })(5, 5);",
            ExpressionStatementNoBox(
                InvocationExpression(
                    InvocationExpressionParenthesizedExpression(
                        InvocationExpressionFunctionExpression(
                            ParameterList(
                                [|
                                    Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("x")))
                                    Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("y")))
                                |]),
                            BuiltinType(IntKeyword()),
                            BlockStatementNoBox(
                                [|
                                    ExpressionStatement(
                                        AddExpression(
                                            IdentifierName(Identifier("x")),
                                            IdentifierName(Identifier("y"))
                                            )
                                        )
                                |])
                            )
                        ),
                    ArgumentList(
                        [|
                            NumericLiteralExpression(5)
                            NumericLiteralExpression(5)
                        |])
                    )
                )
        )
        (
            "((fn(int x, int y) : int { x + y; }))(5, 5);",
            ExpressionStatementNoBox(
                InvocationExpression(
                    InvocationExpressionParenthesizedExpression(
                        InvocationExpressionParenthesizedExpression(
                            InvocationExpressionFunctionExpression(
                                ParameterList(
                                    [|
                                        Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("x")))
                                        Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("y")))
                                    |]),
                                BuiltinType(IntKeyword()),
                                BlockStatementNoBox(
                                    [|
                                        ExpressionStatement(
                                            AddExpression(
                                                IdentifierName(Identifier("x")),
                                                IdentifierName(Identifier("y"))
                                                )
                                            )
                                    |])
                                )
                            )
                        ),
                    ArgumentList(
                        [|
                            NumericLiteralExpression(5)
                            NumericLiteralExpression(5)
                        |])
                    )
                )
        )
        (
            "((((fn(int x, int y) : int { x + y; }))))(5, 5);",
            ExpressionStatementNoBox(
                InvocationExpression(
                    InvocationExpressionParenthesizedExpression(
                        InvocationExpressionParenthesizedExpression(
                            InvocationExpressionParenthesizedExpression(
                                InvocationExpressionParenthesizedExpression(
                                    InvocationExpressionFunctionExpression(
                                        ParameterList(
                                            [|
                                                Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("x")))
                                                Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("y")))
                                            |]),
                                        BuiltinType(IntKeyword()),
                                        BlockStatementNoBox(
                                            [|
                                                ExpressionStatement(
                                                    AddExpression(
                                                        IdentifierName(Identifier("x")),
                                                        IdentifierName(Identifier("y"))
                                                        )
                                                    )
                                            |])
                                        )
                                    )
                                )
                            )
                        ),
                    ArgumentList(
                        [|
                            NumericLiteralExpression(5)
                            NumericLiteralExpression(5)
                        |])
                    )
                )
        )
        (
            "(fn([string -> unit] logger, int offset, bool verbose) : unit {})(fn(string str) : unit { log(str); }, if (5 > 2) { 5; } else { 10; }, 3 + 4 * 5 == 3 * 1 + 4 * 5);",
            ExpressionStatementNoBox(
                InvocationExpression(
                    InvocationExpressionParenthesizedExpression(
                        InvocationExpressionFunctionExpression(
                            ParameterList(
                                [|
                                    Parameter(
                                        FunctionType(
                                            [|
                                                BuiltinType(StringKeyword())
                                                NameType(IdentifierNameNoBox(Identifier("unit")))
                                            |]),
                                        SimpleIdentifierNoBox(Identifier("logger"))
                                        )
                                    Parameter(
                                        BuiltinType(IntKeyword()),
                                        SimpleIdentifierNoBox(Identifier("offset"))
                                        )
                                    Parameter(
                                        BuiltinType(BoolKeyword()),
                                        SimpleIdentifierNoBox(Identifier("verbose"))
                                        )
                                |]),
                            NameType(IdentifierNameNoBox(Identifier("unit"))),
                            BlockStatementNoBox(
                                [|
                                |])
                            )
                        ),
                    ArgumentList(
                        [|
                            FunctionExpression(
                                ParameterList(
                                    [|
                                        Parameter(
                                            BuiltinType(StringKeyword()),
                                            SimpleIdentifierNoBox(Identifier("str"))
                                            )
                                    |]),
                                NameType(IdentifierNameNoBox(Identifier("unit"))),
                                BlockStatementNoBox(
                                    [|
                                        ExpressionStatement(
                                            InvocationExpression(
                                                InvocationExpressionIdentifierName(Identifier("log")),
                                                ArgumentList(
                                                    [|
                                                        IdentifierName(Identifier("str"))
                                                    |])
                                                )
                                            )
                                    |])
                                )
                            IfExpression(
                                GreaterThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(2)),
                                BlockStatementNoBox(
                                    [|
                                        ExpressionStatementNoBox(NumericLiteralExpression(5)) |> StatementSyntax.ExpressionStatementSyntax
                                    |]),
                                ElseClause(
                                    BlockStatementNoBox(
                                        [|
                                            ExpressionStatementNoBox(NumericLiteralExpression(10)) |> StatementSyntax.ExpressionStatementSyntax
                                        |]
                                        )
                                    )
                                )
                            EqualsExpression(
                                AddExpression(
                                    NumericLiteralExpression(3),
                                    MultiplicationExpression(
                                        NumericLiteralExpression(4),
                                        NumericLiteralExpression(5))),
                                AddExpression(
                                    MultiplicationExpression(
                                        NumericLiteralExpression(3),
                                        NumericLiteralExpression(1)
                                        ),
                                    MultiplicationExpression(
                                        NumericLiteralExpression(4),
                                        NumericLiteralExpression(5)
                                        )
                                    )
                                )
                        |])
                    )
                )
        )
        (
            "System.Console.WriteLine(\"Hello World\");",
            ExpressionStatementNoBox(
                InvocationExpression(
                    InvocationExpressionQualifiedName(
                        [|
                            Identifier("System")
                            Identifier("Console")
                            Identifier("WriteLine")
                        |]
                        ),
                    ArgumentList(
                        [|
                            StringLiteralExpression("Hello World")
                        |])
                    )
                )
        )
    |]
    
    // call function by identifier nam
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    [<TestCase(5)>]
    
    // inline function invocation
    [<TestCase(6)>]
    [<TestCase(7)>]
    [<TestCase(8)>]
    [<TestCase(9)>]
    [<TestCase(10)>]
    
    [<TestCase(11)>]
    member this.``E: Invocation expression parsing tests``(testCaseIndex: int) =
        let castedTestCases = this.TestCases |> Array.map (fun (input, expected) -> (input, expected |> StatementSyntax.ExpressionStatementSyntax |> MonkeySyntaxNode.StatementSyntax))
        let input, expectedSyntaxTree = castedTestCases[testCaseIndex]
        let tokens = tokenize input
        
        let monkeyCompilationUnit, parseErrors = parseTokens tokens
        let asMonkeySyntaxNodes = monkeyCompilationUnit.Statements |> Array.map MonkeySyntaxNode.StatementSyntax
        match Array.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = asMonkeySyntaxNodes
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareMonkeyStatements input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
            
            
            
[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type LetStatementParsing() =
    member this.TestCases : (string * StatementSyntax) array = [|
        (
            "let foobar = 5;",
            VariableDeclarationStatement(
                Identifier("foobar"),
                NumericLiteralExpression(5)
                )
        )
        (
            "let foobar = 5 + 5;",
            VariableDeclarationStatement(
                Identifier("foobar"),
                AddExpression(NumericLiteralExpression(5), NumericLiteralExpression(5))
                )
        )
        (
            "let foobar = 5 - 5;",
            VariableDeclarationStatement(
                Identifier("foobar"),
                SubtractExpression(NumericLiteralExpression(5), NumericLiteralExpression(5))
                )
        )
        (
            "let foobar = 5 * 5;",
            VariableDeclarationStatement(
                Identifier("foobar"),
                MultiplicationExpression(NumericLiteralExpression(5), NumericLiteralExpression(5))
                )
        )
        (
            "let foobar = 5 / 5;",
            VariableDeclarationStatement(
                Identifier("foobar"),
                DivideExpression(NumericLiteralExpression(5), NumericLiteralExpression(5))
                )
        )
        (
            "let foobar = 5 > 5;",
            VariableDeclarationStatement(
                Identifier("foobar"),
                GreaterThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(5))
                )
        )
        (
            "let foobar = 5 < 5;",
            VariableDeclarationStatement(
                Identifier("foobar"),
                LessThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(5))
                )
        )
        (
            "let foobar = 5 >= 5;",
            VariableDeclarationStatement(
                Identifier("foobar"),
                GreaterThanOrEqExpression(NumericLiteralExpression(5), NumericLiteralExpression(5))
                )
        )
        (
            "let foobar = 5 <= 5;",
            VariableDeclarationStatement(
                Identifier("foobar"),
                LessThanOrEqExpression(NumericLiteralExpression(5), NumericLiteralExpression(5))
                )
        )
        (
            "let foobar = 5 == 5;",
            VariableDeclarationStatement(
                Identifier("foobar"),
                EqualsExpression(NumericLiteralExpression(5), NumericLiteralExpression(5))
                )
        )
        (
            "let foobar : bool = 5 != 5;",
            VariableDeclarationStatement(
                Identifier("foobar"),
                NotEqualsExpression(NumericLiteralExpression(5), NumericLiteralExpression(5)),
                Some (VariableTypeAnnotation(BuiltinType(BoolKeyword())))
                )
        )
        
        (
            "let some_thing = some_function(if (5 > 2) { 5; } else { 10; }, 5);",
            VariableDeclarationStatement(
                Identifier("some_thing"),
                InvocationExpression(
                    InvocationExpressionIdentifierName(Identifier("some_function")),
                    ArgumentList(
                        [|
                            IfExpression(
                                GreaterThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(2)),
                                BlockStatementNoBox(
                                    [|
                                        ExpressionStatementNoBox(NumericLiteralExpression(5)) |> StatementSyntax.ExpressionStatementSyntax
                                    |]),
                                ElseClause(
                                    BlockStatementNoBox(
                                        [|
                                            ExpressionStatementNoBox(NumericLiteralExpression(10)) |> StatementSyntax.ExpressionStatementSyntax
                                        |]
                                        )
                                    )
                                )
                            NumericLiteralExpression(5)
                        |])
                    )
                )
        )
        (
            "let some_thing : [int -> string -> bool] = some_function(if (5 > 2) { 5; } else { 10; }, 5);",
            VariableDeclarationStatement(
                Identifier("some_thing"),
                InvocationExpression(
                    InvocationExpressionIdentifierName(Identifier("some_function")),
                    ArgumentList(
                        [|
                            IfExpression(
                                GreaterThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(2)),
                                BlockStatementNoBox(
                                    [|
                                        ExpressionStatementNoBox(NumericLiteralExpression(5)) |> StatementSyntax.ExpressionStatementSyntax
                                    |]),
                                ElseClause(
                                    BlockStatementNoBox(
                                        [|
                                            ExpressionStatementNoBox(NumericLiteralExpression(10)) |> StatementSyntax.ExpressionStatementSyntax
                                        |]
                                        )
                                    )
                                )
                            NumericLiteralExpression(5)
                        |])
                    ),
                Some (
                    VariableTypeAnnotation(
                        FunctionType(
                            [|
                                BuiltinType(IntKeyword())
                                BuiltinType(StringKeyword())
                                BuiltinType(BoolKeyword())
                            |])
                        )
                    )
                )
        )
        (
            "let result = (fn(int x, int y) : int { x + y; })(5, 5);",
            VariableDeclarationStatement(
                Identifier("result"),
                InvocationExpression(
                    InvocationExpressionParenthesizedExpression(
                        InvocationExpressionFunctionExpression(
                            ParameterList(
                                [|
                                    Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("x")))
                                    Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("y")))
                                |]),
                            BuiltinType(IntKeyword()),
                            BlockStatementNoBox(
                                [|
                                    ExpressionStatement(
                                        AddExpression(
                                            IdentifierName(Identifier("x")),
                                            IdentifierName(Identifier("y"))
                                            )
                                        )
                                |])
                            )
                        ),
                    ArgumentList(
                        [|
                            NumericLiteralExpression(5)
                            NumericLiteralExpression(5)
                        |])
                    )
                )
        )
        (
            "let result: bool = (fn(int x, int y) : int { x + y; })(5, 5);",
            VariableDeclarationStatement(
                Identifier("result"),
                InvocationExpression(
                    InvocationExpressionParenthesizedExpression(
                        InvocationExpressionFunctionExpression(
                            ParameterList(
                                [|
                                    Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("x")))
                                    Parameter(BuiltinType(IntKeyword()), SimpleIdentifierNoBox(Identifier("y")))
                                |]),
                            BuiltinType(IntKeyword()),
                            BlockStatementNoBox(
                                [|
                                    ExpressionStatement(
                                        AddExpression(
                                            IdentifierName(Identifier("x")),
                                            IdentifierName(Identifier("y"))
                                            )
                                        )
                                |])
                            )
                        ),
                    ArgumentList(
                        [|
                            NumericLiteralExpression(5)
                            NumericLiteralExpression(5)
                        |])
                    ),
                Some (VariableTypeAnnotation(BuiltinType(BoolKeyword())))
                )
        )
        (
            "let the_quick_brown_fox: int = if (5 > 2) { 5; } else { 10; };",
            VariableDeclarationStatement(
                Identifier("the_quick_brown_fox"),
                IfExpression(
                    GreaterThanExpression(NumericLiteralExpression(5), NumericLiteralExpression(2)),
                    BlockStatementNoBox(
                        [|
                            ExpressionStatementNoBox(NumericLiteralExpression(5)) |> StatementSyntax.ExpressionStatementSyntax
                        |]),
                    ElseClause(
                        BlockStatementNoBox(
                            [|
                                ExpressionStatementNoBox(NumericLiteralExpression(10)) |> StatementSyntax.ExpressionStatementSyntax
                            |]
                            )
                        )
                    ),
                Some (VariableTypeAnnotation(BuiltinType(IntKeyword())))
                )
        )
    |]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    [<TestCase(5)>]
    [<TestCase(6)>]
    [<TestCase(7)>]
    [<TestCase(8)>]
    [<TestCase(9)>]
    [<TestCase(10)>]
    
    [<TestCase(11)>]
    [<TestCase(12)>]
    [<TestCase(13)>]
    [<TestCase(14)>]
    [<TestCase(15)>]
    member this.``F: Variable assignment (let statement) parsing``(testCaseIndex: int) =
        let castedTestCases = this.TestCases |> Array.map (fun (input, expected) -> (input, expected |> MonkeySyntaxNode.StatementSyntax))
        let input, expectedSyntaxTree = castedTestCases[testCaseIndex]
        let tokens = tokenize input
        
        let monkeyCompilationUnit, parseErrors = parseTokens tokens
        let asMonkeySyntaxNodes = monkeyCompilationUnit.Statements |> Array.map MonkeySyntaxNode.StatementSyntax
        match Array.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = asMonkeySyntaxNodes
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareMonkeyStatements input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
            
            
[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type TypeSyntaxParsing() =
    member this.TestCases : (string * TypeSyntax) array = [|
        (
            "int",
            BuiltinType(IntKeyword())
        )
        (
            "unit",
            NameType(IdentifierNameNoBox(Identifier("unit")))
        )
        (
            "Map",
            NameType(IdentifierNameNoBox(Identifier("Map")))
        )
        (
            "int[]",
            ArrayType(BuiltinType(IntKeyword()))
        )
        (
            "int[][]",
            ArrayType(ArrayType(BuiltinType(IntKeyword())))
        )
        (
            "Map<int, string>",
            GenericType(
                NameType(IdentifierNameNoBox(Identifier("Map"))),
                [|
                    BuiltinType(IntKeyword())
                    BuiltinType(StringKeyword())
                |]
                )
        )
        (
            "Map<int, string[]>",
            GenericType(
                NameType(IdentifierNameNoBox(Identifier("Map"))),
                [|
                    BuiltinType(IntKeyword())
                    ArrayType(BuiltinType(StringKeyword()))
                |]
                )
        )
        (
            "Map<int, string>[]",
            ArrayType(
                GenericType(
                    NameType(IdentifierNameNoBox(Identifier("Map"))),
                    [|
                        BuiltinType(IntKeyword())
                        BuiltinType(StringKeyword())
                    |]
                    )
                )
        )
        (
            "Result<ExpressionSyntax, ParseError>",
            GenericType(
                NameType(IdentifierNameNoBox(Identifier("Result"))),
                [|
                    NameType(IdentifierNameNoBox(Identifier("ExpressionSyntax")))
                    NameType(IdentifierNameNoBox(Identifier("ParseError")))
                |]
                )
        )
        (
            "Result<Option<MonkeySyntaxNode>, ParseError>",
            GenericType(
                NameType(IdentifierNameNoBox(Identifier("Result"))),
                [|
                    GenericType(
                        NameType(IdentifierNameNoBox(Identifier("Option"))),
                        [|
                            NameType(IdentifierNameNoBox(Identifier("MonkeySyntaxNode")))
                        |]
                        )
                    NameType(IdentifierNameNoBox(Identifier("ParseError")))
                |]
                )
        )
        (
            "Result<string, System.Collections.Generic.List<string>>",
            GenericType(
                NameType(IdentifierNameNoBox(Identifier("Result"))),
                [|
                    BuiltinType(StringKeyword())
                    
                    GenericType(
                        NameType(QualifiedName(
                            [|
                                Identifier("System")
                                Identifier("Collections")
                                Identifier("Generic")
                                Identifier("List")
                            |])),
                        [|
                            BuiltinType(StringKeyword())
                        |]
                        )
                |]
                )
        )
    |]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    [<TestCase(5)>]
    [<TestCase(6)>]
    [<TestCase(7)>]
    [<TestCase(8)>]
    [<TestCase(9)>]
    [<TestCase(10)>]
    member this.``G: Test type syntax parsing``(testCaseIndex: int) =
        // we keep the test case in 'ExpressionStatementSyntax' to avoid having to cast each during declaration
        let castedTestCases =
            this.TestCases
            |> Array.map (fun (input, expected) -> (input, expected |> ExpressionSyntax.TypeSyntax |> MonkeySyntaxNode.ExpressionSyntax))
        let input, expectedNode = castedTestCases[testCaseIndex]
        let sourceText = SourceText.From(input)
        let tokens = tokenize input
        
        let parserState = MonkeyAstParserState(tokens)
        let typeSyntaxResult = PrefixExpressions.tryParseTypeSyntax (PlaceholderError()) parserState
        match typeSyntaxResult with
        | Ok typeSyntax ->
            let asMonkeyNode = typeSyntax |> ExpressionSyntax.TypeSyntax |> MonkeySyntaxNode.ExpressionSyntax
            match compareMonkeyStatements input [| expectedNode |] [| asMonkeyNode |] with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | Error error ->
            let filePath = @"C:\Users\Public\Program.mk"
            printfn $"{error.GetFormattedMessage(sourceText, Some filePath)}"
            Assert.Fail()



[<TestFixture>]
[<ParserComponent(ParserComponentType.Expressions)>]
type ArrayExpressionParsing() =
    member this.TestCases : (string * StatementSyntax) array = [|
        (
            "[];",
            ExpressionStatement(
                ValueInitArrayExpression(
                    [|
                        
                    |]
                    ) |> ArrayExpressionSyntax.ValueBasedInstantiation |> ExpressionSyntax.ArrayExpressionSyntax
                )
        )
        (
            "[1];",
            ExpressionStatement(
                ValueInitArrayExpression(
                    [|
                        NumericLiteralExpression(1)
                    |]
                    ) |> ArrayExpressionSyntax.ValueBasedInstantiation |> ExpressionSyntax.ArrayExpressionSyntax
                )
        )
        (
            "[1, 2, 3, 4, 5];",
            ExpressionStatement(
                ValueInitArrayExpression(
                    [|
                        NumericLiteralExpression(1)
                        NumericLiteralExpression(2)
                        NumericLiteralExpression(3)
                        NumericLiteralExpression(4)
                        NumericLiteralExpression(5)
                    |]
                    ) |> ArrayExpressionSyntax.ValueBasedInstantiation |> ExpressionSyntax.ArrayExpressionSyntax
                )
        )
        
        (
            "int[5];",
            ExpressionStatement(
                SizeInitArrayExpression(
                    BuiltinType(IntKeyword()),
                    NumericLiteralExpression(5)
                    ) |> ArrayExpressionSyntax.SizeBasedInitialization |> ExpressionSyntax.ArrayExpressionSyntax
                )
        )
        (
            "int[][5];",
            ExpressionStatement(
                SizeInitArrayExpression(
                    ArrayType(BuiltinType(IntKeyword())),
                    NumericLiteralExpression(5)
                    ) |> ArrayExpressionSyntax.SizeBasedInitialization |> ExpressionSyntax.ArrayExpressionSyntax
                )
        )
        (
            "string[3 * 1 + 4 * 5];",
            ExpressionStatement(
                SizeInitArrayExpression(
                    BuiltinType(StringKeyword()),
                    AddExpression(
                        MultiplicationExpression(
                            NumericLiteralExpression(3),
                            NumericLiteralExpression(1)
                            ),
                        MultiplicationExpression(
                            NumericLiteralExpression(4),
                            NumericLiteralExpression(5)
                            )
                        )
                    ) |> ArrayExpressionSyntax.SizeBasedInitialization |> ExpressionSyntax.ArrayExpressionSyntax
                )
        )
        (
            "Map<int, string>[5];",
            ExpressionStatement(
                SizeInitArrayExpression(
                    GenericType(
                        NameType(IdentifierNameNoBox(Identifier("Map"))),
                        [|
                            BuiltinType(IntKeyword())
                            BuiltinType(StringKeyword())
                        |]
                        ),
                    NumericLiteralExpression(5)
                    ) |> ArrayExpressionSyntax.SizeBasedInitialization |> ExpressionSyntax.ArrayExpressionSyntax
                )
        )
    |]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    [<TestCase(5)>]
    [<TestCase(6)>]
    member this.``H: Array expression parsing test``(testCaseIndex: int) =
        let castedTestCases = this.TestCases |> Array.map (fun (input, expected) -> (input, expected |> MonkeySyntaxNode.StatementSyntax))
        let input, expectedSyntaxTree = castedTestCases[testCaseIndex]
        let tokens = tokenize input
        
        let monkeyCompilationUnit, parseErrors = parseTokens tokens
        let asMonkeySyntaxNodes = monkeyCompilationUnit.Statements |> Array.map MonkeySyntaxNode.StatementSyntax
        match Array.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = asMonkeySyntaxNodes
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareMonkeyStatements input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            for parseError in parseErrors do
                printfn $"{count}. {parseError}"
                count <- count + 1
            Assert.Fail()
            
            
[<TestFixture>]
type UsingDirectiveParsing() =
    member this.TestCases : (string * UsingDirectiveSyntax) array = [|
        (
            "using System;",
            UsingDirective(SimpleIdentifier(Identifier("System")))
        )
        (
            "using System.Console;",
            UsingDirective(
                QualifiedName(
                    [|
                        Identifier("System")
                        Identifier("Console")
                    |]
                    )
                )
        )
    |]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    member this.``I: Using directive parsing test``(testCaseIndex: int) =
        // we keep the test case in 'ExpressionStatementSyntax' to avoid having to cast each during declaration
        let castedTestCases = this.TestCases
        let input, expectedSyntaxTree = castedTestCases[testCaseIndex]
        let tokens = tokenize input
        
        let monkeyCompilationUnit, parseErrors = parseTokens tokens
        let asMonkeySyntaxNodes = monkeyCompilationUnit.UsingDirectives |> Array.map MonkeySyntaxNode.UsingDirectiveSyntax
        match Array.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = asMonkeySyntaxNodes
            let expectedSyntaxNodes = [| expectedSyntaxTree |> MonkeySyntaxNode.UsingDirectiveSyntax |]
            match compareMonkeyStatements input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let sourceText = SourceText.From(input)
            let mutable count = 1
            for parseError in parseErrors do
                let filePath = @"C:\Users\Public\Program.mk"
                printfn $"{parseError.GetFormattedMessage(sourceText, Some filePath)}"
                count <- count + 1
            Assert.Fail()
            
            
            
[<TestFixture>]
type NamespaceDeclarationParsing() =
    member this.TestCases : (string * NamespaceDeclarationSyntax) array = [|
        (
            "namespace Program;",
            NamespaceDeclaration(SimpleIdentifier(Identifier("Program")))
        )
        (
            "namespace Monkey.Frontend.CLR.CLI;",
            NamespaceDeclaration(
                QualifiedName(
                    [|
                        Identifier("Monkey")
                        Identifier("Frontend")
                        Identifier("CLR")
                        Identifier("CLI")
                    |]
                    )
                )
        )
    |]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    member this.``J: Namespace declaration parsing test``(testCaseIndex: int) =
        // we keep the test case in 'ExpressionStatementSyntax' to avoid having to cast each during declaration
        let castedTestCases = this.TestCases
        let input, expectedSyntaxTree = castedTestCases[testCaseIndex]
        let tokens = tokenize input
        
        let monkeyCompilationUnit, parseErrors = parseTokens tokens
        let asMonkeySyntaxNodes = monkeyCompilationUnit.NamespaceDeclarations |> Array.map MonkeySyntaxNode.NamespaceDeclarationSyntax
        match Array.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = asMonkeySyntaxNodes
            let expectedSyntaxNodes = [| expectedSyntaxTree |> MonkeySyntaxNode.NamespaceDeclarationSyntax |]
            match compareMonkeyStatements input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let sourceText = SourceText.From(input)
            let mutable count = 1
            for parseError in parseErrors do
                let filePath = @"C:\Users\Public\Program.mk"
                printfn $"{parseError.GetFormattedMessage(sourceText, Some filePath)}"
                count <- count + 1
            Assert.Fail()



[<TestFixture>]
type StringInterpolationParsingTests() =
    member this.TestCases : (string * StatementSyntax) array = [|
        (
            "$\"Hello, {name}!\";",
            ExpressionStatement(
                InterpolatedString(
                    DollarToken(),
                    [|
                    |]
                    )
                )
        )
    |]
    
    // disabled for now
    // [<TestCase(0)>]
    member this.``K: String interpolation parsing tests``(testCaseIndex: int) =
        PrintTraverserConfigSingleton.Instance.PrintSyntaxTokens <- false
        
        let castedTestCases = this.TestCases |> Array.map (fun (input, expected) -> (input, expected |> MonkeySyntaxNode.StatementSyntax))
        let input, expectedSyntaxTree = castedTestCases[testCaseIndex]
        let tokens = tokenize input
        
        let monkeyCompilationUnit, parseErrors = parseTokens tokens
        let asMonkeySyntaxNodes = monkeyCompilationUnit.Statements |> Array.map MonkeySyntaxNode.StatementSyntax
        match Array.length parseErrors with
        | 0 ->
            let actualSyntaxNodes = asMonkeySyntaxNodes
            let expectedSyntaxNodes = [| expectedSyntaxTree |]
            match compareMonkeyStatements input expectedSyntaxNodes actualSyntaxNodes with
            | true -> Assert.Pass()
            | false -> Assert.Fail()
        | _ ->
            let mutable count = 1
            let filePath = @"C:\Users\Public\Program.mk"
            let sourceText = SourceText.From(input)
            
            for parseError in parseErrors do
                printfn $"{count}.\n{parseError.GetFormattedMessage(sourceText, Some filePath)}"
                count <- count + 1
            Assert.Fail()
