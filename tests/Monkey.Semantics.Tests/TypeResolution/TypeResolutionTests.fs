namespace Monkey.Semantics.Tests.TypeResolution.BasicTypeResolutionTests

open Microsoft.CodeAnalysis.Text
open Microsoft.CodeAnalysis.CSharp

open FsToolkit.ErrorHandling
open Monkey.AST
open Monkey.Semantics.Interface
open Monkey.Semantics.Tests.TypeResolution.Helpers
open NUnit.Framework

open Monkey.Parser.Errors
open Monkey.Parser.Parser
open Monkey.Parser.Tokenizer

open Monkey.Semantics.Symbols
open Monkey.Semantics.Types

open type Monkey.AST.SyntaxFactory.MonkeySyntaxTokenFactory
open type Monkey.AST.SyntaxFactory.MonkeyExpressionSyntaxFactory
open type Monkey.AST.SyntaxFactory.MonkeyStatementSyntaxFactory


[<AutoOpen>]
module private ErrorTestsHelpers =
    let printErrors (sourceText: SourceText) (errors: ParseError array) =
        let counts = [| 1 .. errors.Length |]
        let filePath = @"C:\Users\admin\Documents\SampleFile.mk"
        for count, error in Array.zip counts errors do
            printfn $"{count}."
            printfn $"{error.GetFormattedMessage(sourceText, Some filePath)}"
            
    let functionTs (types: TypeSymbol array) =
        let returnType = Array.last types
        let parameterTypes = types[.. types.Length - 2]
        FunctionTypeSymbol(parameterTypes, returnType) |> TypeSymbol.FunctionTypeSymbol
        
    let userDefTs (name: string) =
        NamedTypeSymbol({ Name = name; Namespace = "" } |> Type.UserDefinedType) |> TypeSymbol.NamedTypeSymbol
            
            
[<RequireQualifiedAccess>]
module private Aliases =
    let int32 = NamedTypeSymbol(BuiltinType.Int32 |> Type.BuiltinType) |> TypeSymbol.NamedTypeSymbol
    let bool = NamedTypeSymbol(BuiltinType.Boolean |> Type.BuiltinType) |> TypeSymbol.NamedTypeSymbol
    let string = NamedTypeSymbol(BuiltinType.String |> Type.BuiltinType) |> TypeSymbol.NamedTypeSymbol
            


[<TestFixture>]
type Int32Expressions() =
    member this.TestCases: (string * TypeSymbol) array =
        [|
            ("5;", Aliases.int32)
            ("5 + 2 * 1;", Aliases.int32)
            ("1 + 2;", Aliases.int32)
            ("5 - 3;", Aliases.int32)
            ("4 * 2;", Aliases.int32)
            ("9 / 3;", Aliases.int32)
            ("7 % 4;", Aliases.int32)
            ("(1 + 2) * 3;", Aliases.int32)
            ("10 - (4 + 2);", Aliases.int32)
            ("(8 / 2) + (6 * 1);", Aliases.int32)
            ("(12 % 5) * 3;", Aliases.int32)
            ("(4 + 6) / (2 + 1);", Aliases.int32)
            ("((3 + 5) * 2) - (10 / 2);", Aliases.int32)
            ("((10 % 3) + 4) * (6 - 2);", Aliases.int32)
            ("(8 + (6 / 2)) * (7 - 3);", Aliases.int32)
            ("((15 - 5) * 2 + (9 % 4)) / 2;", Aliases.int32)
        |]
    
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    [<TestCase(5)>]
    // [<TestCase(6)>]
    [<TestCase(7)>]
    [<TestCase(8)>]
    [<TestCase(9)>]
    [<TestCase(10)>]
    [<TestCase(11)>]
    [<TestCase(12)>]
    [<TestCase(13)>]
    [<TestCase(14)>]
    [<TestCase(15)>]
    member this.``Execute Test Cases``(testCaseIndex: int) =
        let input, expectedTypeSymbol = this.TestCases[testCaseIndex]
        inferTypeFromExpressionStatement input expectedTypeSymbol
        
        
[<TestFixture>]
type BooleanExpressions() =
    member this.TestCases: (string * TypeSymbol) array =
        [|
            ("1 == 1;", Aliases.bool)
            ("2 != 3;", Aliases.bool)
            ("4 < 5;", Aliases.bool)
            ("5 <= 5;", Aliases.bool)
            ("6 > 2;", Aliases.bool)
            ("7 >= 7;", Aliases.bool)
            ("true && false;", Aliases.bool)
            ("true || false;", Aliases.bool)
            ("!false;", Aliases.bool)
            ("(1 == 1) && (2 < 3);", Aliases.bool)
            ("(3 > 2) || (4 < 1);", Aliases.bool)
            ("!(5 == 6);", Aliases.bool)
            ("(1 + 2 == 3) && (4 * 2 == 8);", Aliases.bool)
            ("(10 / 2 > 4) || (5 % 2 == 1);", Aliases.bool)
            ("!(3 + 3 != 6);", Aliases.bool)
            ("((2 < 5) && (1 + 1 == 2)) || false;", Aliases.bool)
        |]
    
    // TODO: '&&' and '||' is not supported, i kinda forgor
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    [<TestCase(5)>]
    // [<TestCase(6)>]
    // [<TestCase(7)>]
    [<TestCase(8)>]
    // [<TestCase(9)>]
    // [<TestCase(10)>]
    [<TestCase(11)>]
    // [<TestCase(12)>]
    // [<TestCase(13)>]
    [<TestCase(14)>]
    // [<TestCase(15)>]
    member this.``Execute Test Cases``(testCaseIndex: int) =
        let input, expectedTypeSymbol = this.TestCases[testCaseIndex]
        inferTypeFromExpressionStatement input expectedTypeSymbol
        
        
[<TestFixture>]
type StringExpressions() =
    member this.TestCases: (string * TypeSymbol) array =
        [|
            ("\"hello\" + \"world\";", Aliases.string)
            ("\"foo\" + \"bar\";", Aliases.string)
            ("\"a\" + \"b\" + \"c\";", Aliases.string)
            ("\"test\" + \" \" + \"case\";", Aliases.string)
            ("\"one\" + (\"two\" + \"three\");", Aliases.string)
            ("(\"a\" + \"b\") + (\"c\" + \"d\");", Aliases.string)
            ("\"prefix_\" + (\"inner\" + \"_suffix\");", Aliases.string)
            ("(\"x\" + \"y\") + (\"z\" + (\"1\" + \"2\"));", Aliases.string)
            ("\"start\" + (\"middle\" + (\"end\" + \"!\"));", Aliases.string)
            ("(\"a\" + \"b\") + ((\"c\" + \"d\") + (\"e\" + \"f\"));", Aliases.string)
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
    member this.``Execute Test Cases``(testCaseIndex: int) =
        let input, expectedTypeSymbol = this.TestCases[testCaseIndex]
        inferTypeFromExpressionStatement input expectedTypeSymbol
        
        
[<TestFixture>]
type PrefixExpressions() =
    member this.TestCases: (string * TypeSymbol) array =
        [|
            ("-1;", Aliases.int32)
            ("-(-2);", Aliases.int32)
            ("-(3 + 4);", Aliases.int32)
            ("-(5 * 2 - 1);", Aliases.int32)
            ("!true;", Aliases.bool)
            ("!false;", Aliases.bool)
            ("!(1 == 1);", Aliases.bool)
            ("!(2 != 3);", Aliases.bool)
            ("!((4 + 1) == 5);", Aliases.bool)
            ("!(true && false);", Aliases.bool)
            ("-(-(1 + 2));", Aliases.int32)
            ("!(!false);", Aliases.bool)
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
    // [<TestCase(9)>]
    [<TestCase(10)>]
    [<TestCase(11)>]
    member this.``Execute Test Cases``(testCaseIndex: int) =
        let input, expectedTypeSymbol = this.TestCases[testCaseIndex]
        inferTypeFromExpressionStatement input expectedTypeSymbol
        
        
[<TestFixture>]
type FunctionExpressions() =
    member this.TestCases: (string * TypeSymbol) array =
        [|
            (
                """fn(int x, int y) : int {
        x + y;
    };
    """,
                functionTs [| Aliases.int32; Aliases.int32; Aliases.int32 |]
            )
            (
                """fn() : int {
        x + 12;
    };
    """,
                functionTs [| Aliases.int32 |]
            )
            (
                """fn() : unit {
    };
    """,
                functionTs [| userDefTs "unit" |]
            )
            (
                """fn([int -> int] transform, int init_value) : int {
        init_value;
    };
    """,
                functionTs
                    [|
                        functionTs
                            [|
                                Aliases.int32
                                Aliases.int32
                            |]
                        Aliases.int32
                        Aliases.int32
                    |]
            )
            (
                """fn([int -> int -> int] full_transform, int init_value) : [int -> int] {
        init_value;
    };
    """,
                functionTs
                    [|
                        functionTs
                            [|
                                Aliases.int32
                                Aliases.int32
                                Aliases.int32
                            |]
                        Aliases.int32
                        functionTs
                            [|
                                Aliases.int32
                                Aliases.int32
                            |]
                    |]
            )
        |]
        
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    member this.``Execute Test Cases``(testCaseIndex: int) =
        let input, expectedTypeSymbol = this.TestCases[testCaseIndex]
        inferTypeFromExpressionStatement input expectedTypeSymbol
        
        
[<TestFixture>]
type IdentifierExpressions() =
    // last statement in the input is expected to be an expression statement.
    // the previous statements is expected to add context (adding symbols).
    member this.TestCases: (string * TypeSymbol) array =
        [|
            (
                """let x = ((15 - 5) * 2 + (9 % 4)) / 2;
x;""",
                Aliases.int32
            )
            (
                """let a = 3;
let b = 5;
let c = a + b * 2;
let d = (a * b) - (c / 2);
let e = d + (a + 1);
let f = e + (b - a * 2) / 3;
f + 1;""",
                Aliases.int32
            )
            (
                """let a = 10;
let b = 4;
let c = a * 2 + b - 3;
let d = (c - a) / (b + 1);
let e = (a + b) * (c - d) - (a - b) * 2;
let f = ((e + d) * (a - 1) - (c / 2)) / (b + 2);
let g = a + b + c + d + e + f - ((a + c) / (d + 1));
g;""",
                Aliases.int32
            )
            (
                """let x = 10;
let y = 4;
let ok = (x > y) && ((x - y) % 2 == 0);
let fail = !(x == y || y > x);
let result = ok || fail && (x - y * 2 == 2);""",
                Aliases.bool
            )
            (
                """let a = "hello";
let b = "world";
let c = a + " " + b;
let d = c + "!";
let e = "Greeting: " + d;
let f = e + " (" + a + ", " + b + ")";
let g = "Final: " + (f + " -- done.");
g;""",
                Aliases.string
            )
        |]
        
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    // [<TestCase(3)>]
    [<TestCase(4)>]
    member this.``Execute Test Cases``(testCaseIndex: int) =
        let input, expectedTypeSymbol = this.TestCases[testCaseIndex]
        inferTypeFromExpressionStatement input expectedTypeSymbol
        
        
[<TestFixture>]
type InvocationExpressions() =
    // last statement in the input is expected to be an expression statement.
    // the previous statements is expected to add context (adding symbols).
    member this.TestCases: (string * TypeSymbol) array =
        [|
            (
                """let add = fn(int x, int y) : int {
    x + y;
};
add(1, 2);""",
                Aliases.int32
            )
            (
                """let get_default_msg = fn() : string {
    "Hello World!";
};
get_default_msg();""",
                Aliases.string
            )
            (
                """let log = fn(string log_level, string msg) : string {
    "[" + log_level + "] " + msg;
};
let info = fn(string msg) : string {
    log("INFO", msg);
};
info("Some message");
""",
                Aliases.string
            )
            (
                """let get_transform = fn(string src) : [int -> int] {
    fn(int x) : int {
        x + 1;
    };
};
get_transform("gaussian");
""",
                functionTs
                    [|
                        Aliases.int32
                        Aliases.int32
                    |]
            )
            (
                """let are_equal = fn(string fst_str, string snd_str) : bool {
    fst_str == snd_str;
};
are_equal("something", "something");
""",
                Aliases.bool
            )
        |]
        
    [<TestCase(0)>]
    [<TestCase(1)>]
    [<TestCase(2)>]
    [<TestCase(3)>]
    [<TestCase(4)>]
    member this.``Execute Test Cases``(testCaseIndex: int) =
        let input, expectedTypeSymbol = this.TestCases[testCaseIndex]
        inferTypeFromExpressionStatement input expectedTypeSymbol
