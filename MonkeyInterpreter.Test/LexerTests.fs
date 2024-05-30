namespace MonkeyInterpreter.Test

open FsToolkit.ErrorHandling

open MonkeyInterpreter.Lexer
open MonkeyInterpreter.Token
open NUnit.Framework


module private LexerLog =
    let mutable count = 0
    
    let mutable log = "Token#, TokenType, Literal\n"
    
    let addTokenToLog (token: Token) =
        count <- count + 1
        log <- log + $"{count}, {TokenType.ToCaseString token.Type}, {token.Literal}\n"
        
        
[<AutoOpen>]
module private Helpers = 
    let addNumbersToTestCase testCases =
        let mutable testCount = 0
        
        let addTestCountToTuple (tokenType, literal) =
            testCount <- testCount + 1
            (testCount, tokenType, literal)
            
        testCases
        |> List.map addTestCountToTuple
        
    
    let assertTokenTypeIsExpectedTokenType (testCount: int) (expectedTokenType: TokenType) (actualTokenType: TokenType) =
        if actualTokenType = expectedTokenType then
            Ok ()
        else
            let expectedTokenTypeStr = TokenType.ToCaseString expectedTokenType
            let actualTokenTypeStr = TokenType.ToCaseString actualTokenType 
            Error $"[test #{testCount}] Expected token type '{expectedTokenTypeStr}', but found '{actualTokenTypeStr}'"
            
    let assertLiteralIsExpectedLiteral (testCount: int) (expectedLiteral: string) (actualLiteral: string) =
        if expectedLiteral = actualLiteral then
            Ok ()
        else
            Error $"[test #{testCount}] Expected literal '{expectedLiteral}', but found '{actualLiteral}'"
            
    /// If any 'Result' DU is 'ERROR', return 'ERROR + error message', returns OK otherwise
    let processResultsList (resultsList: Result<unit, string> list) : Result<unit, string> =
        result {
            for testResult in resultsList do
                do! testResult
            return ()
        }


[<TestFixture>]
type LexerTests() =
            
    let testLexer testInput testCases =
        let lexer = Lexer(testInput)
        
        // Actual testing
        let testResult = 
            addNumbersToTestCase testCases
            |> List.map (fun testCase ->
                result {
                    let testCount, expectedTokenType, expectedLiteral = testCase
                    
                    let token = lexer.NextToken()
                    LexerLog.addTokenToLog token
                    
                    do! assertTokenTypeIsExpectedTokenType testCount expectedTokenType token.Type
                    do! assertLiteralIsExpectedLiteral testCount expectedLiteral token.Literal
                    return () 
                })
            |> processResultsList
            
        // Clean-up, post logs, etc. before passing/failing
        printfn $"Log:\n{LexerLog.log}"
        
        // Return test result
        testResult
        
    
    [<Test>]
    member this.``Test next token 1``() =
        let testInput = "=+(){},;"
        
        let testCases = [
            (TokenType.ASSIGN,    "=")
            (TokenType.PLUS,      "+")
            (TokenType.LPAREN,    "(")
            (TokenType.RPAREN,    ")")
            (TokenType.LBRACE,    "{")
            (TokenType.RBRACE,    "}")
            (TokenType.COMMA,     ",")
            (TokenType.SEMICOLON, ";")
            (TokenType.EOF,       "")
        ]
        
        testLexer testInput testCases
        |> function
            | Ok _ ->Assert.Pass()
            | Error errorMsg -> Assert.Fail(errorMsg)
            
    [<Test>]
    member this.``Test next token 2``() =
        let testInput = """let five = 5;
let ten = 10;
            
let add = fn(x, y) {
    x + y;
};
            
let result = add(five, ten);"""
        
        let testCases = [
            (TokenType.LET, "let")
            (TokenType.IDENT, "five")
            (TokenType.ASSIGN, "=")
            (TokenType.INT, "5")
            (TokenType.SEMICOLON, ";")
            (TokenType.LET, "let")
            (TokenType.IDENT, "ten")
            (TokenType.ASSIGN, "=")
            (TokenType.INT, "10")
            (TokenType.SEMICOLON, ";")
            (TokenType.LET, "let")
            (TokenType.IDENT, "add")
            (TokenType.ASSIGN, "=")
            (TokenType.FUNCTION, "fn")
            (TokenType.LPAREN, "(")
            (TokenType.IDENT, "x")
            (TokenType.COMMA, ",")
            (TokenType.IDENT, "y")
            (TokenType.RPAREN, ")")
            (TokenType.LBRACE, "{")
            (TokenType.IDENT, "x")
            (TokenType.PLUS, "+")
            (TokenType.IDENT, "y")
            (TokenType.SEMICOLON, ";")
            (TokenType.RBRACE, "}")
            (TokenType.SEMICOLON, ";")
            (TokenType.LET, "let")
            (TokenType.IDENT, "result")
            (TokenType.ASSIGN, "=")
            (TokenType.IDENT, "add")
            (TokenType.LPAREN, "(")
            (TokenType.IDENT, "five")
            (TokenType.COMMA, ",")
            (TokenType.IDENT, "ten")
            (TokenType.RPAREN, ")")
            (TokenType.SEMICOLON, ";")
            (TokenType.EOF, "")
        ]
        
        testLexer testInput testCases
        |> function
            | Ok _ ->Assert.Pass()
            | Error errorMsg -> Assert.Fail(errorMsg)
