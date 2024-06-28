namespace MonkeyInterpreter.Test

open System
open MonkeyInterpreter.Helpers
open MonkeyInterpreter.Token
open NUnit.Framework

open FsToolkit.ErrorHandling

open MonkeyInterpreter


[<TestFixture>]
type AstTests() =
    
    /// Tests whether the monkey code `let myVar = anotherVar;` has the correct string representation
    /// given a valid statement.
    [<Test>]
    member this.``Test 'ToString()' 1``() = 
        let program =
            { Statements =
                [
                    // Statement # 1
                    Statement.LetStatement
                        { Token =
                            { Type = TokenType.LET
                              Literal = "let" }
                            
                          Name = (
                             { Token =
                                    { Type = TokenType.IDENT
                                      Literal = "myVar"}
                               Value = "myVar" }
                             : Identifier)
                          
                          Value = (Expression.Identifier
                             { Token =
                                 { Type = TokenType.IDENT
                                   Literal = "anotherVar"}
                               Value = "anotherVar" })}
                ];
              Errors = [] }
        
        let expectedOutput = "let myVar = anotherVar;"
        let actualOutput = (List.head program.Statements).ToString()
        
        if (expectedOutput <> actualOutput) then
            Assert.Fail($"Expected \n\"{expectedOutput}\"\n\nbut received\n\"{actualOutput}\'")
        else
            Assert.Pass()
