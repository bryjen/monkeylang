namespace MonkeyInterpreter

open System
open MonkeyInterpreter.Token

[<AutoOpen>]
module Ast =
    
    ///
    type Identifier =
        { TokenType: TokenType // 'TokenType.IDENT' token
          Value: string }
        
        member this.TokenLiteral() = this.Value
        
        static member FromToken token =
            if token.Type <> TokenType.IDENT then
                let errorMsg = $"Expected token type of \"IDENT\", but received \"{TokenType.ToCaseString token.Type}\""
                raise (ArgumentException(errorMsg))
            else
                { TokenType = TokenType.IDENT; Value = token.Literal }
            

    ///
    type Node =
        | Statement of Statement
        | Expression of Expression
        
        member this.TokenLiteral() =
            match this with
            | Statement statement -> statement.TokenLiteral() 
            | Expression expression -> expression.TokenLiteral() 
        
        
    ///
    and Statement =
        | LetStatement of LetStatement
        
        member this.TokenLiteral() =
            match this with
            | LetStatement letStatement -> letStatement.TokenLiteral()
            
        member this.StatementNode() =
            failwith "todo"
            
        
    ///
    and Expression =
        | Something
        
        member this.TokenLiteral() =
            match this with
            | Something -> "todo"
            
        member this.ExpressionNode() =
            failwith "todo"
        
        
    ///
    and LetStatement =
         { Token: Token // 'TokenType.LET' token 
           Name: Identifier 
           Value: Expression }
        
         member this.TokenLiteral() = this.Token.Literal
            
         member this.StatementNode() =
             failwith "todo"
        
            
    ///
    type Program =
        { Statements: Statement list }
        
        member this.TokenLiteral() =
            match this.Statements with
            | firstStatement :: _ -> firstStatement.TokenLiteral()
            | [] -> ""