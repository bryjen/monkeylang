namespace MonkeyInterpreter

open System
open MonkeyInterpreter.Token

    
///
type Identifier =
    { Token: Token // 'TokenType.IDENT' token
      Value: string }
with
    member this.TokenLiteral() = this.Value
        

///
type Node =
    | Statement of Statement
    | Expression of Expression
with
    member this.TokenLiteral() =
        match this with
        | Statement statement -> statement.TokenLiteral() 
        | Expression expression -> expression.TokenLiteral() 
    
    
///
and Statement =
    | LetStatement of LetStatement
with
    member this.TokenLiteral() =
        match this with
        | LetStatement letStatement -> letStatement.TokenLiteral()
        
    member this.StatementNode() =
        match this with
        | LetStatement letStatement -> letStatement.StatementNode()
        
    /// <inheritdoc/>
    override this.ToString() =
        match this with
        | LetStatement letStatement -> letStatement.ToString()
        
    
///
and Expression =
    | Something
with
    member this.TokenLiteral() =
        match this with
        | Something -> "todo"
        
    member this.ExpressionNode() =
        failwith "todo"
        
    /// <inheritdoc/>
    override this.ToString() =
        // TODO: Update when expression actually does something
        "EXPRESSION"
    
    
///
and LetStatement =
     { Token: Token // 'TokenType.LET' token 
       Name: Identifier 
       Value: Expression }
with
    member this.TokenLiteral() = this.Token.Literal
        
    member this.StatementNode() =
        failwith "todo"
        
    /// <inheritdoc/>
    override this.ToString() =
        $"\"let {this.Name.Value} = {this.Value}\""
    
        
///
type Program =
    { Statements: Statement list }
with
    member this.TokenLiteral() =
        match this.Statements with
        | firstStatement :: _ -> firstStatement.TokenLiteral()
        | [] -> ""
        
        
    /// <inheritdoc/>
    override this.ToString() =
        this.Statements
        |> List.map (_.ToString())
        |> String.concat "\n"