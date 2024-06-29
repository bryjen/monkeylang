namespace MonkeyInterpreter

open System.Collections.Generic

open MonkeyInterpreter.Token

    
///
type Identifier =
    { Token: Token // 'Token and.IDENT' token
      Value: string }
with
    member this.GetTokenLiteral() = this.Value
    
    /// <inheritdoc/>
    override this.ToString() = $"{this.Value}"
    
    
///
type Program =
    { Statements: Statement list
      Errors: string list }
with
    member this.GetTokenLiteral() =
        match this.Statements with
        | firstStatement :: _ -> firstStatement.GetTokenLiteral()
        | [] -> ""
        
    /// <inheritdoc/>
    override this.ToString() =
        this.Statements
        |> List.map (_.ToString())
        |> String.concat "\n"
        
        
///
and Node =
    | Statement of Statement 
    | Expression of Expression
with
    member this.GetTokenLiteral() =
        match this with
        | Statement statement -> statement.GetTokenLiteral() 
        | Expression expression -> expression.GetTokenLiteral()
        

///
and Expression =
    | PrefixExpression of PrefixExpression 
    | InfixExpression of InfixExpression 
    | IfExpression of IfExpression 
    | CallExpression of CallExpression 
    | IndexExpression of IndexExpression
    | IntegerLiteral of IntegerLiteral
    | FunctionLiteral of FunctionLiteral
    | StringLiteral of StringLiteral
    | ArrayLiteral of ArrayLiteral
    | HashLiteral of HashLiteral
    | MacroLiteral of MacroLiteral
    | Identifier of Identifier
with
    member this.GetTokenLiteral() =
        match this with
        | PrefixExpression prefixExpression ->
            prefixExpression.Token.Literal
        | InfixExpression infixExpression -> 
            infixExpression.Token.Literal
        | IfExpression ifExpression -> 
            ifExpression.Token.Literal
        | CallExpression callExpression -> 
            callExpression.Token.Literal
        | IndexExpression indexExpression -> 
            indexExpression.Token.Literal
        | IntegerLiteral integerLiteral ->
            integerLiteral.Token.Literal
        | FunctionLiteral functionLiteral ->
            functionLiteral.Token.Literal
        | StringLiteral stringLiteral ->
            stringLiteral.Token.Literal
        | ArrayLiteral arrayLiteral ->
            arrayLiteral.Token.Literal
        | HashLiteral hashLiteral ->
            hashLiteral.Token.Literal
        | MacroLiteral macroLiteral ->
            macroLiteral.Token.Literal
        | Identifier identifier ->
            identifier.Token.Literal
            
    /// <inheritdoc/>
    override this.ToString() =
        match this with
        | PrefixExpression prefixExpression ->
            prefixExpression.ToString()
        | InfixExpression infixExpression -> 
            infixExpression.ToString()
        | IfExpression ifExpression -> 
            ifExpression.ToString()
        | CallExpression callExpression -> 
            callExpression.ToString()
        | IndexExpression indexExpression -> 
            indexExpression.ToString()
        | IntegerLiteral integerLiteral ->
            integerLiteral.ToString()
        | FunctionLiteral functionLiteral ->
            functionLiteral.ToString()
        | StringLiteral stringLiteral ->
            stringLiteral.ToString()
        | ArrayLiteral arrayLiteral ->
            arrayLiteral.ToString()
        | HashLiteral hashLiteral ->
            hashLiteral.ToString()
        | MacroLiteral macroLiteral ->
            macroLiteral.ToString()
        | Identifier identifier ->
            identifier.ToString()
    
    
///
and Statement =
    | LetStatement of LetStatement
    | ReturnStatement of ReturnStatement
    | ExpressionStatement of ExpressionStatement
    | BlockStatement of BlockStatement
with
    member this.GetTokenLiteral() =
        match this with
        | LetStatement letStatement ->
            letStatement.Token.Literal 
        | ReturnStatement returnStatement ->
            returnStatement.Token.Literal
        | ExpressionStatement expressionStatement ->
            expressionStatement.Token.Literal
        | BlockStatement blockStatement ->
            blockStatement.Token.Literal
            
    static member internal FromUnionCases(instance: obj): Statement =
        match instance with
        | :? LetStatement as letStatement ->
            Statement.LetStatement letStatement 
        | :? ReturnStatement as returnStatement ->
            Statement.ReturnStatement returnStatement 
        | :? ExpressionStatement as expressionStatement ->
            Statement.ExpressionStatement expressionStatement 
        | :? BlockStatement as blockStatement ->
            Statement.BlockStatement blockStatement
        | _ -> failwith $"Expected a \"Statement\" DU case type, got \"{instance.GetType()}\""
            
    /// <inheritdoc/>
    override this.ToString() =
        match this with
        | LetStatement letStatement ->
            letStatement.ToString() 
        | ReturnStatement returnStatement ->
            returnStatement.ToString() 
        | ExpressionStatement expressionStatement ->
            expressionStatement.ToString() 
        | BlockStatement blockStatement ->
            blockStatement.ToString() 


// 'General' Expression ands
///
and PrefixExpression =
    { Token: Token // the prefix token, ex. '!'
      Operator: string
      Right: Expression }
with
    member this.GetTokenLiteral() = this.Token.Literal
    
    /// <inheritdoc/>
    override this.ToString() = $"({this.Operator}{this.Right.ToString()})" 
    
    
///
and InfixExpression =
    { Token: Token // the operator token, ex. '+'
      Left: Expression
      Operator: string
      Right: Expression }


///
and IfExpression =
    { Token: Token
      Condition: Expression
      Consequence: BlockStatement
      Alternative: BlockStatement }
    
    
///
and CallExpression =
    { Token: Token
      Function: Expression
      Arguments: Expression list }


///
and IndexExpression =
    { Token: Token
      Left: Expression
      Index: Expression }
    
    
// Literal Expressions
///
and IntegerLiteral =
    { Token: Token
      Value: int64 }
with
    member this.GetTokenLiteral() = this.Token.Literal
    
    /// <inheritdoc/>
    override this.ToString() = $"{this.GetTokenLiteral()}" 
    
    
///
and FunctionLiteral =
    { Token: Token
      Parameters: Identifier list
      Body: BlockStatement }
    
    
///
and StringLiteral =
    { Token: Token
      Value: string }
    
    
///
and ArrayLiteral =
    { Token: Token
      Elements: Expression list }
    
    
///
and HashLiteral =
    { Token: Token
      Pairs: Dictionary<Expression, Expression> }
    
    
///
and MacroLiteral =
    { Token: Token
      Parameters: Identifier list
      Body: BlockStatement }
    
    
// Statement ands
///
and LetStatement =
     { Token: Token // 'Token and.LET' token 
       Name: Identifier 
       Value: Expression }
with
    /// <inheritdoc/>
    override this.ToString() = $"{this.Token.Literal} {this.Name.Value} = {this.Value};"
        

///
and ReturnStatement =
    { Token: Token // 'Token and.RETURN' token
      ReturnValue: Expression }
with
    /// <inheritdoc/>
    override this.ToString() = $"\"{this.Token.Literal} {this.ReturnValue};\""
        
        
///
and ExpressionStatement =
    { Token: Token // The first token of the expression
      Expression: Expression }
with
    /// <inheritdoc/>
    override this.ToString() = $"\"{this.Expression}\""
    
    
///
and BlockStatement =
    { Token: Token
      Statements: Statement list }