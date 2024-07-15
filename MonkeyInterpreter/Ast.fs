namespace rec MonkeyInterpreter

open System.Collections.Generic
open System.Diagnostics


    
///
type Identifier =
    { Token: Token // 'Token and.IDENT' token
      Value: string }
with
    member this.GetTokenLiteral() = this.Value
    
    /// <inheritdoc/>
    override this.ToString() = $"{this.Value}"
    
#if DEBUG
    static member internal Default = { Token = { Literal = ""; Type = EOF }; Value = "" }
#endif
    
    
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
type Node =
    | Statement of Statement 
    | Expression of Expression
with
    member this.GetTokenLiteral() =
        match this with
        | Statement statement -> statement.GetTokenLiteral() 
        | Expression expression -> expression.GetTokenLiteral()
        
///
[<DebuggerDisplay("{ToString()}")>] 
type Expression =
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
    | BooleanLiteral of BooleanLiteral
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
        | BooleanLiteral booleanLiteral ->
            booleanLiteral.Token.Literal
            
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
        | BooleanLiteral booleanLiteral ->
            booleanLiteral.ToString()
    
    
///
[<DebuggerDisplay("{ToString()}")>] 
type Statement =
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
type PrefixExpression =
    { Token: Token // the prefix token, ex. '!'
      Operator: string
      Right: Expression }
with
    member this.GetTokenLiteral() = this.Token.Literal
    
    /// <inheritdoc/>
    override this.ToString() = $"({this.Operator}{this.Right.ToString()})" 
    
    
///
type InfixExpression =
    { Token: Token // the operator token, ex. '+'
      Left: Expression
      Operator: string
      Right: Expression }
with
    member this.GetTokenLiteral() = this.Token.Literal
    
    /// <inheritdoc/>
    override this.ToString() = $"({this.Left.ToString()} {this.Operator} {this.Right.ToString()})" 


///
type IfExpression =
    { Token: Token
      Condition: Expression
      Consequence: BlockStatement
      Alternative: BlockStatement Option }
with
    member this.GetTokenLiteral() = this.Token.Literal
    
    /// <inheritdoc/>
    override this.ToString() =
        let baseString = $"if {this.Condition.ToString()} {{ {this.Consequence.ToString()} }}"
        
        match this.Alternative with
        | Some alternative -> baseString + $" else {{ {alternative.ToString()} }}" 
        | None -> baseString 

///
type IndexExpression =
    { Token: Token
      Left: Expression
      Index: Expression }
    
    
    
// Literal Expressions
///
type IntegerLiteral =
    { Token: Token
      Value: int64 }
with
    member this.GetTokenLiteral() = this.Token.Literal
    
    /// <inheritdoc/>
    override this.ToString() = $"{this.GetTokenLiteral()}" 
    
    
///
type FunctionLiteral =
    { Token: Token
      Parameters: Identifier list
      Body: BlockStatement }
with
    member this.GetTokenLiteral() = this.Token.Literal
    
    /// <inheritdoc/>
    override this.ToString() =
        let commaSeparatedParameters = String.concat ", " (this.Parameters |> List.map (_.Value)) 
        $"{this.GetTokenLiteral()} ({commaSeparatedParameters}) {{ {this.Body.ToString()} }}" 
    
    
///
type CallExpression =
    { Token: Token  // The '(' token
      Function: CallExpr 
      Arguments: Expression list }
with
    member this.GetTokenLiteral() = this.Token.Literal
    
    /// <inheritdoc/>
    override this.ToString() =
        let commaSeparatedArguments = String.concat ", " (this.Arguments |> List.map (_.ToString()))
        $"{this.Function.ToString()}({commaSeparatedArguments})"
        
and CallExpr =
    | Identifier of Identifier
    | FunctionLiteral of FunctionLiteral
with
    static member FromExpression (expression: Expression) =
        match expression with
        | Expression.Identifier identifier -> Some (CallExpr.Identifier identifier)
        | Expression.FunctionLiteral functionLiteral -> Some (CallExpr.FunctionLiteral functionLiteral)
        | _ -> None
        
    static member ToExpression (callExpr: CallExpr) =
        match callExpr with
        | Identifier identifier -> Expression.Identifier identifier 
        | FunctionLiteral functionLiteral -> Expression.FunctionLiteral functionLiteral 
    
    /// <inheritdoc/>
    override this.ToString() =
        match this with
        | Identifier identifier -> identifier.ToString() 
        | FunctionLiteral functionLiteral -> functionLiteral.ToString() 
    
    
    
    
    
///
type StringLiteral =
    { Token: Token
      Value: string }
    
    
///
type ArrayLiteral =
    { Token: Token
      Elements: Expression list }
    
    
///
type HashLiteral =
    { Token: Token
      Pairs: Dictionary<Expression, Expression> }
    
    
///
type MacroLiteral =
    { Token: Token
      Parameters: Identifier list
      Body: BlockStatement }
    
///
type BooleanLiteral =
    { Token: Token
      Value: bool }
with
    member this.GetTokenLiteral() = this.Token.Literal
    
    /// <inheritdoc/>
    override this.ToString() = $"{this.GetTokenLiteral()}" 
    
    
    
// Statement ands
///
type LetStatement =
     { Token: Token // 'Token and.LET' token 
       Name: Identifier 
       Value: Expression }
with
    /// <inheritdoc/>
    override this.ToString() = $"{this.Token.Literal} {this.Name.Value} = {this.Value};"
        

///
type ReturnStatement =
    { Token: Token // 'Token and.RETURN' token
      ReturnValue: Expression }
with
    /// <inheritdoc/>
    override this.ToString() = $"{this.Token.Literal} {this.ReturnValue};"
        
        
///
type ExpressionStatement =
    { Token: Token // The first token of the expression
      Expression: Expression }
with
    /// <inheritdoc/>
    override this.ToString() = $"{this.Expression}"
    
    
///
type BlockStatement =
    { Token: Token
      Statements: Statement list }
with
    /// <inheritdoc/>
    override this.ToString() =
        let statementToString statement =
            match statement with
            | ExpressionStatement exprStatement -> exprStatement.ToString() + ";"
            | statement -> statement.ToString()
            
        let statementStrings = List.map statementToString this.Statements
        String.concat " " statementStrings
