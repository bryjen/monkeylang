namespace rec Monkey.Frontend.Ast

open System.Diagnostics
open Monkey.Frontend.Token



type Program =
    { Statements: Statement list
      Errors: string list }
with
    member this.GetTokenLiteral() =
        match this.Statements with
        | firstStatement :: _ -> firstStatement.GetTokenLiteral()
        | [] -> ""
        
    override this.ToString() =
        this.Statements
        |> List.map (_.ToString())
        |> String.concat "\n"


    
type Identifier =
    { Token: Token // 'Token and.IDENT' token
      Value: string }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; Value = "" }
#endif
    
    member this.GetTokenLiteral() = this.Value
    
    override this.ToString() = $"{this.Value}"
    
    
    
[<DebuggerDisplay("{ToString()}")>] 
type Node =
    | Statement of Statement 
    | Expression of Expression
with
    member this.GetTokenLiteral() =
        match this with
        | Statement statement -> statement.GetTokenLiteral() 
        | Expression expression -> expression.GetTokenLiteral()
        
        
        
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
#if DEBUG
    // Default is an int literal
    static member internal Default: Expression = Expression.IntegerLiteral IntegerLiteral.Default
#endif

    member this.GetTokenLiteral() =
        match this with
        | PrefixExpression prefixExpression -> prefixExpression.Token.Literal
        | InfixExpression infixExpression -> infixExpression.Token.Literal
        | IfExpression ifExpression -> ifExpression.Token.Literal
        | CallExpression callExpression -> callExpression.Token.Literal
        | IndexExpression indexExpression -> indexExpression.Token.Literal
        | IntegerLiteral integerLiteral -> integerLiteral.Token.Literal
        | FunctionLiteral functionLiteral -> functionLiteral.Token.Literal
        | StringLiteral stringLiteral -> stringLiteral.Token.Literal
        | ArrayLiteral arrayLiteral -> arrayLiteral.Token.Literal
        | HashLiteral hashLiteral -> hashLiteral.Token.Literal
        | MacroLiteral macroLiteral -> macroLiteral.Token.Literal
        | Identifier identifier -> identifier.Token.Literal
        | BooleanLiteral booleanLiteral -> booleanLiteral.Token.Literal
        
#if DEBUG 
    member internal this.GetNonUnionCaseType() =
        match this with
        | PrefixExpression value -> value.GetType()
        | InfixExpression value -> value.GetType()
        | IfExpression value -> value.GetType()
        | CallExpression value -> value.GetType()
        | IndexExpression value -> value.GetType()
        | IntegerLiteral value -> value.GetType()
        | FunctionLiteral value -> value.GetType()
        | StringLiteral value -> value.GetType()
        | ArrayLiteral value -> value.GetType()
        | HashLiteral value -> value.GetType()
        | MacroLiteral value -> value.GetType()
        | Identifier value -> value.GetType()
        | BooleanLiteral value -> value.GetType()
#endif
            
    override this.ToString() =
        match this with
        | PrefixExpression prefixExpression -> prefixExpression.ToString()
        | InfixExpression infixExpression ->  infixExpression.ToString()
        | IfExpression ifExpression ->  ifExpression.ToString()
        | CallExpression callExpression ->  callExpression.ToString()
        | IndexExpression indexExpression -> indexExpression.ToString()
        | IntegerLiteral integerLiteral -> integerLiteral.ToString()
        | FunctionLiteral functionLiteral -> functionLiteral.ToString()
        | StringLiteral stringLiteral -> stringLiteral.ToString()
        | ArrayLiteral arrayLiteral -> arrayLiteral.ToString()
        | HashLiteral hashLiteral -> hashLiteral.ToString()
        | MacroLiteral macroLiteral -> macroLiteral.ToString()
        | Identifier identifier -> identifier.ToString()
        | BooleanLiteral booleanLiteral -> booleanLiteral.ToString()
    
    
    
[<DebuggerDisplay("{ToString()}")>] 
type Statement =
    | LetStatement of LetStatement
    | ReturnStatement of ReturnStatement
    | ExpressionStatement of ExpressionStatement
    | BlockStatement of BlockStatement
with
#if DEBUG
    // Default is expression statement containing the default expression
    static member internal Default: Statement = Statement.ExpressionStatement ExpressionStatement.Default
#endif
    
    member this.GetTokenLiteral() =
        match this with
        | LetStatement letStatement -> letStatement.Token.Literal 
        | ReturnStatement returnStatement -> returnStatement.Token.Literal
        | ExpressionStatement expressionStatement -> expressionStatement.Token.Literal
        | BlockStatement blockStatement -> blockStatement.Token.Literal
            
    override this.ToString() =
        match this with
        | LetStatement letStatement -> letStatement.ToString() 
        | ReturnStatement returnStatement -> returnStatement.ToString() 
        | ExpressionStatement expressionStatement -> expressionStatement.ToString() 
        | BlockStatement blockStatement -> blockStatement.ToString()


// 'General' Expression ands
type PrefixExpression =
    { Token: Token // the prefix token, ex. '!'
      Operator: string
      Right: Expression }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; Operator = ""; Right = Expression.Default }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() = $"({this.Operator}{this.Right.ToString()})" 
    
    
type InfixExpression =
    { Token: Token // the operator token, ex. '+'
      Left: Expression
      Operator: string
      Right: Expression }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; Left = Expression.Default; Operator = ""; Right = Expression.Default }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() = $"({this.Left.ToString()} {this.Operator} {this.Right.ToString()})" 


type IfExpression =
    { Token: Token
      Condition: Expression
      Consequence: BlockStatement
      Alternative: BlockStatement Option }
with
#if DEBUG
    static member internal Default = { Token = Token.Default
                                       Condition = Expression.Default
                                       Consequence = BlockStatement.Default
                                       Alternative = None }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() =
        let baseString = $"if {this.Condition.ToString()} {{ {this.Consequence.ToString()} }}"
        
        match this.Alternative with
        | Some alternative -> baseString + $" else {{ {alternative.ToString()} }}" 
        | None -> baseString 


type IndexExpression =
    { Token: Token
      Left: Expression
      Index: Expression }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; Left = Expression.Default; Index = Expression.Default }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() = $"({this.Left})[{this.Index}]" 
    
    
    
// Literal Expressions
type IntegerLiteral =
    { Token: Token
      Value: int64 }
with
#if DEBUG
    static member internal Default: IntegerLiteral = { Token = Token.Default; Value = 0 }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() = $"{this.GetTokenLiteral()}" 
    
    
type StringLiteral =
    { Token: Token
      Value: string }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; Value = "" }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() = $"{this.GetTokenLiteral()}" 
    
    
type ArrayLiteral =
    { Token: Token
      Elements: Expression array }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; Elements = [| |] }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() = $"[ ... ], Length = {this.Elements.Length}" 
    
    
type HashLiteral =
    { Token: Token
      Pairs: Map<Expression, Expression> }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; Pairs = Map.ofList [] }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() = $"{{ ... }}, Count = {this.Pairs.Count}" 
    
    
type FunctionLiteral =
    { Token: Token
      Parameters: Identifier list
      Body: BlockStatement }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; Parameters = [ ]; Body = BlockStatement.Default }
#endif
        
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() =
        let commaSeparatedParameters = String.concat ", " (this.Parameters |> List.map (_.Value)) 
        $"{this.GetTokenLiteral()} ({commaSeparatedParameters}) {{ {this.Body.ToString()} }}" 
    
    
type CallExpression =
    { Token: Token  // The '(' token
      Function: CallExpr 
      Arguments: Expression list }
with
#if DEBUG
    static member internal Default = { Token = Token.Default
                                       Function = CallExpr.Identifier Identifier.Default
                                       Arguments = [ ] }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() =
        let commaSeparatedArguments = String.concat ", " (this.Arguments |> List.map (_.ToString()))
        $"{this.Function.ToString()}({commaSeparatedArguments})"
        
// Create 'sub' union type to restrict the expressions that can be considered as values for 'CallExpression.Function'
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
    
    override this.ToString() =
        match this with
        | Identifier identifier -> identifier.ToString() 
        | FunctionLiteral functionLiteral -> functionLiteral.ToString() 
    
    
type MacroLiteral =
    { Token: Token
      Parameters: Identifier list
      Body: BlockStatement }
with
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() = $"{this.GetTokenLiteral()}" 
    
    
type BooleanLiteral =
    { Token: Token
      Value: bool }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; Value = true }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() = $"{this.GetTokenLiteral()}" 
    
    
    
// Statement types
type LetStatement =
     { Token: Token // 'Token and.LET' token 
       Name: Identifier 
       Value: Expression }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; Name = Identifier.Default; Value = Expression.Default }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() = $"{this.Token.Literal} {this.Name.Value} = {this.Value};"
        

type ReturnStatement =
    { Token: Token // 'Token and.RETURN' token
      ReturnValue: Expression }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; ReturnValue = Expression.Default }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() = $"{this.Token.Literal} {this.ReturnValue};"
        
        
type ExpressionStatement =
    { Token: Token // The first token of the expression
      Expression: Expression }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; Expression = Expression.Default }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() = $"{this.Expression}"
    
    
type BlockStatement =
    { Token: Token
      Statements: Statement list }
with
#if DEBUG
    static member internal Default = { Token = Token.Default; Statements = [ Statement.Default ] }
#endif
    
    member this.GetTokenLiteral() = this.Token.Literal
    
    override this.ToString() =
        let statementToString statement =
            match statement with
            | ExpressionStatement exprStatement -> exprStatement.ToString() + ";"
            | statement -> statement.ToString()
            
        let statementStrings = List.map statementToString this.Statements
        String.concat " " statementStrings
