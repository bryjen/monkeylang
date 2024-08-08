module Monkey.Backend.Compiler

open FsToolkit.ErrorHandling

open Monkey.Frontend.Ast
open Monkey.Frontend.Eval.Object
open Monkey.Backend.Code

type Compiler =
    { Instructions: Instructions
      Constants: Object list }
with
    static member New : Compiler =
        { Instructions = Instructions [|  |]
          Constants = [] }
        
    // START Private members
    member private this.AddConstant(object: Object) =
        let newCompiler = { this with Constants = object :: this.Constants }
        newCompiler, newCompiler.Constants.Length - 1   // will rev when converting to bytecode
        
    member private this.Emit(opcode: Opcode, operands: int array) =
        let instruction = make opcode operands
        let newCompiler, pos = this.AddInstruction(instruction)
        newCompiler, pos

    member private this.AddInstruction(instruction: byte array) =
        let newInstructions = Array.append (this.Instructions.GetBytes()) instruction
        let newCompiler = { this with Instructions = Instructions newInstructions }
        newCompiler, instruction.Length
        
    member private this.ProcessPrefixOperator(operator: string) =
        match operator with
        | "-" -> this.Emit(Opcode.OpMinus, [|  |]) |> Ok
        | "!" -> this.Emit(Opcode.OpBang, [|  |]) |> Ok
        | s -> Error $"'{operator}' is not a valid prefix expression operator" 
        
    member private this.ProcessInfixExprOperator(operator: string) =
        match operator with
        | "+" -> this.Emit(Opcode.OpAdd, [|  |]) |> Ok
        | "-" -> this.Emit(Opcode.OpSub, [|  |]) |> Ok
        | "*" -> this.Emit(Opcode.OpMul, [|  |]) |> Ok
        | "/" -> this.Emit(Opcode.OpDiv, [|  |]) |> Ok
        
        | "==" -> this.Emit(Opcode.OpEqual, [|  |]) |> Ok
        | "!=" -> this.Emit(Opcode.OpNotEqual, [|  |]) |> Ok
        | ">" -> this.Emit(Opcode.OpGreaterThan, [|  |]) |> Ok
        // less than operator doesn't exist, code re-orders expression to use greater than instead
        
        | s -> Error $"'{operator}' is not a valid infix expression operator" 
    // END Private members
        
    member this.Compile(node: Node) : Result<Compiler, string> =
        match node with
        | Statement statement -> this.CompileStatement(statement)
        | Expression expression -> this.CompileExpression(expression)
        
    member private this.CompileStatement(statement: Statement) =
        match statement with
        | LetStatement letStatement -> failwith "todo"
        | ReturnStatement returnStatement -> failwith "todo"
        | ExpressionStatement expressionStatement ->
            this.CompileExpression(expressionStatement.Expression)
            |> Result.map (_.Emit(Opcode.OpPop, [|  |]))
            |> Result.map fst
        | BlockStatement blockStatement -> failwith "todo"
        
    member private this.CompileExpression(expression: Expression) =
        match expression with
        | IntegerLiteral integerLiteral ->
            let integerObj = Object.IntegerType integerLiteral.Value
            let newCompiler, constPos = this.AddConstant(integerObj)
            let newCompiler, _ = newCompiler.Emit(Opcode.OpConstant, [| constPos |])
            Ok newCompiler
        | BooleanLiteral booleanLiteral ->
            let opcodeToEmit = if booleanLiteral.Value then Opcode.OpTrue else Opcode.OpFalse
            let newCompiler, _ = this.Emit(opcodeToEmit, [|  |])
            Ok newCompiler
        | StringLiteral stringLiteral -> failwith "todo"
        | Expression.FunctionLiteral functionLiteral -> failwith "todo"
        | ArrayLiteral arrayLiteral -> failwith "todo"
        | HashLiteral hashLiteral -> failwith "todo"
        | MacroLiteral macroLiteral -> failwith "todo"
        | Expression.Identifier identifier -> failwith "todo"
        
        | PrefixExpression prefixExpression ->
            this.CompilePrefixExpression(prefixExpression)
        | InfixExpression infixExpression ->
            this.CompileInfixExpression(infixExpression)
        | IfExpression ifExpression -> failwith "todo"
        | CallExpression callExpression -> failwith "todo"
        | IndexExpression indexExpression -> failwith "todo"
        
    member private this.CompilePrefixExpression(prefixExpression: PrefixExpression) =
        this.CompileExpression(prefixExpression.Right)
        |> Result.bind (_.ProcessPrefixOperator(prefixExpression.Operator))
        |> Result.map fst
        
    member private this.CompileInfixExpression(infixExpression: InfixExpression) =
        // We perform code re-ordering here
        let left, right, operator =
            match infixExpression.Operator with
            | "<" -> infixExpression.Right, infixExpression.Left, ">"
            | _ -> infixExpression.Left, infixExpression.Right, infixExpression.Operator
            
        this.CompileExpression(left)
        |> Result.bind (_.CompileExpression(right))
        |> Result.bind (_.ProcessInfixExprOperator(operator))
        |> Result.map fst
        
        
    member this.Bytecode() : Bytecode =
        { Instructions = this.Instructions 
          Constants = this.Constants |> List.toArray |> Array.rev }
    
    
and Bytecode =
    { Instructions: Instructions
      Constants: Object array }
      
