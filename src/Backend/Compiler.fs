module rec Monkey.Backend.Compiler

open FsToolkit.ErrorHandling

open Monkey.Backend.SymbolTable
open Monkey.Frontend.Ast
open Monkey.Frontend.Eval.Object
open Monkey.Backend.Code


let private makePrefixOperator (operator: string) : Result<byte array, string> =
    match operator with
    | "-" -> make Opcode.OpMinus [|  |] |> Ok
    | "!" -> make Opcode.OpBang [|  |] |> Ok
    | s -> Error $"'{operator}' is not a valid prefix expression operator" 
    
let private makeInfixOperator (operator: string) : Result<byte array, string> =
    match operator with
    | "+" -> make Opcode.OpAdd [|  |] |> Ok
    | "-" -> make Opcode.OpSub [|  |] |> Ok
    | "*" -> make Opcode.OpMul [|  |] |> Ok
    | "/" -> make Opcode.OpDiv [|  |] |> Ok
    
    | "==" -> make Opcode.OpEqual [|  |] |> Ok
    | "!=" -> make Opcode.OpNotEqual [|  |] |> Ok
    | ">" -> make Opcode.OpGreaterThan [|  |] |> Ok
    // less than operator doesn't exist, code re-orders expression to use greater than instead
    
    | s -> Error $"'{operator}' is not a valid infix expression operator" 



type Compiler =
    { Instructions: Instructions
      Constants: Object list
      SymbolTable: SymbolTable }
with
    static member New : Compiler =
        { Instructions = Instructions [|  |]
          Constants = []
          SymbolTable = SymbolTable.New }
        
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
    // END Private members
    

    // TODO: See if you can re-write this, looks like shit
    member this.CompileNodes(nodes: Node array) : Result<Compiler, string> =
        let rec something (compiler: Compiler) (currentIndex: int) =
            match currentIndex with
            | i when i < nodes.Length -> 
                let node = nodes[currentIndex]
                let compilationResult = 
                    match node with
                    | Statement statement -> compiler.CompileStatement(true, statement)
                    | Expression expression -> compiler.CompileExpression(expression)
                    
                match compilationResult with
                | Ok (newCompiler, compiledBytes) ->
                    let newCompiler, _ = newCompiler.AddInstruction(compiledBytes)
                    something newCompiler (currentIndex + 1)
                | Error error -> failwith "todo"
            | i ->
                Ok compiler
                
        something this 0 
            
    member this.CompileNode(node: Node) =
        match node with
        | Statement statement ->
            this.CompileStatement(true, statement)
        | Expression expression ->
            this.CompileExpression(expression)
        |> Result.map (fun (compiler, byteArr) -> compiler.AddInstruction(byteArr)) 
        |> Result.map fst
        

    (*
    The 'byte array' in 'Compiler * byte array' signifies the compiled byte instructions from the given statement or
    expression. The 'Compiler' represents the 'new' compiler with an updated constant pool if the statement or expression
    introduces constants.
    
    Adding the compiled insturctions to the compiler is delegated upwards to one of the caller methods.
    *)
        
    // TODO: See if you can find a 'better' than using the 'generateOpPop' param
    member private this.CompileStatement(generateOpPop: bool, statement: Statement) : Result<Compiler * byte array, string> =
        match statement with
        | LetStatement letStatement ->
            result {
                let! newCompiler, exprBytes = this.CompileExpression(letStatement.Value)
                let newSymbolTable, symbol = this.SymbolTable.Define(letStatement.Name.Value)
                
                let varBindingBytes = make Opcode.OpSetGlobal [| symbol.Index |]
                let bytes = Array.concat [| exprBytes; varBindingBytes |]
                return ({ newCompiler with SymbolTable = newSymbolTable }, bytes)
            }
        | ReturnStatement returnStatement -> failwith "todo"
        | ExpressionStatement expressionStatement ->
            result {
                let! newCompiler, exprBytes = this.CompileExpression(expressionStatement.Expression)
                let opPopBytes = if generateOpPop then make Opcode.OpPop [|  |] else [| |]
                let exprStatementBytes = Array.concat [| exprBytes; opPopBytes |]
                return newCompiler, exprStatementBytes
            }
        | BlockStatement blockStatement ->
            let rec compileMultipleStatements (compiler: Compiler) (statements: Statement list) (compiledInstructions: byte array) =
                let isLastStatement = statements.Length <= 1
                match statements with
                | statement :: remaining ->
                    match compiler.CompileStatement(not isLastStatement, statement) with
                    | Ok (newCompiler, bytes) ->
                        let newCompiledInstructions = Array.append compiledInstructions bytes 
                        compileMultipleStatements newCompiler remaining newCompiledInstructions 
                    | Error error ->
                        Error error
                | [] ->
                    Ok (compiler, compiledInstructions)
                    
            compileMultipleStatements this blockStatement.Statements [| |]
        
    member private this.CompileExpression(expression: Expression) : Result<Compiler * byte array, string> =
        match expression with
        | IntegerLiteral integerLiteral ->
            let integerObj = Object.IntegerType integerLiteral.Value
            let newCompiler, constIndex = this.AddConstant(integerObj)
            let opConstantBytes = make Opcode.OpConstant [| constIndex |]
            Ok (newCompiler, opConstantBytes)
        | BooleanLiteral booleanLiteral ->
            let opcodeToEmit = if booleanLiteral.Value then Opcode.OpTrue else Opcode.OpFalse
            let booleanBytes = make opcodeToEmit [|  |]
            Ok (this, booleanBytes)
        | StringLiteral stringLiteral -> failwith "todo"
        | Expression.FunctionLiteral functionLiteral -> failwith "todo"
        | ArrayLiteral arrayLiteral -> failwith "todo"
        | HashLiteral hashLiteral -> failwith "todo"
        | MacroLiteral macroLiteral -> failwith "todo"
        | Expression.Identifier identifier ->
            this.CompileIdentifier identifier
        
        | PrefixExpression prefixExpression ->
            this.CompilePrefixExpression(prefixExpression)
        | InfixExpression infixExpression ->
            this.CompileInfixExpression(infixExpression)
        | IfExpression ifExpression ->
            this.CompileIfExpression(ifExpression)
        | CallExpression callExpression -> failwith "todo"
        | IndexExpression indexExpression -> failwith "todo"
        
    member private this.CompileIdentifier(identifier: Identifier) : Result<Compiler * byte array, string> =
        match this.SymbolTable.Resolve identifier.Value with
        | Some symbol -> Ok (this, make Opcode.OpGetGlobal [| symbol.Index |])
        | None -> Error $"Undefined variable \"{identifier.Value}\""
        
    member private this.CompilePrefixExpression(prefixExpression: PrefixExpression) : Result<Compiler * byte array, string> =
        result {
            let! newCompiler, rightExprBytes = this.CompileExpression(prefixExpression.Right)
            let! prefixOperatorBytes = makePrefixOperator prefixExpression.Operator
            let prefixExprBytes = Array.concat [| rightExprBytes; prefixOperatorBytes |]
            return newCompiler, prefixExprBytes
        }
        
    member private this.CompileInfixExpression(infixExpression: InfixExpression) : Result<Compiler * byte array, string> =
        let left, right, operator =
            match infixExpression.Operator with
            | "<" -> infixExpression.Right, infixExpression.Left, ">"
            | _ -> infixExpression.Left, infixExpression.Right, infixExpression.Operator
            
        result {
            let! newCompiler, leftBytes = this.CompileExpression(left)
            let! newCompiler, rightBytes = newCompiler.CompileExpression(right)
            let! infixOperatorBytes = makeInfixOperator operator
            let infixExprBytes = Array.concat [| leftBytes; rightBytes; infixOperatorBytes |]
            return newCompiler, infixExprBytes
        }
        
    member private this.CompileIfExpression(ifExpression: IfExpression) : Result<Compiler * byte array, string> =
        result {
            let! newCompiler, conditionBytes = this.CompileExpression(ifExpression.Condition)
            let! newCompiler, consequenceBytes = newCompiler.CompileStatement(false, Statement.BlockStatement ifExpression.Consequence)
            
            let! newCompiler, alternateBytes =
                match ifExpression.Alternative with
                | Some altBlockStatement -> newCompiler.CompileStatement(false, Statement.BlockStatement altBlockStatement)
                | None -> Ok (newCompiler, make Opcode.OpNull [| |])
                
                
            let jumpInstructionLen = 3
            let initialIndex = this.Instructions.GetBytes().Length
                
            let opJumpWhenFalseAddress = initialIndex + conditionBytes.Length + consequenceBytes.Length + (2 * jumpInstructionLen)
            let opJumpWhenFalseByte = make Opcode.OpJumpWhenFalse [| opJumpWhenFalseAddress |]
                
            let opJumpAddress = initialIndex + conditionBytes.Length + consequenceBytes.Length + (2 * jumpInstructionLen) + alternateBytes.Length 
            let opJumpBytes = make Opcode.OpJump [| opJumpAddress |]
                
            let ifExprBytes = Array.concat [| conditionBytes; opJumpWhenFalseByte; consequenceBytes; opJumpBytes; alternateBytes |]
            return (newCompiler, ifExprBytes)
        }
        
        
    member this.Bytecode() : Bytecode =
        { Instructions = this.Instructions 
          Constants = this.Constants |> List.toArray |> Array.rev }
    
    
and Bytecode =
    { Instructions: Instructions
      Constants: Object array }
      
