module Monkey.Backend.Compiler

open FsToolkit.ErrorHandling
open Monkey.Backend.SymbolTable
open Monkey.Frontend.Ast
open Monkey.Frontend.Eval.Object
open Monkey.Backend.Code



type Compiler =
    { Instructions: Instructions
      Constants: Object list
      SymbolTable: SymbolTable }
    
type Bytecode =
    { Instructions: Instructions
      Constants: Object array }
 
 

[<RequireQualifiedAccess>] 
module Compiler =
    
    [<AutoOpen>]
    module Utils = 
        let inline (>>=) result func = Result.bind func result  // Alias for Result.bind
        
        let inline (<!>) result func = Result.map func result  // Alias for Result.map

        let inline appendBytes bytesToAppend (compiler, bytes) = (compiler, Array.append bytes bytesToAppend)
        
    [<AutoOpen>]
    module Make =
        let makePrefixOperator (operator: string) : Result<byte array, string> =
            match operator with
            | "-" -> make Opcode.OpMinus [|  |] |> Ok
            | "!" -> make Opcode.OpBang [|  |] |> Ok
            | _ -> Error $"'{operator}' is not a valid prefix expression operator" 
            
        let makeInfixOperator (operator: string) : Result<byte array, string> =
            match operator with
            | "+" -> make Opcode.OpAdd [|  |] |> Ok
            | "-" -> make Opcode.OpSub [|  |] |> Ok
            | "*" -> make Opcode.OpMul [|  |] |> Ok
            | "/" -> make Opcode.OpDiv [|  |] |> Ok
            
            | "==" -> make Opcode.OpEqual [|  |] |> Ok
            | "!=" -> make Opcode.OpNotEqual [|  |] |> Ok
            | ">" -> make Opcode.OpGreaterThan [|  |] |> Ok
            // less than operator doesn't exist, code re-orders expression to use greater than instead
            
            | _ -> Error $"'{operator}' is not a valid infix expression operator"
        
    [<AutoOpen>]
    module Compilation =
        let addConstant (compiler: Compiler) (object: Object) =
            let newCompiler = { compiler with Constants = object :: compiler.Constants }
            newCompiler, newCompiler.Constants.Length - 1   // will rev when converting to bytecode
            
        let addInstruction (compiler: Compiler) (bytes: byte array) =
            let newInstructions = Array.append (compiler.Instructions.GetBytes()) bytes 
            let newCompiler = { compiler with Instructions = Instructions newInstructions }
            newCompiler, bytes.Length
        
        let rec compileExpression (compiler: Compiler) (expression: Expression) : Result<Compiler * byte array, string> =
            
            let compileExprArr (exprArray: Expression array) =
                let bytesList = Array.zeroCreate<byte array> exprArray.Length
                let rec helper (currentCompiler: Compiler) currentIndex =
                    match currentIndex with
                    | i when i >= 0 && i < exprArray.Length ->
                        match compileExpression currentCompiler (exprArray[i]) with
                        | Ok (newCompiler, compiledBytes) ->
                            bytesList[i] <- compiledBytes
                            helper newCompiler (currentIndex + 1)
                        | Error error -> Error error 
                    | _ ->
                        Ok (currentCompiler, Array.concat bytesList)
                helper compiler 0
                
            let compileIndexExpression (indexExpression: IndexExpression) =
                result {
                    let opIndexBytes = make Opcode.OpIndex [| |]
                    let! newCompiler, exprBytes = compileExpression compiler indexExpression.Left 
                    let! newCompiler, indexBytes = compileExpression newCompiler indexExpression.Index
                    return (newCompiler, Array.concat [| exprBytes; indexBytes; opIndexBytes |])
                }
                
            let compileIdentifier (identifier: Identifier) =
                match compiler.SymbolTable.Resolve identifier.Value with
                | Some symbol -> Ok (compiler, make Opcode.OpGetGlobal [| symbol.Index |])
                | None -> Error $"Undefined variable \"{identifier.Value}\""
                
            let compilePrefixExpression (prefixExpression: PrefixExpression) =
                result {
                    let! newCompiler, rightExprBytes = compileExpression compiler prefixExpression.Right
                    let! prefixOperatorBytes = makePrefixOperator prefixExpression.Operator
                    return newCompiler, Array.concat [| rightExprBytes; prefixOperatorBytes |]
                }
                
            let compileInfixExpression (infixExpression: InfixExpression) =
                let left, right, operator =
                    match infixExpression.Operator with
                    | "<" -> infixExpression.Right, infixExpression.Left, ">"
                    | _ -> infixExpression.Left, infixExpression.Right, infixExpression.Operator
                    
                result {
                    let! newCompiler, leftBytes = compileExpression compiler left
                    let! newCompiler, rightBytes = compileExpression newCompiler right
                    let! infixOperatorBytes = makeInfixOperator operator
                    return newCompiler, Array.concat [| leftBytes; rightBytes; infixOperatorBytes |]
                }
                
            let rec compileIfExpression (ifExpression: IfExpression) =
                result {
                    let! newCompiler, conditionBytes = compileExpression compiler ifExpression.Condition
                    let! newCompiler, consequenceBytes = compileStatement newCompiler false (Statement.BlockStatement ifExpression.Consequence)
                    
                    let! newCompiler, alternateBytes =
                        match ifExpression.Alternative with
                        | Some altBlockStatement -> compileStatement newCompiler false (Statement.BlockStatement altBlockStatement)
                        | None -> Ok (newCompiler, make Opcode.OpNull [| |])
                        
                    let jumpInstructionLen = 3
                    let initialIndex = compiler.Instructions.GetBytes().Length  // getting 'this' compilers instructions length, to calcualte jump positions
                        
                    let opJumpWhenFalseAddress = initialIndex + conditionBytes.Length + consequenceBytes.Length + (2 * jumpInstructionLen)
                    let opJumpWhenFalseByte = make Opcode.OpJumpWhenFalse [| opJumpWhenFalseAddress |]
                        
                    let opJumpAddress = initialIndex + conditionBytes.Length + consequenceBytes.Length + (2 * jumpInstructionLen) + alternateBytes.Length 
                    let opJumpBytes = make Opcode.OpJump [| opJumpAddress |]
                        
                    let ifExprBytes = Array.concat [| conditionBytes; opJumpWhenFalseByte; consequenceBytes; opJumpBytes; alternateBytes |]
                    return (newCompiler, ifExprBytes)
                }
                
            match expression with
            | IntegerLiteral integerLiteral ->
                let integerObj = Object.IntegerType integerLiteral.Value
                let newCompiler, constIndex = addConstant compiler integerObj
                Ok (newCompiler, make Opcode.OpConstant [| constIndex |])
            | BooleanLiteral booleanLiteral ->
                let opcodeToEmit = if booleanLiteral.Value then Opcode.OpTrue else Opcode.OpFalse
                Ok (compiler, make opcodeToEmit [| |])
            | StringLiteral stringLiteral ->
                let stringObj = Object.StringType stringLiteral.Value
                let newCompiler, constIndex = addConstant compiler stringObj
                Ok (newCompiler, make Opcode.OpConstant [| constIndex |])
            | Expression.FunctionLiteral functionLiteral ->
                failwith "todo"
            | CallExpression callExpression ->
                failwith "todo"
            | ArrayLiteral arrayLiteral ->
                let opArrayBytes = make Opcode.OpArray [| arrayLiteral.Elements.Length |]
                compileExprArr arrayLiteral.Elements <!> appendBytes opArrayBytes
            | HashLiteral hashLiteral ->
                let exprsArray = hashLiteral.Pairs |> Map.toArray |> Array.collect (fun (a, b) -> [| a; b |])
                let opHashBytes = make Opcode.OpHash [| exprsArray.Length |]
                compileExprArr exprsArray <!> appendBytes opHashBytes
            | IndexExpression indexExpression ->
                compileIndexExpression indexExpression
            | MacroLiteral macroLiteral ->
                failwith "todo"
            | Expression.Identifier identifier ->
                compileIdentifier identifier
            
            | PrefixExpression prefixExpression ->
                compilePrefixExpression prefixExpression
            | InfixExpression infixExpression ->
                compileInfixExpression infixExpression
            | IfExpression ifExpression ->
                compileIfExpression ifExpression
            
        and compileStatement (compiler: Compiler) (generateOpPop: bool) (statement: Statement) : Result<Compiler * byte array, string> =
            match statement with
            | LetStatement letStatement ->
                result {
                    let! newCompiler, exprBytes = compileExpression compiler letStatement.Value
                    let newSymbolTable, symbol = newCompiler.SymbolTable.Define(letStatement.Name.Value)
                    
                    let varBindingBytes = make Opcode.OpSetGlobal [| symbol.Index |]
                    let bytes = Array.concat [| exprBytes; varBindingBytes |]
                    return ({ newCompiler with SymbolTable = newSymbolTable }, bytes)
                }
            | ExpressionStatement expressionStatement ->
                result {
                    let! newCompiler, exprBytes = compileExpression compiler expressionStatement.Expression
                    let opPopBytes = if generateOpPop then make Opcode.OpPop [|  |] else [| |]
                    let exprStatementBytes = Array.concat [| exprBytes; opPopBytes |]
                    return newCompiler, exprStatementBytes
                }
            | BlockStatement blockStatement ->
                compileMultipleStatements compiler blockStatement.Statements [| |]
            | ReturnStatement returnStatement ->
                failwith "todo"
                
        and compileMultipleStatements (compiler: Compiler) (statements: Statement list) (compiledInstructions: byte array) =
            let isLastStatement = statements.Length <= 1
            match statements with
            | statement :: remaining ->
                match compileStatement compiler (not isLastStatement) statement with
                | Ok (newCompiler, bytes) ->
                    let newCompiledInstructions = Array.append compiledInstructions bytes 
                    compileMultipleStatements newCompiler remaining newCompiledInstructions 
                | Error error ->
                    Error error
            | [] ->
                Ok (compiler, compiledInstructions)

    
    
    let inline createNew () = 
        { Instructions = Instructions [|  |]
          Constants = []
          SymbolTable = SymbolTable.New }
    
    let rec compileNodes (nodes: Node list) (compiler: Compiler) : Result<Compiler, string> =
        let inline addToCompiler (_compiler, bytes) = (addInstruction _compiler bytes) |> fst
        
        match nodes with
        | currentNode :: remaining ->
            match currentNode with
            | Statement statement -> compileStatement compiler true statement
            | Expression expression -> compileExpression compiler expression
            <!> addToCompiler
            >>= compileNodes remaining
        | [] ->
            Ok compiler
            
    let inline toByteCode (compiler: Compiler) = 
        { Instructions = compiler.Instructions 
          Constants = compiler.Constants |> List.toArray |> Array.rev }
