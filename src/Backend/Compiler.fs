module Monkey.Backend.Compiler

open System.Collections.Generic
open FsToolkit.ErrorHandling

open Monkey.Backend.Operators
open Monkey.Backend.Code
open Monkey.Frontend.Ast
open Monkey.Frontend.Eval
open Monkey.Frontend.Eval.Object
open Monkey.Backend.SymbolTable


type CompilationScope =
    { Instructions: Instructions }
with
    static member New = { Instructions = Instructions [||] }

type Compiler =
    { Constants: Object list
      SymbolTable: SymbolTable
      
      Scopes: CompilationScope list }
    
type Bytecode =
    { Instructions: Instructions
      Constants: Object array }
 
 

[<RequireQualifiedAccess>] 
module Compiler =
    
    [<AutoOpen>]
    module Utils = 
        let inline appendBytes bytesToAppend (compiler, bytes) = (compiler, Array.append bytes bytesToAppend)
        
        let inline replaceHead list newHead =
            match list with
            | [] -> [ newHead ] 
            | _ :: tail -> newHead :: tail
            
        let getBuiltinIndexByName builtinName = Array.findIndex (fun (name, _) -> name = builtinName) Builtins.builtinsArray
            
        
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
    module Scope =
        let internal currentInstructions compiler = compiler.Scopes.Head.Instructions.GetBytes()
        
        let internal enterScope (compiler: Compiler) : Compiler =
            { compiler with
                Scopes = CompilationScope.New :: compiler.Scopes
                SymbolTable = SymbolTable.createNewEnclosed compiler.SymbolTable }
            
        let internal leaveScope (compiler: Compiler) : Compiler * byte array =
            let outer = compiler.SymbolTable.Outer
            let outerST = if Option.isSome outer then Option.get outer else failwith "FATAL: Attempting to leave the global scope."
            
            let newCompiler = { compiler with
                                    Scopes = compiler.Scopes.Tail
                                    SymbolTable = outerST }
            newCompiler, currentInstructions compiler
            
    [<AutoOpen>]
    module Compilation =
        let internal addConstant (compiler: Compiler) (object: Object) =
            let newCompiler = { compiler with Constants = object :: compiler.Constants }
            newCompiler, newCompiler.Constants.Length - 1   // will rev when converting to bytecode
        
        let inline internal setCurrentInstructions compiler newInstructions =
            let scopeWithUpdatedInstructions = { Instructions = Instructions newInstructions }
            { compiler with Scopes = replaceHead compiler.Scopes scopeWithUpdatedInstructions }
            
        let internal addInstruction (compiler: Compiler) (bytes: byte array) =
            let newInstructions = Array.append (currentInstructions compiler) bytes
            let newCompiler = setCurrentInstructions compiler newInstructions
            newCompiler, newInstructions.Length
        
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
                
                
            let rec compileIdentifier (identifier: Identifier) =
                match SymbolTable.resolve compiler.SymbolTable identifier.Value with
                | Some symbol ->
                    let varGetOpcode = match symbol.Scope with
                                       | LocalScope -> Opcode.OpGetLocal
                                       | GlobalScope -> Opcode.OpGetGlobal 
                                       | BuiltinScope -> Opcode.OpGetBuiltin 
                    Ok (compiler, make varGetOpcode [| symbol.Index |])
                | None ->
                    Error $"Undefined variable \"{identifier.Value}\""
                    
(*
                    tryResolveAsBuiltin identifier
                    
            and tryResolveAsBuiltin identifier =
                try
                    let builtinIndex = getBuiltinIndexByName identifier.Value
                    Ok (compiler, make Opcode.OpGetBuiltin [| builtinIndex |])
                with
                | :? KeyNotFoundException as _ ->
                    Error $"Undefined variable \"{identifier.Value}\""  // identifier is not a defined variable or a builtin
*)
                
                
            let rec compileFunctionLiteral (functionLiteral: FunctionLiteral) =
                let parsingCallback isLastStatement statement =
                    match isLastStatement, statement with
                    | true, ReturnStatement _ -> [| |]
                    | true, _ -> make Opcode.OpReturnValue [| |]
                    | false, ExpressionStatement _ -> make Opcode.OpPop [| |]
                    | _ -> [| |]
                    
                result {
                    let compiler_scoped = enterScope compiler
                    
                    // update symbol table for parameters
                    let newSymbolTable = defineFunctionParams compiler_scoped.SymbolTable functionLiteral.Parameters
                    let newCompiler_scoped = { compiler_scoped with SymbolTable = newSymbolTable }
                    
                    // parse function body
                    let! newCompiler_scoped, body_bytes = if functionLiteral.Body.Statements.IsEmpty
                                                          then Ok (newCompiler_scoped, make Opcode.OpReturn [| |])
                                                          else compileMultipleStatements newCompiler_scoped parsingCallback functionLiteral.Body.Statements
                    let newCompiler_scoped, _ = addInstruction newCompiler_scoped body_bytes
                    
                    // leave scope & push compiled func to stack
                    let numLocals = newCompiler_scoped.SymbolTable.Count
                    let compiler_unscoped, scoped_bytes = leaveScope newCompiler_scoped
                    let compiledFunction = { InstructionBytes = scoped_bytes
                                             NumLocals = numLocals
                                             NumParameters = functionLiteral.Parameters.Length }
                                           |> CompiledFunction |> FunctionType
                    let newCompiler, constIndex = addConstant compiler_unscoped compiledFunction
                    
                    // TODO: Modify the second operand when handling of free variables is implemented
                    return (newCompiler, make Opcode.OpClosure [| constIndex; 0 |])
                }
                
            and defineFunctionParams currentSymbolTable paramsList =
                match paramsList with
                | param :: remainingParams ->
                    let newSymbolTable, _ = SymbolTable.define currentSymbolTable param.Value
                    defineFunctionParams newSymbolTable remainingParams
                | [] -> currentSymbolTable 
                
            let rec compileCallExpression (callExpression: CallExpression) =
                let callExprAsExpr = 
                    match callExpression.Function with
                    | CallExpr.Identifier identifier -> Expression.Identifier identifier 
                    | CallExpr.FunctionLiteral functionLiteral -> Expression.FunctionLiteral functionLiteral 
                
                result {
                    let! newCompiler, argBytes = compileArgs compiler callExpression.Arguments []
                    let! newCompiler, funcBytes = compileExpression newCompiler callExprAsExpr
                    
                    let bytes = Array.concat [|
                        funcBytes
                        argBytes
                        make Opcode.OpCall [| callExpression.Arguments.Length |]
                    |]
                        
                    return (newCompiler, bytes) 
                }
                
            and compileArgs currentCompiler expressionsToParse compiledBytes =
                match expressionsToParse with
                | headExpr :: remaining ->
                    match compileExpression currentCompiler headExpr with
                    | Ok (newCompiler, bytes) -> compileArgs newCompiler remaining (bytes :: compiledBytes)
                    | Error error -> Error error 
                | [] ->
                    let combinedBytes = compiledBytes |> List.toArray |> Array.rev |> Array.concat
                    Ok (currentCompiler, combinedBytes)
                    
                
            let compileIndexExpression (indexExpression: IndexExpression) =
                result {
                    let opIndexBytes = make Opcode.OpIndex [| |]
                    let! newCompiler, exprBytes = compileExpression compiler indexExpression.Left 
                    let! newCompiler, indexBytes = compileExpression newCompiler indexExpression.Index
                    return (newCompiler, Array.concat [| exprBytes; indexBytes; opIndexBytes |])
                }
                
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
                    let! newCompiler, consequenceBytes = compileStatement newCompiler (Statement.BlockStatement ifExpression.Consequence)
                    
                    let! newCompiler, alternateBytes =
                        match ifExpression.Alternative with
                        | Some altBlockStatement -> compileStatement newCompiler (Statement.BlockStatement altBlockStatement)
                        | None -> Ok (newCompiler, make Opcode.OpNull [| |])
                        
                    let jumpInstructionLen = 3
                    let initialIndex = compiler |> currentInstructions |> (_.Length)  // getting 'this' compilers instructions length, to calculate jump positions
                        
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
                compileFunctionLiteral functionLiteral
            | CallExpression callExpression ->
                compileCallExpression callExpression
            | ArrayLiteral arrayLiteral ->
                let opArrayBytes = make Opcode.OpArray [| arrayLiteral.Elements.Length |]
                compileExprArr arrayLiteral.Elements >> appendBytes opArrayBytes
            | HashLiteral hashLiteral ->
                let exprsArray = hashLiteral.Pairs |> Map.toArray |> Array.collect (fun (a, b) -> [| a; b |])
                let opHashBytes = make Opcode.OpHash [| exprsArray.Length |]
                compileExprArr exprsArray >> appendBytes opHashBytes
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
                
        and compileLetStatement compiler letStatement =  
            result {
                let! newCompiler, exprBytes = compileExpression compiler letStatement.Value
                let newSymbolTable, symbol = SymbolTable.define newCompiler.SymbolTable letStatement.Name.Value
                
                let varBindingOpcode = if SymbolTable.isGlobalScope newSymbolTable then Opcode.OpSetGlobal else Opcode.OpSetLocal
                let varBindingBytes = make varBindingOpcode [| symbol.Index |]
                let bytes = Array.concat [| exprBytes; varBindingBytes |]
                
                return ({ newCompiler with SymbolTable = newSymbolTable }, bytes)
            }
            
        and compileExpressionStatement compiler expressionStatement =
            result {
                let! newCompiler, exprBytes = compileExpression compiler expressionStatement.Expression
                return newCompiler, exprBytes 
            }
            
        and compileReturnStatement compiler returnStatement =
            result {
                let! newCompiler, exprBytes = compileExpression compiler returnStatement.ReturnValue
                let returnOpCode = make Opcode.OpReturnValue [| |]
                return newCompiler, Array.concat [| exprBytes; returnOpCode |]
            }
            
        and compileStatement (compiler: Compiler) (statement: Statement) : Result<Compiler * byte array, string> =
            match statement with
            | LetStatement letStatement ->
                compileLetStatement compiler letStatement
            | ReturnStatement returnStatement ->
                compileReturnStatement compiler returnStatement
            | ExpressionStatement expressionStatement ->
                compileExpressionStatement compiler expressionStatement
                >> (fun (newCompiler, bytes) -> (newCompiler, Array.append bytes (make Opcode.OpPop [| |])))
            | BlockStatement blockStatement ->
                let parsingCallback isLastStatement statement =
                    match isLastStatement, statement with
                    | true, _ -> [| |]
                    | false, _ -> make Opcode.OpPop [| |]
                
                compileMultipleStatements compiler parsingCallback blockStatement.Statements
                
        and compileMultipleStatements (compiler: Compiler) (callback: bool -> Statement -> byte array) (statements: Statement list) =
            
            // Compile statement that doesnt generate 'OpPop' after an expression statement
            let compileStatementAlt (currentCompiler: Compiler) (statement: Statement) =
                match statement with
                | LetStatement letStatement -> compileLetStatement currentCompiler letStatement
                | ReturnStatement returnStatement -> compileReturnStatement currentCompiler returnStatement
                | ExpressionStatement expressionStatement -> compileExpressionStatement currentCompiler expressionStatement
                | BlockStatement blockStatement -> compileMultipleStatements currentCompiler callback blockStatement.Statements 
            
            let rec helper (currentCompiler: Compiler) (currentStatements: Statement list) (compiledBytes: byte array) =
                match currentStatements with
                | [ ] ->
                    Ok (currentCompiler, compiledBytes)
                | [ lastStatement ] ->
                    match compileStatementAlt currentCompiler lastStatement with
                    | Ok (newCompiler, bytes) ->
                        let newCompiledBytes = Array.concat [| compiledBytes; bytes; callback true lastStatement |]
                        Ok (newCompiler, newCompiledBytes)
                    | Error error ->
                        Error error
                | headStatement :: remaining -> 
                    match compileStatementAlt currentCompiler headStatement with
                    | Ok (newCompiler, bytes) ->
                        let newCompiledBytes = Array.concat [| compiledBytes; bytes; callback false headStatement |]
                        helper newCompiler remaining newCompiledBytes 
                    | Error error ->
                        Error error

            helper compiler statements [| |]
    
    
    let inline createNew () =
        let mainScope = { Instructions = Instructions [| |] } 
        
        { Constants = []
          SymbolTable = SymbolTable.createNew ()
          
          Scopes = [ mainScope ] }
    
    let rec compileNodes (nodes: Node list) (compiler: Compiler) : Result<Compiler, string> =
        let inline addToCompiler (_compiler, bytes) = (addInstruction _compiler bytes) |> fst 
        
        match nodes with
        | currentNode :: remaining ->
            match currentNode with
            | Statement statement -> compileStatement compiler statement
            | Expression expression -> compileExpression compiler expression
            >> addToCompiler
            >>= compileNodes remaining
        | [] ->
            Ok compiler
            
    let toByteCode (compiler: Compiler) =
        { Instructions = compiler |> currentInstructions |> Instructions
          Constants = compiler.Constants |> List.toArray |> Array.rev }
