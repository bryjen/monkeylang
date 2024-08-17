module Monkey.Backend.Compiler

open FsToolkit.ErrorHandling

open Monkey.Backend.Operators
open Monkey.Backend.Code
open Monkey.Frontend.Ast
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
        let internal addConstant (compiler: Compiler) (object: Object) =
            let newCompiler = { compiler with Constants = object :: compiler.Constants }
            newCompiler, newCompiler.Constants.Length - 1   // will rev when converting to bytecode
            
        let internal currentInstructions compiler = compiler.Scopes.Head.Instructions.GetBytes()
        
        let inline internal setCurrentInstructions compiler newInstructions =
            let scopeWithUpdatedInstructions = { Instructions = Instructions newInstructions }
            { compiler with Scopes = replaceHead compiler.Scopes scopeWithUpdatedInstructions }
            
        
        let internal enterScope (compiler: Compiler) : Compiler =
            let newScope = CompilationScope.New
            { compiler with Scopes = newScope :: compiler.Scopes }
            
        let internal leaveScope (compiler: Compiler) : Compiler * byte array =
            let instructions = currentInstructions compiler
            let newCompiler = { compiler with Scopes = compiler.Scopes.Tail }
            newCompiler, instructions
            
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
                
                
            let compileIdentifier (identifier: Identifier) =
                match compiler.SymbolTable.Resolve identifier.Value with
                | Some symbol -> Ok (compiler, make Opcode.OpGetGlobal [| symbol.Index |])
                | None -> Error $"Undefined variable \"{identifier.Value}\""
                
            let compileFunctionLiteral (functionLiteral: FunctionLiteral) =
                let parsingCallback isLastStatement statement =
                    match isLastStatement, statement with
                    | true, ReturnStatement _ -> [| |]
                    | true, _ -> make Opcode.OpReturnValue [| |]
                    | false, ExpressionStatement _ -> make Opcode.OpPop [| |]
                    | _ -> [| |]
                    
                result {
                    let compiler_scoped = enterScope compiler
                    
                    let! newCompiler_scoped, body_bytes = if functionLiteral.Body.Statements.IsEmpty
                                                          then Ok (compiler_scoped, make Opcode.OpReturn [| |])
                                                          else compileMultipleStatements compiler_scoped parsingCallback functionLiteral.Body.Statements
                    let newCompiler_scoped, _ = addInstruction newCompiler_scoped body_bytes
                    
                    let compiler_unscoped, scoped_bytes = leaveScope newCompiler_scoped
                    let compiledFunction = Object.CompiledFunctionType { InstructionBytes = scoped_bytes }
                    
                    let newCompiler, constIndex = addConstant compiler_unscoped compiledFunction
                    return (newCompiler, make Opcode.OpConstant [| constIndex |])
                }
                
            let rec compileCallExpression (callExpression: CallExpression) =
                let appendOpCall (compiler, bytes) = (compiler, Array.concat [| bytes; make Opcode.OpCall [| |] |])
                
                match callExpression.Function with
                | Identifier identifier -> compileIdentifier identifier
                | FunctionLiteral functionLiteral -> compileFunctionLiteral functionLiteral
                <!> appendOpCall
                
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
                
        and compileLetStatement compiler letStatement =  
            result {
                let! newCompiler, exprBytes = compileExpression compiler letStatement.Value
                let newSymbolTable, symbol = newCompiler.SymbolTable.Define(letStatement.Name.Value)
                
                let varBindingBytes = make Opcode.OpSetGlobal [| symbol.Index |]
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
                <!> (fun (newCompiler, bytes) -> (newCompiler, Array.append bytes (make Opcode.OpPop [| |])))
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
          SymbolTable = SymbolTable.New
          
          Scopes = [ mainScope ] }
    
    let rec compileNodes (nodes: Node list) (compiler: Compiler) : Result<Compiler, string> =
        let inline addToCompiler (_compiler, bytes) = (addInstruction _compiler bytes) |> fst 
        
        match nodes with
        | currentNode :: remaining ->
            match currentNode with
            | Statement statement -> compileStatement compiler statement
            | Expression expression -> compileExpression compiler expression
            <!> addToCompiler
            >>= compileNodes remaining
        | [] ->
            Ok compiler
            
    let toByteCode (compiler: Compiler) =
        { Instructions = compiler |> currentInstructions |> Instructions
          Constants = compiler.Constants |> List.toArray |> Array.rev }
