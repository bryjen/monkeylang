module Monkey.Semantics.Interface

open Microsoft.CodeAnalysis.CSharp

open FsToolkit.ErrorHandling

open Monkey.AST
open Monkey.Semantics.Diagnostics.Errors
open Monkey.Semantics.Diagnostics.Warnings
open Monkey.Semantics.SemanticErrors
open Monkey.Semantics.SymbolTable
open Monkey.Semantics.Symbols
open Monkey.Semantics.Types


// type aliases for more concise code
type private Expression = ExpressionSyntax
type private Node = MonkeySyntaxNode


type Diagnostic =
    | Warning of WarningBase
    | Error of SemanticErrorBase
    
    
    
type SemanticModel() =
    member val internal ExpressionToInferredTypeMap: Map<int, TypeSymbol>
    
    member val internal Diagnostics: ResizeArray<Diagnostic> = ResizeArray<Diagnostic>()
        
    member val internal SymbolTable: SymbolTable = SymbolTable()
    
    member val internal Usings: ResizeArray<UsingDirectiveSyntax> = ResizeArray<UsingDirectiveSyntax>()
        with get, set
        
    member val internal NamespaceDeclarations: ResizeArray<NamespaceDeclarationSyntax> = ResizeArray<NamespaceDeclarationSyntax>()
        with get, set
with
    member internal this.NsDeclared() = this.NamespaceDeclarations.Count >= 1



type private AnalyzerState() =
    /// The 'preamble' can be thought as the 'header' of a compilation unit. Monkey files start with using/import and
    /// namespace declaration statements, and can't be defined when a statement or expression has been declared.
    /// After that, then any such declarations are invalid.
    member val FinishedPreamble: bool = false with get, set
    


let rec private createSymbolTable (monkeyCompilationUnit: MonkeyCompilationUnit) =
    let mutable analyzerState = AnalyzerState()
    let mutable semanticModel = SemanticModel()
    for node in monkeyCompilationUnit.SyntaxNodes do
        let newState, newSemanticModel = onMonkeySyntaxNode analyzerState semanticModel node
        analyzerState <- newState
        semanticModel <- newSemanticModel
        
    semanticModel
    
    
and private onMonkeySyntaxNode
        (state: AnalyzerState)
        (semanticModel: SemanticModel)
        (monkeySyntaxNode: MonkeySyntaxNode)
        : AnalyzerState * SemanticModel =
    match monkeySyntaxNode with
    | UsingDirectiveSyntax usingDirectiveSyntax ->
        onUsingDirective state semanticModel usingDirectiveSyntax
    | NamespaceDeclarationSyntax namespaceDeclarationSyntax ->
        onNamespaceDeclaration state semanticModel namespaceDeclarationSyntax
    | ArgumentListSyntax argumentListSyntax ->
        failwith "todo"
    | ParameterListSyntax parameterListSyntax ->
        failwith "todo"
    | ExpressionSyntax expressionSyntax ->
        tryInferExpressionType state semanticModel expressionSyntax |> ignore
        state, semanticModel
    | StatementSyntax statementSyntax ->
        onStatement state semanticModel statementSyntax
       
and private onUsingDirective state semanticModel usingDirective =  
    match state.FinishedPreamble with
    | true ->
        let error = MisplacedUsingDirective(usingDirective) :> SemanticErrorBase |> Diagnostic.Error
        semanticModel.Diagnostics.Add(error)
    | false ->
        semanticModel.Usings.Add(usingDirective)
    state, semanticModel
        
and private onNamespaceDeclaration state semanticModel namespaceDeclaration = 
    match state.FinishedPreamble, semanticModel.NsDeclared() with
    | true, true ->
        semanticModel.Diagnostics.Add(multipleNamespaceErr namespaceDeclaration)
    | true, false -> 
        semanticModel.Diagnostics.Add(misplacedNamespaceErr namespaceDeclaration)
    | false, true -> 
        semanticModel.Diagnostics.Add(multipleNamespaceErr namespaceDeclaration)
    | false, false ->
        semanticModel.NamespaceDeclarations.Add(namespaceDeclaration)
    state, semanticModel
    
    
and private misplacedNamespaceErr namespaceDeclarationSyntax =
    MisplacedNamespaceDeclaration(namespaceDeclarationSyntax) :> SemanticErrorBase |> Diagnostic.Error
    
and private multipleNamespaceErr namespaceDeclarationSyntax =
    MultipleNamespaceDeclarations(namespaceDeclarationSyntax) :> SemanticErrorBase |> Diagnostic.Error
    
    
and private tryInferExpressionType
        (state: AnalyzerState)
        (semanticModel: SemanticModel)
        (expression: ExpressionSyntax)
        : TypeSymbol option =
    state.FinishedPreamble <- true
            
    match expression with
    | ParenthesizedExpressionSyntax parenthesizedExpressionSyntax ->
        tryInferExpressionType state semanticModel parenthesizedExpressionSyntax.Expression
    | ExpressionSyntax.FunctionExpressionSyntax functionExpressionSyntax ->
        tryInferFunctionExpressionType semanticModel functionExpressionSyntax
        |> Option.map TypeSymbol.FunctionTypeSymbol
    | BinaryExpressionSyntax binaryExpressionSyntax ->
        tryInferBinaryExpressionType state semanticModel binaryExpressionSyntax
    | InterpolatedStringExpressionSyntax interpolatedStringExpressionSyntax ->
        failwith "todo"
    | InvocationExpressionSyntax invocationExpressionSyntax ->
        tryInferInvocationExpressionType state semanticModel invocationExpressionSyntax
    | LiteralExpressionSyntax literalExpressionSyntax ->
        tryInferLiteralExpressionType state semanticModel literalExpressionSyntax
    | PostfixExpressionSyntax postfixExpressionSyntax ->
        failwith "todo"
    | PrefixExpressionSyntax prefixExpressionSyntax ->
        tryInferPrefixExpressionType state semanticModel prefixExpressionSyntax
    | ExpressionSyntax.IdentifierSyntax identifierSyntax ->
        tryInferIdentifierType state semanticModel identifierSyntax
    | TypeSyntax typeSyntax ->
        tryConvertTypeSyntax semanticModel typeSyntax
    | IfExpressionSyntax ifExpressionSyntax ->
        tryInferIfExpressionType state semanticModel ifExpressionSyntax
    | ArrayExpressionSyntax arrayExpressionSyntax ->
        failwith "todo"
        
and private tryConvertTypeSyntax semanticModel typeSyntax =
    match typeSyntax with
    | NameSyntax nameSyntax ->
        tryConvertNameSyntax nameSyntax
    | BuiltinTypeSyntax builtinTypeSyntax ->
        tryConvertBuiltinTypeSyntax semanticModel builtinTypeSyntax 
    | FunctionTypeSyntax functionTypeSyntax ->
        tryConvertFunctionTypeSyntax semanticModel functionTypeSyntax
    | ArrayTypeSyntax arrayTypeSyntax ->
        tryConvertArrayTypeSyntax semanticModel arrayTypeSyntax
    | GenericTypeSyntax genericTypeSyntax ->
        tryConvertGenericTypeSyntax semanticModel genericTypeSyntax
        
and private tryConvertNameSyntax nameSyntax =
    match nameSyntax.Identifier with
    | SimpleIdentifier simpleIdentifier ->
        // parser guarantees that we return a 'IdentifierToken' kind
        { Name = simpleIdentifier.Token.Text.Trim(); Namespace = "" } |> Type.UserDefinedType
    | QualifiedIdentifier qualifiedIdentifier ->
        let fullName = System.String.Join(".", Array.map (_.ToString().Trim()) qualifiedIdentifier.Tokens)
        { Name = fullName; Namespace = "" } |> Type.UserDefinedType
    |> NamedTypeSymbol
    |> TypeSymbol.NamedTypeSymbol
    |> Some
    
and private tryConvertBuiltinTypeSyntax semanticModel builtinSyntax =
    match builtinSyntax.Token with
    | token when token.Kind = SyntaxKind.IntKeyword ->
        BuiltinType.Int32 |> Some
    | token when token.Kind = SyntaxKind.BoolKeyword ->
        BuiltinType.Boolean |> Some
    | token when token.Kind = SyntaxKind.StringKeyword ->
        BuiltinType.String |> Some
    | _ ->
        let error = InternalError(builtinSyntax |> TypeSyntax.BuiltinTypeSyntax |> Expression.TypeSyntax |> Node.ExpressionSyntax)
        semanticModel.Diagnostics.Add(error :> SemanticErrorBase |> Diagnostic.Error)
        None
    |> Option.map Type.BuiltinType
    |> Option.map NamedTypeSymbol
    |> Option.map TypeSymbol.NamedTypeSymbol
    
and private tryConvertFunctionTypeSyntax semanticModel functionTypeSyntax =
    let typeSymbolOptions = functionTypeSyntax.ParameterTypes |> Array.map (tryConvertTypeSyntax semanticModel)
    match Array.contains None typeSymbolOptions with
    | true ->
        None  // no diag since it's assumed that it's done by the caller
    | false ->
        let types = Array.map Option.get typeSymbolOptions
        if Array.isEmpty types then
            let error = InternalError(functionTypeSyntax |> TypeSyntax.FunctionTypeSyntax |> Expression.TypeSyntax |> Node.ExpressionSyntax)
            semanticModel.Diagnostics.Add(error :> SemanticErrorBase |> Diagnostic.Error)
            None
        else
            let last = Array.last types
            let rest = types[.. types.Length - 2]
            FunctionTypeSymbol(rest, last) |> TypeSymbol.FunctionTypeSymbol |> Some
            
and private tryConvertArrayTypeSyntax semanticModel arrayTypeSyntax =
    match tryConvertTypeSyntax semanticModel arrayTypeSyntax.Type with
    | Some _type ->
        ArrayTypeSymbol(_type) |> TypeSymbol.ArrayTypeSymbol |> Some
    | None ->
        None  // no diag since it's assumed that it's done by the caller
        
and private tryConvertGenericTypeSyntax semanticModel genericTypeSyntax =
    option {
        let! typeSymbol = tryConvertTypeSyntax semanticModel genericTypeSyntax.Type
        let genericTypeSymbolOptions = genericTypeSyntax.GenericTypes |> Array.map (tryConvertTypeSyntax semanticModel)
        let! genericTypeSymbols =
            match Array.contains None genericTypeSymbolOptions with
            | true -> 
                let error = InternalError(genericTypeSyntax |> TypeSyntax.GenericTypeSyntax |> Expression.TypeSyntax |> Node.ExpressionSyntax)
                semanticModel.Diagnostics.Add(error :> SemanticErrorBase |> Diagnostic.Error)
                None
            | false ->
                genericTypeSymbolOptions |> Array.map Option.get |> Some
                
        return GenericTypeSymbol(typeSymbol, genericTypeSymbols) |> TypeSymbol.GenericTypeSymbol
    }
            
    
    
    
and private tryInferLiteralExpressionType state semanticModel literalExpression =
    let namedTypeInfoOption =
        match literalExpression with
        | token when token.Kind = SyntaxKind.NumericLiteralExpression ->
            Some (BuiltinType.Int32, true)
        | token when token.Kind = SyntaxKind.StringLiteralExpression ->
            Some (BuiltinType.String, false)
        | token when token.Kind = SyntaxKind.TrueLiteralExpression ->
            Some (BuiltinType.Boolean, true)
        | token when token.Kind = SyntaxKind.FalseLiteralExpression ->
            Some (BuiltinType.Boolean, true)
        | _ -> None
        
    match namedTypeInfoOption with
    | None ->
        let error = InternalError(literalExpression |> Expression.LiteralExpressionSyntax |> Node.ExpressionSyntax)
        semanticModel.Diagnostics.Add(error :> SemanticErrorBase |> Diagnostic.Error)
        None
    | Some (_type, _) ->
        NamedTypeSymbol(BuiltinType _type) |> TypeSymbol.NamedTypeSymbol |> Some
        
        
and private tryInferBinaryExpressionType state semanticModel binaryExpression =
    // aliases
    let ubt (* unsupported builtin type*) expression builtinType =
        UnsupportedBinaryExpressionOperand.BuiltinType(expression, builtinType) :> SemanticErrorBase |> Diagnostic.Error
    let uudt (* unsupported user defined type *) expression userDefinedType =
        UnsupportedBinaryExpressionOperand.UserDefinedType(expression, userDefinedType) :> SemanticErrorBase |> Diagnostic.Error
    let ierr (* internal error *) expression =
        InternalError(expression |> Node.ExpressionSyntax) :> SemanticErrorBase |> Diagnostic.Error
    let mbeo (* mismatched binary expression operands *) left right =
        MismatchedBinaryExpressionOperands(binaryExpression, left, right) :> SemanticErrorBase |> Diagnostic.Error
    
    let assertIsValidBuiltinType expression typeSymbol =
        match typeSymbol with
        | NamedTypeSymbol namedTypeSymbol ->
            match namedTypeSymbol.Type with
            | BuiltinType builtinType ->
                Some builtinType
            | UserDefinedType userDefinedType ->
                semanticModel.Diagnostics.Add(uudt expression userDefinedType)
                None
        | _ ->
            semanticModel.Diagnostics.Add(ierr expression)
            None
            
    let onLogicalOperatorToken leftType rightType =
        match leftType, rightType with
        | Boolean, Boolean -> BuiltinType.Boolean |> Some
        | String, String -> BuiltinType.Boolean |> Some
        | Int32, Int32 -> BuiltinType.Boolean |> Some
        | _ ->
            semanticModel.Diagnostics.Add(mbeo leftType rightType)
            None
        
    let onArithmeticOperatorToken leftType rightType =
        match leftType, rightType with
        | Int32, Int32 -> BuiltinType.Int32 |> Some
        | Int32, String -> BuiltinType.String |> Some
        | String, Int32 -> BuiltinType.String |> Some
        | String, String -> BuiltinType.String |> Some
        | _ ->
            semanticModel.Diagnostics.Add(mbeo leftType rightType)
            None
    
    option {
        let! leftType = tryInferExpressionType state semanticModel binaryExpression.Left
        let! rightType = tryInferExpressionType state semanticModel binaryExpression.Right
        
        let! leftType = leftType |> assertIsValidBuiltinType binaryExpression.Left
        let! rightType = rightType |> assertIsValidBuiltinType binaryExpression.Right
        
        return!
            match binaryExpression.OperatorToken.Kind with
            | SyntaxKind.EqualsEqualsToken | SyntaxKind.ExclamationEqualsToken
            | SyntaxKind.GreaterThanToken  | SyntaxKind.GreaterThanEqualsToken
            | SyntaxKind.LessThanToken     | SyntaxKind.LessThanEqualsToken ->
                onLogicalOperatorToken leftType rightType
            | SyntaxKind.PlusToken         | SyntaxKind.MinusToken
            | SyntaxKind.AsteriskToken     | SyntaxKind.SlashToken
            | SyntaxKind.PercentToken ->
                onArithmeticOperatorToken leftType rightType
            | _ ->
                semanticModel.Diagnostics.Add(ierr (binaryExpression |> Expression.BinaryExpressionSyntax))
                None
    }
    |> Option.map Type.BuiltinType
    |> Option.map NamedTypeSymbol
    |> Option.map TypeSymbol.NamedTypeSymbol
    
    
and private tryInferPrefixExpressionType state semanticModel prefixExpression =
    option {
        let! operandType = tryInferExpressionType state semanticModel prefixExpression.Operand
        let! nameTypeSymbol =
            match operandType with
            | NamedTypeSymbol namedTypeSymbol ->
                Some namedTypeSymbol
            | _ ->
                let unsupportedTypeErr = UnsupportedPrefixExpressionOperand.TypeSymbol(prefixExpression, operandType)
                semanticModel.Diagnostics.Add(unsupportedTypeErr :> SemanticErrorBase |> Diagnostic.Error)
                None
        
        let! builtinType =
            match nameTypeSymbol.Type with
            | BuiltinType builtinType ->
                Some builtinType
            | UserDefinedType userDefinedType ->
                let unsupportedTypeErr = UnsupportedPrefixExpressionOperand.UserDefinedType(prefixExpression, userDefinedType)
                semanticModel.Diagnostics.Add(unsupportedTypeErr :> SemanticErrorBase |> Diagnostic.Error)
                None
                
        let! resultingType =
            match prefixExpression.Kind, builtinType with
            | SyntaxKind.UnaryMinusExpression, Int32 ->
                NamedTypeSymbol(BuiltinType Int32) |> TypeSymbol.NamedTypeSymbol |> Some
            | SyntaxKind.LogicalNotExpression, Boolean ->
                NamedTypeSymbol(BuiltinType Boolean) |> TypeSymbol.NamedTypeSymbol |> Some
            | _, _ ->
                let unsupportedTypeErr = UnsupportedPrefixExpressionOperand.InvalidBuiltinType(prefixExpression, builtinType)
                semanticModel.Diagnostics.Add(unsupportedTypeErr :> SemanticErrorBase |> Diagnostic.Error)
                None
                
        return resultingType
    }
    
    
and private tryInferIdentifierType state semanticModel identifierSyntax =
    match identifierSyntax with
    | SimpleIdentifier simpleIdentifier ->
        tryInferSimpleIdentifierType semanticModel identifierSyntax simpleIdentifier
    | QualifiedIdentifier qualifiedIdentifier ->
        tryInferQualifiedIdentifierType state semanticModel identifierSyntax qualifiedIdentifier
    
and private tryInferSimpleIdentifierType semanticModel identifierSyntax simpleIdentifier = 
    // could be variable or a type
    // SyntaxKind.IdentifierToken from the parser
    option {
        let lastSymbol =
            simpleIdentifier.Token.Text.Trim()
            |> semanticModel.SymbolTable.SearchForSymbol
            |> Option.bind List.tryHead
            
        let! symbol =
            match lastSymbol with
            | None ->
                let err = UnresolvedIdentifier(identifierSyntax) :> SemanticErrorBase |> Diagnostic.Error
                semanticModel.Diagnostics.Add(err)
                None
            | Some value ->
                Some value
        
        return!         
            match symbol with
            | LocalSymbol localSymbol -> Some localSymbol.Type
            | ParameterSymbol parameterSymbol -> Some parameterSymbol.Type
            | TypeSymbol typeSymbol -> Some typeSymbol
            | NamespaceSymbol _ ->
                // use generic error, otherwise we use internal error
                let err = UnresolvedIdentifier(identifierSyntax) :> SemanticErrorBase |> Diagnostic.Error
                semanticModel.Diagnostics.Add(err)
                None
    }
    
and private tryInferQualifiedIdentifierType state semanticModel identifierSyntax qualifiedIdentifier =
    // variable names can't be qualified, so can only be type
    option {
        let lastSymbol =
            qualifiedIdentifier.Tokens
            |> (fun tokens -> System.String.Join(".", tokens))
            |> semanticModel.SymbolTable.SearchForSymbol
            |> Option.bind List.tryHead
            
        let! symbol =
            match lastSymbol with
            | None ->
                let err = UnresolvedIdentifier(identifierSyntax) :> SemanticErrorBase |> Diagnostic.Error
                semanticModel.Diagnostics.Add(err)
                None
            | Some value ->
                Some value
        
        return!         
            match symbol with
            | LocalSymbol localSymbol -> Some localSymbol.Type
            | ParameterSymbol parameterSymbol -> Some parameterSymbol.Type
            | TypeSymbol typeSymbol -> Some typeSymbol
            | NamespaceSymbol _ ->
                // use generic error, otherwise we use internal error
                let err = UnresolvedIdentifier(identifierSyntax) :> SemanticErrorBase |> Diagnostic.Error
                semanticModel.Diagnostics.Add(err)
                None
    }
    
and private tryInferFunctionExpressionType semanticModel functionExpression =
    option {
        let parameterTypeOptions =
            functionExpression.ParameterList.Parameters
            |> Array.map _.Type
            |> Array.map (tryConvertTypeSyntax semanticModel)
            
        let! parameterTypes =
            match Array.contains None parameterTypeOptions with
            | true -> None  // no diag since it's assumed that it's done by the caller
            | false ->
                parameterTypeOptions |> Array.map Option.get |> Some
                
        let! returnTypeSymbol = tryConvertTypeSyntax semanticModel functionExpression.ReturnType
        return FunctionTypeSymbol(parameterTypes, returnTypeSymbol) 
    }
    
and private tryInferInvocationExpressionType state semanticModel invocationExpression =
    option {
        let leftExprTypeSymbolOption: FunctionTypeSymbol option = tryInferInvocationExpressionLeftExpressionType semanticModel invocationExpression.Expression
        let! leftExprTypeSymbol = leftExprTypeSymbolOption  // BUG: Above statement errors out; can't infer the option for some reason ?
        
        let arguments = invocationExpression.Arguments.Arguments
        let typeSymbolOptions = arguments |> Array.map (tryInferExpressionType state semanticModel)
        let! argumentTypes =
            match Array.contains None typeSymbolOptions with
            | true -> None  // no diag since it's assumed that it's done by the caller
            | false -> typeSymbolOptions |> Array.map Option.get |> Some
            
        let parameterTypes = leftExprTypeSymbol.ParameterTypes
        
        let paramAndArgTypes = Array.zip parameterTypes argumentTypes
        let typeMatches = paramAndArgTypes |> Array.map (fun (paramType, argType) -> paramType.Equals(argType))
            
        let mismatches =
            typeMatches
            |> Array.zip3 paramAndArgTypes arguments
            |> Array.filter (fun (_, _, matches) -> not matches)
            |> Array.map (fun (typeSymbols, argument, _) -> (fst typeSymbols, snd typeSymbols, argument))
            
        if mismatches |> Array.isEmpty |> not then
            let errors = mismatches |> Array.map InvalidArgumentType |> Array.map (fun err -> err :> SemanticErrorBase |> Diagnostic.Error)
            semanticModel.Diagnostics.AddRange(errors)
            ()
            
        // we validated that the parameter and argument types match, we return the function type anyways irrespective
        // of any errors so that the type can still be resolved even if the invocation expression itself is invalid.
        return leftExprTypeSymbol.ReturnType
    }
    
and private tryInferInvocationExpressionLeftExpressionType semanticModel leftExpr =
    match leftExpr with
    | ParenthesizedFunctionExpressionSyntax invocationParenthesizedExpressionSyntax ->
        tryInferInvocationExpressionLeftExpressionType semanticModel invocationParenthesizedExpressionSyntax.Expression
    | FunctionExpressionSyntax functionExpressionSyntax ->
        tryInferFunctionExpressionType semanticModel functionExpressionSyntax
    | IdentifierSyntax identifierSyntax ->
        match semanticModel.SymbolTable.SearchForSymbol(identifierSyntax.ToString().Trim()) with
        | None ->
            let err = UnresolvedIdentifier(identifierSyntax) :> SemanticErrorBase |> Diagnostic.Error
            semanticModel.Diagnostics.Add(err)
            None
        | Some declaredSymbols ->
            tryGetLeftExprTypeFromSymbol semanticModel leftExpr (List.head declaredSymbols)
            
and private tryGetLeftExprTypeFromSymbol semanticModel invocationExpression symbol =
    let onInvalid () =
        let err = InvalidInvocationLeftExpression(invocationExpression) :> SemanticErrorBase |> Diagnostic.Error
        semanticModel.Diagnostics.Add(err)
        None
    
    match symbol with
    | LocalSymbol localSymbol ->
        match localSymbol.Type with
        | FunctionTypeSymbol functionTypeSymbol -> functionTypeSymbol |> Some
        | _ -> onInvalid ()
    | _ ->
        onInvalid ()
    
and private tryInferIfExpressionType state semanticModel ifExpression =
    option {
    }
    |> ignore
    failwith "todo"
    
    
and private onStatement
        (state: AnalyzerState)
        (semanticModel: SemanticModel)
        (statement: StatementSyntax)
        : AnalyzerState * SemanticModel =
    match statement with
    | BlockSyntax blockSyntax -> onBlockStatement state semanticModel blockSyntax
    | ExpressionStatementSyntax expressionStatementSyntax -> onExpressionStatement state semanticModel expressionStatementSyntax
    | VariableDeclarationStatementSyntax variableDeclarationStatementSyntax -> onVariableDeclaration state semanticModel variableDeclarationStatementSyntax
    
and private onBlockStatement state semanticModel block =
    for statement in block.Statements do
        onStatement state semanticModel statement |> ignore
    state, semanticModel
    
and private onExpressionStatement state semanticModel expressionStatement =
    tryInferExpressionType state semanticModel expressionStatement.Expression |> ignore  // ignored just to collect diagnostics, we can't really use it here
    state, semanticModel
   
// TODO: Refactor, created late at night, tired ash 
and private onVariableDeclaration state semanticModel variableDeclaration =
    option {
        let! expressionTypeSymbol = tryInferExpressionType state semanticModel variableDeclaration.Expression
        
        // validating annotation type with expression type
        do! match variableDeclaration.TypeAnnotation with
            | Some varAnnot ->
                match tryConvertTypeSyntax semanticModel varAnnot.Type with
                | None ->
                    let err = InvalidVariableAnnotation(varAnnot) :> SemanticErrorBase |> Diagnostic.Error
                    semanticModel.Diagnostics.Add(err)
                    None
                | Some varAnnotTypeSymbol when varAnnotTypeSymbol.Equals(expressionTypeSymbol)  ->
                    Some ()
                | Some varAnnotTypeSymbol ->
                    let err = VariableAnnotationMismatch(variableDeclaration.Expression, varAnnotTypeSymbol, expressionTypeSymbol) :> SemanticErrorBase |> Diagnostic.Error
                    semanticModel.Diagnostics.Add(err)
                    None
            | None -> Some ()
        
        semanticModel.SymbolTable.AddSymbol(variableDeclaration.Name.Text.Trim(), expressionTypeSymbol |> Symbol.TypeSymbol)
        return ()
    }
    |> ignore
    
    state, semanticModel
