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
    failwith "todo"
    
and private onMonkeySyntaxNode
        (state: AnalyzerState)
        (semanticModel: SemanticModel)
        (monkeySyntaxNode: MonkeySyntaxNode)
        : AnalyzerState * SemanticModel =
    match monkeySyntaxNode with
    | UsingDirectiveSyntax usingDirectiveSyntax ->
        match state.FinishedPreamble with
        | true ->
            let error = MisplacedUsingDirective(usingDirectiveSyntax) :> SemanticErrorBase |> Diagnostic.Error
            semanticModel.Diagnostics.Add(error)
        | false ->
            semanticModel.Usings.Add(usingDirectiveSyntax)
        state, semanticModel
    | NamespaceDeclarationSyntax namespaceDeclarationSyntax ->
        match state.FinishedPreamble, semanticModel.NsDeclared() with
        | true, true ->
            semanticModel.Diagnostics.Add(multipleNamespaceErr namespaceDeclarationSyntax)
        | true, false -> 
            semanticModel.Diagnostics.Add(misplacedNamespaceErr namespaceDeclarationSyntax)
        | false, true -> 
            semanticModel.Diagnostics.Add(multipleNamespaceErr namespaceDeclarationSyntax)
        | false, false ->
            semanticModel.NamespaceDeclarations.Add(namespaceDeclarationSyntax)
        state, semanticModel
    | ArgumentListSyntax argumentListSyntax -> failwith "todo"
    | ParameterListSyntax parameterListSyntax -> failwith "todo"
    | ExpressionSyntax expressionSyntax ->
        tryInferExpressionType state semanticModel expressionSyntax |> ignore
        state, semanticModel
    | StatementSyntax statementSyntax ->
        onStatement state semanticModel statementSyntax
    
and private tryInferExpressionType
        (state: AnalyzerState)
        (semanticModel: SemanticModel)
        (expression: ExpressionSyntax)
        : TypeSymbol option =
    state.FinishedPreamble <- true
            
    match expression with
    | ParenthesizedExpressionSyntax parenthesizedExpressionSyntax ->
        tryInferExpressionType state semanticModel parenthesizedExpressionSyntax.Expression
    | ExpressionSyntax.FunctionExpressionSyntax functionExpressionSyntax -> failwith "todo"
    | BinaryExpressionSyntax binaryExpressionSyntax ->
        tryInferBinaryExpressionType state semanticModel binaryExpressionSyntax
    | InterpolatedStringExpressionSyntax interpolatedStringExpressionSyntax -> failwith "todo"
    | InvocationExpressionSyntax invocationExpressionSyntax -> failwith "todo"
    | LiteralExpressionSyntax literalExpressionSyntax ->
        tryInferLiteralExpressionType state semanticModel literalExpressionSyntax
    | PostfixExpressionSyntax postfixExpressionSyntax ->
        failwith "todo"
    | PrefixExpressionSyntax prefixExpressionSyntax ->
        tryInferPrefixExpressionType state semanticModel prefixExpressionSyntax
    | ExpressionSyntax.IdentifierSyntax identifierSyntax ->
        tryInferIdentifierType state semanticModel identifierSyntax
    | TypeSyntax typeSyntax -> failwith "todo"
    | IfExpressionSyntax ifExpressionSyntax -> failwith "todo"
    | ArrayExpressionSyntax arrayExpressionSyntax -> failwith "todo"
    
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
        | ArrayTypeSymbol _ ->
            semanticModel.Diagnostics.Add(ubt expression BuiltinType.Array)
            None
        | FunctionTypeSymbol _ ->
            semanticModel.Diagnostics.Add(ierr expression)
            None
        | TypeParameterSymbol _ ->
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
    | QualifiedIdentifier qualifiedIdentifier ->  // can only be a type
        failwith "todo"
    
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
            | FunctionSymbol _ -> failwith "todo"  // omitted because i'm still not sure what to do with it
            | NamespaceSymbol _ ->
                // use generic error, otherwise we use internal error
                let err = UnresolvedIdentifier(identifierSyntax) :> SemanticErrorBase |> Diagnostic.Error
                semanticModel.Diagnostics.Add(err)
                None
    }
    
    
and private onStatement
        (state: AnalyzerState)
        (semanticModel: SemanticModel)
        (statement: StatementSyntax)
        : AnalyzerState * SemanticModel =
    match statement with
    | BlockSyntax blockSyntax -> failwith "todo"
    | ExpressionStatementSyntax expressionStatementSyntax -> failwith "todo"
    | VariableDeclarationStatementSyntax variableDeclarationStatementSyntax -> failwith "todo"
    
    
and private misplacedNamespaceErr namespaceDeclarationSyntax =
    MisplacedNamespaceDeclaration(namespaceDeclarationSyntax) :> SemanticErrorBase |> Diagnostic.Error
    
and private multipleNamespaceErr namespaceDeclarationSyntax =
    MultipleNamespaceDeclarations(namespaceDeclarationSyntax) :> SemanticErrorBase |> Diagnostic.Error
