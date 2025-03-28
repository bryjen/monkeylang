module rec Monkey.Frontend.CLR.Syntax.AstTraverser

open Microsoft.CodeAnalysis.CSharp
open Monkey.Frontend.CLR.Syntax.Ast


let printMonkeySyntaxNodeTree (monkeySyntaxNode: MonkeySyntaxNode) =
    let indentation = 0
    match monkeySyntaxNode with
    | UsingDirectiveSyntax usingDirectiveSyntax -> onUsingDirectiveSyntax indentation usingDirectiveSyntax
    | NamespaceDeclarationSyntax namespaceDeclarationSyntax -> onNamespaceDeclarationSyntax indentation namespaceDeclarationSyntax
    | ArgumentListSyntax argumentListSyntax -> onArgumentListSyntax indentation argumentListSyntax
    | ParameterListSyntax parameterListSyntax -> onParameterListSyntax indentation parameterListSyntax
    | ExpressionSyntax expressionSyntax -> onExpressionSyntax indentation expressionSyntax
    | StatementSyntax statementSyntax -> onStatementSyntax indentation statementSyntax
    
let private indentationStr = "    "

let private onUsingDirectiveSyntax (indentation: int) (usingDirective: UsingDirectiveSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(UsingDirectiveSyntax)) (usingDirective.ToString() |> normalizeString)
    
let private onNamespaceDeclarationSyntax (indentation: int) (namespaceDeclaration: NamespaceDeclarationSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(NamespaceDeclarationSyntax)) (namespaceDeclaration.ToString() |> normalizeString)
        
let private onSyntaxToken (indentation: int) (syntaxToken: SyntaxToken) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(SyntaxToken)) (syntaxToken.ToString() |> normalizeString)
    onSyntaxKind (indentation + 1) syntaxToken.Kind
    
    let value = syntaxToken.Value
    let indentation = indentation + 1
    printfn "%svalue type : %s" (String.replicate indentation indentationStr) (value.GetType().ToString())
    printfn "%svalue : %s" (String.replicate indentation indentationStr) (value.ToString())
        
    
let private onSyntaxKind (indentation: int) (syntaxKind: SyntaxKind) =
    printfn "%sKind : %s" (String.replicate indentation indentationStr) (syntaxKind.ToString()) 
    
   
    
let private normalizeString (str: string) =
    let normalziedStr = str.Replace("\n", "\\n").Replace("\r", "\\r").Trim()
    $"`{normalziedStr}`"
    
    
let private onArgumentListSyntax (indentation: int) (argumentListSyntax: ArgumentListSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(ArgumentListSyntax)) (argumentListSyntax.ToString() |> normalizeString)
    argumentListSyntax.Arguments |> Array.iter (onExpressionSyntax (indentation + 1))
    
let private onParameterListSyntax (indentation: int) (parameterListSyntax: ParameterListSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(ParameterListSyntax)) (parameterListSyntax.ToString() |> normalizeString)
    // parameterListSyntax.ParameterSyntax |> Array.iter (onParameterListSyntax (indentation + 1))
    // TODO: onParameterSyntax
    
    
let rec private onExpressionSyntax (indentation: int) (expressionSyntax: ExpressionSyntax) =
    match expressionSyntax with
    | ParenthesizedExpressionSyntax parenthesizedExpressionSyntax -> onParenthesizedExpressionSyntax indentation parenthesizedExpressionSyntax
    | ExpressionSyntax.FunctionExpressionSyntax functionExpressionSyntax -> onFunctionExpressionSyntax indentation functionExpressionSyntax
    | BinaryExpressionSyntax binaryExpressionSyntax -> onBinaryExpressionSyntax indentation binaryExpressionSyntax
    | InterpolatedStringExpressionSyntax interpolatedStringExpressionSyntax -> onInterpolatedStringExpressionSyntax indentation interpolatedStringExpressionSyntax
    | InvocationExpressionSyntax invocationExpressionSyntax -> onInvocationExpressionSyntax indentation invocationExpressionSyntax
    | LiteralExpressionSyntax literalExpressionSyntax -> onLiteralExpressionSyntax indentation literalExpressionSyntax
    | PostfixExpressionSyntax postfixExpressionSyntax -> onPostfixExpressionSyntax indentation postfixExpressionSyntax
    | PrefixExpressionSyntax prefixExpressionSyntax -> onPrefixExpressionSyntax indentation prefixExpressionSyntax
    | ExpressionSyntax.IdentifierSyntax identifierSyntax -> onIdentifierSyntax indentation identifierSyntax
    | TypeSyntax typeSyntax -> onTypeSyntax indentation typeSyntax
    | IfExpressionSyntax ifExpressionSyntax -> onIfExpressionSyntax indentation ifExpressionSyntax
    | ArrayExpressionSyntax arrayExpressionSyntax -> onArrayExpressionSyntax indentation arrayExpressionSyntax
    
    
and private onArrayExpressionSyntax (indentation: int) (arrayExpressionSyntax: ArrayExpressionSyntax) =
    match arrayExpressionSyntax with
    | ListInitialization listInitialization -> onArrayListInitialization indentation listInitialization
    | SizeBasedInitialization sizeBasedInitialization -> onArraySizeBasedInitialization indentation sizeBasedInitialization
    
and private onArrayListInitialization (indentation: int) (onArrayListInitialization: ListInitialization) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(ListInitialization)) (onArrayListInitialization.ToString() |> normalizeString)
    onSyntaxToken (indentation + 1) onArrayListInitialization.OpenBracketToken
    onSyntaxToken (indentation + 1) onArrayListInitialization.CloseBracketToken
    onArrayListInitialization.Values |> Array.iter (onExpressionSyntax (indentation + 1))
    
and private onArraySizeBasedInitialization (indentation: int) (sizeBasedInitialization: SizeBasedInitialization) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(SizeBasedInitialization)) (sizeBasedInitialization.ToString() |> normalizeString)
    onSyntaxToken (indentation + 1) sizeBasedInitialization.NewToken
    onSyntaxToken (indentation + 1) sizeBasedInitialization.TypeToken
    onSyntaxToken (indentation + 1) sizeBasedInitialization.OpenBracketToken
    onSyntaxToken (indentation + 1) sizeBasedInitialization.CloseBracketToken


and private onParenthesizedExpressionSyntax (indentation: int) (parenthesizedExpressionSyntax: ParenthesizedExpressionSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(ParenthesizedExpressionSyntax)) (parenthesizedExpressionSyntax.ToString() |> normalizeString)
    onSyntaxToken (indentation + 1) parenthesizedExpressionSyntax.OpenParenToken
    onExpressionSyntax (indentation + 1) parenthesizedExpressionSyntax.Expression
    onSyntaxToken (indentation + 1) parenthesizedExpressionSyntax.CloseParenToken
    
and private onFunctionExpressionSyntax (indentation: int) (functionExpressionSyntax: FunctionExpressionSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(FunctionExpressionSyntax)) (functionExpressionSyntax.ToString() |> normalizeString)
    onParameterListSyntax (indentation + 1) functionExpressionSyntax.ParameterList
    onBlockSyntax (indentation + 1) functionExpressionSyntax.Body
    onTypeSyntax (indentation + 1) functionExpressionSyntax.ReturnType
    
and private onBinaryExpressionSyntax (indentation: int) (binaryExpressionSyntax: BinaryExpressionSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(BinaryExpressionSyntax)) (binaryExpressionSyntax.ToString() |> normalizeString)
    onExpressionSyntax (indentation + 1) binaryExpressionSyntax.Left
    onExpressionSyntax (indentation + 1) binaryExpressionSyntax.Right
    
and private onInterpolatedStringExpressionSyntax (indentation: int) (interpolatedStringExpressionSyntax: InterpolatedStringExpressionSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(InterpolatedStringExpressionSyntax)) (interpolatedStringExpressionSyntax.ToString() |> normalizeString)
    // TODO: interpolatedStringExpressionSyntax.Contents
    
and private onInvocationExpressionSyntax (indentation: int) (invocationExpressionSyntax: InvocationExpressionSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(InvocationExpressionSyntax)) (invocationExpressionSyntax.ToString() |> normalizeString)
    
    match invocationExpressionSyntax.Expression with
    | InvocationExpressionLeftExpression.FunctionExpressionSyntax functionExpressionSyntax ->
        onFunctionExpressionSyntax (indentation + 1) functionExpressionSyntax
    | InvocationExpressionLeftExpression.IdentifierSyntax identifierNameSyntax ->
        onIdentifierSyntax (indentation + 1) identifierNameSyntax
    | InvocationExpressionLeftExpression.ParenthesizedFunctionExpressionSyntax parenthesizedExpressionSyntax ->
        onInvocationParenthesizedExpressionSyntax (indentation + 1) parenthesizedExpressionSyntax
        
    onArgumentListSyntax (indentation + 1) invocationExpressionSyntax.Arguments
    
and private onInvocationParenthesizedExpressionSyntax (indentation: int) (invocationParenthesizedExpressionSyntax: InvocationParenthesizedExpressionSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(InvocationParenthesizedExpressionSyntax)) (invocationParenthesizedExpressionSyntax.ToString() |> normalizeString)
    onSyntaxToken (indentation + 1) invocationParenthesizedExpressionSyntax.OpenParenToken
    
    match invocationParenthesizedExpressionSyntax.Expression with
    | InvocationExpressionLeftExpression.FunctionExpressionSyntax functionExpressionSyntax ->
        onFunctionExpressionSyntax (indentation + 1) functionExpressionSyntax
    | InvocationExpressionLeftExpression.IdentifierSyntax identifierNameSyntax ->
        onIdentifierSyntax (indentation + 1) identifierNameSyntax
    | InvocationExpressionLeftExpression.ParenthesizedFunctionExpressionSyntax parenthesizedExpressionSyntax ->
        onInvocationParenthesizedExpressionSyntax (indentation + 1) parenthesizedExpressionSyntax
        
    onSyntaxToken (indentation + 1) invocationParenthesizedExpressionSyntax.CloseParenToken
    

    
and private onLiteralExpressionSyntax (indentation: int) (literalExpressionSyntax: LiteralExpressionSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(LiteralExpressionSyntax)) (literalExpressionSyntax.ToString() |> normalizeString)
    onSyntaxKind (indentation + 1) literalExpressionSyntax.Kind
    onSyntaxToken (indentation + 1) literalExpressionSyntax.Token
    
and private onPostfixExpressionSyntax (indentation: int) (postfixExpressionSyntax: PostfixExpressionSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(PostfixExpressionSyntax)) (postfixExpressionSyntax.ToString() |> normalizeString)
    onSyntaxKind (indentation + 1) postfixExpressionSyntax.Kind
    onExpressionSyntax (indentation + 1) postfixExpressionSyntax.Operand
    onSyntaxToken (indentation + 1) postfixExpressionSyntax.OperatorToken
    
and private onPrefixExpressionSyntax (indentation: int) (prefixExpressionSyntax: PrefixExpressionSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(PrefixExpressionSyntax)) (prefixExpressionSyntax.ToString() |> normalizeString)
    onSyntaxKind (indentation + 1) prefixExpressionSyntax.Kind
    onSyntaxToken (indentation + 1) prefixExpressionSyntax.OperatorToken
    onExpressionSyntax (indentation + 1) prefixExpressionSyntax.Operand
    
    
and private onIdentifierSyntax (indentation: int) (identifierNameSyntax: IdentifierSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(IdentifierSyntax)) (identifierNameSyntax.ToString() |> normalizeString)
    
and private onSimpleIdentifier (indentation: int) (simpleIdentifier: SimpleIdentifier) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(SimpleIdentifier)) (simpleIdentifier.ToString() |> normalizeString)
    
and private onQualifiedIdentifier (indentation: int) (qualifiedIdentifier: QualifiedIdentifier) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(QualifiedIdentifier)) (qualifiedIdentifier.ToString() |> normalizeString)
    qualifiedIdentifier.Tokens |> Array.iter (fun token -> onSyntaxToken (indentation + 1) token)
    
    
and private onTypeSyntax (indentation: int) (typeSyntax: TypeSyntax) =
    match typeSyntax with
    | NameSyntax nameSyntax -> onNameSyntax indentation nameSyntax
    | BuiltinTypeSyntax builtinTypeSyntax -> onBuiltinTypeSyntax indentation builtinTypeSyntax
    | FunctionTypeSyntax functionTypeSyntax -> onFunctionTypeSyntax indentation functionTypeSyntax
    | ArrayTypeSyntax arrayTypeSyntax -> onArrayTypeSyntax indentation arrayTypeSyntax
    | GenericTypeSyntax genericTypeSyntax -> onGenericTypeSyntax indentation genericTypeSyntax
    
and private onNameSyntax (indentation: int) (nameSyntax: NameSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(NameSyntax)) (nameSyntax.ToString() |> normalizeString)
    match nameSyntax.Identifier with
    | SimpleIdentifier simpleIdentifier -> onSimpleIdentifier (indentation + 1) simpleIdentifier
    | QualifiedIdentifier qualifiedIdentifier -> onQualifiedIdentifier (indentation + 1) qualifiedIdentifier
    
and private onBuiltinTypeSyntax (indentation: int) (builtinTypeSyntax: BuiltinTypeSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(BuiltinTypeSyntax)) (builtinTypeSyntax.ToString() |> normalizeString)
    
and private onFunctionTypeSyntax (indentation: int) (functionTypeSyntax: FunctionTypeSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(FunctionTypeSyntax)) (functionTypeSyntax.ToString() |> normalizeString)
    Seq.iter (onTypeSyntax (indentation + 1)) functionTypeSyntax.ParameterTypes
    
and private onArrayTypeSyntax (indentation: int) (arrayTypeSyntax: ArrayTypeSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(ArrayTypeSyntax)) (arrayTypeSyntax.ToString() |> normalizeString)
    onTypeSyntax (indentation + 1) arrayTypeSyntax.Type
    
and private onGenericTypeSyntax (indentation: int) (genericTypeSyntax: GenericTypeSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(GenericTypeSyntax)) (genericTypeSyntax.ToString() |> normalizeString)
    onTypeSyntax (indentation + 1) genericTypeSyntax.Type
    Seq.iter (onTypeSyntax (indentation + 1)) genericTypeSyntax.GenericTypes

    
    
and private onIfExpressionSyntax (indentation: int) (ifStatementSyntax: IfExpressionSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(IfExpressionSyntax)) (ifStatementSyntax.ToString() |> normalizeString)
    onExpressionSyntax (indentation + 1) ifStatementSyntax.Condition
    onBlockSyntax (indentation + 1) ifStatementSyntax.Clause
    
    ifStatementSyntax.ElseIfClauses |> Array.iter (onElseIfClauseSyntax (indentation + 1))
    
    if ifStatementSyntax.ElseClause.IsSome then
        onElseClauseSyntax (indentation + 1) ifStatementSyntax.ElseClause.Value
    
and private onElseIfClauseSyntax (indentation: int) (elseIfClauseSyntax: ElseIfClauseSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(ElseIfClauseSyntax)) (elseIfClauseSyntax.ToString() |> normalizeString)
    onExpressionSyntax (indentation + 1) elseIfClauseSyntax.Condition
    onBlockSyntax (indentation + 1) elseIfClauseSyntax.Clause
    
and private onElseClauseSyntax (indentation: int) (elseClauseSyntax: ElseClauseSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(ElseClauseSyntax)) (elseClauseSyntax.ToString() |> normalizeString)
    onBlockSyntax (indentation + 1) elseClauseSyntax.ElseClause
    
    
    
let rec private onStatementSyntax (indentation: int) (statementSyntax: StatementSyntax) =
    match statementSyntax with
    | BlockSyntax blockSyntax -> onBlockSyntax indentation blockSyntax
    | ExpressionStatementSyntax expressionStatementSyntax -> onExpressionStatementSyntax indentation expressionStatementSyntax
    | VariableDeclarationStatementSyntax variableDeclarationStatementSyntax -> onVariableDeclarationStatementSyntax indentation variableDeclarationStatementSyntax
    
and private onBlockSyntax (indentation: int) (blockSyntax: BlockSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(BlockSyntax)) (blockSyntax.ToString() |> normalizeString)
    blockSyntax.Statements |> Array.iter (onStatementSyntax (indentation + 1))
    
and private onExpressionStatementSyntax (indentation: int) (expressionStatementSyntax: ExpressionStatementSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(ExpressionStatementSyntax)) (expressionStatementSyntax.ToString() |> normalizeString)
    onExpressionSyntax (indentation + 1) expressionStatementSyntax.Expression 
    
and private onVariableDeclarationStatementSyntax (indentation: int) (variableDeclarationStatementSyntax: VariableDeclarationStatementSyntax) =
    printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(VariableDeclarationStatementSyntax)) (variableDeclarationStatementSyntax.ToString() |> normalizeString)
    
    if variableDeclarationStatementSyntax.TypeAnnotation.IsSome then
        printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(VariableTypeAnnotation)) (variableDeclarationStatementSyntax.TypeAnnotation.Value.ToString() |> normalizeString)
