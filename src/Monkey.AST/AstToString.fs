module Monkey.AST.AstToString

open Microsoft.CodeAnalysis.CSharp
open Monkey.AST.AstPrinter

type private IndentationState =
    { Indentation: int }
with
    member this.Inc() =
        { this with Indentation = this.Indentation + 1 }
        
    member this.GetIdent() =
        let indentationStr = "    "
        String.replicate this.Indentation indentationStr
        

[<AutoOpen>]
module private rec SyntaxWalkerFunctions =
    // TODO: Replace with out own config
    type Config = PrintTraverserConfigSingleton
    
    let indentationStr = "    "
    
    
    let onMonkeySyntaxNode (indentation: int) (monkeySyntaxNode: MonkeySyntaxNode) =
        match monkeySyntaxNode with
        | UsingDirectiveSyntax usingDirectiveSyntax -> failwith "todo"
        | NamespaceDeclarationSyntax namespaceDeclarationSyntax -> failwith "todo"
        | ArgumentListSyntax argumentListSyntax -> failwith "todo"
        | ParameterListSyntax parameterListSyntax -> failwith "todo"
        | ExpressionSyntax expressionSyntax -> onExpressionSyntax indentation expressionSyntax
        | StatementSyntax statementSyntax -> onStatementSyntax indentation statementSyntax
    

    let onUsingDirectiveSyntax (indentation: int) (usingDirective: UsingDirectiveSyntax) =
        $"%s{String.replicate indentation indentationStr}%s{nameof(UsingDirectiveSyntax)} : %s{usingDirective.ToString() |> normalizeString}"
        
    let onNamespaceDeclarationSyntax (indentation: int) (namespaceDeclaration: NamespaceDeclarationSyntax) =
        $"%s{String.replicate indentation indentationStr}%s{nameof(NamespaceDeclarationSyntax)} : %s{namespaceDeclaration.ToString() |> normalizeString}"
            
    let onSyntaxToken (indentation: int) (syntaxToken: SyntaxToken) =
        let syntaxTokenStr =  $"%s{String.replicate indentation indentationStr}%s{nameof(SyntaxToken)} : %s{syntaxToken.ToString() |> normalizeString}"
        let syntaxKindStr = onSyntaxKind (indentation + 1) syntaxToken.Kind
        
        let value = syntaxToken.Value
        let indentation = indentation + 1
        let valueTypeStr = $"%s{String.replicate indentation indentationStr}value type : %s{value.GetType().ToString()}"
        let valueStr = $"%s{String.replicate indentation indentationStr}value : %s{value.ToString()}"
        System.String.Join("\n", [| syntaxTokenStr ; syntaxKindStr ; valueTypeStr ; valueStr |])
            
        
    let onSyntaxKind (indentation: int) (syntaxKind: SyntaxKind) =
        $"%s{String.replicate indentation indentationStr}Kind : %s{syntaxKind.ToString()}" 
        
       
        
    let normalizeString (str: string) =
        let normalziedStr = str.Replace("\n", "\\n").Replace("\r", "\\r").Trim()
        $"`{normalziedStr}`"
        
        
    let onArgumentListSyntax (indentation: int) (argumentListSyntax: ArgumentListSyntax) =
        let argumentListStr = $"%s{String.replicate indentation indentationStr}%s{nameof(ArgumentListSyntax)} : %s{argumentListSyntax.ToString() |> normalizeString}"
        let argumentsStr = argumentListSyntax.Arguments |> Array.map (onExpressionSyntax (indentation + 1))
        System.String.Join("\n", Array.concat [| [| argumentListStr |]; argumentsStr |])
        
    let onParameterListSyntax (indentation: int) (parameterListSyntax: ParameterListSyntax) =
        $"%s{String.replicate indentation indentationStr}%s{nameof(ParameterListSyntax)} : %s{parameterListSyntax.ToString() |> normalizeString}"
        
        
    let rec onExpressionSyntax (indentation: int) (expressionSyntax: ExpressionSyntax) : string =
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
        
        
    and onArrayExpressionSyntax (indentation: int) (arrayExpressionSyntax: ArrayExpressionSyntax) =
        match arrayExpressionSyntax with
        | ListInitialization listInitialization -> onArrayListInitialization indentation listInitialization
        | SizeBasedInitialization sizeBasedInitialization -> onArraySizeBasedInitialization indentation sizeBasedInitialization
        
    and onArrayListInitialization (indentation: int) (onArrayListInitialization: ListInitialization) =
        let resizeArr = ResizeArray<string>()
        let arrayInitStr = $"%s{String.replicate indentation indentationStr}%s{nameof(ListInitialization)} : %s{onArrayListInitialization.ToString() |> normalizeString}"
        resizeArr.Add(arrayInitStr)
        
        if Config.Instance.PrintSyntaxTokens then
            resizeArr.Add(onSyntaxToken (indentation + 1) onArrayListInitialization.OpenBracketToken)
            
        let valueStrs = onArrayListInitialization.Values |> Array.map (onExpressionSyntax (indentation + 1))
        resizeArr.AddRange(valueStrs)
        
        if Config.Instance.PrintSyntaxTokens then
            resizeArr.Add(onSyntaxToken (indentation + 1) onArrayListInitialization.CloseBracketToken)
            
        System.String.Join("\n", resizeArr.ToArray())
        
        
    and onArraySizeBasedInitialization (indentation: int) (sizeBasedInitialization: SizeBasedInitialization) =
        let resizeArr = ResizeArray<string>()
        let arraySizeInitStr =  $"%s{String.replicate indentation indentationStr}%s{nameof(SizeBasedInitialization)} : %s{sizeBasedInitialization.ToString() |> normalizeString}"
        
        if Config.Instance.PrintSyntaxTokens then
            resizeArr.Add(onSyntaxToken (indentation + 1) sizeBasedInitialization.NewToken)
            
        resizeArr.Add(onSyntaxToken (indentation + 1) sizeBasedInitialization.TypeToken);  // mandatory, since key information is stored in the tokens
        
        if Config.Instance.PrintSyntaxTokens then
            resizeArr.Add(onSyntaxToken (indentation + 1) sizeBasedInitialization.OpenBracketToken);
            resizeArr.Add(onSyntaxToken (indentation + 1) sizeBasedInitialization.CloseBracketToken);
            
        System.String.Join("\n", resizeArr.ToArray())


    and onParenthesizedExpressionSyntax (indentation: int) (parenthesizedExpressionSyntax: ParenthesizedExpressionSyntax) =
        let resizeArr = ResizeArray<string>()
        let parenStr = $"%s{String.replicate indentation indentationStr}%s{nameof(ParenthesizedExpressionSyntax)} : %s{parenthesizedExpressionSyntax.ToString() |> normalizeString}"
        resizeArr.Add(parenStr);
        
        if Config.Instance.PrintSyntaxTokens then
            resizeArr.Add(onSyntaxToken (indentation + 1) parenthesizedExpressionSyntax.OpenParenToken);
            
        resizeArr.Add(onExpressionSyntax (indentation + 1) parenthesizedExpressionSyntax.Expression);
        
        if Config.Instance.PrintSyntaxTokens then
            resizeArr.Add(onSyntaxToken (indentation + 1) parenthesizedExpressionSyntax.CloseParenToken)
        
        System.String.Join("\n", resizeArr.ToArray())
        
        
    and onFunctionExpressionSyntax (indentation: int) (functionExpressionSyntax: FunctionExpressionSyntax) =
        let fnExprStr = $"%s{String.replicate indentation indentationStr}%s{nameof(FunctionExpressionSyntax)} : %s{functionExpressionSyntax.ToString() |> normalizeString}"
        let parameterListStr = onParameterListSyntax (indentation + 1) functionExpressionSyntax.ParameterList
        let blockSyntaxStr = onBlockSyntax (indentation + 1) functionExpressionSyntax.Body
        let typeSyntaxStr = onTypeSyntax (indentation + 1) functionExpressionSyntax.ReturnType
        
        System.String.Join("\n", [| fnExprStr ; parameterListStr ; blockSyntaxStr ; typeSyntaxStr  |])
        
        
    and onBinaryExpressionSyntax (indentation: int) (binaryExpressionSyntax: BinaryExpressionSyntax) =
        System.String.Join("\n", [|
            $"%s{String.replicate indentation indentationStr}%s{nameof(BinaryExpressionSyntax)} : %s{binaryExpressionSyntax.ToString() |> normalizeString}"
            onExpressionSyntax (indentation + 1) binaryExpressionSyntax.Left
            onExpressionSyntax (indentation + 1) binaryExpressionSyntax.Right
        |])
        
        
    and onInterpolatedStringExpressionSyntax (indentation: int) (interpolatedStringExpressionSyntax: InterpolatedStringExpressionSyntax) =
        let interpolatedStrStr =  $"%s{String.replicate indentation indentationStr}%s{nameof(InterpolatedStringExpressionSyntax)} : %s{interpolatedStringExpressionSyntax.ToString() |> normalizeString}"
        let interpolatedStringContentsArr = Array.map (onInterpolatedStringContents (indentation + 1)) interpolatedStringExpressionSyntax.Contents
        System.String.Join("\n", Array.concat [| [| interpolatedStrStr |] ; interpolatedStringContentsArr |])
        
    and onInterpolatedStringContents (indentation: int) (interpolatedStringContent: InterpolatedStringContent) =
        match interpolatedStringContent with
        | InterpolatedStringText interpolatedStringText -> onInterpolatedStringText indentation interpolatedStringText
        | Interpolation interpolation -> onInterpolation indentation interpolation
        
    and onInterpolatedStringText (indentation: int) (interpolatedStringText: InterpolatedStringText) =
        let interpolatedStrTextStr = $"%s{String.replicate indentation indentationStr}%s{nameof(InterpolatedStringText)} : %s{interpolatedStringText.ToString() |> normalizeString}"
        let syntaxTokenStr = onSyntaxToken (indentation + 1) (interpolatedStringText.TextToken)  // mandatory, since key information is stored in the tokens
        System.String.Join("\n", [| interpolatedStrTextStr; syntaxTokenStr |])
        
    and onInterpolation (indentation: int) (interpolation: Interpolation) =
        let resizeArr = ResizeArray<string>()
        let interpolationStr = $"%s{String.replicate indentation indentationStr}%s{nameof(Interpolation)} : %s{interpolation.ToString() |> normalizeString}"
        resizeArr.Add(interpolationStr)
        
        if Config.Instance.PrintSyntaxTokens then
            resizeArr.Add(onSyntaxToken (indentation + 1) (interpolation.OpenBraceToken))
            
        resizeArr.Add(onExpressionSyntax (indentation + 1) (interpolation.Expression))
        
        if Config.Instance.PrintSyntaxTokens then
            resizeArr.Add(onSyntaxToken (indentation + 1) (interpolation.OpenBraceToken))
            
        System.String.Join("\n", resizeArr.ToArray())
        
        
    and onInvocationExpressionSyntax (indentation: int) (invocationExpressionSyntax: InvocationExpressionSyntax) =
        let invocationExpressionStr = $"%s{String.replicate indentation indentationStr}%s{nameof(InvocationExpressionSyntax)} : %s{invocationExpressionSyntax.ToString() |> normalizeString}"
        
        let invocationExpressionExpressionStr = 
            match invocationExpressionSyntax.Expression with
            | InvocationExpressionLeftExpression.FunctionExpressionSyntax functionExpressionSyntax ->
                onFunctionExpressionSyntax (indentation + 1) functionExpressionSyntax
            | InvocationExpressionLeftExpression.IdentifierSyntax identifierNameSyntax ->
                onIdentifierSyntax (indentation + 1) identifierNameSyntax
            | InvocationExpressionLeftExpression.ParenthesizedFunctionExpressionSyntax parenthesizedExpressionSyntax ->
                onInvocationParenthesizedExpressionSyntax (indentation + 1) parenthesizedExpressionSyntax
            
        let argumentListStr = onArgumentListSyntax (indentation + 1) invocationExpressionSyntax.Arguments
        System.String.Join("\n", [| invocationExpressionStr; invocationExpressionExpressionStr; argumentListStr |])
        
    and onInvocationParenthesizedExpressionSyntax (indentation: int) (invocationParenthesizedExpressionSyntax: InvocationParenthesizedExpressionSyntax) =
        let resizeArr = ResizeArray<string>()
        let invocationParenthesizedExpressionStr = $"%s{String.replicate indentation indentationStr}%s{nameof(InvocationParenthesizedExpressionSyntax)} : %s{invocationParenthesizedExpressionSyntax.ToString() |> normalizeString}"
        
        if Config.Instance.PrintSyntaxTokens then
            resizeArr.Add(onSyntaxToken (indentation + 1) invocationParenthesizedExpressionSyntax.OpenParenToken)
       
        let invocationParenthesizedExpressionStr =  
            match invocationParenthesizedExpressionSyntax.Expression with
            | InvocationExpressionLeftExpression.FunctionExpressionSyntax functionExpressionSyntax ->
                onFunctionExpressionSyntax (indentation + 1) functionExpressionSyntax
            | InvocationExpressionLeftExpression.IdentifierSyntax identifierNameSyntax ->
                onIdentifierSyntax (indentation + 1) identifierNameSyntax
            | InvocationExpressionLeftExpression.ParenthesizedFunctionExpressionSyntax parenthesizedExpressionSyntax ->
                onInvocationParenthesizedExpressionSyntax (indentation + 1) parenthesizedExpressionSyntax
        resizeArr.Add(invocationParenthesizedExpressionStr)
            
        if Config.Instance.PrintSyntaxTokens then
            resizeArr.Add(onSyntaxToken (indentation + 1) invocationParenthesizedExpressionSyntax.CloseParenToken)
        
        System.String.Join("\n", resizeArr.ToArray())

        
    and onLiteralExpressionSyntax (indentation: int) (literalExpressionSyntax: LiteralExpressionSyntax) =
        let resizeArr = ResizeArray<string>()
        let literalExpressionStr = $"%s{String.replicate indentation indentationStr}%s{nameof(LiteralExpressionSyntax)} : %s{literalExpressionSyntax.ToString() |> normalizeString}"
        resizeArr.Add(literalExpressionStr)
        resizeArr.Add(onSyntaxKind (indentation + 1) literalExpressionSyntax.Kind)
        resizeArr.Add(onSyntaxToken (indentation + 1) literalExpressionSyntax.Token)  // mandatory, since key information is stored in the tokens
        System.String.Join("\n", resizeArr.ToArray())
        
    and onPostfixExpressionSyntax (indentation: int) (postfixExpressionSyntax: PostfixExpressionSyntax) =
        let resizeArr = ResizeArray<string>()
        let postfixExprStr = $"%s{String.replicate indentation indentationStr}%s{nameof(PostfixExpressionSyntax)} : %s{postfixExpressionSyntax.ToString() |> normalizeString}"
        resizeArr.Add(postfixExprStr)
        resizeArr.Add(onSyntaxKind (indentation + 1) postfixExpressionSyntax.Kind)
        resizeArr.Add(onExpressionSyntax (indentation + 1) postfixExpressionSyntax.Operand)
        resizeArr.Add(onSyntaxToken (indentation + 1) postfixExpressionSyntax.OperatorToken)  // mandatory, since key information is stored in the tokens
        System.String.Join("\n", resizeArr.ToArray())
        
    and onPrefixExpressionSyntax (indentation: int) (prefixExpressionSyntax: PrefixExpressionSyntax) =
        let resizeArr = ResizeArray<string>()
        let prefixExprStr = $"%s{String.replicate indentation indentationStr}%s{nameof(PrefixExpressionSyntax)} : %s{prefixExpressionSyntax.ToString() |> normalizeString}"
        resizeArr.Add(prefixExprStr)
        resizeArr.Add(onSyntaxKind (indentation + 1) prefixExpressionSyntax.Kind)
        resizeArr.Add(onSyntaxToken (indentation + 1) prefixExpressionSyntax.OperatorToken)  // mandatory, since key information is stored in the tokens
        resizeArr.Add(onExpressionSyntax (indentation + 1) prefixExpressionSyntax.Operand)
        System.String.Join("\n", resizeArr.ToArray())
        
        
    and onIdentifierSyntax (indentation: int) (identifierNameSyntax: IdentifierSyntax) =
        $"%s{String.replicate indentation indentationStr}%s{nameof(IdentifierSyntax)} : %s{identifierNameSyntax.ToString() |> normalizeString}"
        
    and onSimpleIdentifier (indentation: int) (simpleIdentifier: SimpleIdentifier) =
        $"%s{String.replicate indentation indentationStr}%s{nameof(SimpleIdentifier)} : %s{simpleIdentifier.ToString() |> normalizeString}"
        
    and onQualifiedIdentifier (indentation: int) (qualifiedIdentifier: QualifiedIdentifier) =
        let qualifiedIdentifierStr = $"%s{String.replicate indentation indentationStr}%s{nameof(QualifiedIdentifier)} : %s{qualifiedIdentifier.ToString() |> normalizeString}"
        let nameStrs = qualifiedIdentifier.Tokens |> Array.map (fun token -> onSyntaxToken (indentation + 1) token)
        System.String.Join("\n", Array.concat [| [| qualifiedIdentifierStr |]; nameStrs |])
        
        
    and onTypeSyntax (indentation: int) (typeSyntax: TypeSyntax) =
        match typeSyntax with
        | NameSyntax nameSyntax -> onNameSyntax indentation nameSyntax
        | BuiltinTypeSyntax builtinTypeSyntax -> onBuiltinTypeSyntax indentation builtinTypeSyntax
        | FunctionTypeSyntax functionTypeSyntax -> onFunctionTypeSyntax indentation functionTypeSyntax
        | ArrayTypeSyntax arrayTypeSyntax -> onArrayTypeSyntax indentation arrayTypeSyntax
        | GenericTypeSyntax genericTypeSyntax -> onGenericTypeSyntax indentation genericTypeSyntax
        
    and onNameSyntax (indentation: int) (nameSyntax: NameSyntax) =
        printfn "%s%s : %s" (String.replicate indentation indentationStr) (nameof(NameSyntax)) (nameSyntax.ToString() |> normalizeString)
        match nameSyntax.Identifier with
        | SimpleIdentifier simpleIdentifier -> onSimpleIdentifier (indentation + 1) simpleIdentifier
        | QualifiedIdentifier qualifiedIdentifier -> onQualifiedIdentifier (indentation + 1) qualifiedIdentifier
        
    and onBuiltinTypeSyntax (indentation: int) (builtinTypeSyntax: BuiltinTypeSyntax) =
        $"%s{String.replicate indentation indentationStr}%s{nameof(BuiltinTypeSyntax)} : %s{builtinTypeSyntax.ToString() |> normalizeString}"
        
    and onFunctionTypeSyntax (indentation: int) (functionTypeSyntax: FunctionTypeSyntax) =
        let functionTypeStr = $"%s{String.replicate indentation indentationStr}%s{nameof(FunctionTypeSyntax)} : %s{functionTypeSyntax.ToString() |> normalizeString}"
        let typeStrs = Array.map (onTypeSyntax (indentation + 1)) functionTypeSyntax.ParameterTypes
        System.String.Join("\n", Array.concat [| [| functionTypeStr |]; typeStrs |])
        
    and onArrayTypeSyntax (indentation: int) (arrayTypeSyntax: ArrayTypeSyntax) =
        let arrayTypeStr = $"%s{String.replicate indentation indentationStr}%s{nameof(ArrayTypeSyntax)} : %s{arrayTypeSyntax.ToString() |> normalizeString}"
        let typeStr = onTypeSyntax (indentation + 1) arrayTypeSyntax.Type
        System.String.Join("\n", [| arrayTypeStr; typeStr |])
        
    and onGenericTypeSyntax (indentation: int) (genericTypeSyntax: GenericTypeSyntax) =
        let genericTypeStr =  $"%s{String.replicate indentation indentationStr}%s{nameof(GenericTypeSyntax)} : %s{genericTypeSyntax.ToString() |> normalizeString}"
        let typeStr = onTypeSyntax (indentation + 1) genericTypeSyntax.Type
        let genericSubTypeStrs = Array.map (onTypeSyntax (indentation + 1)) genericTypeSyntax.GenericTypes
        System.String.Join("\n", Array.concat [| [| genericTypeStr |]; [| typeStr |]; genericSubTypeStrs |])

        
        
    and onIfExpressionSyntax (indentation: int) (ifStatementSyntax: IfExpressionSyntax) =
        let resizeArr = ResizeArray<string>()
        
        let ifExpressionStr = $"%s{String.replicate indentation indentationStr}%s{nameof(IfExpressionSyntax)} : %s{ifStatementSyntax.ToString() |> normalizeString}"
        resizeArr.Add(ifExpressionStr)
        
        let expressionStr = onExpressionSyntax (indentation + 1) ifStatementSyntax.Condition
        resizeArr.Add(expressionStr)
        
        let blockStr = onBlockSyntax (indentation + 1) ifStatementSyntax.Clause
        resizeArr.Add(blockStr)
        
        let elseIfClausesStr = ifStatementSyntax.ElseIfClauses |> Array.map (onElseIfClauseSyntax (indentation + 1))
        resizeArr.AddRange(elseIfClausesStr)
        
        if ifStatementSyntax.ElseClause.IsSome then
            resizeArr.Add(onElseClauseSyntax (indentation + 1) ifStatementSyntax.ElseClause.Value)
            
        System.String.Join("\n", resizeArr.ToArray())
        
        
    and onElseIfClauseSyntax (indentation: int) (elseIfClauseSyntax: ElseIfClauseSyntax) =
        let elseIfStr = $"%s{String.replicate indentation indentationStr}%s{nameof(ElseIfClauseSyntax)} : %s{elseIfClauseSyntax.ToString() |> normalizeString}"
        let conditionStr = onExpressionSyntax (indentation + 1) elseIfClauseSyntax.Condition
        let blockStr = onBlockSyntax (indentation + 1) elseIfClauseSyntax.Clause
        System.String.Join("\n", [| elseIfStr; conditionStr; blockStr |])
        
    and onElseClauseSyntax (indentation: int) (elseClauseSyntax: ElseClauseSyntax) =
        let elseStr = $"%s{String.replicate indentation indentationStr}%s{nameof(ElseClauseSyntax)} : %s{elseClauseSyntax.ToString() |> normalizeString}"
        let blockStr = onBlockSyntax (indentation + 1) elseClauseSyntax.ElseClause
        System.String.Join("\n", [| elseStr; blockStr |])
        
        
        
    let rec onStatementSyntax (indentation: int) (statementSyntax: StatementSyntax) =
        match statementSyntax with
        | BlockSyntax blockSyntax -> onBlockSyntax indentation blockSyntax
        | ExpressionStatementSyntax expressionStatementSyntax -> onExpressionStatementSyntax indentation expressionStatementSyntax
        | VariableDeclarationStatementSyntax variableDeclarationStatementSyntax -> onVariableDeclarationStatementSyntax indentation variableDeclarationStatementSyntax
        
    and onBlockSyntax (indentation: int) (blockSyntax: BlockSyntax) =
        let blockStr = $"%s{String.replicate indentation indentationStr}%s{nameof(BlockSyntax)} : %s{blockSyntax.ToString() |> normalizeString}"
        let blockStatementStrs = blockSyntax.Statements |> Array.map (onStatementSyntax (indentation + 1))
        System.String.Join("\n", Array.concat [| [| blockStr |]; blockStatementStrs |])
        
    and onExpressionStatementSyntax (indentation: int) (expressionStatementSyntax: ExpressionStatementSyntax) =
        let expressionStatementStr = $"%s{String.replicate indentation indentationStr}%s{nameof(ExpressionStatementSyntax)} : %s{expressionStatementSyntax.ToString() |> normalizeString}"
        let expressionStr = onExpressionSyntax (indentation + 1) expressionStatementSyntax.Expression
        System.String.Join("\n", [| expressionStatementStr; expressionStr |])
        
    and onVariableDeclarationStatementSyntax (indentation: int) (variableDeclarationStatementSyntax: VariableDeclarationStatementSyntax) =
        let resizeArr = ResizeArray<string>()
        let varDecStatStr = $"%s{String.replicate indentation indentationStr}%s{nameof(VariableDeclarationStatementSyntax)} : %s{variableDeclarationStatementSyntax.ToString() |> normalizeString}"
        resizeArr.Add(varDecStatStr)
        
        if variableDeclarationStatementSyntax.TypeAnnotation.IsSome then
            let varTypeAnotStr = $"%s{String.replicate indentation indentationStr}%s{nameof(VariableTypeAnnotation)} : %s{variableDeclarationStatementSyntax.TypeAnnotation.Value.ToString() |> normalizeString}"
            resizeArr.Add(varTypeAnotStr)
            
        System.String.Join("\n", resizeArr.ToArray())
        
        
let nodeToString (node: MonkeySyntaxNode) =
    onMonkeySyntaxNode 0 node

let statementSyntaxToString (statement: StatementSyntax) =
    onStatementSyntax 0 statement
    
