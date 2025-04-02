module Monkey.AST.AstTraverser


type SyntaxWalker<'CustomState> =
    {
      // Syntax Node
      OnUsingDirectiveSyntax: 'CustomState -> UsingDirectiveSyntax -> ('CustomState * UsingDirectiveSyntax)
      OnNamespaceDeclarationSyntax: 'CustomState -> NamespaceDeclarationSyntax -> ('CustomState * NamespaceDeclarationSyntax)
      OnArgumentListSyntax: 'CustomState -> ArgumentListSyntax -> ('CustomState * ArgumentListSyntax)
      OnParameterListSyntax: 'CustomState -> ParameterListSyntax -> ('CustomState * ParameterListSyntax)
      OnExpressionSyntax: 'CustomState -> ExpressionSyntax -> ('CustomState * ExpressionSyntax)
      OnStatementSyntax: 'CustomState -> StatementSyntax -> ('CustomState * StatementSyntax)
      
      // Statements
      OnBlockSyntax: 'CustomState -> BlockSyntax -> ('CustomState * BlockSyntax)
      OnExpressionStatementSyntax: 'CustomState -> ExpressionStatementSyntax -> ('CustomState * ExpressionStatementSyntax)
      OnVariableDeclarationStatementSyntax: 'CustomState -> VariableDeclarationStatementSyntax -> ('CustomState * VariableDeclarationStatementSyntax)
      
      // Expressions
      OnParenthesizedExpressionSyntax: 'CustomState -> ParenthesizedExpressionSyntax -> ('CustomState * ParenthesizedExpressionSyntax)
      OnFunctionExpressionSyntax: 'CustomState -> FunctionExpressionSyntax -> ('CustomState * FunctionExpressionSyntax)
      OnBinaryExpressionSyntax: 'CustomState -> BinaryExpressionSyntax -> ('CustomState * BinaryExpressionSyntax)
      OnInterpolatedStringExpressionSyntax: 'CustomState -> InterpolatedStringExpressionSyntax -> ('CustomState * InterpolatedStringExpressionSyntax)
      OnInvocationExpressionSyntax: 'CustomState -> InvocationExpressionSyntax -> ('CustomState * InvocationExpressionSyntax)
      OnLiteralExpressionSyntax: 'CustomState -> LiteralExpressionSyntax -> ('CustomState * LiteralExpressionSyntax)
      OnPostfixExpressionSyntax: 'CustomState -> PostfixExpressionSyntax -> ('CustomState * PostfixExpressionSyntax)
      OnPrefixExpressionSyntax: 'CustomState -> PrefixExpressionSyntax -> ('CustomState * PrefixExpressionSyntax)
      OnIdentifierSyntax: 'CustomState -> IdentifierSyntax -> ('CustomState * IdentifierSyntax)
      OnTypeSyntax: 'CustomState -> TypeSyntax -> ('CustomState * TypeSyntax)
      OnIfExpressionSyntax: 'CustomState -> IfExpressionSyntax -> ('CustomState * IfExpressionSyntax)
      OnArrayExpressionSyntax: 'CustomState -> ArrayExpressionSyntax -> ('CustomState * ArrayExpressionSyntax)
       }
with
    static member Create(
          ?onUsingDirectiveSyntax: 'CustomState -> UsingDirectiveSyntax -> ('CustomState * UsingDirectiveSyntax),
          ?onNamespaceDeclarationSyntax: 'CustomState -> NamespaceDeclarationSyntax -> ('CustomState * NamespaceDeclarationSyntax),
          ?onArgumentListSyntax: 'CustomState -> ArgumentListSyntax -> ('CustomState * ArgumentListSyntax),
          ?onParameterListSyntax: 'CustomState -> ParameterListSyntax -> ('CustomState * ParameterListSyntax),
          ?onExpressionSyntax: 'CustomState -> ExpressionSyntax -> ('CustomState * ExpressionSyntax),
          ?onStatementSyntax: 'CustomState -> StatementSyntax -> ('CustomState * StatementSyntax),
          ?onBlockSyntax: 'CustomState -> BlockSyntax -> ('CustomState * BlockSyntax),
          ?onExpressionStatementSyntax: 'CustomState -> ExpressionStatementSyntax -> ('CustomState * ExpressionStatementSyntax),
          ?onVariableDeclarationStatementSyntax: 'CustomState -> VariableDeclarationStatementSyntax -> ('CustomState * VariableDeclarationStatementSyntax),
          ?onParenthesizedExpressionSyntax: 'CustomState -> ParenthesizedExpressionSyntax -> ('CustomState * ParenthesizedExpressionSyntax),
          ?onFunctionExpressionSyntax: 'CustomState -> FunctionExpressionSyntax -> ('CustomState * FunctionExpressionSyntax),
          ?onBinaryExpressionSyntax: 'CustomState -> BinaryExpressionSyntax -> ('CustomState * BinaryExpressionSyntax),
          ?onInterpolatedStringExpressionSyntax: 'CustomState -> InterpolatedStringExpressionSyntax -> ('CustomState * InterpolatedStringExpressionSyntax),
          ?onInvocationExpressionSyntax: 'CustomState -> InvocationExpressionSyntax -> ('CustomState * InvocationExpressionSyntax),
          ?onLiteralExpressionSyntax: 'CustomState -> LiteralExpressionSyntax -> ('CustomState * LiteralExpressionSyntax),
          ?onPostfixExpressionSyntax: 'CustomState -> PostfixExpressionSyntax -> ('CustomState * PostfixExpressionSyntax),
          ?onPrefixExpressionSyntax: 'CustomState -> PrefixExpressionSyntax -> ('CustomState * PrefixExpressionSyntax),
          ?onIdentifierSyntax: 'CustomState -> IdentifierSyntax -> ('CustomState * IdentifierSyntax),
          ?onTypeSyntax: 'CustomState -> TypeSyntax -> ('CustomState * TypeSyntax),
          ?onIfExpressionSyntax: 'CustomState -> IfExpressionSyntax -> ('CustomState * IfExpressionSyntax),
          ?onArrayExpressionSyntax: 'CustomState -> ArrayExpressionSyntax -> ('CustomState * ArrayExpressionSyntax)
        ) =
        
        let onUsingDirectiveSyntax = Option.defaultValue (fun state o -> state, o) onUsingDirectiveSyntax
        let onNamespaceDeclarationSyntax = Option.defaultValue (fun state o -> state, o) onNamespaceDeclarationSyntax
        let onArgumentListSyntax = Option.defaultValue (fun state o -> state, o) onArgumentListSyntax
        let onParameterListSyntax = Option.defaultValue (fun state o -> state, o) onParameterListSyntax
        let onExpressionSyntax = Option.defaultValue (fun state o -> state, o) onExpressionSyntax
        let onStatementSyntax = Option.defaultValue (fun state o -> state, o) onStatementSyntax
        let onBlockSyntax = Option.defaultValue (fun state o -> state, o) onBlockSyntax
        let onExpressionStatementSyntax = Option.defaultValue (fun state o -> state, o) onExpressionStatementSyntax
        let onVariableDeclarationStatementSyntax = Option.defaultValue (fun state o -> state, o) onVariableDeclarationStatementSyntax
        let onParenthesizedExpressionSyntax = Option.defaultValue (fun state o -> state, o) onParenthesizedExpressionSyntax
        let onFunctionExpressionSyntax = Option.defaultValue (fun state o -> state, o) onFunctionExpressionSyntax
        let onBinaryExpressionSyntax = Option.defaultValue (fun state o -> state, o) onBinaryExpressionSyntax
        let onInterpolatedStringExpressionSyntax = Option.defaultValue (fun state o -> state, o) onInterpolatedStringExpressionSyntax
        let onInvocationExpressionSyntax = Option.defaultValue (fun state o -> state, o) onInvocationExpressionSyntax
        let onLiteralExpressionSyntax = Option.defaultValue (fun state o -> state, o) onLiteralExpressionSyntax
        let onPostfixExpressionSyntax = Option.defaultValue (fun state o -> state, o) onPostfixExpressionSyntax
        let onPrefixExpressionSyntax = Option.defaultValue (fun state o -> state, o) onPrefixExpressionSyntax
        let onIdentifierSyntax = Option.defaultValue (fun state o -> state, o) onIdentifierSyntax
        let onTypeSyntax = Option.defaultValue (fun state o -> state, o) onTypeSyntax
        let onIfExpressionSyntax = Option.defaultValue (fun state o -> state, o) onIfExpressionSyntax
        let onArrayExpressionSyntax = Option.defaultValue (fun state o -> state, o) onArrayExpressionSyntax
        
        { OnUsingDirectiveSyntax = onUsingDirectiveSyntax
          OnNamespaceDeclarationSyntax = onNamespaceDeclarationSyntax
          OnArgumentListSyntax = onArgumentListSyntax
          OnParameterListSyntax = onParameterListSyntax
          OnExpressionSyntax = onExpressionSyntax
          OnStatementSyntax = onStatementSyntax
          OnBlockSyntax = onBlockSyntax
          OnExpressionStatementSyntax = onExpressionStatementSyntax
          OnVariableDeclarationStatementSyntax = onVariableDeclarationStatementSyntax
          OnParenthesizedExpressionSyntax = onParenthesizedExpressionSyntax
          OnFunctionExpressionSyntax = onFunctionExpressionSyntax
          OnBinaryExpressionSyntax = onBinaryExpressionSyntax
          OnInterpolatedStringExpressionSyntax = onInterpolatedStringExpressionSyntax
          OnInvocationExpressionSyntax = onInvocationExpressionSyntax
          OnLiteralExpressionSyntax = onLiteralExpressionSyntax
          OnPostfixExpressionSyntax = onPostfixExpressionSyntax
          OnPrefixExpressionSyntax = onPrefixExpressionSyntax
          OnIdentifierSyntax = onIdentifierSyntax
          OnTypeSyntax = onTypeSyntax
          OnIfExpressionSyntax = onIfExpressionSyntax
          OnArrayExpressionSyntax = onArrayExpressionSyntax }
        
    member this.Walk(initialState: 'CustomState, root: MonkeyCompilationUnit) : MonkeyCompilationUnit =
        let rec walkCore (state: 'CustomState) (msn: MonkeySyntaxNode) : 'CustomState * MonkeySyntaxNode =
            match msn with
            | UsingDirectiveSyntax usingDirectiveSyntax -> 
                this.OnUsingDirectiveSyntax state usingDirectiveSyntax
                |> (fun (cs, o) -> cs, MonkeySyntaxNode.UsingDirectiveSyntax o)
            | NamespaceDeclarationSyntax namespaceDeclarationSyntax -> 
                this.OnNamespaceDeclarationSyntax state namespaceDeclarationSyntax
                |> (fun (cs, o) -> cs, MonkeySyntaxNode.NamespaceDeclarationSyntax o)
            | ArgumentListSyntax argumentListSyntax -> 
                this.OnArgumentListSyntax state argumentListSyntax
                |> (fun (cs, o) -> cs, MonkeySyntaxNode.ArgumentListSyntax o)
            | ParameterListSyntax parameterListSyntax -> 
                this.OnParameterListSyntax state parameterListSyntax
                |> (fun (cs, o) -> cs, MonkeySyntaxNode.ParameterListSyntax o)
            | ExpressionSyntax expressionSyntax -> 
                (*
                this.OnExpressionSyntax state expressionSyntax
                |> (fun (cs, o) -> cs, MonkeySyntaxNode.ExpressionSyntax o)
                *)
                onExpression state expressionSyntax
                |> (fun (cs, o) -> cs, MonkeySyntaxNode.ExpressionSyntax o)
            | StatementSyntax statementSyntax ->
                (*
                this.OnStatementSyntax state statementSyntax
                |> (fun (cs, o) -> cs, MonkeySyntaxNode.StatementSyntax o)
                *)
                onStatement state statementSyntax
                |> (fun (cs, o) -> cs, MonkeySyntaxNode.StatementSyntax o)
            
        and onExpression (state: 'CustomState) (expr: ExpressionSyntax) : 'CustomState * ExpressionSyntax =
            match expr with
            | ParenthesizedExpressionSyntax parenthesizedExpressionSyntax -> 
                this.OnParenthesizedExpressionSyntax state parenthesizedExpressionSyntax
                |> (fun (cs, o) -> cs, ExpressionSyntax.ParenthesizedExpressionSyntax o)
            | ExpressionSyntax.FunctionExpressionSyntax functionExpressionSyntax -> 
                this.OnFunctionExpressionSyntax state functionExpressionSyntax
                |> (fun (cs, o) -> cs, ExpressionSyntax.FunctionExpressionSyntax o)
            | BinaryExpressionSyntax binaryExpressionSyntax -> 
                this.OnBinaryExpressionSyntax state binaryExpressionSyntax
                |> (fun (cs, o) -> cs, ExpressionSyntax.BinaryExpressionSyntax o)
            | InterpolatedStringExpressionSyntax interpolatedStringExpressionSyntax -> 
                this.OnInterpolatedStringExpressionSyntax state interpolatedStringExpressionSyntax
                |> (fun (cs, o) -> cs, ExpressionSyntax.InterpolatedStringExpressionSyntax o)
            | InvocationExpressionSyntax invocationExpressionSyntax -> 
                this.OnInvocationExpressionSyntax state invocationExpressionSyntax
                |> (fun (cs, o) -> cs, ExpressionSyntax.InvocationExpressionSyntax o)
            | LiteralExpressionSyntax literalExpressionSyntax -> 
                this.OnLiteralExpressionSyntax state literalExpressionSyntax
                |> (fun (cs, o) -> cs, ExpressionSyntax.LiteralExpressionSyntax o)
            | PostfixExpressionSyntax postfixExpressionSyntax -> 
                this.OnPostfixExpressionSyntax state postfixExpressionSyntax
                |> (fun (cs, o) -> cs, ExpressionSyntax.PostfixExpressionSyntax o)
            | PrefixExpressionSyntax prefixExpressionSyntax -> 
                this.OnPrefixExpressionSyntax state prefixExpressionSyntax
                |> (fun (cs, o) -> cs, ExpressionSyntax.PrefixExpressionSyntax o)
            | ExpressionSyntax.IdentifierSyntax identifierSyntax -> 
                this.OnIdentifierSyntax state identifierSyntax
                |> (fun (cs, o) -> cs, ExpressionSyntax.IdentifierSyntax o)
            | TypeSyntax typeSyntax -> 
                this.OnTypeSyntax state typeSyntax
                |> (fun (cs, o) -> cs, ExpressionSyntax.TypeSyntax o)
            | IfExpressionSyntax ifExpressionSyntax -> 
                this.OnIfExpressionSyntax state ifExpressionSyntax
                |> (fun (cs, o) -> cs, ExpressionSyntax.IfExpressionSyntax o)
            | ArrayExpressionSyntax arrayExpressionSyntax ->
                this.OnArrayExpressionSyntax state arrayExpressionSyntax
                |> (fun (cs, o) -> cs, ExpressionSyntax.ArrayExpressionSyntax o)
            
        and onStatement (state: 'CustomState) (stat: StatementSyntax)  =
            match stat with
            | BlockSyntax blockSyntax -> 
                this.OnBlockSyntax state blockSyntax
                |> (fun (cs, o) -> cs, StatementSyntax.BlockSyntax o)
            | ExpressionStatementSyntax expressionStatementSyntax -> 
                this.OnExpressionStatementSyntax state expressionStatementSyntax
                |> (fun (cs, o) -> cs, StatementSyntax.ExpressionStatementSyntax o)
            | VariableDeclarationStatementSyntax variableDeclarationStatementSyntax -> 
                this.OnVariableDeclarationStatementSyntax state variableDeclarationStatementSyntax
                |> (fun (cs, o) -> cs, StatementSyntax.VariableDeclarationStatementSyntax o)

        let newNodes =
            root.SyntaxNodes
            |> Array.map (walkCore initialState)
            |> Array.map snd
        { SyntaxNodes = newNodes }