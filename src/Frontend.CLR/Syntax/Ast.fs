// Due to F# limitations, we can not declare types across several files for a better project structure.
module rec Monkey.Frontend.CLR.Syntax.Ast

open System.Runtime.InteropServices
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text


let private interleave (xs: string[]) (ys: string[]) =
    let m = min xs.Length ys.Length
    let head = [| for i in 0 .. m - 1 do yield xs.[i]; yield ys.[i] |]
    let tail = if xs.Length > m then xs.[m..] else [||]
    Array.append head tail

/// <summary>
/// 
/// </summary>
/// <remarks>
/// <ul>
///     <li>
///     We are unable to control Roslyn's 'TextSpan' properties which is very useful for displaying error messages. Hence,
///     why this type exists.
///     </li>
/// </ul>
/// </remarks>
type SyntaxToken =
    { Kind: SyntaxKind
      Text: string
      Value: obj option
      
      TextSpan: TextSpan  // excludes trivia
      FullTextSpan: TextSpan  // includes trivia
      
      LeadingTrivia: SyntaxTriviaList
      TrailingTrivia: SyntaxTriviaList }
with
    override this.ToString() =
        $"{this.LeadingTrivia.ToFullString()}{this.Text}{this.TrailingTrivia.ToFullString()}"
   
    static member AreEquivalent(st1: SyntaxToken, st2: SyntaxToken) =
        let valueComparison =
            match st1.Value, st2.Value with
            | Some val1, Some val2 -> val1 = val2
            | None, None -> true
            | _ -> false
            
        (st1.Kind = st2.Kind) && valueComparison


/// <summary>
/// Represents a non-terminal node in the syntax tree.
/// </summary>
type MonkeySyntaxNode =
    | CompilationUnitSyntax
    | UsingDeclarationSyntax
    | ArgumentListSyntax of ArgumentListSyntax
    | ParameterListSyntax of ParameterListSyntax
    | ExpressionSyntax of ExpressionSyntax
    | StatementSyntax of StatementSyntax
with
    override this.ToString() =
        match this with
        | CompilationUnitSyntax -> failwith "todo"
        | UsingDeclarationSyntax -> failwith "todo"
        | ArgumentListSyntax als -> als.ToString()
        | ParameterListSyntax pls -> pls.ToString()
        | ExpressionSyntax es -> es.ToString()
        | StatementSyntax ss -> ss.ToString()
        
    static member AreEquivalent(msn1: MonkeySyntaxNode, msn2: MonkeySyntaxNode) =
        match msn1, msn2 with
        | CompilationUnitSyntax, CompilationUnitSyntax -> failwith "todo"
        | UsingDeclarationSyntax, UsingDeclarationSyntax -> failwith "todo"
        | ArgumentListSyntax als1, ArgumentListSyntax als2 ->
            ArgumentListSyntax.AreEquivalent(als1, als2)
        | ParameterListSyntax pls1, ParameterListSyntax pls2 ->
            ParameterListSyntax.AreEquivalent(pls1, pls2)
        | ExpressionSyntax es1, ExpressionSyntax es2 ->
            ExpressionSyntax.AreEquivalent(es1, es2)
        | StatementSyntax ss1, StatementSyntax ss2 ->
            StatementSyntax.AreEquivalent(ss1, ss2)
        | _ -> false


/// <summary>
/// The parent type for all expression syntax types.
/// </summary>
type ExpressionSyntax =
    // | ArrayInitializationExpression
    | ParenthesizedExpressionSyntax of ParenthesizedExpressionSyntax
    | FunctionExpressionSyntax of FunctionExpressionSyntax
    | BinaryExpressionSyntax of BinaryExpressionSyntax
    | InterpolatedStringExpressionSyntax of InterpolatedStringExpressionSyntax
    | InvocationExpressionSyntax of InvocationExpressionSyntax
    | LiteralExpressionSyntax of LiteralExpressionSyntax
    | PostfixExpressionSyntax of PostfixExpressionSyntax
    | PrefixExpressionSyntax of PrefixExpressionSyntax
    | IdentifierNameSyntax of IdentifierNameSyntax
    | TypeSyntax of TypeSyntax
with
    override this.ToString() =
        match this with
        | ParenthesizedExpressionSyntax parenthesizedExpressionSyntax -> parenthesizedExpressionSyntax.ToString()
        | FunctionExpressionSyntax functionExpression -> functionExpression.ToString()
        | BinaryExpressionSyntax binaryExpression -> binaryExpression.ToString()
        | InterpolatedStringExpressionSyntax interpolatedStringExpression -> interpolatedStringExpression.ToString()
        | InvocationExpressionSyntax invocationExpressionSyntax -> invocationExpressionSyntax.ToString()
        | LiteralExpressionSyntax literalExpressionSyntax -> literalExpressionSyntax.ToString()
        | PostfixExpressionSyntax postfixExpressionSyntax -> postfixExpressionSyntax.ToString()
        | PrefixExpressionSyntax prefixExpressionSyntax -> prefixExpressionSyntax.ToString()
        | IdentifierNameSyntax identifierNameSyntax -> identifierNameSyntax.ToString()
        | TypeSyntax typeSyntax -> typeSyntax.ToString()
        
    static member AreEquivalent(es1: ExpressionSyntax, es2: ExpressionSyntax) =
        match es1, es2 with
        | ParenthesizedExpressionSyntax pes1, ParenthesizedExpressionSyntax pes2 ->
            ParenthesizedExpressionSyntax.AreEquivalent(pes1, pes2)
        | FunctionExpressionSyntax fe1, FunctionExpressionSyntax fe2 ->
            FunctionExpressionSyntax.AreEquivalent(fe1, fe2)
        | BinaryExpressionSyntax be1, BinaryExpressionSyntax be2 ->
            BinaryExpressionSyntax.AreEquivalent(be1, be2)
        | InterpolatedStringExpressionSyntax ise1, InterpolatedStringExpressionSyntax ise2 ->
            InterpolatedStringExpressionSyntax.AreEquivalent(ise1, ise2)
        | InvocationExpressionSyntax ies1, InvocationExpressionSyntax ies2 -> 
            InvocationExpressionSyntax.AreEquivalent(ies1, ies2)
        | LiteralExpressionSyntax les1, LiteralExpressionSyntax les2 ->
            LiteralExpressionSyntax.AreEquivalent(les1, les2)
        | PostfixExpressionSyntax pes1, PostfixExpressionSyntax pes2 ->
            PostfixExpressionSyntax.AreEquivalent(pes1, pes2)
        | PrefixExpressionSyntax pes1, PrefixExpressionSyntax pes2 ->
            PrefixExpressionSyntax.AreEquivalent(pes1, pes2)
        | IdentifierNameSyntax ins1, IdentifierNameSyntax ins2 ->
            IdentifierNameSyntax.AreEquivalent(ins1, ins2)
        | TypeSyntax ts1, TypeSyntax ts2 ->
            TypeSyntax.AreEquivalent(ts1, ts2)
        | _ -> false
    
    
    
/// <summary>
/// The parent type for all statement syntax types.
/// </summary>
type StatementSyntax =
    | BlockSyntax of BlockSyntax
    | ExpressionStatementSyntax of ExpressionStatementSyntax
    | IfStatementSyntax of IfStatementSyntax
    | VariableDeclarationStatementSyntax of VariableDeclarationStatementSyntax
with
    override this.ToString() =
        match this with
        | BlockSyntax blockSyntax -> blockSyntax.ToString()
        | ExpressionStatementSyntax expressionStatementSyntax -> expressionStatementSyntax.ToString()
        | IfStatementSyntax ifStatementSyntax -> ifStatementSyntax.ToString()
        | VariableDeclarationStatementSyntax variableDeclarationStatementSyntax -> variableDeclarationStatementSyntax.ToString()
        
    static member AreEquivalent(ss1: StatementSyntax, ss2: StatementSyntax) =
        match ss1, ss2 with
        | BlockSyntax bs1, BlockSyntax bs2 ->
            BlockSyntax.AreEquivalent(bs1, bs2)
        | ExpressionStatementSyntax ess1, ExpressionStatementSyntax ess2 ->
            ExpressionStatementSyntax.AreEquivalent(ess1, ess2)
        | IfStatementSyntax iss1, IfStatementSyntax iss2 ->
            IfStatementSyntax.AreEquivalent(iss1, iss2)
        | VariableDeclarationStatementSyntax vdss1, VariableDeclarationStatementSyntax vdss2 ->
            VariableDeclarationStatementSyntax.AreEquivalent(vdss1, vdss2)
        | _ -> false
    
    
    
(* #REGION Supporting Types *)

type TypeSyntax =
    | NameSyntax of NameSyntax
    | BuiltinTypeSyntax of BuiltinTypeSyntax
    | FunctionTypeSyntax of FunctionTypeSyntax
with
    override this.ToString() =
        match this with
        | NameSyntax nameSyntax -> nameSyntax.ToString()
        | BuiltinTypeSyntax builtinTypeSyntax -> builtinTypeSyntax.ToString()
        | FunctionTypeSyntax functionTypeSyntax -> functionTypeSyntax.ToString()
        
    static member AreEquivalent(ts1: TypeSyntax, ts2: TypeSyntax) =
        match ts1, ts2 with
        | NameSyntax ns1, NameSyntax ns2 -> NameSyntax.AreEquivalent(ns1, ns2)
        | BuiltinTypeSyntax bts1, BuiltinTypeSyntax bts2 -> BuiltinTypeSyntax.AreEquivalent(bts1, bts2)
        | FunctionTypeSyntax fts1, FunctionTypeSyntax fts2 -> FunctionTypeSyntax.AreEquivalent(fts1, fts2)
        | _ -> false
        
/// <remarks>
/// Example: <code>TypeSyntax</code>
/// </remarks>
and NameSyntax =
    { Identifier: SyntaxToken }
with
    override this.ToString() =
        this.Identifier.ToString()
        
    static member AreEquivalent(ns1: NameSyntax, ns2: NameSyntax) =
        SyntaxToken.AreEquivalent(ns1.Identifier, ns2.Identifier)
        
/// <remarks>
/// Example: <code>int</code>
/// </remarks>
and BuiltinTypeSyntax =
    { Identifier: SyntaxToken }
with
    override this.ToString() =
        this.Identifier.ToString()
        
    static member AreEquivalent(bts1: BuiltinTypeSyntax, bts2: BuiltinTypeSyntax) =
        SyntaxToken.AreEquivalent(bts1.Identifier, bts2.Identifier)
        
/// <remarks>
/// Example: <code>[int -> int]</code>
/// </remarks>
and FunctionTypeSyntax =
    { OpenBracketToken: SyntaxToken
      ParameterTypes: TypeSyntax array
      ArrowTokens: SyntaxToken array
      CloseBracketToken: SyntaxToken }
with
    override this.ToString() =
        let parameterTypesStrings = this.ParameterTypes |> Array.map _.ToString()
        let arrowTokensStrings = this.ArrowTokens |> Array.map _.ToString()
        let funcSigCoreStr = (interleave parameterTypesStrings arrowTokensStrings) |> String.concat ""
        $"{this.OpenBracketToken.ToString()}{funcSigCoreStr}{this.CloseBracketToken.ToString()}"
        
    static member AreEquivalent(fts1: FunctionTypeSyntax, fts2: FunctionTypeSyntax) =
        Array.zip fts1.ParameterTypes fts2.ParameterTypes
        |> Array.map TypeSyntax.AreEquivalent
        |> Array.forall id
    
    
/// <remarks>
/// Example: <code>&lt;&lt;fn&gt;&gt;(PARAM_TYPE_1 PARAM_1_NAME, PARAM_TYPE_2 PARAM_2_NAME, ..., PARAM_TYPE_N PARAM_N_NAME)</code>
/// </remarks>
type ParameterListSyntax =
    { OpenParenToken: SyntaxToken
      ParameterSyntax: ParameterSyntax array
      CommaTokens: SyntaxList array
      CloseParenToken: SyntaxToken }
with
    override this.ToString() =
        let parameterSyntaxStrings = this.ParameterSyntax |> Array.map _.ToString()
        let commaTokensStrings = this.CommaTokens |> Array.map _.ToString()
        let paramListCoreStr = (interleave parameterSyntaxStrings commaTokensStrings) |> String.concat ""
        $"{this.OpenParenToken.ToString()}{paramListCoreStr}{this.CloseParenToken.ToString()}"
        
    static member AreEquivalent(ns1: ParameterListSyntax, ns2: ParameterListSyntax) =
        Array.zip ns1.ParameterSyntax ns2.ParameterSyntax
        |> Array.map ParameterSyntax.AreEquivalent
        |> Array.forall id
        
/// <remarks>
/// Example: <code>PARAM_TYPE PARAM_NAME)</code>
/// </remarks>
and ParameterSyntax =
    { Type: TypeSyntax
      Identifier: SyntaxToken }
with
    override this.ToString() =
        $"{this.Type.ToString()}{this.Identifier.ToString()}"
        
    static member AreEquivalent(ps1: ParameterSyntax, ps2: ParameterSyntax) =
        TypeSyntax.AreEquivalent(ps1.Type, ps2.Type) && SyntaxToken.AreEquivalent(ps1.Identifier, ps2.Identifier)
    

type ArgumentListSyntax =
    { OpenParenToken: SyntaxToken
      ArgumentSyntax: ExpressionSyntax array
      CommaTokens: SyntaxList array
      CloseParenToken: SyntaxToken }
with
    override this.ToString() =
        let argumentSyntaxStrings = this.ArgumentSyntax |> Array.map _.ToString()
        let commaTokensStrings = this.CommaTokens |> Array.map _.ToString()
        let argListCoreStr = (interleave argumentSyntaxStrings commaTokensStrings) |> String.concat ""
        $"{this.OpenParenToken.ToString()}{argListCoreStr}{this.CloseParenToken.ToString()}"
        
    static member AreEquivalent(ns1: ArgumentListSyntax, ns2: ArgumentListSyntax) =
        Array.zip ns1.ArgumentSyntax ns2.ArgumentSyntax
        |> Array.map ExpressionSyntax.AreEquivalent
        |> Array.forall id
    
    
    
(* #REGION Expressions *)

type FunctionExpressionSyntax =
    { FunctionKeywordToken: SyntaxToken
      OpenParenToken: SyntaxToken
      ParameterList: ParameterListSyntax
      CloseParenToken: SyntaxToken
      ReturnType: TypeSyntax
      
      OpenBraceToken: SyntaxToken
      Body: BlockSyntax
      CloseBraceToken: SyntaxToken }
with
    override this.ToString() =
        $"{this.FunctionKeywordToken.ToString()}{this.OpenParenToken.ToString()}{this.ParameterList.ToString()}{this.CloseParenToken.ToString()}{this.ReturnType.ToString()}{this.OpenBraceToken.ToString()}{this.Body.ToString()}{this.CloseBraceToken.ToString()}"
        
    static member AreEquivalent(fe1: FunctionExpressionSyntax, fe2: FunctionExpressionSyntax) =
        ParameterListSyntax.AreEquivalent(fe1.ParameterList, fe2.ParameterList)
            && TypeSyntax.AreEquivalent(fe1.ReturnType, fe2.ReturnType)
            && BlockSyntax.AreEquivalent(fe1.Body, fe2.Body)
    
    
type BinaryExpressionSyntax =
    { Left: ExpressionSyntax
      OperatorToken: SyntaxToken
      Right: ExpressionSyntax }
with
    override this.ToString() =
        $"{this.Left.ToString()}{this.OperatorToken.ToString()}{this.Right.ToString()}"
        
    static member AreEquivalent(bs1: BinaryExpressionSyntax, bs2: BinaryExpressionSyntax) =
        ExpressionSyntax.AreEquivalent(bs1.Left, bs2.Left)
            && SyntaxToken.AreEquivalent(bs1.OperatorToken, bs2.OperatorToken)
            && ExpressionSyntax.AreEquivalent(bs1.Right, bs2.Right)
            
    
    
type InterpolatedStringExpressionSyntax =
    { InterpolatedStringStartToken: SyntaxToken
      Contents: InterpolatedStringContent array }
with
    override this.ToString() =
        $"{this.InterpolatedStringStartToken.ToString()}{this.Contents.ToString()}"
        
    static member AreEquivalent(bs1: InterpolatedStringExpressionSyntax, bs2: InterpolatedStringExpressionSyntax) =
        Array.zip bs1.Contents bs2.Contents
        |> Array.map InterpolatedStringContent.AreEquivalent
        |> Array.forall id
        
and InterpolatedStringContent =
    | InterpolatedStringText of InterpolatedStringText
    | Interpolation of Interpolation
with
    override this.ToString() =
        match this with
        | InterpolatedStringText interpolatedStringText -> interpolatedStringText.ToString()
        | Interpolation interpolation -> interpolation.ToString()
        
    static member AreEquivalent(bs1: InterpolatedStringContent, bs2: InterpolatedStringContent) =
        match bs1, bs2 with
        | InterpolatedStringText ist1, InterpolatedStringText ist2 -> InterpolatedStringText.AreEquivalent(ist1, ist2)
        | Interpolation i1, Interpolation i2 -> Interpolation.AreEquivalent(i1, i2)
        | _ -> false
        
and InterpolatedStringText =
    { TextToken: SyntaxToken }
with
    override this.ToString() =
        this.TextToken.ToString()
        
    static member AreEquivalent(ist1: InterpolatedStringText, ist2: InterpolatedStringText) =
        SyntaxToken.AreEquivalent(ist1.TextToken, ist2.TextToken)
        
and Interpolation =
    { OpenBraceToken: SyntaxToken
      Expression: ExpressionSyntax
      CloseBraceToken: SyntaxToken }
with
    override this.ToString() =
        $"{this.OpenBraceToken.ToString()}{this.Expression.ToString()}{this.CloseBraceToken.ToString()}"
        
    static member AreEquivalent(i1: Interpolation, i2: Interpolation) =
        ExpressionSyntax.AreEquivalent(i1.Expression, i2.Expression)
        
    
    
type InvocationExpressionSyntax =
    { LeftExpression: ExpressionSyntax  // typically a fn identifier or an inline function
      OpenParenToken: SyntaxToken
      Arguments: ArgumentListSyntax
      CloseParenToken: SyntaxToken }
with
    override this.ToString() =
        $"{this.LeftExpression.ToString()}{this.OpenParenToken.ToString()}{this.Arguments.ToString()}{this.CloseParenToken.ToString()}"
        
    static member AreEquivalent(ies1: InvocationExpressionSyntax, ies2: InvocationExpressionSyntax) =
        ExpressionSyntax.AreEquivalent(ies1.LeftExpression, ies2.LeftExpression) && ArgumentListSyntax.AreEquivalent(ies1.Arguments, ies2.Arguments)
    
    
type LiteralExpressionSyntax =
    { Kind: SyntaxKind
      Token: SyntaxToken }
with
    override this.ToString() =
        this.Token.ToString()
        
    static member AreEquivalent(les1: LiteralExpressionSyntax, les2: LiteralExpressionSyntax) =
        les1.Kind = les2.Kind && SyntaxToken.AreEquivalent(les1.Token, les2.Token)
    
    
type PrefixExpressionSyntax =
    { Kind: SyntaxKind
      OperatorToken: SyntaxToken
      Operand: ExpressionSyntax }
with
    override this.ToString() =
        $"{this.OperatorToken.ToString()}{this.Operand.ToString()}"
        
    static member AreEquivalent(pes1: PrefixExpressionSyntax, pes2: PrefixExpressionSyntax) =
        pes1.Kind = pes2.Kind
            && SyntaxToken.AreEquivalent(pes1.OperatorToken, pes2.OperatorToken)
            && ExpressionSyntax.AreEquivalent(pes1.Operand, pes2.Operand)
    
type PostfixExpressionSyntax =
    { Kind: SyntaxKind
      OperatorToken: SyntaxToken
      Operand: ExpressionSyntax }
with
    override this.ToString() =
        $"{this.Operand.ToString()}{this.OperatorToken.ToString()}"
        
    static member AreEquivalent(pes1: PostfixExpressionSyntax, pes2: PostfixExpressionSyntax) =
        pes1.Kind = pes2.Kind
            && SyntaxToken.AreEquivalent(pes1.OperatorToken, pes2.OperatorToken)
            && ExpressionSyntax.AreEquivalent(pes1.Operand, pes2.Operand)
    
type IdentifierNameSyntax =
    { Token: SyntaxToken }
with
    override this.ToString() =
        this.Token.ToString()
        
    static member AreEquivalent(ins1: IdentifierNameSyntax, ins2: IdentifierNameSyntax) =
        SyntaxToken.AreEquivalent(ins1.Token, ins2.Token)
    
type ParenthesizedExpressionSyntax =
    { OpenParenToken: SyntaxToken
      Expression: ExpressionSyntax
      CloseParenToken: SyntaxToken }
with
    override this.ToString() =
        $"{this.OpenParenToken.ToString()}{this.Expression.ToString()}{this.CloseParenToken.ToString()}"
        
    static member AreEquivalent(pes1: ParenthesizedExpressionSyntax, pes2: ParenthesizedExpressionSyntax) =
        ExpressionSyntax.AreEquivalent(pes1.Expression, pes2.Expression)
    

    
    



(* #REGION Statements *)

type BlockSyntax =
    { OpenBraceToken: SyntaxToken
      Statements: StatementSyntax array
      CloseBraceToken: SyntaxToken }
with
    override this.ToString() =
        let statementsStr = this.Statements |> Array.map _.ToString()
        $"{this.OpenBraceToken.ToString()}{statementsStr}{this.CloseBraceToken.ToString()}"
        
    static member AreEquivalent(bs1: BlockSyntax, bs2: BlockSyntax) =
        Array.zip bs1.Statements bs2.Statements
        |> Array.map StatementSyntax.AreEquivalent
        |> Array.forall id
    
type ExpressionStatementSyntax =
    { Expression: ExpressionSyntax
      SemicolonToken: SyntaxToken }
with
    override this.ToString() =
        $"{this.Expression.ToString()}{this.SemicolonToken.ToString()}"
        
    static member AreEquivalent(ess1: ExpressionStatementSyntax, ess2: ExpressionStatementSyntax) =
        ExpressionSyntax.AreEquivalent(ess1.Expression, ess2.Expression)
    
type IfStatementSyntax =
    { IfKeyword: SyntaxToken
      Condition: ExpressionSyntax
      Clause: BlockSyntax
      ElseIfClauses: ElseIfClauseSyntax array
      ElseClause: ElseClauseSyntax option }
with
    override this.ToString() =
        let elseIfClausesStr = this.ElseIfClauses |> Array.map _.ToString()
        let elseClauseStr = defaultArg (this.ElseClause |> Option.map _.ToString()) ""
        $"{this.IfKeyword.ToString()}{this.Condition.ToString()}{this.Clause.ToString()}{elseIfClausesStr}{elseClauseStr}"
        
    static member AreEquivalent(iss1: IfStatementSyntax, iss2: IfStatementSyntax) =
        let areElseIfClausesEquivalent =
            Array.zip iss1.ElseIfClauses iss2.ElseIfClauses
            |> Array.map ElseIfClauseSyntax.AreEquivalent
            |> Array.forall id
            
        let isElseClauseEquivalent =
            match iss1.ElseClause, iss2.ElseClause with
            | Some ecs1, Some ecs2 -> ElseClauseSyntax.AreEquivalent(ecs1, ecs2)
            | None, None -> true
            | _ -> false
        
        ExpressionSyntax.AreEquivalent(iss1.Condition, iss2.Condition)
            && BlockSyntax.AreEquivalent(iss1.Clause, iss2.Clause)
            && areElseIfClausesEquivalent
            && isElseClauseEquivalent
            
and ElseIfClauseSyntax =
    { ElseKeyword: SyntaxToken
      IfKeyword: SyntaxToken
      Condition: ExpressionSyntax
      Clause: BlockSyntax }
with
    override this.ToString() =
        $"{this.ElseKeyword.ToString()}{this.IfKeyword.ToString()}{this.Condition.ToString()}{this.Clause.ToString()}"
        
    static member AreEquivalent(eiss1: ElseIfClauseSyntax, eiss2: ElseIfClauseSyntax) =
        ExpressionSyntax.AreEquivalent(eiss1.Condition, eiss2.Condition)
            && BlockSyntax.AreEquivalent(eiss1.Clause, eiss2.Clause)
            
and ElseClauseSyntax =
    { ElseKeyword: SyntaxToken
      ElseClause: BlockSyntax }
with
    override this.ToString() =
        $"{this.ElseKeyword.ToString()}{this.ElseClause.ToString()}"
        
    static member AreEquivalent(ecs1: ElseClauseSyntax, ecs2: ElseClauseSyntax) =
        BlockSyntax.AreEquivalent(ecs1.ElseClause, ecs2.ElseClause)
    
    
type VariableDeclarationStatementSyntax =
    { LetKeyword: SyntaxToken
      Name: SyntaxToken
      EqualsToken: SyntaxToken
      TypeAnnotation: VariableTypeAnnotation option }
with
    override this.ToString() =
        let typeAnnotationStr = defaultArg (this.TypeAnnotation |> Option.map _.ToString()) ""
        $"{this.LetKeyword.ToString()}{this.Name.ToString()}{this.EqualsToken.ToString()}{typeAnnotationStr}"
        
    static member AreEquivalent(vdss1: VariableDeclarationStatementSyntax, vdss2: VariableDeclarationStatementSyntax) =
        let isTypeAnnotationEquivalent =
            match vdss1.TypeAnnotation, vdss2.TypeAnnotation with
            | Some ta1, Some ta2 -> VariableTypeAnnotation.AreEquivalent(ta1, ta2)
            | None, None -> true
            | _ -> false
            
        SyntaxToken.AreEquivalent(vdss1.Name, vdss2.Name) && isTypeAnnotationEquivalent
        
and VariableTypeAnnotation =
    { ColonToken: SyntaxToken
      Type: TypeSyntax }
with
    override this.ToString() =
        $"{this.ColonToken.ToString()}{this.Type.ToString()}"
        
    static member AreEquivalent(vta1: VariableTypeAnnotation, vta2: VariableTypeAnnotation) =
        TypeSyntax.AreEquivalent(vta1.Type, vta2.Type)
