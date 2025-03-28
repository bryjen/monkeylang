// Due to F# limitations, we can not declare types across several files for a better project structure.
namespace rec Monkey.AST

open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text



[<AutoOpen>]
module private Helpers =
    let interleave (xs: string[]) (ys: string[]) =
        let m = min xs.Length ys.Length
        let head = [| for i in 0 .. m - 1 do yield xs.[i]; yield ys.[i] |]
        let tail = if xs.Length > m then xs.[m..] else [||]
        Array.append head tail
    


/// <summary>
/// Represents the components of a Monkey (.mk) source file.
/// </summary>
type MonkeyCompilationUnit =
    { SyntaxNodes: MonkeySyntaxNode array }
with
    member this.WithSyntaxNodes(syntaxNodes: MonkeySyntaxNode array) =
        { this with SyntaxNodes = syntaxNodes }
        
    member this.WithUsings(usings: UsingDirectiveSyntax array) =
        let usingsAsSyntaxNodes = usings |> Array.map MonkeySyntaxNode.UsingDirectiveSyntax
        let namespaceDeclarationsAsSyntaxNodes = this.NamespaceDeclarations |> Array.map MonkeySyntaxNode.NamespaceDeclarationSyntax
        let currentStatementsAsSyntaxNodes = this.Statements |> Array.map MonkeySyntaxNode.StatementSyntax
        { this with SyntaxNodes = Array.concat [| usingsAsSyntaxNodes; namespaceDeclarationsAsSyntaxNodes; currentStatementsAsSyntaxNodes |] }
        
    member this.WithStatements(statements: StatementSyntax array) =
        let currentUsingsAsSyntaxNodes = this.UsingDirectives |> Array.map MonkeySyntaxNode.UsingDirectiveSyntax
        let namespaceDeclarationsAsSyntaxNodes = this.NamespaceDeclarations |> Array.map MonkeySyntaxNode.NamespaceDeclarationSyntax
        let statementsAsSyntaxNodes = statements |> Array.map MonkeySyntaxNode.StatementSyntax
        { this with SyntaxNodes = Array.concat [| currentUsingsAsSyntaxNodes; namespaceDeclarationsAsSyntaxNodes; statementsAsSyntaxNodes |] }
        
    member this.WithNamespaceDeclaration(namespaceDeclaration: NamespaceDeclarationSyntax) =
        let statementsAsSyntaxNodes = this.Statements |> Array.map MonkeySyntaxNode.StatementSyntax
        let currentNamespace = [| namespaceDeclaration |> MonkeySyntaxNode.NamespaceDeclarationSyntax |]
        let currentUsingsAsSyntaxNodes = this.UsingDirectives |> Array.map MonkeySyntaxNode.UsingDirectiveSyntax
        { this with SyntaxNodes = Array.concat [| currentUsingsAsSyntaxNodes; currentNamespace; statementsAsSyntaxNodes |] }
        
        
    member this.NamespaceDeclarations =
        let isNamespaceDeclaration monkeySyntaxNode =
            match monkeySyntaxNode with
            | NamespaceDeclarationSyntax namespaceDeclaration -> Some namespaceDeclaration
            | _ -> None
        
        this.SyntaxNodes |> Array.map isNamespaceDeclaration |> Array.choose id
        
    member this.UsingDirectives =
        let isUsingDirective monkeySyntaxNode =
            match monkeySyntaxNode with
            | UsingDirectiveSyntax usingDirective -> Some usingDirective
            | _ -> None
        
        this.SyntaxNodes |> Array.map isUsingDirective |> Array.choose id
        
    member this.Statements =
        let isStatement monkeySyntaxNode =
            match monkeySyntaxNode with
            | StatementSyntax statement -> Some statement
            | _ -> None
        
        this.SyntaxNodes |> Array.map isStatement |> Array.choose id



/// <summary>
/// Represents a token in the syntax tree.
/// </summary>
/// <remarks>
/// <ul>
///     <li>
///         We are unable to control Roslyn's 'TextSpan' properties which is very useful for displaying error messages.
///         Hence, why this type exists.
///     </li>
///     <li>
///         Inspired by Roslyn's interpretation of <a href="https://learn.microsoft.com/en-us/dotnet/api/microsoft.codeanalysis.syntaxtoken?view=roslyn-dotnet-4.13.0">syntax tokens</a>.
///         The original input text and the resulting tokens array are round-trippable from each other.
///     </li>
/// </ul>
/// </remarks>
type SyntaxToken =
    { Kind: SyntaxKind
      Text: string
      Value: obj
      
      TextSpan: TextSpan  // excludes trivia
      FullTextSpan: TextSpan  // includes trivia
      
      LeadingTrivia: SyntaxTriviaList
      TrailingTrivia: SyntaxTriviaList }
with
    override this.ToString() =
        $"{this.LeadingTrivia.ToFullString()}{this.Text}{this.TrailingTrivia.ToFullString()}"
   
    static member AreEquivalent(st1: SyntaxToken, st2: SyntaxToken) =
        (st1.Kind = st2.Kind) && (st1.Value = st2.Value) 



/// <summary>
/// Represents a non-terminal node in the syntax tree.
/// </summary>
type MonkeySyntaxNode =
    | UsingDirectiveSyntax of UsingDirectiveSyntax
    | NamespaceDeclarationSyntax of NamespaceDeclarationSyntax
    | ArgumentListSyntax of ArgumentListSyntax
    | ParameterListSyntax of ParameterListSyntax
    | ExpressionSyntax of ExpressionSyntax
    | StatementSyntax of StatementSyntax
with
    override this.ToString() =
        match this with
        | UsingDirectiveSyntax uds -> uds.ToString()
        | NamespaceDeclarationSyntax nss -> nss.ToString()
        | ArgumentListSyntax als -> als.ToString()
        | ParameterListSyntax pls -> pls.ToString()
        | ExpressionSyntax es -> es.ToString()
        | StatementSyntax ss -> ss.ToString()
        
    static member AreEquivalent(msn1: MonkeySyntaxNode, msn2: MonkeySyntaxNode) =
        match msn1, msn2 with
        | UsingDirectiveSyntax uds1, UsingDirectiveSyntax uds2 ->
            UsingDirectiveSyntax.AreEquivalent(uds1, uds2)
        | NamespaceDeclarationSyntax nds1, NamespaceDeclarationSyntax nds2 ->
            NamespaceDeclarationSyntax.AreEquivalent(nds1, nds2)
        | ArgumentListSyntax als1, ArgumentListSyntax als2 ->
            ArgumentListSyntax.AreEquivalent(als1, als2)
        | ParameterListSyntax pls1, ParameterListSyntax pls2 ->
            ParameterListSyntax.AreEquivalent(pls1, pls2)
        | ExpressionSyntax es1, ExpressionSyntax es2 ->
            ExpressionSyntax.AreEquivalent(es1, es2)
        | StatementSyntax ss1, StatementSyntax ss2 ->
            StatementSyntax.AreEquivalent(ss1, ss2)
        | _ -> false
        
        
        
type UsingDirectiveSyntax =
    { UsingToken: SyntaxToken
      Name: IdentifierSyntax
      SemicolonToken: SyntaxToken }
with
    override this.ToString() =
        $"{this.UsingToken.ToString()}{this.Name.ToString()}{this.SemicolonToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        let semicolonTextSpan = this.SemicolonToken.TextSpan
        TextSpan(this.UsingToken.TextSpan.Start, semicolonTextSpan.End - this.UsingToken.TextSpan.Start)
        
    static member AreEquivalent(uds1: UsingDirectiveSyntax, uds2: UsingDirectiveSyntax) =
        IdentifierSyntax.AreEquivalent(uds1.Name, uds2.Name)
    
    
type NamespaceDeclarationSyntax =
    { NamespaceToken: SyntaxToken
      Name: IdentifierSyntax
      SemicolonToken: SyntaxToken }
with
    override this.ToString() =
        $"{this.NamespaceToken.ToString()}{this.Name.ToString()}{this.SemicolonToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        let semicolonTextSpan = this.SemicolonToken.TextSpan
        TextSpan(this.NamespaceToken.TextSpan.Start, semicolonTextSpan.End - this.NamespaceToken.TextSpan.Start)
        
    static member AreEquivalent(nds1: NamespaceDeclarationSyntax, nds2: NamespaceDeclarationSyntax) =
        IdentifierSyntax.AreEquivalent(nds1.Name, nds2.Name)



/// <summary>
/// The parent type for all expression syntax types.
/// </summary>
type ExpressionSyntax =
    | ParenthesizedExpressionSyntax of ParenthesizedExpressionSyntax
    | FunctionExpressionSyntax of FunctionExpressionSyntax
    | BinaryExpressionSyntax of BinaryExpressionSyntax
    | InterpolatedStringExpressionSyntax of InterpolatedStringExpressionSyntax
    | InvocationExpressionSyntax of InvocationExpressionSyntax
    | LiteralExpressionSyntax of LiteralExpressionSyntax
    | PostfixExpressionSyntax of PostfixExpressionSyntax
    | PrefixExpressionSyntax of PrefixExpressionSyntax
    | IdentifierSyntax of IdentifierSyntax
    | TypeSyntax of TypeSyntax
    | IfExpressionSyntax of IfExpressionSyntax
    | ArrayExpressionSyntax of ArrayExpressionSyntax
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
        | IdentifierSyntax identifierSyntax -> identifierSyntax.ToString()
        | TypeSyntax typeSyntax -> typeSyntax.ToString()
        | IfExpressionSyntax ifExpressionSyntax -> ifExpressionSyntax.ToString()
        | ArrayExpressionSyntax arrayExpressionSyntax -> arrayExpressionSyntax.ToString()
        
    member this.TextSpan() =
        match this with
        | ParenthesizedExpressionSyntax parenthesizedExpressionSyntax -> parenthesizedExpressionSyntax.TextSpan()
        | FunctionExpressionSyntax functionExpressionSyntax -> functionExpressionSyntax.TextSpan()
        | BinaryExpressionSyntax binaryExpressionSyntax -> binaryExpressionSyntax.TextSpan()
        | InterpolatedStringExpressionSyntax interpolatedStringExpressionSyntax -> interpolatedStringExpressionSyntax.TextSpan()
        | InvocationExpressionSyntax invocationExpressionSyntax -> invocationExpressionSyntax.TextSpan()
        | LiteralExpressionSyntax literalExpressionSyntax -> literalExpressionSyntax.TextSpan()
        | PostfixExpressionSyntax postfixExpressionSyntax -> postfixExpressionSyntax.TextSpan()
        | PrefixExpressionSyntax prefixExpressionSyntax -> prefixExpressionSyntax.TextSpan()
        | IdentifierSyntax identifierNameSyntax -> identifierNameSyntax.TextSpan()
        | TypeSyntax typeSyntax -> typeSyntax.TextSpan()
        | IfExpressionSyntax ifExpressionSyntax -> ifExpressionSyntax.TextSpan()
        | ArrayExpressionSyntax arrayExpressionSyntax -> arrayExpressionSyntax.TextSpan()
        
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
        | IdentifierSyntax ins1, IdentifierSyntax ins2 ->
            IdentifierSyntax.AreEquivalent(ins1, ins2)
        | TypeSyntax ts1, TypeSyntax ts2 ->
            TypeSyntax.AreEquivalent(ts1, ts2)
        | IfExpressionSyntax ifs1, IfExpressionSyntax ifs2 ->
            IfExpressionSyntax.AreEquivalent(ifs1, ifs2)
        | ArrayExpressionSyntax aes1, ArrayExpressionSyntax aes2 ->
            ArrayExpressionSyntax.AreEquivalent(aes1, aes2)
        | _ -> false
    
    
    
/// <summary>
/// The parent type for all statement syntax types.
/// </summary>
type StatementSyntax =
    | BlockSyntax of BlockSyntax
    | ExpressionStatementSyntax of ExpressionStatementSyntax
    | VariableDeclarationStatementSyntax of VariableDeclarationStatementSyntax
with
    override this.ToString() =
        match this with
        | BlockSyntax blockSyntax -> blockSyntax.ToString()
        | ExpressionStatementSyntax expressionStatementSyntax -> expressionStatementSyntax.ToString()
        | VariableDeclarationStatementSyntax variableDeclarationStatementSyntax -> variableDeclarationStatementSyntax.ToString()
        
    member this.TextSpan () : TextSpan =
        match this with
        | BlockSyntax blockSyntax -> blockSyntax.TextSpan()
        | ExpressionStatementSyntax expressionStatementSyntax -> expressionStatementSyntax.TextSpan()
        | VariableDeclarationStatementSyntax variableDeclarationStatementSyntax -> variableDeclarationStatementSyntax.TextSpan()
        
    static member AreEquivalent(ss1: StatementSyntax, ss2: StatementSyntax) =
        match ss1, ss2 with
        | BlockSyntax bs1, BlockSyntax bs2 ->
            BlockSyntax.AreEquivalent(bs1, bs2)
        | ExpressionStatementSyntax ess1, ExpressionStatementSyntax ess2 ->
            ExpressionStatementSyntax.AreEquivalent(ess1, ess2)
        | VariableDeclarationStatementSyntax vdss1, VariableDeclarationStatementSyntax vdss2 ->
            VariableDeclarationStatementSyntax.AreEquivalent(vdss1, vdss2)
        | _ -> false
    
    
    
(* #REGION Supporting Types *)

type TypeSyntax =
    | NameSyntax of NameSyntax
    | BuiltinTypeSyntax of BuiltinTypeSyntax
    | FunctionTypeSyntax of FunctionTypeSyntax
    | ArrayTypeSyntax of ArrayTypeSyntax
    | GenericTypeSyntax of GenericTypeSyntax
with
    override this.ToString() =
        match this with
        | NameSyntax nameSyntax -> nameSyntax.ToString()
        | BuiltinTypeSyntax builtinTypeSyntax -> builtinTypeSyntax.ToString()
        | FunctionTypeSyntax functionTypeSyntax -> functionTypeSyntax.ToString()
        | ArrayTypeSyntax arrayTypeSyntax -> arrayTypeSyntax.ToString()
        | GenericTypeSyntax genericTypeSyntax -> genericTypeSyntax.ToString()
        
    member this.TextSpan () : TextSpan =
        match this with
        | NameSyntax nameSyntax -> nameSyntax.TextSpan()
        | BuiltinTypeSyntax builtinTypeSyntax -> builtinTypeSyntax.TextSpan()
        | FunctionTypeSyntax functionTypeSyntax -> functionTypeSyntax.TextSpan()
        | ArrayTypeSyntax arrayTypeSyntax -> arrayTypeSyntax.TextSpan()
        | GenericTypeSyntax genericTypeSyntax -> genericTypeSyntax.TextSpan()
        
    static member AreEquivalent(ts1: TypeSyntax, ts2: TypeSyntax) =
        match ts1, ts2 with
        | NameSyntax ns1, NameSyntax ns2 -> NameSyntax.AreEquivalent(ns1, ns2)
        | BuiltinTypeSyntax bts1, BuiltinTypeSyntax bts2 -> BuiltinTypeSyntax.AreEquivalent(bts1, bts2)
        | FunctionTypeSyntax fts1, FunctionTypeSyntax fts2 -> FunctionTypeSyntax.AreEquivalent(fts1, fts2)
        | ArrayTypeSyntax ats1, ArrayTypeSyntax ats2 -> ArrayTypeSyntax.AreEquivalent(ats1, ats2)
        | GenericTypeSyntax gts1, GenericTypeSyntax gts2 -> GenericTypeSyntax.AreEquivalent(gts1, gts2)
        | _ -> false
        
/// <remarks>
/// Example: <code>TypeSyntax</code>
/// </remarks>
and NameSyntax =
    { Identifier: IdentifierSyntax }
with
    override this.ToString() =
        this.Identifier.ToString()
        
    member this.TextSpan () : TextSpan =
        this.Identifier.TextSpan()
        
    static member AreEquivalent(ns1: NameSyntax, ns2: NameSyntax) =
        IdentifierSyntax.AreEquivalent(ns1.Identifier, ns2.Identifier)
        
/// <remarks>
/// Example: <code>int</code>
/// </remarks>
and BuiltinTypeSyntax =
    { Token: SyntaxToken }
with
    override this.ToString() =
        this.Token.ToString()
        
    member this.TextSpan () : TextSpan =
        this.Token.TextSpan
        
    static member AreEquivalent(bts1: BuiltinTypeSyntax, bts2: BuiltinTypeSyntax) =
        SyntaxToken.AreEquivalent(bts1.Token, bts2.Token)
        
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
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.OpenBracketToken.TextSpan.Start, this.CloseBracketToken.TextSpan.End - this.OpenBracketToken.TextSpan.Start)
        
    static member AreEquivalent(fts1: FunctionTypeSyntax, fts2: FunctionTypeSyntax) =
        Array.zip fts1.ParameterTypes fts2.ParameterTypes
        |> Array.map TypeSyntax.AreEquivalent
        |> Array.forall id
        
/// <remarks>
/// Example: <code>[int -> int]</code>
/// </remarks>
and ArrayTypeSyntax =
    { Type: TypeSyntax
      OpenBracketToken: SyntaxToken
      CloseBracketToken: SyntaxToken }
with
    override this.ToString() =
        $"{this.Type.ToString()}{this.OpenBracketToken.ToString()}{this.CloseBracketToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        let typeTextSpan = this.Type.TextSpan()
        TextSpan(typeTextSpan.Start, this.CloseBracketToken.TextSpan.End - typeTextSpan.Start)
        
    static member AreEquivalent(ats1: ArrayTypeSyntax, ats2: ArrayTypeSyntax) =
        TypeSyntax.AreEquivalent(ats1.Type, ats2.Type)
        
/// <remarks>
/// Example: <code>[int -> int]</code>
/// </remarks>
and GenericTypeSyntax =
    { Type: TypeSyntax
      LessThanToken: SyntaxToken
      GenericTypes: TypeSyntax array
      Commas: SyntaxToken array
      GreaterThanToken: SyntaxToken }
with
    override this.ToString() =
        let genericTypesString = this.GenericTypes |> Array.map _.ToString()
        let commasString = this.Commas |> Array.map _.ToString()
        let genericTypeCoreStr = (interleave genericTypesString commasString) |> String.concat ""
        $"{this.Type.ToString()}{this.LessThanToken.ToString()}{genericTypeCoreStr}{this.GreaterThanToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        let typeTextSpan = this.Type.TextSpan()
        TextSpan(typeTextSpan.Start, this.GreaterThanToken.TextSpan.End - typeTextSpan.Start)
        
    static member AreEquivalent(gts1: GenericTypeSyntax, gts2: GenericTypeSyntax) =
        Array.zip gts1.GenericTypes gts2.GenericTypes
        |> Array.map TypeSyntax.AreEquivalent
        |> Array.forall id
    
    
/// <remarks>
/// Example: <code>&lt;&lt;fn&gt;&gt;(PARAM_TYPE_1 PARAM_1_NAME, PARAM_TYPE_2 PARAM_2_NAME, ..., PARAM_TYPE_N PARAM_N_NAME)</code>
/// </remarks>
type ParameterListSyntax =
    { OpenParenToken: SyntaxToken
      Parameters: ParameterSyntax array
      Commas: SyntaxToken array
      CloseParenToken: SyntaxToken }
with
    override this.ToString() =
        let parameterSyntaxStrings = this.Parameters |> Array.map _.ToString()
        let commaTokensStrings = this.Commas |> Array.map _.ToString()
        let paramListCoreStr = (interleave parameterSyntaxStrings commaTokensStrings) |> String.concat ""
        $"{this.OpenParenToken.ToString()}{paramListCoreStr}{this.CloseParenToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.OpenParenToken.TextSpan.Start, this.CloseParenToken.TextSpan.End - this.OpenParenToken.TextSpan.Start)
        
    static member AreEquivalent(ns1: ParameterListSyntax, ns2: ParameterListSyntax) =
        Array.zip ns1.Parameters ns2.Parameters
        |> Array.map ParameterSyntax.AreEquivalent
        |> Array.forall id
        
/// <remarks>
/// Example: <code>PARAM_TYPE PARAM_NAME)</code>
/// </remarks>
and ParameterSyntax =
    { Type: TypeSyntax
      Identifier: SimpleIdentifier }
with
    override this.ToString() =
        $"{this.Type.ToString()}{this.Identifier.ToString()}"
        
    member this.TextSpan () : TextSpan =
        let typeTextSpan = this.Type.TextSpan()
        TextSpan(typeTextSpan.Start, this.Identifier.TextSpan().End - typeTextSpan.Start)
        
    static member AreEquivalent(ps1: ParameterSyntax, ps2: ParameterSyntax) =
        TypeSyntax.AreEquivalent(ps1.Type, ps2.Type) && SimpleIdentifier.AreEquivalent(ps1.Identifier, ps2.Identifier)
    

type ArgumentListSyntax =
    { OpenParenToken: SyntaxToken
      Arguments: ExpressionSyntax array
      Commas: SyntaxToken array
      CloseParenToken: SyntaxToken }
with
    override this.ToString() =
        let argumentSyntaxStrings = this.Arguments |> Array.map _.ToString()
        let commaTokensStrings = this.Commas |> Array.map _.ToString()
        let argListCoreStr = (interleave argumentSyntaxStrings commaTokensStrings) |> String.concat ""
        $"{this.OpenParenToken.ToString()}{argListCoreStr}{this.CloseParenToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.OpenParenToken.TextSpan.Start, this.CloseParenToken.TextSpan.End - this.OpenParenToken.TextSpan.Start)
        
    static member AreEquivalent(ns1: ArgumentListSyntax, ns2: ArgumentListSyntax) =
        Array.zip ns1.Arguments ns2.Arguments
        |> Array.map ExpressionSyntax.AreEquivalent
        |> Array.forall id
    
    
    
(* #REGION Expressions *)

type ArrayExpressionSyntax =
    | ListInitialization of ListInitialization
    | SizeBasedInitialization of SizeBasedInitialization
with
    override this.ToString() =
        match this with
        | ListInitialization listInitialization -> listInitialization.ToString()
        | SizeBasedInitialization sizeBasedInitialization -> sizeBasedInitialization.ToString()
        
    member this.TextSpan () : TextSpan =
        match this with
        | ListInitialization listInitialization -> listInitialization.TextSpan()
        | SizeBasedInitialization sizeBasedInitialization -> sizeBasedInitialization.TextSpan()
        
    static member AreEquivalent(aes1: ArrayExpressionSyntax, aes2: ArrayExpressionSyntax) =
        match aes1, aes2 with
        | ListInitialization li1, ListInitialization li2 -> ListInitialization.AreEquivalent(li1, li2)
        | SizeBasedInitialization sbi1, SizeBasedInitialization sbi2 -> SizeBasedInitialization.AreEquivalent(sbi1, sbi2)
        | _ -> false
        
/// <remarks>
/// Example:
/// <code>[EXPRESSION_1, EXPRESSION_2, ..., EXPRESSION_N]</code>
/// </remarks>
and ListInitialization =
    { OpenBracketToken: SyntaxToken
      Values: ExpressionSyntax array
      Commas: SyntaxToken array
      CloseBracketToken: SyntaxToken }
with
    override this.ToString() =
        let valuesStr = this.Values |> Array.map _.ToString() 
        let commasStr = this.Commas |> Array.map _.ToString()
        let contentsStr = (interleave valuesStr commasStr) |> Array.fold (fun acc str -> acc + str) ""
        $"{this.OpenBracketToken.ToString()}{contentsStr}{this.CloseBracketToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.OpenBracketToken.TextSpan.Start, this.CloseBracketToken.TextSpan.End - this.OpenBracketToken.TextSpan.Start)
        
    static member AreEquivalent(li1: ListInitialization, li2: ListInitialization) =
        (Array.zip li1.Values li2.Values) |> Array.map ExpressionSyntax.AreEquivalent |> Array.forall id
/// <remarks>
/// Example:
/// <code>new int[]</code>
/// </remarks>
and SizeBasedInitialization =
    { NewToken: SyntaxToken
      TypeToken: SyntaxToken
      OpenBracketToken: SyntaxToken
      Size: ExpressionSyntax
      CloseBracketToken: SyntaxToken }
with
    override this.ToString() =
        $"{this.NewToken.ToString()}{this.TypeToken.ToString()}{this.OpenBracketToken.ToString()}{this.CloseBracketToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.NewToken.TextSpan.Start, this.CloseBracketToken.TextSpan.End - this.NewToken.TextSpan.Start)
        
    static member AreEquivalent(sbi1: SizeBasedInitialization, sbi2: SizeBasedInitialization) =
        SyntaxToken.AreEquivalent(sbi1.TypeToken, sbi2.TypeToken)
        && ExpressionSyntax.AreEquivalent(sbi1.Size, sbi2.Size)
    

type FunctionExpressionSyntax =
    { FunctionKeywordToken: SyntaxToken
      ParameterList: ParameterListSyntax
      ColonToken: SyntaxToken
      ReturnType: TypeSyntax
      Body: BlockSyntax }
with
    override this.ToString() =
        $"{this.FunctionKeywordToken.ToString()}{this.ParameterList.ToString()}{this.ColonToken.ToString()}{this.ReturnType.ToString()}{this.Body.ToString()}"
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.FunctionKeywordToken.TextSpan.Start, this.Body.CloseBraceToken.TextSpan.End - this.FunctionKeywordToken.TextSpan.Start)
        
    static member AreEquivalent(fe1: FunctionExpressionSyntax, fe2: FunctionExpressionSyntax) =
        ParameterListSyntax.AreEquivalent(fe1.ParameterList, fe2.ParameterList)
            && TypeSyntax.AreEquivalent(fe1.ReturnType, fe2.ReturnType)
            && BlockSyntax.AreEquivalent(fe1.Body, fe2.Body)
    
    
type BinaryExpressionSyntax =
    { Kind: SyntaxKind
      Left: ExpressionSyntax
      OperatorToken: SyntaxToken
      Right: ExpressionSyntax }
with
    override this.ToString() =
        $"{this.Left.ToString()}{this.OperatorToken.ToString()}{this.Right.ToString()}"
        
    member this.TextSpan () : TextSpan =
        let leftTextSpan = this.Left.TextSpan()
        let rightTextSpan = this.Right.TextSpan()
        TextSpan(leftTextSpan.Start, rightTextSpan.End - leftTextSpan.Start)
        
    static member AreEquivalent(bs1: BinaryExpressionSyntax, bs2: BinaryExpressionSyntax) =
        ExpressionSyntax.AreEquivalent(bs1.Left, bs2.Left)
            && SyntaxToken.AreEquivalent(bs1.OperatorToken, bs2.OperatorToken)
            && ExpressionSyntax.AreEquivalent(bs1.Right, bs2.Right)
            && bs1.Kind = bs2.Kind
            
    
    
type InterpolatedStringExpressionSyntax =
    { InterpolatedStringStartToken: SyntaxToken
      Contents: InterpolatedStringContent array }
with
    override this.ToString() =
        $"{this.InterpolatedStringStartToken.ToString()}{this.Contents.ToString()}"
        
    member this.TextSpan () : TextSpan =
        let contentsEnd =
            match this.Contents with
            | [|  |] -> this.InterpolatedStringStartToken.TextSpan.End
            | arr -> arr[arr.Length - 1].TextSpan().End
        TextSpan(this.InterpolatedStringStartToken.TextSpan.Start, contentsEnd - this.InterpolatedStringStartToken.TextSpan.Start)
        
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
        
    member this.TextSpan () : TextSpan =
        match this with
        | InterpolatedStringText interpolatedStringText -> interpolatedStringText.TextSpan()
        | Interpolation interpolation -> interpolation.TextSpan()
        
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
        
    member this.TextSpan () : TextSpan =
        this.TextToken.TextSpan
        
    static member AreEquivalent(ist1: InterpolatedStringText, ist2: InterpolatedStringText) =
        SyntaxToken.AreEquivalent(ist1.TextToken, ist2.TextToken)
        
and Interpolation =
    { OpenBraceToken: SyntaxToken
      Expression: ExpressionSyntax
      CloseBraceToken: SyntaxToken }
with
    override this.ToString() =
        $"{this.OpenBraceToken.ToString()}{this.Expression.ToString()}{this.CloseBraceToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.OpenBraceToken.TextSpan.Start, this.CloseBraceToken.TextSpan.End - this.OpenBraceToken.TextSpan.Start)
        
    static member AreEquivalent(i1: Interpolation, i2: Interpolation) =
        ExpressionSyntax.AreEquivalent(i1.Expression, i2.Expression)
        
    
    
type InvocationExpressionSyntax =
    { Expression: InvocationExpressionLeftExpression
      Arguments: ArgumentListSyntax }
with
    override this.ToString() =
        $"{this.Expression.ToString()}{this.Arguments.ToString()}"
        
    member this.TextSpan () : TextSpan =
        let startTextSpan = this.Expression.TextSpan()
        TextSpan(startTextSpan.Start, this.Arguments.TextSpan().End - startTextSpan.Start)
        
    static member AreEquivalent(ies1: InvocationExpressionSyntax, ies2: InvocationExpressionSyntax) =
        InvocationExpressionLeftExpression.AreEquivalent(ies1.Expression, ies2.Expression) && ArgumentListSyntax.AreEquivalent(ies1.Arguments, ies2.Arguments)
        
and InvocationExpressionLeftExpression =
    | ParenthesizedFunctionExpressionSyntax of InvocationParenthesizedExpressionSyntax
    | FunctionExpressionSyntax of FunctionExpressionSyntax
    | IdentifierSyntax of IdentifierSyntax
with 
    override this.ToString() =
        match this with
        | FunctionExpressionSyntax functionExpressionSyntax -> functionExpressionSyntax.ToString()
        | IdentifierSyntax identifierNameSyntax -> identifierNameSyntax.ToString()
        | ParenthesizedFunctionExpressionSyntax parenthesizedFunctionExpressionSyntax -> parenthesizedFunctionExpressionSyntax.ToString()
        
    member this.TextSpan () : TextSpan =
        match this with
        | ParenthesizedFunctionExpressionSyntax invocationParenthesizedExpressionSyntax -> invocationParenthesizedExpressionSyntax.TextSpan()
        | FunctionExpressionSyntax functionExpressionSyntax -> functionExpressionSyntax.TextSpan()
        | IdentifierSyntax identifierNameSyntax -> identifierNameSyntax.TextSpan()
        
    static member AreEquivalent(ies1: InvocationExpressionLeftExpression, ies2: InvocationExpressionLeftExpression) =
        match ies1, ies2 with
        | FunctionExpressionSyntax fes1, FunctionExpressionSyntax fes2 -> FunctionExpressionSyntax.AreEquivalent(fes1, fes2)
        | IdentifierSyntax ins1, IdentifierSyntax ins2 -> IdentifierSyntax.AreEquivalent(ins1, ins2)
        | ParenthesizedFunctionExpressionSyntax pfes1, ParenthesizedFunctionExpressionSyntax pfes2 -> InvocationParenthesizedExpressionSyntax.AreEquivalent(pfes1, pfes2)
        | _ -> false
        
    static member FromParenthesizedExpression(parenthesizedExpression: ParenthesizedExpressionSyntax) =
        match parenthesizedExpression.Expression with
        | ExpressionSyntax.FunctionExpressionSyntax functionExpressionSyntax ->
            ({ OpenParenToken = parenthesizedExpression.OpenParenToken
               Expression = InvocationExpressionLeftExpression.FunctionExpressionSyntax functionExpressionSyntax
               CloseParenToken = parenthesizedExpression.CloseParenToken }: InvocationParenthesizedExpressionSyntax)
            |> Some
        | ExpressionSyntax.IdentifierSyntax identifierNameSyntax ->
            ({ OpenParenToken = parenthesizedExpression.OpenParenToken
               Expression = InvocationExpressionLeftExpression.IdentifierSyntax identifierNameSyntax
               CloseParenToken = parenthesizedExpression.CloseParenToken }: InvocationParenthesizedExpressionSyntax)
            |> Some
        | ParenthesizedExpressionSyntax parenthesizedExpressionSyntax ->
            match InvocationExpressionLeftExpression.FromParenthesizedExpression(parenthesizedExpressionSyntax) with
            | Some invocationExpressionExpression ->
                ({ OpenParenToken = parenthesizedExpression.OpenParenToken
                   Expression = InvocationExpressionLeftExpression.ParenthesizedFunctionExpressionSyntax invocationExpressionExpression
                   CloseParenToken = parenthesizedExpression.CloseParenToken }: InvocationParenthesizedExpressionSyntax)
                |> Some
            | None ->
                None
        | _ -> None
            
and InvocationParenthesizedExpressionSyntax =
    { OpenParenToken: SyntaxToken
      Expression: InvocationExpressionLeftExpression
      CloseParenToken: SyntaxToken }
with
    override this.ToString() =
        $"{this.OpenParenToken.ToString()}{this.Expression.ToString()}{this.CloseParenToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.OpenParenToken.TextSpan.Start, this.CloseParenToken.TextSpan.End - this.OpenParenToken.TextSpan.Start)
        
    static member AreEquivalent(pes1: InvocationParenthesizedExpressionSyntax, pes2: InvocationParenthesizedExpressionSyntax) =
        InvocationExpressionLeftExpression.AreEquivalent(pes1.Expression, pes2.Expression)
        
    
    
type LiteralExpressionSyntax =
    { Kind: SyntaxKind
      Token: SyntaxToken }
with
    override this.ToString() =
        this.Token.ToString()
        
    member this.TextSpan () : TextSpan =
        this.Token.TextSpan
        
    static member AreEquivalent(les1: LiteralExpressionSyntax, les2: LiteralExpressionSyntax) =
        les1.Kind = les2.Kind && SyntaxToken.AreEquivalent(les1.Token, les2.Token)
    
    
type PrefixExpressionSyntax =
    { Kind: SyntaxKind
      OperatorToken: SyntaxToken
      Operand: ExpressionSyntax }
with
    override this.ToString() =
        $"{this.OperatorToken.ToString()}{this.Operand.ToString()}"
        
    member this.TextSpan () : TextSpan =
        let operandTextSpan = this.Operand.TextSpan()
        TextSpan(this.OperatorToken.TextSpan.Start, operandTextSpan.End - this.OperatorToken.TextSpan.Start)
        
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
        
    member this.TextSpan () : TextSpan =
        let operandTextSpan = this.Operand.TextSpan()
        TextSpan(operandTextSpan.Start, this.OperatorToken.TextSpan.End - operandTextSpan.Start)
        
    static member AreEquivalent(pes1: PostfixExpressionSyntax, pes2: PostfixExpressionSyntax) =
        pes1.Kind = pes2.Kind
            && SyntaxToken.AreEquivalent(pes1.OperatorToken, pes2.OperatorToken)
            && ExpressionSyntax.AreEquivalent(pes1.Operand, pes2.Operand)
    

and IdentifierSyntax =
    | SimpleIdentifier of SimpleIdentifier
    | QualifiedIdentifier of QualifiedIdentifier
with
    override this.ToString() =
        match this with
        | SimpleIdentifier simpleIdentifier -> simpleIdentifier.ToString()
        | QualifiedIdentifier qualifiedIdentifier -> qualifiedIdentifier.ToString()
        
    member this.TextSpan () : TextSpan =
        match this with
        | SimpleIdentifier simpleIdentifier -> simpleIdentifier.TextSpan()
        | QualifiedIdentifier qualifiedIdentifier -> qualifiedIdentifier.TextSpan()
        
    static member AreEquivalent(it1: IdentifierSyntax, it2: IdentifierSyntax) =
        match it1, it2 with
        | SimpleIdentifier si1, SimpleIdentifier si2 -> SimpleIdentifier.AreEquivalent(si1, si2)
        | QualifiedIdentifier qi1, QualifiedIdentifier qi2 -> QualifiedIdentifier.AreEquivalent(qi1, qi2)
        | _ -> false
            
and SimpleIdentifier =
    { Token: SyntaxToken }
with
    override this.ToString() =
        this.Token.ToString()
        
    member this.TextSpan () : TextSpan =
        this.Token.TextSpan
        
    static member AreEquivalent(si1: SimpleIdentifier, si2: SimpleIdentifier) =
        SyntaxToken.AreEquivalent(si1.Token, si2.Token)
            
and QualifiedIdentifier =
    { Tokens: SyntaxToken array
      Dots: SyntaxToken array }
with
    override this.ToString() =
        let tokenStrings = this.Tokens |> Array.map _.ToString()
        let periodStrings = this.Dots |> Array.map _.ToString()
        let stringsArr = interleave tokenStrings periodStrings
        System.String.Join("", stringsArr)
        
    member this.TextSpan () : TextSpan =
        match this.Tokens with
        | [| |] -> TextSpan(0, 0)
        | arr ->
            TextSpan(arr[0].TextSpan.Start, arr[arr.Length - 1].TextSpan.End - arr[0].TextSpan.Start)
            
            (*
            if arr[arr.Length - 1].TextSpan.End - arr[0].TextSpan.Start < 0 then
                TextSpan(arr[0].TextSpan.Start, 0)
            else
                TextSpan(arr[0].TextSpan.Start, arr[arr.Length - 1].TextSpan.End - arr[0].TextSpan.Start)
            *)
        
    static member AreEquivalent(qi1: QualifiedIdentifier, qi2: QualifiedIdentifier) =
        Array.zip qi1.Tokens qi2.Tokens
        |> Array.map SyntaxToken.AreEquivalent
        |> Array.forall id
    

    
type ParenthesizedExpressionSyntax =
    { OpenParenToken: SyntaxToken
      Expression: ExpressionSyntax
      CloseParenToken: SyntaxToken }
with
    override this.ToString() =
        $"{this.OpenParenToken.ToString()}{this.Expression.ToString()}{this.CloseParenToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.OpenParenToken.TextSpan.Start, this.CloseParenToken.TextSpan.End - this.OpenParenToken.TextSpan.Start)
        
    static member AreEquivalent(pes1: ParenthesizedExpressionSyntax, pes2: ParenthesizedExpressionSyntax) =
        ExpressionSyntax.AreEquivalent(pes1.Expression, pes2.Expression)
    
type IfExpressionSyntax =
    { IfKeyword: SyntaxToken
      OpenParenToken: SyntaxToken
      Condition: ExpressionSyntax
      CloseParenToken: SyntaxToken
      Clause: BlockSyntax
      ElseIfClauses: ElseIfClauseSyntax array
      ElseClause: ElseClauseSyntax option }
with
    override this.ToString() =
        let elseIfClausesStr = this.ElseIfClauses |> Array.map _.ToString() |> Array.fold (fun acc str -> acc + str) ""
        let elseClauseStr = defaultArg (this.ElseClause |> Option.map _.ToString()) ""
        $"{this.IfKeyword.ToString()}{this.OpenParenToken.ToString()}{this.Condition.ToString()}{this.CloseParenToken.ToString()}{this.Clause.ToString()}{elseIfClausesStr}{elseClauseStr}"
        
    member this.TextSpan () : TextSpan =
        let spanEnd =
            match this.ElseClause with
            | Some elseClause -> elseClause.TextSpan().End
            | None ->
                match this.ElseIfClauses with
                | [|  |] -> this.Clause.CloseBraceToken.TextSpan.End
                | arr -> arr[arr.Length - 1].TextSpan().End
        TextSpan(this.IfKeyword.TextSpan.Start, spanEnd - this.IfKeyword.TextSpan.Start)
        
    static member AreEquivalent(iss1: IfExpressionSyntax, iss2: IfExpressionSyntax) =
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
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.ElseKeyword.TextSpan.Start, this.Clause.CloseBraceToken.TextSpan.End - this.ElseKeyword.TextSpan.Start)
        
    static member AreEquivalent(eiss1: ElseIfClauseSyntax, eiss2: ElseIfClauseSyntax) =
        ExpressionSyntax.AreEquivalent(eiss1.Condition, eiss2.Condition)
            && BlockSyntax.AreEquivalent(eiss1.Clause, eiss2.Clause)
            
and ElseClauseSyntax =
    { ElseKeyword: SyntaxToken
      ElseClause: BlockSyntax }
with
    override this.ToString() =
        $"{this.ElseKeyword.ToString()}{this.ElseClause.ToString()}"
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.ElseKeyword.TextSpan.Start, this.ElseClause.CloseBraceToken.TextSpan.End - this.ElseKeyword.TextSpan.Start)
        
    static member AreEquivalent(ecs1: ElseClauseSyntax, ecs2: ElseClauseSyntax) =
        BlockSyntax.AreEquivalent(ecs1.ElseClause, ecs2.ElseClause)
    
    



(* #REGION Statements *)

type BlockSyntax =
    { OpenBraceToken: SyntaxToken
      Statements: StatementSyntax array
      CloseBraceToken: SyntaxToken }
with
    override this.ToString() =
        let statementsStr = this.Statements |> Array.map _.ToString() |> Array.fold (fun acc str -> acc + str) ""
        $"{this.OpenBraceToken.ToString()}{statementsStr}{this.CloseBraceToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.OpenBraceToken.TextSpan.Start, this.CloseBraceToken.TextSpan.End - this.OpenBraceToken.TextSpan.Start)
        
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
        
    member this.TextSpan () : TextSpan =
        let expressionTextSpan = this.Expression.TextSpan()
        TextSpan(expressionTextSpan.Start, this.SemicolonToken.TextSpan.End - expressionTextSpan.Start)
        
    static member AreEquivalent(ess1: ExpressionStatementSyntax, ess2: ExpressionStatementSyntax) =
        ExpressionSyntax.AreEquivalent(ess1.Expression, ess2.Expression)
    
type VariableDeclarationStatementSyntax =
    { LetKeyword: SyntaxToken
      Name: SyntaxToken
      TypeAnnotation: VariableTypeAnnotation option
      EqualsToken: SyntaxToken
      Expression: ExpressionSyntax
      SemicolonToken: SyntaxToken }
with
    override this.ToString() =
        let typeAnnotationStr = defaultArg (this.TypeAnnotation |> Option.map _.ToString()) ""
        $"{this.LetKeyword.ToString()}{this.Name.ToString()}{typeAnnotationStr}{this.EqualsToken.ToString()}{this.Expression.ToString()}{this.SemicolonToken.ToString()}"
        
    member this.TextSpan () : TextSpan =
        TextSpan(this.LetKeyword.TextSpan.Start, this.SemicolonToken.TextSpan.End - this.LetKeyword.TextSpan.Start)
        
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
        
    member this.TextSpan () : TextSpan =
        let typeSyntaxTextSpan = this.Type.TextSpan()
        TextSpan(this.ColonToken.TextSpan.Start, typeSyntaxTextSpan.End - this.ColonToken.TextSpan.Start)
        
    static member AreEquivalent(vta1: VariableTypeAnnotation, vta2: VariableTypeAnnotation) =
        TypeSyntax.AreEquivalent(vta1.Type, vta2.Type)
