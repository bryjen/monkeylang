// ReSharper disable FSharpRedundantParens
/// <summary>
/// Functions for converting a monkey AST to an equivalent C# AST.
/// </summary>
module rec Monkey.Frontend.CLR.Converter.AstConverter

open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp

open FsToolkit.ErrorHandling

open Microsoft.CodeAnalysis.CSharp.Syntax
open Monkey.Frontend.CLR.Syntax.Ast
open Monkey.Frontend.CLR.Converter.ConverterErrors.ConverterError


open type Microsoft.CodeAnalysis.CSharp.SyntaxFactory

type MonkeyStatement = Monkey.Frontend.CLR.Syntax.Ast.StatementSyntax
type MonkeyExpression = Monkey.Frontend.CLR.Syntax.Ast.ExpressionSyntax
type MonkeySyntaxToken = Monkey.Frontend.CLR.Syntax.Ast.SyntaxToken

type CSharpStatement = Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax
type CSharpExpression = Microsoft.CodeAnalysis.CSharp.Syntax.ExpressionSyntax
type CSharpSyntaxToken = Microsoft.CodeAnalysis.SyntaxToken



[<AutoOpen>]
module private ConverterHelpers =
    let monkeyToCSharpSyntaxToken (monkeySyntaxToken: MonkeySyntaxToken) : CSharpSyntaxToken =
        Token(SyntaxTriviaList.Empty, monkeySyntaxToken.Kind, monkeySyntaxToken.Text, monkeySyntaxToken.Value.ToString(), SyntaxTriviaList.Empty)
        
    let separateResultArray (results: Result<'a, 'b> array) =
        let rec separateResultArrayCore (oks: ResizeArray<'a>) (errors: ResizeArray<'b>) (currentIdx: int) =
            match currentIdx with
            | i when i >= 0 && i < results.Length ->
                match results[i] with
                | Ok ok -> oks.Add(ok)
                | Error error -> errors.Add(error)
                separateResultArrayCore oks errors (currentIdx + 1)
            | _ ->
                (oks.ToArray(), errors.ToArray())
                
        separateResultArrayCore (ResizeArray<'a>()) (ResizeArray<'b>()) 0
        
    /// <summary>
    /// Changes the last statement of a block to a variable assignment statement. Requires that the last statement be an
    /// <b>expression statement</b>.
    /// </summary>
    let rec changeLastStatementToAssignment
            (varName: string)
            (blockSyntax: Microsoft.CodeAnalysis.CSharp.Syntax.BlockSyntax)
            : Result<Microsoft.CodeAnalysis.CSharp.Syntax.BlockSyntax, ConverterError> =
        match blockSyntax.Statements.LastOrDefault() |> Option.ofObj with
        | Some lastStatement ->
            match lastStatement with
            | :? Microsoft.CodeAnalysis.CSharp.Syntax.ExpressionStatementSyntax as expressionStatement ->
                let newLastStatement =
                    ExpressionStatement(
                        AssignmentExpression(
                            SyntaxKind.SimpleAssignmentExpression,
                            IdentifierName(varName),
                            Token(SyntaxKind.EqualsToken),
                            expressionStatement.Expression)
                        ) :> Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax
                let statements = blockSyntax.Statements |> Seq.toArray
                let newStatements = Array.append statements [| newLastStatement |] |> SyntaxList
                blockSyntax.WithStatements(newStatements) |> Ok
            | _ ->
                ConverterError("Expected the last statement of the block to be an expression statement.") |> Error
        | None ->
            ConverterError("The block has no statements.") |> Error

    let flattenArray (arr: 'a array array) : 'a array =
        match arr.Length with
        | i when i = 0 ->
            [| |]
        | i when i = 1 ->
            arr[0]
        | _ ->
            Array.concat arr


let toCSharpCompilationUnit
        (statements: StatementSyntax array)
        : Result<Microsoft.CodeAnalysis.CSharp.Syntax.CompilationUnitSyntax, ConverterError array> =
    let conversionResults = statements |> Array.map tryConvertStatement
    let statements, errors = separateResultArray conversionResults
    
    match flattenArray errors with
    | [| |] ->
        let tempUsingDirective = UsingDirective(IdentifierName("System"))  // TODO: This thing
        
        let temp = statements |> flattenArray
        
        let statements =
            statements
            |> Array.concat
            |> Array.map GlobalStatement
            |> Array.map (fun s -> s :> MemberDeclarationSyntax)
        CompilationUnit()
            .WithUsings(SyntaxFactory.List([| tempUsingDirective |]))
            .WithMembers(SyntaxFactory.List(statements))
            |> Ok
    | errors ->
        Error errors
    
       
       
(* Type Conversion *)
let rec internal tryConvertType (typeSyntax: TypeSyntax) : Microsoft.CodeAnalysis.CSharp.Syntax.TypeSyntax =
    match typeSyntax with
    | NameSyntax nameSyntax ->
        tryConvertNameSyntax nameSyntax
    | BuiltinTypeSyntax builtinTypeSyntax ->
        PredefinedType(monkeyToCSharpSyntaxToken builtinTypeSyntax.Token)
    | FunctionTypeSyntax functionTypeSyntax ->
        tryConvertFunctionType functionTypeSyntax
    | ArrayTypeSyntax arrayTypeSyntax -> 
        tryConvertArrayType arrayTypeSyntax
    | GenericTypeSyntax genericTypeSyntax -> 
        tryConvertGenericType genericTypeSyntax
    
and private tryConvertNameSyntax (nameSyntax: NameSyntax) : Microsoft.CodeAnalysis.CSharp.Syntax.NameSyntax =
    match nameSyntax.Identifier with
    | SimpleIdentifier simpleIdentifier ->
        IdentifierName(monkeyToCSharpSyntaxToken simpleIdentifier.Token)
    | QualifiedIdentifier qualifiedIdentifier ->
        tryConvertQualifiedIdentifier qualifiedIdentifier
    
and private tryConvertQualifiedIdentifier (qualifiedIdentifier: QualifiedIdentifier) =
    let tokens = qualifiedIdentifier.Tokens
    let rec tryConvertQualifiedIdentifierCore (currentIdx: int) =
        match currentIdx with
        | i when i > 0 ->
            QualifiedName(tryConvertQualifiedIdentifierCore (currentIdx - 1), IdentifierName(monkeyToCSharpSyntaxToken tokens[i])) :> Microsoft.CodeAnalysis.CSharp.Syntax.NameSyntax
        | i ->
            IdentifierName(monkeyToCSharpSyntaxToken tokens[i]) :> Microsoft.CodeAnalysis.CSharp.Syntax.NameSyntax
            
    tryConvertQualifiedIdentifierCore (tokens.Length - 1)
    
and private tryConvertFunctionType (functionType: FunctionTypeSyntax) =
    let csTypes = functionType.ParameterTypes |> Array.map tryConvertType
    GenericName(Identifier("Func")).WithTypeArgumentList(TypeArgumentList(SeparatedList(csTypes)))
    
and private tryConvertArrayType (arrayType: ArrayTypeSyntax) =
    failwith "todo"
    
and private tryConvertGenericType (genericType: GenericTypeSyntax) =
    let csTypes = genericType.GenericTypes |> Array.map tryConvertType
    GenericName(genericType.Type.ToString()).WithTypeArgumentList(TypeArgumentList(SeparatedList(csTypes)))
    
    

(* Parameter Conversion *)
let private tryConvertParameter (parameter: ParameterSyntax) : Microsoft.CodeAnalysis.CSharp.Syntax.ParameterSyntax =
    let csToken = monkeyToCSharpSyntaxToken parameter.Identifier.Token
    Parameter(csToken).WithType(tryConvertType parameter.Type)
    
let private tryConvertParameterList (parameterList: ParameterListSyntax) : Microsoft.CodeAnalysis.CSharp.Syntax.ParameterListSyntax =
    let csParameters = parameterList.Parameters |> Array.map tryConvertParameter
    
    ParameterList(SeparatedList(csParameters))
        .WithOpenParenToken(monkeyToCSharpSyntaxToken parameterList.OpenParenToken)
        .WithOpenParenToken(monkeyToCSharpSyntaxToken parameterList.CloseParenToken)
        
        
    
let rec internal tryConvertStatement
        (statement: MonkeyStatement)
        : Result<CSharpStatement array, ConverterError array> =
    match statement with
    | BlockSyntax blockSyntax ->
        tryConvertBlock blockSyntax
        |> Result.map (fun bs -> [| bs :> CSharpStatement |])
    | ExpressionStatementSyntax expressionStatementSyntax -> tryConvertExpressionStatement expressionStatementSyntax
    | VariableDeclarationStatementSyntax variableDeclarationStatementSyntax -> tryConvertVariableDeclarationStatement variableDeclarationStatementSyntax
    
and internal tryConvertBlock
        (block: BlockSyntax) 
        : Result<Microsoft.CodeAnalysis.CSharp.Syntax.BlockSyntax, ConverterError array> =
    let csharpStatements, errors =
        block.Statements
        |> Array.map tryConvertStatement
        |> separateResultArray
        |> (fun (arrs, error) -> (Array.concat arrs, error))
        
    match errors with
    | [|  |] ->
        let blockStatement =
            Block()
                .WithStatements(SyntaxList(csharpStatements))
                .WithOpenBraceToken(monkeyToCSharpSyntaxToken block.OpenBraceToken)
                .WithCloseBraceToken(monkeyToCSharpSyntaxToken block.CloseBraceToken)
        Ok blockStatement
    | _ ->
        errors |> Array.concat |> Error
        
    
and internal tryConvertExpressionStatement
        (expressionStatement: ExpressionStatementSyntax)
        : Result<CSharpStatement array, ConverterError array> =
    let csStatements = ResizeArray<CSharpStatement>()
    let addStatements newStatements = csStatements.AddRange(newStatements)
            
    match tryConvertExpression addStatements expressionStatement.Expression with
    | Error errors ->
        Error errors
    | Ok csExpression ->
        let csExpressionStatement =
            [|
                ExpressionStatement(csExpression)
                    .WithSemicolonToken(monkeyToCSharpSyntaxToken expressionStatement.SemicolonToken)
                    :> CSharpStatement
            |]
        Array.append (csStatements.ToArray()) csExpressionStatement |> Ok
    
    
and internal tryConvertVariableDeclarationStatement
        (variableDeclarationStatement: VariableDeclarationStatementSyntax)
        : Result<CSharpStatement array, ConverterError array> =
    let csStatements = ResizeArray<CSharpStatement>()
    let addStatements newStatements = csStatements.AddRange(newStatements)
            
    result {
        let variableType =
            match variableDeclarationStatement.TypeAnnotation with
            | Some variableTypeAnnotation -> tryConvertType variableTypeAnnotation.Type
            | None -> IdentifierName("var") :> Microsoft.CodeAnalysis.CSharp.Syntax.TypeSyntax
            
        let! csExpression = tryConvertExpression addStatements variableDeclarationStatement.Expression
        
        let csStatement =
            [|
                LocalDeclarationStatement(
                    VariableDeclaration(variableType)
                        .WithVariables(
                            SingletonSeparatedList(
                                VariableDeclarator(monkeyToCSharpSyntaxToken variableDeclarationStatement.Name)
                                    .WithInitializer(
                                        EqualsValueClause(
                                            csExpression
                                        )
                                )
                            )
                        )
                    ) :> Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax
            |]
        
        return Array.append (csStatements.ToArray()) csStatement
    }





let rec internal tryConvertExpression
        (addStatements: CSharpStatement array -> unit)
        (expression: MonkeyExpression)
        : Result<CSharpExpression, ConverterError array> =
    match expression with
    | ParenthesizedExpressionSyntax parenthesizedExpressionSyntax ->
        tryConvertParenthesizedExpression addStatements parenthesizedExpressionSyntax
    | MonkeyExpression.FunctionExpressionSyntax functionExpressionSyntax -> 
        tryConvertFunctionExpression functionExpressionSyntax
    | BinaryExpressionSyntax binaryExpressionSyntax -> 
        tryConvertBinaryExpression addStatements binaryExpressionSyntax
    | TypeSyntax typeSyntax ->
        tryConvertType typeSyntax :> CSharpExpression |> Ok
    | LiteralExpressionSyntax literalExpressionSyntax ->
        tryConvertLiteralExpression literalExpressionSyntax :> CSharpExpression |> Ok
    | PrefixExpressionSyntax prefixExpressionSyntax ->
        tryConvertPrefixExpression addStatements prefixExpressionSyntax
    | MonkeyExpression.IdentifierSyntax identifierSyntax ->
        tryConvertIdentifier identifierSyntax |> Ok
    | IfExpressionSyntax ifExpressionSyntax ->
        tryConvertIfExpression addStatements ifExpressionSyntax
    
    | ArrayExpressionSyntax arrayExpressionSyntax ->
        failwith "todo"
    | InterpolatedStringExpressionSyntax interpolatedStringExpressionSyntax ->
        failwith "todo"
    | InvocationExpressionSyntax invocationExpressionSyntax ->
        failwith "todo"
    | PostfixExpressionSyntax postfixExpressionSyntax ->
        failwith "todo"
    
and internal tryConvertLiteralExpression
        (literalExpression: LiteralExpressionSyntax)
        : Microsoft.CodeAnalysis.CSharp.Syntax.LiteralExpressionSyntax =
    let value =
        match literalExpression.Token.Value with
        | None -> failwith "no value for token?"
        | Some value ->
            match value with
            | :? int as intValue -> Literal(intValue)
            | :? string as stringValue -> Literal(stringValue)
            | _ -> failwith "unrecognized value type"
    LiteralExpression(literalExpression.Kind, value)
    
and internal tryConvertParenthesizedExpression
        (addStatements: CSharpStatement array -> unit)
        (parenthesizedExpression: ParenthesizedExpressionSyntax)
        : Result<CSharpExpression, ConverterError array> =
    result {
        let! csExpression = tryConvertExpression addStatements parenthesizedExpression.Expression
        let csOpenParenToken = monkeyToCSharpSyntaxToken parenthesizedExpression.OpenParenToken
        let csCloseParenToken = monkeyToCSharpSyntaxToken parenthesizedExpression.CloseParenToken
        return ParenthesizedExpression(csOpenParenToken, csExpression, csCloseParenToken)
    }
    
    
and internal tryConvertBinaryExpression
        (addStatements: CSharpStatement array -> unit)
        (binaryExpression: BinaryExpressionSyntax)
        : Result<CSharpExpression, ConverterError array> =
    result {
        let! csLeftExpression = tryConvertExpression addStatements binaryExpression.Left
        let! csRightExpression = tryConvertExpression addStatements binaryExpression.Right
        let csOperatorToken = monkeyToCSharpSyntaxToken binaryExpression.OperatorToken
        return BinaryExpression(binaryExpression.Kind, csLeftExpression, csOperatorToken, csRightExpression)
    }
    
and internal tryConvertInterpolatedStringExpression
        (addStatements: CSharpStatement array -> unit)
        (interpolatedStringExpression: InterpolatedStringExpressionSyntax)
        : Result<CSharpExpression, ConverterError array> =
    failwith "todo"
    
and internal tryConvertPostfixExpression
        (addStatements: CSharpStatement array -> unit)
        (postfixExpression: PostfixExpressionSyntax)
        : Result<CSharpExpression, ConverterError array> =
    failwith "todo"
    
and internal tryConvertPrefixExpression
        (addStatements: CSharpStatement array -> unit)
        (prefixExpression: PrefixExpressionSyntax)
        : Result<CSharpExpression, ConverterError array> =
    result {
        let! csOperand = tryConvertExpression addStatements prefixExpression.Operand
        let operatorToken = monkeyToCSharpSyntaxToken prefixExpression.OperatorToken
        return PrefixUnaryExpression(prefixExpression.Kind, operatorToken, csOperand)
    }
    
and internal tryConvertIdentifier
        (identifier: IdentifierSyntax)
        : CSharpExpression =
    match identifier with
    | SimpleIdentifier simpleIdentifier ->
        IdentifierName(monkeyToCSharpSyntaxToken simpleIdentifier.Token)
    | QualifiedIdentifier qualifiedIdentifier ->
        tryConvertQualifiedIdentifier qualifiedIdentifier
    
    
    
and internal tryConvertFunctionExpression
        (functionExpression: FunctionExpressionSyntax)
        : Result<CSharpExpression, ConverterError array> =
    result {
        let parameterList = tryConvertParameterList functionExpression.ParameterList
        let! functionBlock = tryConvertBlock functionExpression.Body
        let returnType = tryConvertType functionExpression.ReturnType
        return ParenthesizedLambdaExpression(parameterList, functionBlock).WithReturnType(returnType)
    }
    
    
/// <remarks>
/// In C#, 'if' clauses are solely statements, and not expressions. To work around this, we 'extract' the 'if' clause logic,
/// assign its last expression into a generated variable, then just return that variable as the expression.
/// <br/><br/>
/// Take the following for example:
/// <code>
/// // monkey
/// if (5 > 2) { 5; } else { 10; }
/// <br/><br/>
/// // equivalent csharp
/// object ZbpPsejc;
/// if (5 > 2)
/// {
///     ZbpPsejc = 5;
/// }
/// else
/// {
///     ZbpPsejc = 10;
/// }
/// </code>
/// </remarks>
and internal tryConvertIfExpression
        (addStatements: CSharpStatement array -> unit)
        (ifExpression: IfExpressionSyntax)
        : Result<CSharpExpression, ConverterError array> =
    result {
        let localDeclarationStatement, varName = generatePlaceholderVariable ()
        
        let! condition = tryConvertExpression addStatements ifExpression.Condition
        let! block = tryConvertBlock ifExpression.Clause
        let! block = changeLastStatementToAssignment varName block |> Result.mapError (fun err -> [| err |])
        
        let! elseClauseOption =
            match ifExpression.ElseClause with
            | None -> Ok None
            | Some monkeyElseClause -> tryConvertElseClause varName monkeyElseClause |> Result.map Some
        let elseClauseNullable: Microsoft.CodeAnalysis.CSharp.Syntax.ElseClauseSyntax | null =
            match elseClauseOption with
            | None -> null
            | Some csElseClause -> csElseClause
            
        let ifStatement = IfStatement(condition, block, elseClauseNullable)
        
        addStatements [| localDeclarationStatement; ifStatement |]
        return IdentifierName(varName)
    }
   
and private tryConvertElseClause
        (varName: string)
        (elseClause: ElseClauseSyntax)
        : Result<Microsoft.CodeAnalysis.CSharp.Syntax.ElseClauseSyntax, ConverterError array> =
    result {
        let! block = tryConvertBlock elseClause.ElseClause
        let! block = changeLastStatementToAssignment varName block |> Result.mapError (fun err -> [| err |])
        return ElseClause(Token(SyntaxKind.ElseKeyword), block)
    }
    
and private generatePlaceholderVariable () =
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let random = Random()
    let varName = Array.init 8 (fun _ -> chars.[random.Next(chars.Length)]) |> String
    
    let localDeclarationStatement =
        LocalDeclarationStatement(VariableDeclaration(
                PredefinedType(Token(SyntaxKind.ObjectKeyword)),
                SeparatedList(
                    [|
                    VariableDeclarator(Identifier(varName))
                    |]
                    )
                )
            ) :> Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax
    localDeclarationStatement, varName