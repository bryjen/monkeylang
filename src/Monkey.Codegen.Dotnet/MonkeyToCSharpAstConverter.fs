// ReSharper disable FSharpRedundantParens
/// <summary>
/// Functions for converting a monkey AST to an equivalent C# AST.
/// </summary>
module rec Monkey.Codegen.Dotnet.MonkeyToCSharpAstConverter

open System
open Microsoft.CodeAnalysis
open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.CSharp.Syntax
open type Microsoft.CodeAnalysis.CSharp.SyntaxFactory

open FsToolkit.ErrorHandling

open Monkey.AST



// type aliasing to be able to differentiate between C# and Monkey syntax options, which use the same name.
type CSharpStatement = Microsoft.CodeAnalysis.CSharp.Syntax.StatementSyntax
type CSharpExpression = Microsoft.CodeAnalysis.CSharp.Syntax.ExpressionSyntax
type CSharpSyntaxToken = Microsoft.CodeAnalysis.SyntaxToken

type MonkeyStatement = Monkey.AST.StatementSyntax
type MonkeyExpression = Monkey.AST.ExpressionSyntax
type MonkeySyntaxToken = Monkey.AST.SyntaxToken



/// TODO: A placeholder for now, expand on this down the line.
type ConverterError(message: string) =
    inherit Exception(message)
    
    

/// Singleton type mainly to control certain options during <b>testing</b>. Is not public-facing.
type internal ConverterConfigSingleton() =
    let mutable randomSeed: int option = None
    
    static let instance = lazy (ConverterConfigSingleton())
    static member Instance = instance.Value
    
    member this.Seed
        with get() = randomSeed
        and set(value) = randomSeed <- value



/// Utilities for converting monkey tokens into csharp syntax tokens. Even though the monkey token type technically
/// contains all the required information to build csharp tokens, we have to generate them through <b>SnytaxFactory</b>.
[<AutoOpen>]
module private TokenConverter =
    let monkeyToCSharpSyntaxToken (monkeySyntaxToken: MonkeySyntaxToken) : Result<CSharpSyntaxToken, ConverterError> =
        match monkeySyntaxToken.Kind with
        | kind when kind = SyntaxKind.NumericLiteralToken ->
            match monkeySyntaxToken.Value with
            | :? int as int -> Literal(int) |> Ok
            | :? float as float -> Literal(float) |> Ok
            | :? byte as byte -> Literal(byte) |> Ok
            | _ -> ConverterError("Tried to convert a non-numeric type to a numeric token") |> Error
        | kind when kind = SyntaxKind.StringLiteralToken ->
            match monkeySyntaxToken.Value with
            | :? string as string -> Literal(string) |> Ok
            | _ -> ConverterError("Tried to convert a non-string type to a string token") |> Error
        | SyntaxKind.IdentifierToken ->
            Identifier(monkeySyntaxToken.Text.Trim()) |> Ok
        | _ ->
            Token(monkeySyntaxToken.Kind) |> Ok
        
    

[<AutoOpen>]
module private ConverterHelpers =
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
        
    let rec appendAssignmentStatementToBlock
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
            
            
    let rec transformLastStatementToAssignmentStatement
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
                let everythingButLastStatement = Array.take (blockSyntax.Statements.Count - 1) (Seq.toArray blockSyntax.Statements)
                let newStatements = Array.append everythingButLastStatement [| newLastStatement |] |> SyntaxList
                blockSyntax.WithStatements(newStatements) |> Ok
            | _ ->
                ConverterError("Expected the last statement of the block to be an expression statement.") |> Error
        | None ->
            ConverterError("The block has no statements.") |> Error
            
            
    let rec transformLastStatementToReturnStatement
            (blockSyntax: Microsoft.CodeAnalysis.CSharp.Syntax.BlockSyntax)
            : Result<Microsoft.CodeAnalysis.CSharp.Syntax.BlockSyntax, ConverterError> =
                
        let unitReturnStatement =
            ReturnStatement(
                ObjectCreationExpression(
                    Token(SyntaxKind.NewKeyword),
                    IdentifierName("unit"),
                    ArgumentList(),
                    null)
                )

        match blockSyntax.Statements.LastOrDefault() |> Option.ofObj with
        | Some lastStatement ->
            match lastStatement with
            | :? Microsoft.CodeAnalysis.CSharp.Syntax.ExpressionStatementSyntax as expressionStatement ->
                let returnStatement = ReturnStatement(expressionStatement.Expression)
                let everythingButLastStatement = Array.take (blockSyntax.Statements.Count - 1) (Seq.toArray blockSyntax.Statements)
                let newStatements = Array.append everythingButLastStatement [| returnStatement |] |> SyntaxList
                blockSyntax.WithStatements(newStatements) |> Ok
            | _ ->
                ConverterError("Expected the last statement of the block to be an expression statement.") |> Error
        | None ->
            Block().WithStatements(List([| unitReturnStatement |])) |> Ok
 

let toCSharpCompilationUnit
        (statements: StatementSyntax array)
        : Result<Microsoft.CodeAnalysis.CSharp.Syntax.CompilationUnitSyntax, ConverterError array> =
    let conversionResults = statements |> Array.map tryConvertStatement
    let statements, errors = separateResultArray conversionResults
    
    match Array.concat errors with
    | [| |] ->
        let tempUsingDirective = UsingDirective(IdentifierName("System"))  // TODO: This thing
        
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
        PredefinedType(Token(builtinTypeSyntax.Token.Kind))
    | FunctionTypeSyntax functionTypeSyntax ->
        tryConvertFunctionType functionTypeSyntax
    | ArrayTypeSyntax arrayTypeSyntax -> 
        tryConvertArrayType arrayTypeSyntax
    | GenericTypeSyntax genericTypeSyntax -> 
        tryConvertGenericType genericTypeSyntax
    
and private tryConvertNameSyntax (nameSyntax: NameSyntax) : Microsoft.CodeAnalysis.CSharp.Syntax.NameSyntax =
    match nameSyntax.Identifier with
    | SimpleIdentifier simpleIdentifier ->
        IdentifierName(Identifier(simpleIdentifier.Token.Text.Trim()))
    | QualifiedIdentifier qualifiedIdentifier ->
        tryConvertQualifiedIdentifier qualifiedIdentifier
    
and private tryConvertQualifiedIdentifier (qualifiedIdentifier: QualifiedIdentifier) =
    let tokens = qualifiedIdentifier.Tokens
    let rec tryConvertQualifiedIdentifierCore (currentIdx: int) =
        match currentIdx with
        | i when i > 0 ->
            QualifiedName(tryConvertQualifiedIdentifierCore (currentIdx - 1), IdentifierName(tokens[i].Text.Trim())) :> Microsoft.CodeAnalysis.CSharp.Syntax.NameSyntax
        | i ->
            IdentifierName(tokens[i].Text.Trim()) :> Microsoft.CodeAnalysis.CSharp.Syntax.NameSyntax
            
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
    let identifierToken = Identifier(parameter.Identifier.Token.ToString().Trim())
    Parameter(identifierToken).WithType(tryConvertType parameter.Type)
    
let private tryConvertParameterList (parameterList: ParameterListSyntax) : Microsoft.CodeAnalysis.CSharp.Syntax.ParameterListSyntax =
    let csParameters = parameterList.Parameters |> Array.map tryConvertParameter
    
    ParameterList(SeparatedList(csParameters))
        .WithOpenParenToken(Token(SyntaxKind.OpenParenToken))
        .WithCloseParenToken(Token(SyntaxKind.CloseParenToken))
        
        
        
(* Argument Conversion *)
let private tryConvertArgumentsList
        (addStatements: CSharpStatement array -> unit)
        (argumentList: ArgumentListSyntax)
        : Result<Microsoft.CodeAnalysis.CSharp.Syntax.ArgumentListSyntax, ConverterError array> =
    result {
        let argumentParseResults = argumentList.Arguments |> Array.map (tryConvertExpression addStatements)
        let expressions, errors = separateResultArray argumentParseResults
        
        do! match Array.concat errors with
            | [| |] -> Ok ()
            | errors -> Error errors
        
        let arguments = expressions |> Array.map Argument
        return ArgumentList(SeparatedList(arguments))
    }
        
        
    
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
                .WithOpenBraceToken(Token(SyntaxKind.OpenBraceToken))
                .WithCloseBraceToken(Token(SyntaxKind.CloseBraceToken))
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
                    .WithSemicolonToken(Token(SyntaxKind.SemicolonToken))
                    :> CSharpStatement
            |]
        Array.append (csStatements.ToArray()) csExpressionStatement |> Ok
    
    
and internal tryConvertVariableDeclarationStatement
        (variableDeclarationStatement: VariableDeclarationStatementSyntax)
        : Result<CSharpStatement array, ConverterError array> =
    let csStatements = ResizeArray<CSharpStatement>()
    let addStatements newStatements = csStatements.AddRange(newStatements)
            
    result {
        let! csExpression = tryConvertExpression addStatements variableDeclarationStatement.Expression
        
        let variableTypeOption =
            match variableDeclarationStatement.TypeAnnotation with
            | Some variableTypeAnnotation ->
                tryConvertType variableTypeAnnotation.Type |> Some
            | None ->
                match csExpression with
                | :? ParenthesizedLambdaExpressionSyntax as ples ->
                    ples |> getParameterizedLambdaExpressionType |> Some
                | _ ->
                    None
                    
        let defaultType = IdentifierName("var") :> Microsoft.CodeAnalysis.CSharp.Syntax.TypeSyntax
        let variableType = defaultArg variableTypeOption defaultType
        
        let csStatement =
            [|
                LocalDeclarationStatement(
                    VariableDeclaration(variableType)
                        .WithVariables(
                            SingletonSeparatedList(
                                VariableDeclarator(Identifier(variableDeclarationStatement.Name.Text.ToString()))
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

and private getParameterizedLambdaExpressionType (parenthesizedLambdaExpressionSyntax: ParenthesizedLambdaExpressionSyntax) =
    let parameterTypes = parenthesizedLambdaExpressionSyntax.ParameterList.Parameters |> Seq.toArray |> Array.map _.Type
    let returnType = parenthesizedLambdaExpressionSyntax.ReturnType
    let fullTypeArr = Array.append parameterTypes [| returnType |]
    
    GenericName(Identifier("Func")).WithTypeArgumentList(TypeArgumentList(SeparatedList(fullTypeArr))) :> Microsoft.CodeAnalysis.CSharp.Syntax.TypeSyntax



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
        tryConvertLiteralExpression literalExpressionSyntax
        |> Result.map (fun les -> les :> CSharpExpression)
        |> Result.mapError (fun err -> [| err |])
    | PrefixExpressionSyntax prefixExpressionSyntax ->
        tryConvertPrefixExpression addStatements prefixExpressionSyntax
    | MonkeyExpression.IdentifierSyntax identifierSyntax ->
        tryConvertIdentifier identifierSyntax |> Ok
    | IfExpressionSyntax ifExpressionSyntax ->
        tryConvertIfExpression addStatements ifExpressionSyntax
    | InvocationExpressionSyntax invocationExpressionSyntax ->
        tryConvertInvocationExpression addStatements invocationExpressionSyntax
    
    | ArrayExpressionSyntax arrayExpressionSyntax ->
        failwith "todo"
    | InterpolatedStringExpressionSyntax interpolatedStringExpressionSyntax ->
        failwith "todo"
    | PostfixExpressionSyntax postfixExpressionSyntax ->
        failwith "todo"
    
and internal tryConvertLiteralExpression
        (literalExpression: LiteralExpressionSyntax)
        : Result<Microsoft.CodeAnalysis.CSharp.Syntax.LiteralExpressionSyntax, ConverterError> =
    result {
        let! token = monkeyToCSharpSyntaxToken literalExpression.Token
        return LiteralExpression(literalExpression.Kind, token)
    }
    
and internal tryConvertParenthesizedExpression
        (addStatements: CSharpStatement array -> unit)
        (parenthesizedExpression: ParenthesizedExpressionSyntax)
        : Result<CSharpExpression, ConverterError array> =
    result {
        let! csExpression = tryConvertExpression addStatements parenthesizedExpression.Expression
        return ParenthesizedExpression(Token(SyntaxKind.OpenParenToken), csExpression, Token(SyntaxKind.CloseParenToken))
    }
    
    
and internal tryConvertBinaryExpression
        (addStatements: CSharpStatement array -> unit)
        (binaryExpression: BinaryExpressionSyntax)
        : Result<CSharpExpression, ConverterError array> =
    result {
        let! csLeftExpression = tryConvertExpression addStatements binaryExpression.Left
        let! csRightExpression = tryConvertExpression addStatements binaryExpression.Right
        return BinaryExpression(binaryExpression.Kind, csLeftExpression, Token(binaryExpression.OperatorToken.Kind), csRightExpression)
    }
    
    
and internal tryConvertInvocationExpression
        (addStatements: CSharpStatement array -> unit)
        (invocationExpression: InvocationExpressionSyntax)
        : Result<CSharpExpression, ConverterError array> =
    result {
        let! leftExpression = tryConvertInvocationExpressionLeftExpression invocationExpression.Expression
        let! argumentsList = tryConvertArgumentsList addStatements invocationExpression.Arguments
        return InvocationExpression(leftExpression, argumentsList)
    }
    
and private tryConvertInvocationExpressionLeftExpression
        (invocationExpressionLeftExpression: InvocationExpressionLeftExpression)
        : Result<CSharpExpression, ConverterError array> =
    match invocationExpressionLeftExpression with
    | ParenthesizedFunctionExpressionSyntax invocationParenthesizedExpressionSyntax ->
        tryConvertInvocationExpressionLeftExpression invocationParenthesizedExpressionSyntax.Expression
        |> Result.map (fun expr -> ParenthesizedExpression(expr))
    | FunctionExpressionSyntax functionExpressionSyntax ->
        tryConvertFunctionExpression functionExpressionSyntax
    | IdentifierSyntax identifierSyntax ->
        match identifierSyntax with
        | SimpleIdentifier simpleIdentifier ->
            IdentifierName(Identifier(simpleIdentifier.Token.Text.Trim())) :> CSharpExpression |> Ok
        | QualifiedIdentifier qualifiedIdentifier ->
            tryConvertQualifiedIdentifierToMemberAccessExpression qualifiedIdentifier :> CSharpExpression |> Ok
        
and private tryConvertQualifiedIdentifierToMemberAccessExpression (qualifiedIdentifier: QualifiedIdentifier) =
    let tokens = qualifiedIdentifier.Tokens
    let rec core (currentIdx: int) =
        match currentIdx with
        | i when i = tokens.Length - 2 ->
            MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                IdentifierName(tokens[tokens.Length - 2].Text.Trim()),
                Token(SyntaxKind.DotToken),
                IdentifierName(tokens[tokens.Length - 1].Text.Trim())
                )
        | i ->
            MemberAccessExpression(
                SyntaxKind.SimpleMemberAccessExpression,
                core (currentIdx + 1),
                Token(SyntaxKind.DotToken),
                IdentifierName(tokens[i].Text.Trim())
                )
            
    core (0)
    
    
    
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
        return PrefixUnaryExpression(prefixExpression.Kind, Token(prefixExpression.OperatorToken.Kind), csOperand)
    }
    
and internal tryConvertIdentifier
        (identifier: IdentifierSyntax)
        : CSharpExpression =
    match identifier with
    | SimpleIdentifier simpleIdentifier ->
        IdentifierName(Identifier(simpleIdentifier.Token.Text.Trim()))
    | QualifiedIdentifier qualifiedIdentifier ->
        tryConvertQualifiedIdentifier qualifiedIdentifier
    
and internal tryConvertFunctionExpression
        (functionExpression: FunctionExpressionSyntax)
        : Result<CSharpExpression, ConverterError array> =
    result {
        let parameterList = tryConvertParameterList functionExpression.ParameterList
        let! functionBlock = tryConvertBlock functionExpression.Body
        let! updatedFunctionBlock = transformLastStatementToReturnStatement functionBlock |> Result.mapError (fun err -> [| err |])
        let returnType = tryConvertType functionExpression.ReturnType
        return ParenthesizedLambdaExpression(parameterList, updatedFunctionBlock).WithReturnType(returnType)
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
        let! block = transformLastStatementToAssignmentStatement varName block |> Result.mapError (fun err -> [| err |])
        
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
        let! block = transformLastStatementToAssignmentStatement varName block |> Result.mapError (fun err -> [| err |])
        return ElseClause(Token(SyntaxKind.ElseKeyword), block)
    }
    
and private generatePlaceholderVariable () =
    let random =
        match ConverterConfigSingleton.Instance.Seed with
        | None -> Random()
        | Some seed -> Random(seed)
    
    let chars = "abcdefghijklmnopqrstuvwxyzABCDEFGHIJKLMNOPQRSTUVWXYZ"
    let varName = Array.init 8 (fun _ -> chars[random.Next(chars.Length)]) |> String
    
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