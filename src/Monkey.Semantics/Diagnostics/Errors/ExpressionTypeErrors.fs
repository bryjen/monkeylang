namespace Monkey.Semantics.Diagnostics.Errors

open Microsoft.CodeAnalysis.CSharp
open Microsoft.CodeAnalysis.Text
open Monkey.AST
open Monkey.Semantics.SemanticErrors
open Monkey.Semantics.Symbols
open Monkey.Semantics.Types


type UnsupportedBinaryExpressionOperand private () =
    inherit SemanticErrorBase()
    
    let mutable _type: UnsupportedBinaryExpressionOperandType = Unchecked.defaultof<UnsupportedBinaryExpressionOperandType>
    member private this.Type
        with get() = _type
        and set (value) = _type <- value

    override this.DetailedHelpMessage() = None
    
    override this.ErrorType() = "Invalid operand."
    
    override this.ErrorMessage() =
        match this.Type with
        | UserDefinedType(_, userDefinedType) ->
            $"The type \"{userDefinedType.Name}\" isn't supported in binary expressions."
        | InvalidBuiltinType(_, builtinType) -> 
            $"The type \"{builtinType.ToString()}\" isn't supported in binary expressions."
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        match this.Type with
        | UserDefinedType(expressionSyntax, _) -> base.Format(sourceText, expressionSyntax.TextSpan(), filePath)
        | InvalidBuiltinType(expressionSyntax, _) -> base.Format(sourceText, expressionSyntax.TextSpan(), filePath)

    
    static member UserDefinedType(expressionSyntax: ExpressionSyntax, userDefinedType: UserDefinedType) =
        let error = UnsupportedBinaryExpressionOperand()
        error.Type <- (expressionSyntax, userDefinedType) |> UnsupportedBinaryExpressionOperandType.UserDefinedType
        error
        
    static member BuiltinType(expressionSyntax: ExpressionSyntax, builtinType: BuiltinType) =
        let error = UnsupportedBinaryExpressionOperand()
        error.Type <- (expressionSyntax, builtinType) |> UnsupportedBinaryExpressionOperandType.InvalidBuiltinType
        error
        
and private UnsupportedBinaryExpressionOperandType =
    | UserDefinedType of ExpressionSyntax * UserDefinedType
    | InvalidBuiltinType of ExpressionSyntax * BuiltinType



type MismatchedBinaryExpressionOperands(binaryExpression: BinaryExpressionSyntax, leftType: BuiltinType, rightType: BuiltinType) =
    inherit SemanticErrorBase()

    override this.DetailedHelpMessage() = None
    
    override this.ErrorType() = "Mismatched binary expression types."
    
    override this.ErrorMessage() = $"The types \"{leftType.ToString()}\" and \"{rightType.ToString()}\" aren't valid in this expression."
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        base.Format(sourceText, binaryExpression.TextSpan(), filePath)
        
        
        
type UnsupportedPrefixExpressionOperand private () =
    inherit SemanticErrorBase()
    
    let mutable _type: UnsupportedPrefixExpressionOperandType = Unchecked.defaultof<UnsupportedPrefixExpressionOperandType>
    member private this.Type
        with get() = _type
        and set (value) = _type <- value
        
    member private this.GetPrefixExpr() =
        match _type with
        | TypeSymbol(prefixExpressionSyntax, _) -> prefixExpressionSyntax
        | UserDefinedType(prefixExpressionSyntax, _) -> prefixExpressionSyntax
        | InvalidBuiltinType(prefixExpressionSyntax, _) -> prefixExpressionSyntax
        
    
    override this.DetailedHelpMessage() = None
    
    override this.ErrorType() = "Invalid prefix expression."
    
    override this.ErrorMessage() =
        let prefixExpressionKindStr =
            match this.GetPrefixExpr().Kind with
            | SyntaxKind.LogicalNotExpression -> "logical not"
            | SyntaxKind.UnaryMinusExpression -> "unary minus"
            | _ -> "<INVALID>"
       
        let typeStr =  
            match _type with
            | TypeSymbol(_, typeSymbol) ->
                typeSymbol.ToString()
            | UserDefinedType(_, userDefinedType) ->
                userDefinedType.Name
            | InvalidBuiltinType(_, builtinType) ->
                builtinType.ToString()
        
        $"The operand of type \"{typeStr}\" isn't valid for the {prefixExpressionKindStr} operation."
        
    
    override this.GetFormattedMessage(sourceText: SourceText, filePath: string option) =
        base.Format(sourceText, this.GetPrefixExpr().TextSpan(), filePath)
        
    static member TypeSymbol(prefixExpression: PrefixExpressionSyntax, typeSymbol: TypeSymbol) =
        let err = UnsupportedPrefixExpressionOperand()
        err.Type <- (prefixExpression, typeSymbol) |> UnsupportedPrefixExpressionOperandType.TypeSymbol
        err
        
    static member UserDefinedType(prefixExpression: PrefixExpressionSyntax, userDefinedType: UserDefinedType) =
        let err = UnsupportedPrefixExpressionOperand()
        err.Type <- (prefixExpression, userDefinedType) |> UnsupportedPrefixExpressionOperandType.UserDefinedType
        err
        
    static member InvalidBuiltinType(prefixExpression: PrefixExpressionSyntax, builtinType: BuiltinType) =
        let err = UnsupportedPrefixExpressionOperand()
        err.Type <- (prefixExpression, builtinType) |> UnsupportedPrefixExpressionOperandType.InvalidBuiltinType
        err
        
        
and private UnsupportedPrefixExpressionOperandType =
    | TypeSymbol of PrefixExpressionSyntax * TypeSymbol
    | UserDefinedType of PrefixExpressionSyntax * UserDefinedType
    | InvalidBuiltinType of PrefixExpressionSyntax * BuiltinType
