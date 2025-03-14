namespace Monkey.Frontend.CLR.Api.Errors

open System
open System.IO
open Microsoft.CodeAnalysis

/// Exception type indicating a general exception.
type CompilationError(
        fileErrorPairs: (FileInfo * Exception list) array,
        ?message: string,
        ?innerException: Exception) =
    inherit Exception()
    
/// Exception type indicating a general exception.
type EmitError(
        ?message: string,
        ?diagnostics: (Diagnostic seq) option,
        ?innerException: Exception) =
    inherit Exception()
