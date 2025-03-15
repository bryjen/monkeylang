namespace Monkey.Frontend.CLR.Api.Errors

open System
open System.IO
open Microsoft.Build.Locator
open Microsoft.CodeAnalysis

/// Represents an error that occurred 
type CSharpProjectGenerationError(
        ?message: string,
        ?innerException: Exception) =
    inherit Exception()
    
/// Represents an error that occurred 
type MonkeyProjectFilesCompilationError(
        ?message: string,
        ?compilationErrors: (FileInfo * Exception list) array,
        ?innerException: Exception) =
    inherit Exception()
    
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
    
/// Represents an error that occurred 
type MSBuildToolsNotFound(
        searchedInstances: VisualStudioInstance list,
        requiredFiles: string list,
        ?innerException: Exception) =
    inherit Exception()
