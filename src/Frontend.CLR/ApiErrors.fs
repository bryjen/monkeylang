namespace Monkey.Frontend.CLR.Api.Errors

open System
open System.IO
open Microsoft.Build.Locator
open Microsoft.CodeAnalysis

type CSharpProjectGenerationError(
        ?message: string,
        ?innerException: Exception) =
    inherit Exception()
    
type MonkeyProjectFilesCompilationError(
        ?message: string,
        ?compilationErrors: (FileInfo * Exception list) array,
        ?innerException: Exception) =
    inherit Exception()
    
type CompilationError(
        fileErrorPairs: (FileInfo * Exception list) array,
        ?message: string,
        ?innerException: Exception) =
    inherit Exception()
    
type EmitError(
        ?message: string,
        ?diagnostics: (Diagnostic seq) option,
        ?innerException: Exception) =
    inherit Exception()
    
type MSBuildToolsNotFoundError(
        searchedInstances: VisualStudioInstance list,
        requiredFiles: string list,
        ?innerException: Exception) =
    inherit Exception()
