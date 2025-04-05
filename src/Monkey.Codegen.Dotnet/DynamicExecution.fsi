[<RequireQualifiedAccess>]
module Monkey.Codegen.Dotnet.DynamicExecution

open System
open System.IO
open Microsoft.CodeAnalysis.CSharp


val compileFiles : FileInfo array -> FileInfo -> Result<CSharpCompilation, Exception>

val dynamicallyRunCompilation : CSharpCompilation -> unit
