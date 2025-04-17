/// <summary>
/// Module containing functionality for directly running transpiled Monkey code without generating any output files.
/// </summary>
[<RequireQualifiedAccess>]
module Monkey.Codegen.Dotnet.DynamicExecution

open System
open System.IO
open Microsoft.CodeAnalysis.CSharp


/// <summary>
/// Create a <c>CSharpCompilation</c> from a Monkey project.
/// </summary>
/// <param name="monkeySourceFileInfos">The list of monkey source files.</param>
/// <param name="projectFileInfo">The monkey project file (<c>.mkproj</c>).</param>
val compileFiles : monkeySourceFileInfos:FileInfo[] -> projectFileInfo:FileInfo -> Result<CSharpCompilation, Exception>


/// <summary>
/// Runs a <c>CSharpCompilation</c> object. 
/// </summary>
// remarks: Located here and not in the CLI to keep any running logic away from the CLI.
val dynamicallyRunCompilation : CSharpCompilation -> unit
