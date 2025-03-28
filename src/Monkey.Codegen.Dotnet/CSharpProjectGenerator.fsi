module Monkey.Codegen.Dotnet.CSharpProjectGenerator

open System
open System.IO



/// <summary>
/// Contains the results of scanning a directory for project related files (.mk, .mkproj files, for example).
/// </summary>
type ScanResults
    
/// <summary>
/// Scans a specified directory for project files (.mk, .mkproj files, for example).
/// </summary>
val scanMonkeyProject : string -> Result<ScanResults, Exception>

/// <summary>
/// Generates a C# project given an output directory and valid Monkey project information (using <c>ScanResults</c>), 
/// </summary>
val generateTempCSharpProject : string -> ScanResults -> Result<FileInfo, Exception>

/// <summary>
/// Given an output path and a C# project directory path (the generated C# project), run MSBuild to the specified output
/// directory.
/// </summary>
val runMsBuild : string -> string -> Result<unit, Exception>
