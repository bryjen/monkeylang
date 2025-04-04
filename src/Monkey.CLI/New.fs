module rec Monkey.CLI.New

open System.IO
open Argu
open Monkey.CLI

let performNew (newArguments: ParseResults<NewArguments>) : int =
    // further argument post-processing
    let name = newArguments.GetResult (Name, defaultValue="MonkeyProject")
    let outputDir = newArguments.GetResult (Output, defaultValue=($"./{name}"))
    let template = newArguments.GetResult (Template, defaultValue="")
    let isVerbose = newArguments.Contains NewArguments.Verbose
    let isInitGit = newArguments.Contains NewArguments.InitGit
    
    let dirInfo = DirectoryInfo(outputDir)
    
    let templateFileContents: string = generateMainProgram ()
    let templateFileInfo = FileInfo(Path.Join(dirInfo.FullName, "main.mk"))
    File.WriteAllText(templateFileInfo.FullName, templateFileContents)
    
    let templateProjFileContents: string = generateProjFileTemplate (name)
    let templateProjFileInfo = FileInfo(Path.Join(dirInfo.FullName, $"{name}.mkproj"))
    File.WriteAllText(templateProjFileInfo.FullName, templateProjFileContents)
    
    0

let private generateMainProgram () = "Console.WriteLine(\"Hello World from Monkey!\")"

let private generateProjFileTemplate (assemblyName: string) =
    $"""
<Project Sdk="Microsoft.NET.Sdk">
    <PropertyGroup>
        <OutputType>Exe</OutputType>
        <TargetFramework>net9.0</TargetFramework>
        <AssemblyName>{assemblyName}</AssemblyName>
    </PropertyGroup>
</Project>
"""
    |> string
    |> _.Trim()