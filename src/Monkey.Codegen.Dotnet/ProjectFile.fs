/// <summary>
/// Module containing functionality for parsing and processing project file (.mkproj) information.
/// </summary>
module Monkey.Codegen.Dotnet.ProjectFile

open System.IO
open YamlDotNet.Serialization
open YamlDotNet.Serialization.NamingConventions

type PackageReference = {
    Include: string
    Version: string
}

type ProjectConfig =
    { Sdk: string
      AssemblyName: string
      TargetFramework: string
      GenerateDocumentationFile: bool
      InternalsVisibleTo: string list
      Content: string list
      Compile: string list
      PackageReferences: PackageReference list
      ProjectReferences: string list }
with
    static member Default =
        { Sdk = ""
          AssemblyName = "DefaultAssemblyName"
          TargetFramework = "net9.0"
          GenerateDocumentationFile = false
          InternalsVisibleTo = []
          Content = []
          Compile = []
          PackageReferences = []
          ProjectReferences = [] }



    
/// <remarks>
/// Assumes that the caller validates the give file info exists.
/// </remarks>
let internal tryParseProjectFile (fileInfo: FileInfo) : ProjectConfig option =
    try 
        let yaml = File.ReadAllText(fileInfo.FullName)
        let deserializer =
            DeserializerBuilder()
                .WithNamingConvention(CamelCaseNamingConvention.Instance)
                .Build()
        deserializer.Deserialize<ProjectConfig>(yaml) |> Some
    with
    | ex ->
        None