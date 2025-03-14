// Module that contains functionality for programatically generating a self-hosted executable that bootstraps the
// generated monkey executable.

module rec Monkey.Frontend.CLR.HostStubGenerator

open System.Diagnostics
open System.IO

module DotnetProjectGen = 
    // TODO: Refine down the line
    let rec generateHostSub (assemblyName: string) (outputAssemblyDir: string) =
        let outputAssemblyDirInfo = Directory.CreateDirectory(Path.Join(outputAssemblyDir, "temp"))
        
        let templateProgram : string = generateTemplateProgram assemblyName (Path.Combine(outputAssemblyDir, $"{assemblyName}.dll"))
        let templateProgramFileInfo = FileInfo(Path.Join(outputAssemblyDirInfo.FullName, "Program.cs"))
        File.WriteAllText(templateProgramFileInfo.FullName, templateProgram)
        
        let templateCsproj : string = generateTemplateCsproj assemblyName
        let templateCsprojFileInfo = FileInfo(Path.Join(outputAssemblyDirInfo.FullName, $"{assemblyName}.csproj"))
        File.WriteAllText(templateCsprojFileInfo.FullName, templateCsproj)
        
        runDotnetPublish outputAssemblyDirInfo
        
        
    and runDotnetPublish (workingDirectory: DirectoryInfo) =
        let command = "dotnet"
        let arguments = "publish -c Release -o ./.."
        
        let psi = ProcessStartInfo(command, arguments)
        psi.WorkingDirectory <- workingDirectory.FullName
        psi.RedirectStandardOutput <- true
        psi.RedirectStandardError <- true
        psi.UseShellExecute <- false
        psi.CreateNoWindow <- true
        
        use proc = new Process(StartInfo = psi)
        proc.Start() |> ignore
        
        let output = proc.StandardOutput.ReadToEnd()
        let error = proc.StandardError.ReadToEnd()
        
        printfn $"{output}"
        
        proc.WaitForExit()
        (proc.ExitCode, output, error)

    let generateTemplateProgram (assemblyName: string) (dllPath: string) =
        $"""
    using System;
    using System.IO;
    using System.Reflection;
    using System.Runtime.Loader;

    class Program
    {{
        static void Main(string[] args)
        {{
            try
            {{
                Assembly assembly = AssemblyLoadContext.Default.LoadFromAssemblyPath(@"{dllPath}");
                
                MethodInfo entryPoint = assembly.EntryPoint;
                
                if (entryPoint == null)
                {{
                    Console.WriteLine("No entry point found in the assembly.");
                    return;
                }}
                
                // Determine the parameters of the entry point.
                object[] parameters = null;
                ParameterInfo[] paramInfos = entryPoint.GetParameters();
                if (paramInfos.Length > 0)
                {{
                    // Assume the entry point is like 'static void Main(string[] args)'
                    parameters = new object[] {{ new string[0] }};
                }}
                
                // Invoke the entry point. For static methods, the first argument is null.
                entryPoint.Invoke(null, parameters);
            }}
            catch (Exception ex)
            {{
                Console.WriteLine("Error: " + ex);
            }}
            
        }}
    }}
    """


        

    let generateTemplateCsproj (assemblyName: string) =
        $"""
    <Project Sdk="Microsoft.NET.Sdk">
      <PropertyGroup>
        <OutputType>Exe</OutputType>
        <TargetFramework>net9.0</TargetFramework>
        <RuntimeIdentifier>win-arm64</RuntimeIdentifier>
        <SelfContained>true</SelfContained>
        <PublishSingleFile>true</PublishSingleFile>
        <DebugSymbols>false</DebugSymbols>
        <DebugType>none</DebugType>
        <AssemblyName>{assemblyName}</AssemblyName>
      </PropertyGroup>
    </Project>

    """
