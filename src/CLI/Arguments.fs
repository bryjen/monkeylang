namespace Monkey.CLI

open Argu

/// <summary>
/// CLI arguments as a DU type.
/// </summary>
/// <remarks>
/// For more information, see <a href="https://github.com/fsprojects/Argu">Argu</a>.
/// </remarks>
type ProgramArguments =
    | [<CliPrefix(CliPrefix.None)>]
        Build of ParseResults<BuildArguments>
    | [<CliPrefix(CliPrefix.None)>]
        New of ParseResults<NewArguments>
    | [<CliPrefix(CliPrefix.None)>]
        Run of ParseResults<RunArguments>
    | [<AltCommandLine("-v")>]
        Version
    
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Build _ -> "Build Monkey (.mk) files."
            | New _ -> "Create a new Monkey project."
            | Run _ -> "Run Monkey files."
            | Version -> "Display toolchain and language versions."


/// <summary>
/// Arguments for the 'build' subcommand.
/// </summary>
and BuildArguments =
    | [<Mandatory; Unique; Last; AltCommandLine("-f")>]
        Files of string list
    | [<Unique; AltCommandLine("-o")>]
        OutputDir of string
    | [<Unique; AltCommandLine("-w")>]
        WorkDir of string
    | [<Unique; AltCommandLine("-t")>]
        Target of CompileTarget
    | [<Unique; AltCommandLine("-strict")>]
        WarningsAsErrors
    | [<Unique; AltCommandLine("-v")>]
        Verbose

with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Files _ -> "Files to build."
            | OutputDir _ -> "Output directory for build files."
            | WorkDir _ -> "Working directory for build files."
            | Target _ -> "The platform to target."
            | WarningsAsErrors -> "Flag. Emits warnings as errors."
            | Verbose -> "Flag. Emits extra build information."
    
    
/// <summary>
/// Arguments for the 'new' subcommand.
/// </summary>
and NewArguments =
    | [<Unique; AltCommandLine("-n")>]
        Name of string
    | InitGit
    | [<Unique; AltCommandLine("-t")>]
        Template 
    
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Name _ -> "The name of the project."
            | InitGit -> "Initializes git and creation of .gitignore, .git/* and .github/* files."
            | Template -> "The application template."
    
    
/// <summary>
/// Arguments for the 'run' subcommand.
/// </summary>
and RunArguments =
    | [<Mandatory; Unique; AltCommandLine("-f")>]
        File of string
    | [<Unique; AltCommandLine("-t")>]
        Target of CompileTarget
    | [<Unique; AltCommandLine("-v")>]
        Verbose
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | File _ -> "The file to run."
            | Target _ -> "The platform to target."
            | Verbose -> "Flag. Emits extra build information."
            
            
/// <summary>
/// Represents the runtime that the emitted bytecode will be for.
/// <br/>
/// The custom vm generates custom bytecode (specific to this project), while the NET option allows the generation of
/// CIL/MSIL.
/// </summary>
and CompileTarget =
    | Integrated  = 1  // the vm's built-in/integrated vm
    | Dotnet      = 2