namespace Monkey.CLI

open Argu

type Verbosity =
    | Quiet
    | Minimal
    | Normal
    | Detailed
    | Diagnostic

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
    | [<CliPrefix(CliPrefix.None)>]
        Test // TODO
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
            | Test -> ""


/// <summary>
/// Arguments for the 'build' subcommand.
/// </summary>
and BuildArguments =
    | [<MainCommand; First>]
        Project of project: string
    | [<Unique; AltCommandLine("-v")>]
        Verbosity of verbosity: Verbosity
    | [<Unique; AltCommandLine("-o")>]
        OutputDir of output: string
    | [<Unique; AltCommandLine("-t")>]
        Target of CompileTarget
    | [<Unique; AltCommandLine("-strict")>]
        WarningsAsErrors
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Project _ -> "The '*mkproj' file to operate on. If a file is not specified, the command will search the current directory for one."
            | Verbosity _ -> "Set the MSBuild verbosity level. Allowed values are q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]."
            | OutputDir _ -> "The output directory to place built artifacts in."
            | Target _ -> "The platform to target."
            | WarningsAsErrors -> "Flag. Emits warnings as errors."
    
    
/// <summary>
/// Arguments for the 'new' subcommand.
/// </summary>
and NewArguments =
    | [<Unique; AltCommandLine("-n")>]
        Name of string
    | [<Unique; AltCommandLine("-o")>]
        Output of string
    | [<Unique; AltCommandLine("-t")>]
        Template of string
    | [<Unique; AltCommandLine("-v")>]
        Verbose
    | [<Unique>]
        InitGit
    
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Name _ -> "The name of the project."
            | Output _ -> "The output directory of the generated project files."
            | Template _ -> "The application template."
            | Verbose -> "Flag. Emits extra generation information."
            | InitGit -> "Flag. Initializes git and creation of .gitignore, .git/* and .github/* files."
    
    
/// <summary>
/// Arguments for the 'run' subcommand.
/// </summary>
and RunArguments =
    | [<MainCommand; First>]
        Project of project: string
    | [<Unique; AltCommandLine("-v")>]
        Verbosity of verbosity: Verbosity
    | [<Unique; AltCommandLine("-o")>]
        BuildOutputDir of buildOutput: string
    | [<Unique; AltCommandLine("-t")>]
        Target of CompileTarget
    | [<Unique; AltCommandLine("-strict")>]
        WarningsAsErrors
    (*
    | [<Unique; Last; AltCommandLine("-a")>]
        Args of string array
    *)
with
    interface IArgParserTemplate with
        member this.Usage =
            match this with
            | Project _ -> "The '*mkproj' file to operate on. If a file is not specified, the command will search the current directory for one."
            | Verbosity _ -> "Set the MSBuild verbosity level. Allowed values are q[uiet], m[inimal], n[ormal], d[etailed], and diag[nostic]."
            | BuildOutputDir _ -> "The output directory to place built artifacts in."
            | Target _ -> "The platform to target."
            | WarningsAsErrors -> "Flag. Emits warnings as errors."
            (*
            | Args _ -> "Arguments to pass into the program."
            *)
            
            
/// <summary>
/// Represents the runtime that the emitted bytecode will be for.
/// <br/>
/// The custom vm generates custom bytecode (specific to this project), while the NET option allows the generation of
/// CIL/MSIL.
/// </summary>
and CompileTarget =
    | Integrated  = 1  // the vm's built-in/integrated vm
    | Dotnet      = 2