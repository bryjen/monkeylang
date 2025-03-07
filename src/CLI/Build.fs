module CLI.Build


open System.IO
open System.Linq

open System.Text
open System.Text.Json
open System.Text.Json.Serialization
open Argu
open FsToolkit.ErrorHandling

open Monkey.Backend.Compiler
open Monkey.Backend.Helpers
open Monkey.CLI
open Monkey.CLI.Helpers
open Monkey.Frontend.Parser


let rec performBuild (buildParseResults: ParseResults<BuildArguments>) : int =
    // further argument post-processing
    let files = buildParseResults.GetResult (Files, defaultValue=[])
    let outputDir = buildParseResults.GetResult (OutputDir, defaultValue="./")
    let workDir = buildParseResults.GetResult (WorkDir, defaultValue="./")
    let compileTarget = buildParseResults.GetResult (BuildArguments.Target, CompileTarget.Integrated)
    let isStrict = buildParseResults.Contains WarningsAsErrors
    let isVerbose = buildParseResults.Contains BuildArguments.Verbose
    
    result {
        let! absOutputDir = tryGetAbsolutePath outputDir
        let! absWorkDir = tryGetAbsolutePath workDir
        
        if isVerbose then
            printArguments files absOutputDir absWorkDir compileTarget isStrict isVerbose
            
        createDirectoryIfNonexistent isVerbose outputDir    
        
        for file in files do
            result {
                let! absFilePath = tryGetAbsolutePath (Path.Join(absWorkDir, file))
                
                if isVerbose then
                    Log.info $"Parsing file \"{absFilePath}\""
                    
                let! bytes = compileFile isVerbose absFilePath
                dumpBinaries isVerbose absOutputDir file bytes
                
            }
            |> function
               | Ok _ ->
                    if isVerbose then
                        Log.info $"Successfully compiled file \"{file}\""
               | Error errorMsg ->
                    Log.error $"Failed to compile file \"{file}\" with error message \"{errorMsg}\""
                    
        return ()
    }
    |> function
       | Ok _ ->
           0
       | Error errorMsg ->
           printfn $"ERROR: {errorMsg}"
           -1
    
    
and private printArguments files outputDir workDir compileTarget isStrict isVerbose =
    let compileTargetStr =
        match compileTarget with
        | CompileTarget.Integrated -> "integrated"
        | CompileTarget.Dotnet -> "dotnet"
        | _ ->
            Log.info "Failed to determine compile target string."
            ""
            
    Log.info $"output dir:\t\t\"{outputDir}\""
    Log.info $"work dir:\t\t\"{workDir}\""
    Log.info $"compile target:\t\t{compileTargetStr}"
    Log.info $"is strict:\t\t{isStrict}"
    Log.info $"is verbose:\t\t{isVerbose}"


and private createDirectoryIfNonexistent isVerbose outputDir =
    if not (Directory.Exists outputDir) then
        if isVerbose then
            Log.info "Output directory does not exist; creating new directory."
            
        let _ = Directory.CreateDirectory outputDir
        ()

    let isDirNotEmpty = Directory.EnumerateFileSystemEntries(outputDir).Any()
    if isVerbose && isDirNotEmpty then
        Log.warning "The provided output directory is not empty"
        
    ()
    
and private assertFileExists filePath =
    if File.Exists filePath then
        Ok ()
    else
        Error $"Could not find the file \"{filePath}\""
    
and private compileFile isVerbose filePath : Result<Bytecode, string> =
    result {
        do! assertFileExists filePath
        let fileContents = File.ReadAllText filePath
        let program = Parser.parseProgram fileContents
        do! assertProgramHasNoErrors program
        let! newCompiler = Compiler.compileNodes (programToNodes program) (Compiler.createNew ())
        let bytecode = Compiler.toByteCode newCompiler
        return bytecode
    }
    
and private dumpBinaries isVerbose outputDir fileName bytecode : unit =
    let fileNameNoExt = Path.GetFileNameWithoutExtension(fileName)
    let outputFilePath = Path.Join(outputDir, $"{fileNameNoExt}.mkil")
    let outputFilePathJson = Path.Join(outputDir, $"{fileNameNoExt}.json")
    
    if isVerbose then
        Log.info $"Saving bytecode for file \"{fileName}\" to \"{outputFilePath}\""
    
    let options = JsonFSharpOptions.Default().ToJsonSerializerOptions()
    options.WriteIndented <- true
    let asJson = JsonSerializer.Serialize(bytecode, options)
    let asBytes = Encoding.UTF8.GetBytes(asJson)
    
    File.WriteAllText(outputFilePathJson, asJson)
    File.WriteAllBytes(outputFilePath, asBytes)
    
    ()
    
