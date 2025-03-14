open System.IO
open System.Runtime.InteropServices

open FsToolkit.ErrorHandling

open Monkey.Backend.Code
open Monkey.Backend.Compiler
open Monkey.Backend.Helpers
open Monkey.Backend.VM
open Monkey.Frontend.Ast
open Monkey.Frontend.Eval.Object
open Monkey.Frontend.Parser

module Repl =
    let private Prompt = ">> "
    
    let rec startRepl (stdIn: TextReader) (stdOut: TextWriter) (compiler: Compiler) (vm: VM) =
        stdout.Write(Prompt) 
        
        match stdIn.ReadLine() with
        | null ->
            ()
        | input ->
            let program = Parser.parseProgram input
            if program.Errors.Length > 0 then
                stdOut.WriteLine($"The following errors occurred when parsing:\n{formatProgramErrors program}")
            else
                let newCompiler, newVm = altEvalLoop stdOut compiler vm program
                startRepl stdIn stdout newCompiler newVm
                
    and private formatProgramErrors program =            
        program.Errors
        |> List.map (fun errorMsg -> "- " + errorMsg)
        |> String.concat "\n"
        
    and internal altEvalLoop (stdout: TextWriter) (compiler: Compiler) (vm: VM) (program: Program) : (Compiler * VM) =
        result {
            do! assertProgramHasNoErrors program
            let! newCompiler = Compiler.compileNodes (programToNodes program) compiler
            let bytecode = Compiler.toByteCode newCompiler
            let vm = VM.fromByteCode bytecode
            let! newVm = VM.run vm
            let resultOption = VM.getLastPoppedStackElement newVm
            return resultOption, newCompiler, newVm
        }
        |> function
           | Ok (objectOption, newCompiler, newVM) ->
               match objectOption with
               | Some object ->
                   match object with
                   | NullType -> ()  // if 'NullType' we don't print anything
                   | _ -> stdout.WriteLine $"{object}"
               | None -> ()
               newCompiler, newVM
           | Error errorMsg ->
               stdout.WriteLine $"ERROR: {errorMsg}"
               compiler, vm
       
/// <summary>
/// Represents the runtime that the emitted bytecode will be for.
/// <br/>
/// The custom vm generates custom bytecode (specific to this project), while the NET option allows the generation of
/// CIL/MSIL.
/// </summary>
type CompileTarget =
    | CustomVM
    | NET
with
    override this.ToString(): string =
        match this with
        | CustomVM -> "CustomVM"
        | NET -> ".NET"

[<EntryPoint>]
let main _ =
    let runtimeIdentifier = RuntimeInformation.RuntimeIdentifier
    let frameworkDescription = RuntimeInformation.FrameworkDescription
    let langVersion = "0.1.0"
    
    let compileTarget = CompileTarget.CustomVM
    let prettyOutputEnabled = false
    printfn $"monkey {langVersion} by bryjen | {frameworkDescription} | {runtimeIdentifier} | compile target: {compileTarget} | pretty output: {prettyOutputEnabled}"
    printfn "Type \"help\" or \"license\" for more information."
    
    let emptyCompiler = Compiler.createNew ()
    let emptyBytecode: Bytecode = { Instructions = Instructions [||]; Constants = [||] }
    let emptyVm = VM.fromByteCode emptyBytecode
    
    Repl.startRepl stdin stdout emptyCompiler emptyVm
    0