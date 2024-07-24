namespace Monkey.Frontend.Repl

open System.IO
open Monkey.Frontend.Ast
open Monkey.Frontend.Parser

module Repl =
    let private Prompt = ">> "
    
    
    let rec startRepl (stdIn: TextReader) (stdOut: TextWriter) =
        stdout.Write(Prompt) 
        
        match stdIn.ReadLine() with
        | null ->
            ()
        | input ->
            let program = Parser.parseProgram input
            handleParseResults stdOut program

        startRepl stdIn stdout
        
    and handleParseResults (stdOut: TextWriter) (program: Program) : unit =
        if program.Errors.Length > 0 then
            let errorMsgsString = program.Errors
                                  |> List.map (fun errorMsg -> "- " + errorMsg)
                                  |> String.concat "\n"
            stdOut.WriteLine($"The following errors occurred when parsing:\n{errorMsgsString}")
        else
            program.Statements |> List.iter (fun stat -> stdOut.WriteLine($"{stat.ToString()}"))