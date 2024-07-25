open System.IO
open Monkey.Frontend.Ast
open Monkey.Frontend.Eval.Object
open Monkey.Frontend.Parser
open Monkey.Frontend.Eval.Evaluator

module Repl =
    let private Prompt = ">> "
    
    let rec startRepl (currentEnv: Environment) (stdIn: TextReader) (stdOut: TextWriter) =
        stdout.Write(Prompt) 
        
        match stdIn.ReadLine() with
        | null ->
            ()
        | input ->
            let program = Parser.parseProgram input
            if program.Errors.Length > 0 then
                stdOut.WriteLine($"The following errors occurred when parsing:\n{formatProgramErrors program}")
            else
                let newEnv = evalLoop stdOut currentEnv program.Statements
                startRepl newEnv stdIn stdout
                
    and private formatProgramErrors program =            
        program.Errors
        |> List.map (fun errorMsg -> "- " + errorMsg)
        |> String.concat "\n"
        
    and private evalLoop stdout currentEnv statements =
        match statements with
        | currentStatement :: remaining ->
            match Evaluator.evalStatement currentEnv currentStatement with
            | Ok (newEnv, resultObj) ->
                stdout.WriteLine(resultObj.Inspect())
                evalLoop stdout newEnv remaining
            | Error errorValue -> 
                stdout.WriteLine($"Error: {errorValue}")
                currentEnv
        | [] ->
            currentEnv
            


[<EntryPoint>]
let main _ =
    Repl.startRepl Environment.Empty stdin stdout
    0