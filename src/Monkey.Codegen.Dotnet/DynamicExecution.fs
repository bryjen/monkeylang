[<RequireQualifiedAccess>]
module Monkey.Codegen.Dotnet.DynamicExecution

open System
open System.IO
open System.Reflection
open Microsoft.CodeAnalysis.CSharp

let compileFiles (fileInfos: FileInfo array) (projectFilePath: FileInfo) : Result<CSharpCompilation, Exception> =
    CSharpCompilationGenerator.compileFiles fileInfos projectFilePath

let dynamicallyRunCompilation (compilation: CSharpCompilation) =
    use ms = new MemoryStream()
    let emitResult = compilation.Emit(ms)

    if emitResult.Success then
        ms.Seek(0L, SeekOrigin.Begin) |> ignore
        let asm = Assembly.Load(ms.ToArray())
        let entry = asm.EntryPoint
        (*
        let args: obj array | null =
            if entry.GetParameters().Length = 0 then
                null
            else
                [| [||] :> obj |]
        *)
        let args: string array = [|  |]
        let boxedArr: obj array = Array.map box args
        entry.Invoke(null, boxedArr) |> ignore
    else
        emitResult.Diagnostics
        |> Seq.iter (fun diag -> Console.WriteLine(diag.ToString()))