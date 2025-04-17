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
        let args : string[] = [||]
        asm.EntryPoint.Invoke(null, [| box args |]) |> ignore
    else
        emitResult.Diagnostics
        |> Seq.iter (fun diag -> Console.WriteLine(diag.ToString()))