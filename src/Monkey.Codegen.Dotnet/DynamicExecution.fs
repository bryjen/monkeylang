[<RequireQualifiedAccess>]
module Monkey.Codegen.Dotnet.DynamicExecution

open System
open System.IO
open System.Reflection
open Microsoft.CodeAnalysis.CSharp

let compileFiles (monkeySourceFileInfos: FileInfo array) (projectFileInfo: FileInfo) : Result<CSharpCompilation, Exception> =
    CSharpCompilationGenerator.compileFiles monkeySourceFileInfos projectFileInfo

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