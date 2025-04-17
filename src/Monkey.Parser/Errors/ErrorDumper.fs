module Monkey.Parser.Errors.ErrorDumper

open System
open System.IO
open System.Reflection

open FsToolkit.ErrorHandling

let dumpBaseDirectory =
    let executingAssemblyDirectory = Path.GetDirectoryName(Assembly.GetExecutingAssembly().Location)
    Path.Join(executingAssemblyDirectory, "log_dump")
    

let rec dump (contents: string) =
    option {
        let dumpDirPath = Path.Join(dumpBaseDirectory, DateTime.Now.ToString("yyyyMMdd_HHmmss"))
        let! dumpDir = 
            try
                Some (Directory.CreateDirectory(dumpDirPath))
            with
            | ex -> None
            
        let filePath = Path.Join(dumpDir.FullName, "internal_error_info.txt")
        let! file = 
            try
                File.WriteAllText(filePath, contents)
                Some (FileInfo(filePath))
            with
            | ex -> None
            
        return file
    }
