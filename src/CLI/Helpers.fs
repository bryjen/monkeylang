module Monkey.CLI.Helpers

open System
open System.Globalization
open System.IO

[<RequireQualifiedAccess>]
module Log =
    let private timestampPrint logLevel str = 
        let formattedTimestamp = DateTime.Now.ToString("T", CultureInfo.CreateSpecificCulture("hr-HR"))
        printfn $"[{logLevel}] [{formattedTimestamp}] {str}"
    
    let info (str: string) = timestampPrint "INFO" str
        
    let warning (str: string) = timestampPrint "WARN" str
    
    let error (str: string) = timestampPrint "ERROR" str
    

let tryGetAbsolutePath path =
    try
        Ok (Path.GetFullPath(path))
    with
    | ex ->
        Error $"Failed to convert to absolute path: \"{path}\" with error \"{ex.Message}\""


