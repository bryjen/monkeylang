module Monkey.Common.UserLocalStorage

open System
open System.IO

// TODO: See what it does in other OSes (Linux, Mac), since this was specifically for Windows

// %LOCALAPPDATA%
let local =
    Environment.SpecialFolder.LocalApplicationData
    |> Environment.GetFolderPath
    |> DirectoryInfo
    
// %LOCALAPPDATA%/mlang
let monkey_base_dir =
    Path.Join(local.FullName, "mlang")
    |> DirectoryInfo
    