module Monkey.Common.SpectreConsole.Spinner

open System
open System.Collections.Generic

open Spectre.Console


// BUG: builtin `Spectre.Console` spinners not working ? workaround being we need to create our own
// Full list of spectre console spinners available here: https://jsfiddle.net/sindresorhus/2eLtsbey/embedded/result/


type internal BinarySpinner () =
    inherit Spinner()
    let spinnerFrames =
        ([0..63]
        |> List.map (fun i -> Convert.ToString(i, 2).PadLeft(6, '0'))
        |> List<string>) :> IReadOnlyList<string>
with
    override this.Interval = TimeSpan.FromMilliseconds 50.0
    override this.IsUnicode = false
    override this.Frames = spinnerFrames
    

type internal EmptySpinner () =
    inherit Spinner()
    let spinnerFrames =
        ([0..63]
        |> List.map (fun i -> Convert.ToString(i, 2).PadLeft(6, '0'))
        |> List<string>) :> IReadOnlyList<string>
with
    override this.Interval = TimeSpan.FromSeconds 1.0
    override this.IsUnicode = false
    override this.Frames = [| "" |]
