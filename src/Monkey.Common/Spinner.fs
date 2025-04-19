module Monkey.Common.Spinner

open System

type ISpinner =
    abstract NextFrame: unit -> string

type BinarySpinner () =
    let chars = 4
    let max = (int (Math.Pow(2, chars))) - 1
    let spinnerFrames = [| 0..max |] |> Array.map (fun i -> Convert.ToString(i, 2).PadLeft(chars, '0'))
    let mutable currentFrame: int = 0

    interface ISpinner with
        member this.NextFrame() =
            if currentFrame >= spinnerFrames.Length - 1 then
                currentFrame <- -1
            currentFrame <- currentFrame + 1
            spinnerFrames[currentFrame]
            
type DotsSpinner () =
    let spinnerFrames =
        [|
            "⣷"
            "⣯"
            "⣟"
            "⡿"
            "⢿"
            "⣻"
            "⣽"
            "⣾"
        |]
    let mutable currentFrame: int = 0

    interface ISpinner with
        member this.NextFrame() =
            if currentFrame >= spinnerFrames.Length - 1 then
                currentFrame <- -1
            currentFrame <- currentFrame + 1
            spinnerFrames[currentFrame]
