#load "./Utils.fsx"

open System.Text.RegularExpressions
open System

let reg = Regex @"Card\s+\d+:\s+([\d\s]+)\s*\|\s*([\d\s]+)"

let getNumbers card =
    let m = reg.Match card

    let extractNumbers (groupIndex: int) =
        m.Groups[groupIndex]
            .Value.Trim()
            .Split([| ' ' |], StringSplitOptions.RemoveEmptyEntries)
        |> Set.ofArray

    (extractNumbers 1, extractNumbers 2)

let getCountofMatches winningNumbers myNumbers =
    myNumbers |> Seq.filter (fun x -> Set.contains x winningNumbers) |> Seq.length

let run filePath =
    Utils.readFile filePath
    |> Seq.choose (fun x ->
        let winningNumbers, myNumbers = getNumbers x
        let matches = getCountofMatches winningNumbers myNumbers

        match matches with
        | 0 -> None
        | n -> Some(Math.Pow(2, float (n - 1)) |> int))
    |> Seq.sum


run "Inputs/Day4/Input.txt" |> printfn "Total Points : %d"
