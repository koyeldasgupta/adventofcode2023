#load "./Utils.fsx"

open System.Text.RegularExpressions
let regOuter = Regex @"\G\s*Game\s+(\d+):((?:\s*\d+\s*[a-zA-Z]+[,;]*\s*)+)"
let regInner = Regex @"\s*(\d+)\s*([a-zA-Z]+)"

let compareCount =
    function
    | ("red", Some count) when count > 12 -> Some "red"
    | ("green", Some count) when count > 13 -> Some "green"
    | ("blue", Some count) when count > 14 -> Some "blue"
    | _ -> None

let isPossibleGame subsets =
    let notPossibleColors =
        regInner.Matches subsets
        |> Seq.map (fun x -> (x.Groups[2].Value, Utils.parseInt x.Groups[1].Value))
        |> Seq.choose (compareCount)

    Seq.length notPossibleColors = 0



let run filePath =
    Utils.readFile filePath
    |> Seq.choose (fun x ->
        let m = regOuter.Match x
        let gameId = Utils.parseInt m.Groups[1].Value
        let subsets = m.Groups[2].Value

        match isPossibleGame subsets with
        | true -> gameId
        | false -> None)
    |> Seq.sum


run "Inputs/Day2/Input.txt" |> printfn "Sum : %d"
