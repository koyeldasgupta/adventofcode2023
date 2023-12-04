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

let updateCardCounts startIndex endIndex currentCardCount (cards: seq<int * int>) =
    cards
    |> Seq.mapi (fun index (matchCount, cardCount) ->
        if index >= startIndex && index <= endIndex then
            (matchCount, currentCardCount + cardCount)
        else
            (matchCount, cardCount))

let rec getUpdatedCards index (cards: seq<int * int>) =
    let total = Seq.length cards

    if index = total then
        cards
    else
        let (currentMatchCount, currentCardCount) = Seq.item index cards
        let startIndex = index + 1
        let endIndex = Math.Min((index + currentMatchCount), total)

        updateCardCounts startIndex endIndex currentCardCount cards
        |> getUpdatedCards (index + 1)

let runPart1 filePath =
    Utils.readFile filePath
    |> Seq.choose (fun x ->
        let winningNumbers, myNumbers = getNumbers x
        let matches = getCountofMatches winningNumbers myNumbers

        match matches with
        | 0 -> None
        | n -> Some(Math.Pow(2, float (n - 1)) |> int))
    |> Seq.sum

let runPart2 filePath =
    Utils.readFile filePath
    |> Seq.map (fun x ->
        let winningNumbers, myNumbers = getNumbers x
        let countOfMatches = getCountofMatches winningNumbers myNumbers
        countOfMatches, 1)
    |> getUpdatedCards 0
    |> Seq.map snd
    |> Seq.sum




runPart1 "Inputs/Day4/Input_Part1.txt" |> printfn "Total Points : %d"
runPart2 "Inputs/Day4/Input_Part2.txt" |> printfn "Total Cards : %d"
