#load "./Utils.fsx"

open System.Text.RegularExpressions
open Utils

type Map = { Source: Range; Destination: Range }

let parseMap (line: string) : Map list =

    Regex.Matches(line, @"\d+")
    |> Seq.map (fun x -> int64 x.Value)
    |> Seq.chunkBySize 3
    |> Seq.toList
    |> List.map (function
        | [| destinationStart; sourceStart; range |] ->
            { Source =
                { Start = sourceStart
                  End = (sourceStart + range - 1L) }
              Destination =
                { Start = destinationStart
                  End = destinationStart + range - 1L } }
        | _ -> failwith "Invalid map format")

let getMappedInput filePath =
    let content = Utils.readFileAsString filePath

    let seeds, maps =
        content.Trim().Split("\n\n")
        |> Array.toList
        |> function
            | first :: rest ->
                let seeds =
                    Regex.Matches(first, @"\d+")
                    |> Seq.map (fun matchResult -> int64 matchResult.Value)
                    |> Seq.toList

                let maps = List.map parseMap rest
                (seeds, maps)
            | _ -> ([], [])

    (seeds, maps)

let rec findLocation sourceId (maps: Map list list) =
    match maps with
    | [] -> sourceId
    | first :: rest ->
        let destinationId =
            first
            |> List.tryFind (fun x -> Range.isWithinRange x.Source sourceId)
            |> function
                | Some map -> map.Destination.Start + (sourceId - map.Source.Start)
                | None -> sourceId

        findLocation destinationId rest

let runPart1 filePath =
    let (seeds, maps) = getMappedInput filePath
    seeds |> List.map (fun x -> findLocation x maps) |> List.min


let runPart2 filePath =
    let (seeds, maps) = getMappedInput filePath

    seeds |> List.chunkBySize 2 |> List.map (fun x -> Range.build x[0] x[1])



// Solution.run "Day5 Part1" runPart1 "Inputs/Day5/Part1.txt"
// Day5 Part1 completed in 9ms with result: 600279879L

Solution.run "Day5 Part2" runPart2 "Inputs/Day5/Example.txt"
