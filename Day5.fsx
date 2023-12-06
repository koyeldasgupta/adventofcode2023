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

module Part1 =
    let rec findLocation (maps: Map list list) sourceId =
        match maps with
        | [] -> sourceId
        | first :: rest ->
            let destinationId =
                first
                |> List.tryFind (fun x -> Range.isWithinRange x.Source sourceId)
                |> function
                    | Some map -> map.Destination.Start + (sourceId - map.Source.Start)
                    | None -> sourceId

            findLocation rest destinationId

    let run filePath =
        let (seeds, maps) = getMappedInput filePath
        seeds |> List.map (findLocation maps) |> List.min


module Part2 =
    let rec findDestinationMaps (source: Range option) (maps: Map list) (destinations: Range list) =
        match source, maps with
        | None, _ -> destinations
        | Some source, [] -> [ source ] @ destinations
        | Some source, first :: rest ->
            let left, intersect, right = Range.mapRange source first.Source

            match intersect with
            | Some r ->
                let startGap = r.Start - first.Source.Start
                let endGap = first.Source.End - r.End

                [ { Start = first.Destination.Start + startGap
                    End = first.Destination.End - endGap } ]
                @ destinations
            | None -> destinations
            |> findDestinationMaps left rest
            |> findDestinationMaps right rest

    let rec findLocation (maps: Map list list) (source: Range list) =
        match maps with
        | [] -> source
        | first :: rest ->
            let destinationMaps =
                source |> List.collect (fun x -> findDestinationMaps (Some x) first [])

            findLocation rest destinationMaps


    let run filePath =
        let (seeds, maps) = getMappedInput filePath

        let seedRanges =
            seeds |> List.chunkBySize 2 |> List.map (fun x -> Range.build x[0] x[1])

        findLocation maps seedRanges |> List.map (fun x -> x.Start) |> List.min



Solution.run "Day5 Part1" Part1.run "Inputs/Day5/Part1.txt"
//Day5 Part1 completed in 8ms with result: 600279879L


Solution.run "Day5 Part2" Part2.run "Inputs/Day5/Part2.txt"
//Day5 Part2 completed in 3ms with result: 20191102L
