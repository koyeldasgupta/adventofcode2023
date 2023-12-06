#load "./Utils.fsx"

open System.Text.RegularExpressions
open Utils

let getWaysToWinCount allowedTime recordDistance =
    let rec calculate holdTime count =
        if holdTime > allowedTime / 2L then
            count
        else
            let travelTime = allowedTime - holdTime
            let distanceTraveled = travelTime * holdTime

            let updatedCount =
                if distanceTraveled > recordDistance then
                    count + 1L
                else
                    count

            calculate (holdTime + 1L) updatedCount

    let waysCount = calculate 0L 0L

    if allowedTime % 2L = 0L then
        waysCount * 2L - 1L
    else
        waysCount * 2L

module Part1 =
    let parseInput filePath =
        let content = readFile filePath |> Seq.toList

        let time =
            Regex.Matches(content[0], @"\d+")
            |> Seq.map (fun m -> int64 m.Value)
            |> Seq.toList

        let distances =
            Regex.Matches(content[1], @"\d+")
            |> Seq.map (fun m -> int64 m.Value)
            |> Seq.toList

        (time, distances)


    let run filePath =
        let time, distances = parseInput filePath

        time
        |> List.mapi (fun i t -> getWaysToWinCount t distances[i])
        |> List.fold (fun acc x -> acc * x) 1L


module Part2 =
    let parseInput filePath =
        let content = readFile filePath |> Seq.toList
        let time = Regex.Replace(content[0], "\\D", "") |> int64
        let distance = Regex.Replace(content[1], "\\D", "") |> int64

        (time, distance)

    let run filePath =
        let time, distance = parseInput filePath
        getWaysToWinCount time distance


Solution.run "Day6 Part1" Part1.run "Inputs/Day6/Part1.txt"
//Day6 Part1 completed in 7ms with result: 288L


Solution.run "Day6 Part2" Part2.run "Inputs/Day6/Part2.txt"
//Day6 Part2 completed in 24ms with result: 29432455L
