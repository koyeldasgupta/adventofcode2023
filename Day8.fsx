#load "./Utils.fsx"

open System.Text.RegularExpressions
open Utils
open System.Collections.Generic

type Node = { Left: string; Right: string }


let parseNode line =
    let matches = Regex.Matches(line, @"\w+") |> Seq.toList

    matches[0].Value,
    { Left = matches[1].Value
      Right = matches[2].Value }

let parseInput filePath =
    let content = readFile filePath
    let instructions = content |> Seq.item 0 |> (fun s -> s.Trim())
    let nodes = content |> Seq.skip 2 |> Seq.map parseNode |> dict
    instructions, nodes

let getStepsCount (instructions: string) (nodes: IDictionary<string, Node>) stop currentNodeKey =

    let rec navigate (nodeKey: string) (counter: int) =
        if stop nodeKey then
            counter
        else
            let success, current = nodes.TryGetValue(nodeKey)

            let next =
                match success, instructions[counter % instructions.Length] with
                | true, 'L' -> current.Left
                | true, 'R' -> current.Right
                | _, _ -> failwith "Failed to find node"

            navigate next (counter + 1)

    navigate currentNodeKey 0


module Part1 =

    let run filePath =
        let instructions, nodes = parseInput filePath
        getStepsCount instructions nodes ((=) "ZZZ") "AAA"

module Part2 =
    let rec gcd a b = if b = 0L then a else gcd b (a % b)

    let lcm a b =
        if a = 0L || b = 0L then 0L else abs (a * b) / gcd a b


    let run filePath =
        let instructions, nodes = parseInput filePath

        nodes.Keys
        |> Seq.filter (String.endsWith "A")
        |> Seq.map (getStepsCount instructions nodes (String.endsWith "Z") >> int64)
        |> Seq.fold lcm 1L

Solution.run "Day8 Part1" Part1.run "Inputs/Day8/Part1.txt"
//Day8 Part1 completed in 4ms with result: 14893

Solution.run "Day8 Part2" Part2.run "Inputs/Day8/Part2.txt"
//Day8 Part2 completed in 15ms with result: 10241191004509
