open System.IO
open System

module Solution =
    open System.Diagnostics

    let run name f input =
        let sw = Stopwatch.StartNew()
        let result = f input
        let elapsedMs = sw.ElapsedMilliseconds
        printfn "%s completed in %dms with result: %A" name elapsedMs result

let readFile (filePath: string) =
    seq {
        use streamReader = new StreamReader(filePath)

        while not streamReader.EndOfStream do
            yield streamReader.ReadLine()
    }

let readFileAsString (filePath: string) : string = File.ReadAllText(filePath)

let parseInt (x: string) =
    match Int32.TryParse(x) with
    | true, n -> Some n
    | _ -> None

type Range = { Start: int64; End: int64 }

module Range =
    let isWithinRange range id = id >= range.Start && id <= range.End

    let build (start: int64) (rangeLength: int64) =
        { Start = start
          End = start + rangeLength - 1L }

    let tryFindIntersect (range1: Range) (range2: Range) : Range option =
        let start = Math.Max(range1.Start, range2.Start)
        let ``end`` = Math.Min(range1.End, range2.End)

        if start > ``end`` then
            None
        else
            Some { Start = start; End = ``end`` }

    let mapRange (range1: Range) (range2: Range) =
        match tryFindIntersect range1 range2 with
        | None -> Some range1, None, None
        | Some intersect ->
            let left =
                if range1.Start < intersect.Start then
                    Some
                        { Start = range1.Start
                          End = intersect.Start - 1L }
                else
                    None

            let right =
                if intersect.End < range1.End then
                    Some
                        { Start = intersect.End + 1L
                          End = range1.End }
                else
                    None

            left, Some intersect, right

module String =
    let endsWith (suffix: string) (str: string) = str.EndsWith suffix
