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
