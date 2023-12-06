open System.IO
open System

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
