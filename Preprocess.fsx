open System.Text
open System.Text.RegularExpressions
open System.IO

//The preprocessor creates one big data file containing all the sentences.
//Each line of the file corresponds to a single sentence

let dir = ".\WSJ-2-12"
let output = "WSJ-Processed.txt"

let directories dir = Directory.GetDirectories(dir)
let files dir = Directory.GetFiles(dir)

let clean text =
    let f2 = Regex.Replace(text, "[\\[\\]]", " ")
    let f3 = Regex.Replace(f2, "\\s+", " ")
    let f4 = Regex.Replace(f3, "\\s+=+(.*?)=+\\s+", "$1\r\n")
    Regex.Replace(f4, "\\./\\.\\s*", " ./.\r\n").Trim()

let processed = 
    Directory.GetDirectories(dir)
    |> Seq.map Directory.GetFiles
    |> Seq.concat
    |> Seq.map (File.ReadAllText >> clean)
    |> Seq.reduce (fun a b -> if b.Length < 3 then a else a + b)

File.WriteAllText(output, processed)