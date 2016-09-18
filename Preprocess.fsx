//Preprocessor

open System.Text
open System.Text.RegularExpressions
open System.IO

//The preprocessor compiles a JSON file of the PennTreeBank dataset.
//Think of the JSON file as a big array of pairs of the form (sentence, tagged sentence)

#load "Core.fs"
#load "Json.fsx"

//Directory containing the PennTreeBank corpora
let dir = ".\WSJ-2-12"
//Where to save the output
let output = ".\Model\WSJ-Preprocessed.json"

//This procedure given a *.POS file returns a new string where each sentence is delimited by
//the newline control character.
let clean text =
    //Remove chunking
    let f2 = Regex.Replace(text, "[\\[\\]]", " ")
    //Trim consecutive whitespaces
    let f3 = Regex.Replace(f2, "\\s+", " ")
    //Attempt to map each sentence in a single line
    let f4 = Regex.Replace(f3, "\\s+=+(.*?)=+\\s+", "$1\r\n")
    //The Penn TreeBank dataset is broken, in below an attempt to find sentences delimited by a full stop
    let f5 = Regex.Replace(f4, "(.*?)\\./\\.\\s*", "<start> $1 ./. <end>\r\n").Trim()
    //Only considers complete sentences with a full stop, may discard some sentences but not a big deal
    let f6 = Regex.Matches(f5, "<start>(.*?)<end>")
    [for m in f6 do yield m.Groups.Item(1).Value] |> Seq.reduce (fun a b -> a + "\r\n" + b)

//This contains all the sentences present in the corpora!
let sentences = 
    //Obtains every subdirectory of the main directory holding the corpora
    Directory.GetDirectories(dir)
    //For each subdirectory obtains a list of files
    |> Seq.map Directory.GetFiles
    //Concatenates the list of files
    |> Seq.concat
    //For each file let's obtain a string with each line containing a sentence
    |> Seq.map (File.ReadAllText >> clean)
    //Let's merge all of them
    |> Seq.reduce (+)

//This is the final object that our JSON file is going to store
let dataset = 
    //Given an entry
    let entry x = 
        let parsed = Core.Parse x
        (Seq.map (fun (w, _) -> w) parsed |> Seq.reduce (fun a b -> a + " " + b), parsed)
    Core.Lines sentences
    |> Seq.map entry
    |> Seq.toArray

//Serialize it.
JSON.serialize dataset output