module Core

open System.Text.RegularExpressions

let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

let (|Match|_|) string input =
    if string = input then Some()
    else None

open System.IO

let Lines = File.ReadLines

let Term str =
    match str with
    | Match "<start>" -> Some([|"<start>"|])
    | Match "<end>" -> Some([|"<end>"|])
    | Regex "(\\w+)/(\\w+)" [a; b] -> Some([|a; b|])
    | _ -> None

let Parse sentence =
    Regex.Split(sentence, "\\s+")
    |> Seq.choose Term