//Corpora parsing facilities

module Core

open System.IO
open System.Text
open System.Text.RegularExpressions

//A tagged word is a pair (word, part of speech tag)
type PosTaggedWord = string * string
//A corpora is a sequence of pairs (sentence, tagged sentence)
type Corpora = (string * seq<PosTaggedWord>) []

//This allows us to build neat matching rules 
//involving regular expressions
//thus providing a nice API.
let (|Regex|_|) pattern input =
        let m = Regex.Match(input, pattern)
        if m.Success then Some(List.tail [ for g in m.Groups -> g.Value ])
        else None

//A function that returns each token in the sentence
let Tokenize sentence =
    Regex.Split(sentence, "\\s+")

//Given a term delimited by the "/" separator returns a tagged pair.
let Term str : Option<PosTaggedWord> =
    match str with
    | Regex "(.*)/(.+)" [a; b] -> Some(a, b)
    | _ -> None

//That's all folks!
//Where the magic happens
//The function to parse a sentence returning the tagged words of the sentence.
let Parse sentence =
    sentence
    |> Tokenize
    |> Seq.choose Term

//Programatically obtained part-of-speech tags from the corpora
//used to speed up some computations.
//P.S. There is the special tag <start> added.
let tagsets = [|"#"; "$"; "''"; "("; ")"; ","; "."; ":"; "<start>"; "CC"; "CD"; "DT"; "EX"; "FW"; "IN"; 
             "IN|RB"; "JJ"; "JJR"; "JJS"; "JJ|NN"; "JJ|NNP"; "JJ|RB"; "LS"; "MD"; "MD|VB"; "NN"; "NNP"; "NNPS"; 
             "NNS"; "NNS|NN"; "NNS|VBZ"; "NN|CD"; "NN|JJ"; "NN|NNS"; "NN|VBG"; "PDT"; "POS"; "PRP"; "PRP$"; "RB"; 
             "RBR"; "RBR|JJR"; "RBS"; "RBS|JJS"; "RB|DT"; "RB|IN"; "RB|JJ"; "RB|RP"; "RP"; "SYM"; "TO"; "UH"; "VB"; "VBD";
             "VBD|VBP"; "VBG"; "VBG|JJ"; "VBG|NN"; "VBG|NN|JJ"; "VBN"; "VBN|JJ"; "VBN|VBD"; "VBP"; "VBP|VB"; "VBP|VBD"; 
             "VBZ"; "VB|IN"; "VB|NN"; "WDT"; "WP"; "WP$"; "WRB"; "``" |]

//I/O Facilities
open System.IO

//Given a string returns a sequence containing its lines.
let Lines (content:string) = seq {
    let ascii = new ASCIIEncoding()
    use sr =
        content
        |> ascii.GetBytes
        |> MemoryStream
        |> StreamReader
    while not sr.EndOfStream do
        yield sr.ReadLine ()
}
