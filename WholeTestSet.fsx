//Testing with no unknown words

printfn "%s" "100% Dataset Testing"

(*
OUTPUT:
100% Dataset Testset
0.971 accuracy
*)

#load "Core.fs"
#load "Json.fsx"
#load "Training.fsx"

open Core

let dataset : Corpora = JSON.deserialize "./Model/WSJ-Preprocessed.json"
let tagger = 
    Training.trainModel dataset
    |> Training.Viterbi

let matching =
    dataset
    |> Seq.map (fun (sentence, target) -> Training.Match (tagger sentence) target )
    |> Seq.reduce (fun a b -> fst a + fst b, snd a + snd b)

let accuracy = (fst matching |> float) / (snd matching |> float)

printfn "%.3f accuracy" accuracy

System.Console.ReadKey()
