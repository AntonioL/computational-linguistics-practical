//K-Fold validation

printfn "%s" "K-FOLD Validation"

(*
OUTPUT:
K-FOLD Validation
1 run
0.933 accuracy
2 run
0.941 accuracy
3 run
0.939 accuracy
4 run
0.945 accuracy
5 run
0.941 accuracy
6 run
0.947 accuracy
7 run
0.945 accuracy
8 run
0.944 accuracy
9 run
0.935 accuracy
10 run
0.938 accuracy

Overall accuracy
0.941
*)


#load "Core.fs"
#load "Json.fsx"
#load "Training.fsx"

open Core

let dataset : Corpora = JSON.deserialize "./Model/WSJ-Preprocessed.json"

//Returns a sequence of "N" equal-sized chunks (except probably the for the last one) 
//of the original sequence
let chunkify N sequence =
    let chunkSize = Seq.length sequence / N
    let rec build s =
        seq {
            yield Seq.take chunkSize s
            yield! build (Seq.skip chunkSize s)
        }
    build sequence |> Seq.take N

let chunks = chunkify 10 dataset

let accuracies = Array.create 10 0.0

for i in [0..9] do
    printfn "%d run" (i + 1)
    //The i-th chunk is going to be the test set
    let testset = Seq.nth i chunks
    //The trainset is comprised of every chunk except the i-th one
    let trainset =
        chunks
        |> Seq.mapi (fun y x -> if y = i then Seq.empty else x)
        |> Seq.concat
        |> Seq.toArray
    //Trains a model from the sentences present in the trainset
    let model = Training.trainModel trainset
    //This is the Tagger function built from the above learned model
    let tagger = Training.Viterbi model
    //A pair of the total number of correctly matched POS tags
    //And the total number of words
    let matching =
        testset
        |> Seq.map (fun (sentence, target) -> Training.Match (tagger sentence) target )
        |> Seq.reduce (fun a b -> fst a + fst b, snd a + snd b)
    let accuracy = (fst matching |> float) / (snd matching |> float)
    printfn "%.3f accuracy" accuracy
    accuracies.[i] <- accuracy

printfn "Overall accuracy"
printfn "%.3f" (Seq.average accuracies)

System.Console.ReadKey()
