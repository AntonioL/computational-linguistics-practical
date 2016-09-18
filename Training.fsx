//All the training facilities
module Training

#load "Core.fs"
#load "Multiset.fs"

open Core
open Multiset
open System.Text.RegularExpressions

//Generic facilities for a 2-Level matrix of counts indexed by a string pair
//Implemented with a Map with each entry holding a Multiset (a set where every element has a cardinality count)
let matrixAdd row column (matrix : Map<string, Multiset<string>>) =
    let row' = matrix.[row]
    Map.add row (Multiset.add column row') matrix

//Given a matrix containing counts, it returns a matrix of probabilities
let fromCountMatrixToProbMatrix (matrix : Map<string, Multiset.Multiset<string>> ) =
    let rowsTotalCardinality = Map.map (fun _ entries -> entries |> Map.toSeq |> Seq.map (fun (_, i) -> i) |> Seq.sum) matrix
    Map.map (fun x entries -> Map.map (fun _ count -> (float count) / (float rowsTotalCardinality.[x])) entries) matrix

//Bigram count

//This is the default bigram count matrix where every pair has initial count of 1. (Laplace smoothing)
let defaultBigramMatrix =
    let smoothedRow = Core.tagsets |> Seq.fold (fun acc tag -> Multiset.add tag acc) Multiset.empty
    Seq.fold (fun map tag -> Map.add tag smoothedRow map) Map.empty Core.tagsets

//Given a sentence and a bigram count matrix returns the update bigram count matrix
let updateBigram bigramMatrix sentence =
    sentence
    |> Seq.map snd //Get the Part of Speech tag of the words
    |> Seq.append [| "<start>" |] //Prepends the <start> tag
    |> Seq.pairwise //Generates a sequence of tuples of consecutive tags (makes easier computing c(w-1, w) ) 
    |> Seq.fold (fun tbl (before, next) -> matrixAdd before next tbl) bigramMatrix //Increase by one the count

//Word Tag Count

//Default count matrix of tags|words, each tag entry holds an empty Multiset containing word
let (defaultWordTagCount : Map<string, Multiset.Multiset<string>>) =
    Core.tagsets |> Seq.map (fun x -> (x, Multiset.empty)) |> Map.ofSeq

//Given a count matrix of tags|words and a sentence it returns the update matrix
let updateWordTagCount matrix sentence =
    let updater mtx (word, pos) = matrixAdd pos word mtx
    sentence |> Seq.fold updater matrix

//The training model

//A Model is comprised of a pair of two probabilities table in spirit of HMM:
//  1. Probability of each bigram
//  2. Probability of a word given a tag
type Model = Map<string,Map<string, float>> * Map<string,Map<string, float>>

//Given a corpora returns a Model
//This is the training step
let trainModel (sentences : Corpora) : Model =
    //The corpora is comprised of a list of pairs of the form (sentence, part-of-speech tagged sentence)
    let sentences' = Array.map snd sentences
    //Compute the probability table of the bigrams
    let bigramProbMatrix = sentences' |> Array.fold updateBigram defaultBigramMatrix |> fromCountMatrixToProbMatrix
    //Comput the probability table of words given tags
    let wordTagMatrix = sentences' |> Array.fold updateWordTagCount defaultWordTagCount |> fromCountMatrixToProbMatrix
    (bigramProbMatrix, wordTagMatrix) 

//Viterbi training
let Viterbi ((bigramProb, wordTagProb) : Model) sentence =
    let sequence = Tokenize sentence |> Seq.toArray
    let (N, K) = (Seq.length sequence, Seq.length tagsets)
    let Score = Seq.fold (fun acc tag -> Map.add tag ((Array.zeroCreate N) : float []) acc) Map.empty tagsets
    let Backpointer = Seq.fold (fun acc tag -> Map.add tag (Array.create N "") acc) Map.empty tagsets
    //Facilities for finding the tags for a given word
    let notFoundWordProbs = Seq.map (fun tag -> (tag, 1.0)) tagsets
    //Precompute a seq with probability 1.0 for every tag, needed for unknown words
    //A function for finding which tags could a word belonging to with its probability P(word|tag)
    let findTagsWord word =
        //Inspecting every tag in the table and picking only tags to which the word could belong to
        //Seq.choose discards is like the "map" function but retains only entries returning Some(x), 
        //thus discarding None (meaning that for that tag the word does not exists)
        let foundTagsProb = 
            Seq.choose (fun tag -> if wordTagProb.[tag].TryFind(word).IsSome then Some(tag, wordTagProb.[tag].[word]) else None) tagsets
        //If the word belongs to some tag then the sequence of found tags is not empty thus we simply return that...
        if Seq.length foundTagsProb > 0 then
            foundTagsProb
        else //...otherwise return a precomputed list of tags, each one with probability 1.0
            notFoundWordProbs
    //1. Initialisation
    sequence.[0]
    //For every tag to which the first can belong to...
    |> findTagsWord
    //...we update the score
    |> Seq.iter (fun (tag, wordProb) -> Score.[tag].[0] <- wordProb * bigramProb.["<start>"].[tag])
    //2. Induction
    //For every word in the sentence...
    for j in [1..N - 1] do
        //...find every tag to which the word can possibly belong to
        let tagsToUpdate = findTagsWord sequence.[j]
        for (tag, wordProb) in tagsToUpdate do
            //Find the best tag_k that maximizes Score(tag_k, j-1) × P(tag_i |tag_k)
            //It returns a pair of the form (tag, score)
            let bestPrevForTag = 
                Seq.map (fun theTag -> theTag, (Score.[theTag].[j-1] * bigramProb.[theTag].[tag]) ) tagsets
                |> Seq.maxBy snd
            Score.[tag].[j] <- (snd bestPrevForTag) * wordProb
            Backpointer.[tag].[j] <- fst bestPrevForTag
    //Backtracing
    let Tagged = Array.create N ""
    Tagged.[N - 1] <- Seq.maxBy (fun tag -> Score.[tag].[N-1]) tagsets
    try
        for i in [N-2 .. -1 .. 0] do
            Tagged.[i] <- Backpointer.[Tagged.[i+1]].[i+1]
    with
    //Debugging purposes: to detect sentences with underflowing problems
    | _ -> printfn "%s" sentence
    Tagged

//Valuation
//Given a tagged part of speech, and target pos tags returns a pair of the form:
//  (number of matching tags, total number of tags)
let Match (model_pos : seq<string>) (target_pos : seq<PosTaggedWord>) =
    (Seq.map2 (=) model_pos (target_pos |> Seq.map snd) |> Seq.filter id |> Seq.length, Seq.length model_pos)