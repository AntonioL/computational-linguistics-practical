//Based on some code found on StackOverflow for serializing
//an instance of a data structure into a JSON file.

//Used for storing the corpora plain and tagged sentences.
//Used to exploit the interactive programming interface so to avoid having
//to reparse the whole file countless of times.

module JSON

open System.IO

#r "System.Runtime.Serialization.dll"
open System.Runtime.Serialization.Json

//A function that takes an object to serialize and the file path
//in which to store that.
let serialize<'a> (x : 'a) filePath = 
    let jsonSerializer = new DataContractJsonSerializer(typedefof<'a>)
    use stream = new MemoryStream()
    jsonSerializer.WriteObject(stream, x)
    File.WriteAllBytes(filePath, stream.ToArray())

//A function that given a JSON file storing a serialized object
//returns the corresponding object.
let deserialize<'a> filePath =
    let jsonSerializer = new DataContractJsonSerializer(typedefof<'a>)
    use fs = File.OpenRead(filePath)
    let obj = jsonSerializer.ReadObject(fs) :?> 'a
    fs.Close |> ignore
    obj