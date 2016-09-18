//This is an adapted version of Multiset
//taken by an Open-Source library.

//I found the idea of Multiset very appealing for making easier
//the building of the probability table.

module Multiset

/// Immutable multisets. Elements are ordered by F# generic comparison.
    type Multiset<'T when 'T : comparison> = Map<'T, uint32>

    let empty : Multiset<'T> =
        Map.empty

    let isEmpty (set : Multiset<'T>) =
        Map.isEmpty set

    let count (set : Multiset<'T>) : int64 =
        (0L, set)
        ||> Map.fold (fun count _ cardinality ->
            Checked.(+) (int64 cardinality) count)

    let contains value (set : Multiset<'T>) =
        Map.containsKey value set

    let card value (set : Multiset<'T>) =
        set
        |> Map.tryFind value
        |> (fun x -> if x.IsNone then 0u else x.Value)

    let add value (set : Multiset<'T>) : Multiset<'T> =
        match Map.tryFind value set with
        | None ->
            Map.add value 1u set
        | Some card ->
            Map.add value (Checked.(+) card 1u) set

    let remove value (set : Multiset<'T>) : Multiset<'T> =
        match Map.tryFind value set with
        | None ->
            set
        | Some card ->
            if card = 1u then
                Map.remove value set
            else
                Map.add value (card - 1u) set
