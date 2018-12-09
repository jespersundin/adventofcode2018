open System
open System.IO
// let input = "dabAcCaCBAcCcaDA"
let inputFile fileName = Path.Combine(__SOURCE_DIRECTORY__,@"./../input/", fileName)
let input =
    File.ReadAllText(inputFile "5.txt").ToCharArray()
    |> Array.filter Char.IsLetter
    |> Array.toList

let (|SameType|_|) (l : char, r : char) =
    match (Char.ToLower l) = (Char.ToLower r) with
    | true -> Some (l, r)
    | false -> None

let (|Upper|Lower|) (c : char) =
    match Char.IsUpper c with
    | true -> Upper
    | false -> Lower

let (|Reaction|NoReaction|) (l : char, r : char) =
    match (l,r) with
    | SameType (Upper, Lower) -> Reaction
    | SameType (Lower, Upper) -> Reaction
    | _ -> NoReaction

let removeReactions (c : char list) =
    
    let rec inner (x : char list) (acc : char list)=
        match x with
        | l :: r :: xs ->
            match (l,r) with
            | Reaction -> inner xs acc
            | NoReaction -> inner (r :: xs) (l :: acc)
        | [l] -> (l :: acc)
        | [] -> acc
    
    inner c []
    |> List.rev

let chainReactions (c : char list) =
    let mutable current = c
    let mutable loop = true
    while loop do
        let candidate = removeReactions current
        if candidate.Length = current.Length then
            loop <- false
        else
            current <- candidate
    current
let answerA =
    chainReactions input // works easily up tp 200
    |> List.length


let removeAllOccurences (cl : char list) (c : char)=
    cl
    |> List.filter (fun i -> (Char.ToLower i) <> c)
let alphabet = [
    'a'
    'b'
    'c'
    'd'
    'e'
    'f'
    'g'
    'h'
    'i'
    'j'
    'k'
    'l'
    'm'
    'n'
    'o'
    'p'
    'q'
    'r'
    's'
    't'
    'u'
    'v'
    'w'
    'x'
    'y'
    'z'
]


let answerB =
    alphabet
    |> List.map ((removeAllOccurences input) >> (chainReactions >> List.length))
    |> List.min

// Pretty slow solution for asnwerB