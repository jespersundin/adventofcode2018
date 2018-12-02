// Input
open System.IO

let inputFile fileName = Path.Combine(__SOURCE_DIRECTORY__,@"./../input/", fileName)
let input =
    File.ReadAllLines(inputFile "1a.txt")
    |> Array.map int
    |> Array.toList

// 1a
let answerA = List.sum input

// 1b
let neverEndingFrequencies =
    seq {
        let mutable acc = 0
        while true do
            for i in input do
                acc <- acc + i
                yield acc
    }

let answerB =
    // Sequence of (``Set of observed frequencies so far`` * ``Current frequency is in this set`` * ``Current frequency``)
    let runningObservedFrequencies =
        neverEndingFrequencies
        |> Seq.scan (fun (accSet, _, _) currentFrequency ->
            (Set.add currentFrequency accSet, Set.contains currentFrequency accSet, currentFrequency)
        ) (Set.empty, false, 0)

    let (frequenciesSeenBefore, _, answer) = runningObservedFrequencies |> Seq.find (fun (_,b,_) -> b)
    answer


