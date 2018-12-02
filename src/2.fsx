// Input
open System.IO
open System

let inputFile fileName = Path.Combine(__SOURCE_DIRECTORY__,@"./../input/", fileName)
let input =
    File.ReadAllLines(inputFile "2a.txt")
    |> Array.toSeq

let uniqueLetterFrequencies (s : string) =
    s.ToCharArray()
    |> Array.countBy id
    |> Array.map snd
    |> Set.ofArray

type ZeroOrOne = int

let toZeroOrOne b : ZeroOrOne = if b then 1 else 0

[<Struct>]
type BoxIdInfo = {ExactlyTwo : ZeroOrOne; ExactlyThree : ZeroOrOne}
with
    static member CreateFromString s =
        let letterFreqs = uniqueLetterFrequencies s
        {
            ExactlyTwo = letterFreqs.Contains(2) |> toZeroOrOne
            ExactlyThree = letterFreqs.Contains(3)|> toZeroOrOne
        }
    static member Zero = { ExactlyTwo = 0; ExactlyThree = 0}
    static member (+) (l, r) = {ExactlyTwo = l.ExactlyTwo + r.ExactlyTwo; ExactlyThree = l.ExactlyThree + r.ExactlyThree}

let answerA =
    let i = 
        Seq.sumBy BoxIdInfo.CreateFromString input
    i.ExactlyThree * i.ExactlyTwo


// 2b

let lettersNotInCommon (s1 : string) (s2 : string) =
    let (a1, a2) = s1.ToCharArray(), s2.ToCharArray()

    Array.zip a1 a2
    |> Array.fold (fun acc (l,r) -> acc + (if l = r then 0 else 1)) 0

let answerB =
    let (_, firstId, secondID) =
        seq {
            for i in input do
                for j in input do
                    yield (lettersNotInCommon i j, i, j)
        }
        |> Seq.find (fun (l, i, j) -> l = 1)
    let (a1, a2) = firstId.ToCharArray(), secondID.ToCharArray()
    let diffPos =
        Array.zip a1 a2
        |> Array.findIndex (fun (i,j) -> i <> j)
    firstId.Remove(diffPos,1)