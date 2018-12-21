open System
open System.IO

let inputFile fileName = Path.Combine(__SOURCE_DIRECTORY__,@"./../input/", fileName)
let input =
    File.ReadAllLines(inputFile "10.txt")
    |> Array.toList

type XY = {X : int; Y : int}
type Point = {Position : XY ; Velocity : XY}
with
    static member step k (p : Point) =
        { p with
            Position =
            { X = p.Position.X + p.Velocity.X*k
              Y = p.Position.Y + p.Velocity.Y*k } }
    
    static member stepAll k (ps : Point seq) = Seq.map (Point.step k) ps

    static member instersectsIn (p1 : Point) (p2 : Point) =
        let x =
            try
                let kx = (p1.Position.X-p2.Position.X)/(p2.Velocity.X-p1.Velocity.X)
                Some kx
            with
                | e -> None
        let y =
            try
                let ky = (p1.Position.Y-p2.Position.Y)/(p2.Velocity.Y-p1.Velocity.Y)
                Some ky
            with
                | e -> None
        x,y

let sortPoints (ps : Point seq) =
    query {
        for p in ps do
        sortBy p.Position.Y
        thenBy p.Position.X
        select p
    }

let parse (s : string) =
    //position=< 9,  1> velocity=< 0,  2>
    let sp = s.Split(',')

    let p1 = sp.[0].Split([|"position=<"|],StringSplitOptions.None).[1] |> int
    let p2 = sp.[1].Split('>').[0] |> int
    
    let v1 = sp.[1].Split('<').[1] |> int
    let v2 = sp.[2].Split('>').[0] |> int
    { Position =
        { X = p1
          Y = p2 }
      Velocity =
        { X = v1
          Y = v2 } }

let points = 
    input
    |> List.map parse
    |> List.toSeq
    |> sortPoints

let timesOfInterest =
    seq {
        for i in points do
            for j in points do
                match Point.instersectsIn i j with
                | Some _, Some ky -> yield ky // Y part probably most interesting for text
                | _, Some ky -> yield ky
                | _ -> ()
    }
    |> Seq.distinct
    |> Seq.toList
    |> List.sort

timesOfInterest.Length
open System.Text
let printSortedPoints (ps : Point array) =
    let minY = ps |> Array.minBy (fun p -> p.Position.Y) |> (fun s -> s.Position.Y)
    let maxY = ps |> Array.maxBy (fun p -> p.Position.Y) |> (fun s -> s.Position.Y)

    let minX = ps |> Array.minBy (fun p -> p.Position.X) |> (fun s -> s.Position.X)
    let maxX = ps |> Array.maxBy (fun p -> p.Position.X) |> (fun s -> s.Position.X)

    let sb = StringBuilder()

    for y in [minY..maxY] do
        for x in [minX..maxX] do
            match ps |> Array.tryFind (fun p -> p.Position.X = x && p.Position.Y = y) with
            | Some _ -> sb.Append("#") |> ignore
            | None -> sb.Append(".") |> ignore
        sb.AppendLine() |> ignore
    sb.ToString()

let checkCandidates () =
    for i in timesOfInterest do
        let ps = Seq.map (Point.step i) points
        let s = printSortedPoints (ps |> Seq.toArray)
        printfn "%s" s
        Console.ReadLine() |> ignore

// The solution seems to be unique in that all corners were filled

let allCornersFilled (ps : Point seq) =
    let minY = ps |> Seq.minBy (fun p -> p.Position.Y) |> (fun s -> s.Position.Y)
    let maxY = ps |> Seq.maxBy (fun p -> p.Position.Y) |> (fun s -> s.Position.Y)

    let minX = ps |> Seq.minBy (fun p -> p.Position.X) |> (fun s -> s.Position.X)
    let maxX = ps |> Seq.maxBy (fun p -> p.Position.X) |> (fun s -> s.Position.X)
    let cornerSum =
        [for y in [minY ; maxY] do
            for x in [minX; maxX] do
                match ps |> Seq.tryFind (fun p -> p.Position.X = x && p.Position.Y = y) with
                | Some _ -> yield 1
                | None -> yield 0]
        |> List.sum
    cornerSum = 4

let answerA =
    timesOfInterest
    |> List.map (fun i -> points |> Seq.map (Point.step i))
    |> List.find allCornersFilled
    |> Seq.toArray
    |> printSortedPoints

printfn "%A" answerA
let answerB =
    timesOfInterest
    |> List.map (fun i -> i,points |> Seq.map (Point.step i))
    |> List.find (snd >> allCornersFilled)
    |> fst

