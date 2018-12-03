// Input
open System.IO
open System

let inputFile fileName = Path.Combine(__SOURCE_DIRECTORY__,@"./../input/", fileName)
let input =
    File.ReadAllLines(inputFile "3a.txt")
    |> Array.toList

type Area = {Id : int; OffsetLeft : int ; OffsetTop : int; Width : int; Height : int}
with
    static member SquareInches x = x.Width * x.Height
    static member Coordinates x =
        [|for i in [1..x.Width] do
            for j in [1..x.Height] do
                yield (x.OffsetLeft+i, x.OffsetTop+j)|]
let parseArea (s : string) =
    //Format: #1 @ 662,777: 18x27
    let spaces = s.Split(' ')
    let id = spaces.[0].Replace("#","") //.Replace(" ","")
    let offsets = spaces.[2].Split(',') |> Array.map (fun i -> i.Replace(":",""))
    let sizes = spaces.[3].Split('x')
    {Id = id |> int; OffsetLeft = offsets.[0] |> int; OffsetTop = offsets.[1] |> int; Width = sizes.[0] |> int; Height = sizes.[1] |> int}

let areas = input |> List.map parseArea

let areasArr = areas |> List.toArray

#time
let answerA =
    [|
        for a in areas do
            yield! Area.Coordinates a
    |]
    |> Array.groupBy id
    |> Array.sumBy (fun (c, v) -> if v.Length >= 2 then 1 else 0)

let answerB =
    [|
        for a in areas do
            for c in Area.Coordinates a do
                yield (a.Id, c)
    |]
    |> Array.groupBy (fun (id, c) -> c)
    |> Array.filter (fun (c, v) -> v.Length = 1) // Where coordinates do not overlap
    |> Array.map (fun (coord, rest) ->
        let id = fst rest.[0] // Length is 1
        (id, coord)
    )
    |> Array.groupBy fst // Group by Id
    |> Array.map (fun (id, coords) -> (id, coords.Length)) // Length of non overlapping coordinates for a given Id
    |> Array.find (fun (id, length) -> areasArr.[id-1] |> Area.Coordinates |> Array.length |> (=) length )
    |> fst