
type Coord = {X : int; Y : int}
with
    static member FromTuple (x,y) = {X = x; Y = y}
    static member Distance c1 c2 = (abs (c1.X - c2.X)) + (abs(c1.Y - c2.Y))

// let input =
//     [
//         1, 1
//         1, 6
//         8, 3
//         3, 4
//         5, 5
//         8, 9
//     ]
//     |> List.map Coord.FromTuple
let input =
    [
        357, 59
        312, 283
        130, 47
        89, 87
        87, 58
        158, 169
        182, 183
        300, 318
        82, 257
        200, 194
        71, 259
        112, 67
        82, 163
        107, 302
        58, 194
        40, 88
        288, 339
        64, 245
        243, 302
        41, 43
        147, 276
        143, 116
        103, 178
        262, 226
        253, 157
        313, 71
        202, 236
        353, 192
        96, 74
        167, 50
        125, 132
        90, 315
        174, 232
        185, 237
        126, 134
        152, 191
        104, 315
        283, 90
        95, 193
        252, 286
        48, 166
        69, 75
        48, 349
        59, 124
        334, 95
        263, 134
        50, 314
        196, 66
        342, 221
        60, 217
    ]
    |> List.map Coord.FromTuple

let (mainGrid, perim) =
    let padding = 1
    let minX =
        input |> List.minBy (fun c -> c.X)
    let minY =
        input |> List.minBy (fun c -> c.Y)
    let maxX =
        input |> List.maxBy (fun c -> c.X)
    let maxY =
        input |> List.maxBy (fun c -> c.Y)
    
    [for i in [(minX.X-padding)..(maxX.X+padding)] do
        for j in [(minY.Y-padding)..(maxY.Y+padding)] do
            yield (i,j) |> Coord.FromTuple]
    ,
    [
        yield! [for i in [(minX.X-padding)..(maxX.X+padding)] do
                yield (i,minY.Y) |> Coord.FromTuple]
        yield! [for i in [(minX.X-padding)..(maxX.X+padding)] do
                yield (i,maxY.Y) |> Coord.FromTuple]
        yield! [for i in [(minY.Y-padding)..(maxY.Y+padding)] do
                yield (minX.X,i) |> Coord.FromTuple]
        yield! [for i in [(minY.Y-padding)..(maxY.Y+padding)] do
                yield (maxX.X,i) |> Coord.FromTuple]
    ]

type NamedCoord = {Id : int; Coord : Coord}
let inputCoords = input |> List.mapi (fun i c -> {Id = i; Coord = c})

let coordsClosest grid =
    [for i in grid do
        let minDistances =
            inputCoords
            |> List.map (fun c -> c, Coord.Distance i c.Coord) // Cord * distance
            |> List.groupBy snd
            |> List.minBy fst
            |> snd
        let d =
            match minDistances.Length with
            | 1 -> minDistances.[0] |> fst |> Some
            | _ -> None
        yield i,d
    ]

let coordsWithSumOfDistances grid =
    [for i in grid do
        let d = inputCoords |> List.sumBy (fun c -> Coord.Distance i c.Coord)
        yield (i,d)
    ]

let closestCoordGrid = coordsClosest mainGrid
let infinitePoints =
    coordsClosest perim
    |> List.filter (fun (c, l) -> l.IsSome)
    |> List.distinctBy (fun (c,l) ->l.Value.Id)
    |> List.map (fun (c,l) -> l.Value)

let finiteIds =
    inputCoords
    |> List.filter (fun c -> List.contains c infinitePoints |> not)
    |> List.map (fun c -> c.Id)

let finiteDistances =
    closestCoordGrid
    |> List.filter (fun (c,l) ->
        match l with
        | Some nc -> List.contains nc.Id finiteIds
        | None -> false)
    |> List.map (fun (c,l) -> c,l.Value)

let answerA =
    finiteDistances
    |> List.groupBy (fun (c,l) -> l.Id)
    |> List.map (fun (c,l) -> c, l |> List.length)
    |> List.maxBy snd
    |> snd

let answerB =
    coordsWithSumOfDistances mainGrid
    |> List.filter (fun (c,d) -> d < 10000)
    |> List.length

