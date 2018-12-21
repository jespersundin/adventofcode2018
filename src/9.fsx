// This approach worked for Part 1. Answer A took 1 minute. Circle 2 implementation took 1 second. None of these scaled to be doable in the second part.
type Circle =
    { Marbles : int list
      CurrentMarbleIndex : int }

type Circle2 =
    { Marbles2 : ResizeArray<int>
      CurrentMarbleIndex : int }

module Circle =

    let init = {Marbles = [0] ; CurrentMarbleIndex = 0}

    let currentMarble (c : Circle) = c.Marbles.[c.CurrentMarbleIndex]
    let insertMarbleBaseCase (m : int) (c : Circle) =
        let ml = c.Marbles.Length
        let newPos =
            match (c.CurrentMarbleIndex+2) % ml with
            | 0 -> ml
            | v -> v
        let (mBelow, mAbove) = List.splitAt newPos c.Marbles
        let newMarbles = List.append (List.append mBelow [m]) mAbove
        {Marbles = newMarbles ; CurrentMarbleIndex = newPos}

    let removeMarbleSpecialCase (c : Circle) =
        let ml = c.Marbles.Length
        let rmvPos =
            match (c.CurrentMarbleIndex-7) % ml with
            | v when v < 0 ->
                ml + v
            | v -> v
        let (below, above) = List.splitAt rmvPos c.Marbles
        let newMarbles = List.append below (above.[1..])
        {Marbles = newMarbles ; CurrentMarbleIndex = rmvPos}, c.Marbles.[rmvPos]

    let iteration (m : int) (c : Circle) = // New state and score of the current move
        match m % 23 with
        | 0 ->
            let (newCircle, rmvVal) = removeMarbleSpecialCase c
            newCircle, rmvVal + m
        | _ ->
            insertMarbleBaseCase m c, 0
    
    type GameState = {CurrentCircle : Circle ; CurrentScore : Map<int,int>}
    with
        static member Init = {CurrentCircle = init ; CurrentScore = Map.empty}
        static member updatePlayerScore player score g =
            let newScore =
                match Map.tryFind player g.CurrentScore with
                | Some currScore -> currScore + score
                | None -> score
            {g with CurrentScore = Map.add player newScore g.CurrentScore}

    let playGame players times =
        let g0 = GameState.Init

        [1..times]
        |> List.fold
            (fun prev marble ->
                let player =
                    match (marble % players) with
                    | 0 -> players
                    | v -> v
                let (newCircle, score) = iteration marble prev.CurrentCircle

                {prev with CurrentCircle = newCircle}
                |> GameState.updatePlayerScore player score
            )
            g0

module Circle2 =

    let init () =
        let m = {Marbles2 = ResizeArray<int>(1000000) ; CurrentMarbleIndex = 0}
        m.Marbles2.Add(0)
        m

    let currentMarble (c : Circle2) = c.Marbles2.[c.CurrentMarbleIndex]
    let insertMarbleBaseCase (m : int) (c : Circle2) =
        let ml = c.Marbles2.Count
        let newPos =
            match (c.CurrentMarbleIndex+2) % ml with
            | 0 -> ml
            | v -> v
        c.Marbles2.Insert(newPos,m)
        {c with CurrentMarbleIndex = newPos}

    let removeMarbles2pecialCase (c : Circle2) =
        let ml = c.Marbles2.Count
        let rmvPos =
            match (c.CurrentMarbleIndex-7) % ml with
            | v when v < 0 ->
                ml + v
            | v -> v
        let rmvVal = c.Marbles2.[rmvPos]
        c.Marbles2.RemoveAt(rmvPos)

        {c with CurrentMarbleIndex = rmvPos}, rmvVal

    let iteration (m : int) (c : Circle2) = // New state and score of the current move
        match m % 23 with
        | 0 ->
            let (newCircle2, rmvVal) = removeMarbles2pecialCase c
            newCircle2, rmvVal + m
        | _ ->
            insertMarbleBaseCase m c, 0
    
    type GameState = {CurrentCircle2 : Circle2 ; CurrentScore : Map<int,int>}
    with
        static member Init () = {CurrentCircle2 = init() ; CurrentScore = Map.empty}
        static member updatePlayerScore player score g =
            let newScore =
                match Map.tryFind player g.CurrentScore with
                | Some currScore -> currScore + score
                | None -> score
            {g with CurrentScore = Map.add player newScore g.CurrentScore}

    let playGame players times =
        let g0 = GameState.Init

        [1..times]
        |> List.fold
            (fun prev marble ->
                let player =
                    match (marble % players) with
                    | 0 -> players
                    | v -> v
                let (newCircle2, score) = iteration marble prev.CurrentCircle2

                
                if score = 0 then
                    {prev with CurrentCircle2 = newCircle2}
                else
                    {prev with CurrentCircle2 = newCircle2}
                    |> GameState.updatePlayerScore player score
            )
            (g0 ())


let m0 = Circle2.init()
let m1 = Circle2.insertMarbleBaseCase 1 m0
let m2 = Circle2.insertMarbleBaseCase 2 m1
let m3 = Circle2.insertMarbleBaseCase 3 m2
let m4 = Circle2.insertMarbleBaseCase 4 m3
let m5 = Circle2.insertMarbleBaseCase 5 m4

let m22 = [1..22] |> List.fold (fun s v -> Circle2.insertMarbleBaseCase v s) (Circle2.init())

let m23 = Circle2.iteration 23 m22
let mm = fst m23
for i in m22.Marbles2 do
    printfn "%d" i

#time
let gameA1 = (Circle2.playGame 10 1618).CurrentScore |> Map.toList |> List.maxBy snd |> snd
let gameA2 = (Circle2.playGame 13 7999).CurrentScore |> Map.toList |> List.maxBy snd |> snd
let gameA3 = (Circle2.playGame 17 1104).CurrentScore |> Map.toList |> List.maxBy snd |> snd
let gameA4 = (Circle2.playGame 21 6111).CurrentScore |> Map.toList |> List.maxBy snd |> snd
let gameA5 = (Circle2.playGame 30 5807).CurrentScore |> Map.toList |> List.maxBy snd |> snd


let answerA = (Circle2.playGame 459 71790).CurrentScore |> Map.toList |> List.maxBy snd |> snd
//let answerB = (Circle2.playGame 459 7179000).CurrentScore |> Map.toList |> List.maxBy snd |> snd
// Answer A with ResizeArray went from 1 minute to 254 ms
// All the list index lookups must be real slow



// It is real slow for the acutal quastion B, instant for the test cases
// The functional Map might be too slow here too


