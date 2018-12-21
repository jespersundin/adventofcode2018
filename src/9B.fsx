open System.Text
open System
open System.Collections.Generic

[<AllowNullLiteral>]
type Node<'a> (v : 'a,  r : Node<'a> , l : Node<'a> ) =
    member val Value = v
    member val Left = l with get, set
    member val Right = r with get, set

type CircularContainer<'a> (v : 'a) =

    let mutable currentNode : Node<'a> =
        let n = Node(v,null,null)
        n.Left <- n
        n.Right <- n
        n
    
    let initNode = currentNode
    let mutable count : int = 1
    member this.Count = count
    member this.Value = currentNode.Value
    member this.GoLeft() =
        currentNode <- currentNode.Left
        this
    member this.GoRight () =
        currentNode <- currentNode.Right
        this

    member this.Go (i : int) =
        if i < 0 then
            for j in [0..(-i-1)] do
                this.GoLeft() |> ignore
            this
        else if i > 0 then
            for j in [0..i-1] do
                this.GoRight() |> ignore
            this
        else
            this

    member this.InsertLeft (v : 'a) =
        let newNode = Node(v,null,null)
        let l = currentNode.Left
        l.Right <- newNode
        currentNode.Left <- newNode
        newNode.Left <- l
        newNode.Right <- currentNode
        currentNode <- newNode
        count <- count + 1
        this
    member this.InsertRight (v : 'a) =
        let newNode = Node(v,null,null)
        let r = currentNode.Right
        r.Left <- newNode
        currentNode.Right <- newNode
        newNode.Left <- currentNode
        newNode.Right <- r
        currentNode <- newNode
        count <- count + 1
        this

    member this.PopRight () =
        let v = currentNode.Value
        let l = currentNode.Left
        let r = currentNode.Right
        l.Right <- r
        r.Left <- l
        currentNode <- r
        count <- count - 1
        v

    override this.ToString() =
        let start = initNode
        let mutable next = start.Right
        let sb = StringBuilder()
        sb.Append(start.Value)
        sb.Append(" ")
        while Object.ReferenceEquals(start,next) |> not do
            sb.Append(next.Value)
            sb.Append(" ")
            next <- next.Right
        sb.ToString()

let run players marbles =
    let game = CircularContainer(0L)
    let scores = new Dictionary<int64,int64>()
    // let players = 13L
    // let marbles : int64 = 7999L
    for marble in [1L..marbles] do
        // printf "Marble %A" marble
        
        // printfn " Player %A" player
        match marble % 23L with
        | 0L ->
            let v = game.Go(-7).PopRight()
            let score = marble + v
            let player =
                match marble % players with
                | 0L -> players
                | v -> v
            
            // printfn "Player %A score %A" player score
            match scores.TryGetValue(player) with
            | true, s -> scores.[player] <- s+score
            | _ -> scores.Add(player,score)
            ()
        | _ ->
            game.GoRight().InsertRight(marble) |> ignore
    
    // printfn "%A" game
    scores.Values
    |> Seq.max

#time
let answerA = run 459L 7179000L
