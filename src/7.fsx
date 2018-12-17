open System.IO
open System
open System.Collections.Generic

let inputFile fileName = Path.Combine(__SOURCE_DIRECTORY__,@"./../input/", fileName)
let input =
    File.ReadAllLines(inputFile "7.txt")

// let input =
//     [
//         "Step C must be finished before step A can begin."
//         "Step C must be finished before step F can begin."
//         "Step A must be finished before step B can begin."
//         "Step A must be finished before step D can begin."
//         "Step B must be finished before step E can begin."
//         "Step D must be finished before step E can begin."
//         "Step F must be finished before step E can begin."
//     ]
//     |> List.toArray

type DependencyDefinition = {Name : char ; DependsOn : char}
let parseLine (s : string) =
    //"Step C must be finished before step A can begin."
    let l = s.Split([|" must be finished before step "|],System.StringSplitOptions.None)
    let d = l.[0].[l.[0].Length-1]
    let n = l.[1].[0]
    {Name = n; DependsOn = d}


let inputDef = input |> Array.map parseLine |> Array.toList

type Node<'a when 'a : comparison>(c : 'a) =

    let edges : ResizeArray<Node<'a>> = ResizeArray()
    member val Edges = edges.AsReadOnly() with get
    member val Name = c with get
    member this.AddEdge (n : Node<'a>) = edges.Add(n)

let dependencyTree (depDefinitions : DependencyDefinition list) =
    let dependOnSomething = depDefinitions |> List.map (fun c -> c.Name) |> List.distinct
    let dependencies = depDefinitions |> List.map (fun c -> c.DependsOn) |> List.distinct
    // Node name thats not in dependencies
    let d1 = Set.ofList dependOnSomething
    let d2 = Set.ofList dependencies

    let m = Dictionary<char,Node<char>>()
    for i in (Set.union d1 d2 |> Set.toList) do
        m.Add(i,Node(i))
    
    // Find root node to return
    let rootCandidates = Set.difference d2 d1 |> Set.toList // These are sorted
    let root =
        match rootCandidates with
        | [x] ->
            m.[x]
        | x :: xs ->
            let r = m.[x]
            for i in xs do
                // Add artificial dependencies on the first
                m.[i].AddEdge(r)
            r
        | [] -> failwithf "No root detected"

    for i in depDefinitions do
        m.[i.Name].AddEdge(m.[i.DependsOn])
    root, [for i in m.Values do yield i]

let (rootNode, allNodes) = dependencyTree inputDef

let sort (source : seq<Node<'a>> ) =
    let sorted = ResizeArray()
    let visited = Dictionary<Node<'a>,bool>()

    let rec visit (item : Node<'a>) =
        match visited.TryGetValue item with
        | true, v when v -> failwith "Cyclic dependency"
        | _ ->
            visited.[item] <- true
            let dependencies = item.Edges
            for dep in dependencies do
                visit dep
            visited.[item] <- false
            sorted.Add(item)

    for item in source do
        visit item
    sorted.AsReadOnly()

let p =
    sort allNodes
    |> Seq.map (fun c -> c.Name)
    |> Seq.toList
    |> List.distinct
    |> List.toArray
    |> String

rootNode