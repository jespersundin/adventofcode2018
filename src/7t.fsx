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

type DependencyDefinition = {Step : char ; DependsOn : char}
let parseLine (s : string) =
    //"Step C must be finished before step A can begin."
    let l = s.Split([|" must be finished before step "|],System.StringSplitOptions.None)
    let d = l.[0].[l.[0].Length-1]
    let n = l.[1].[0]
    {Step = n; DependsOn = d}

// Credit to Michael Gilliland https://www.youtube.com/watch?v=LQNXAdHu-WI
let order (graph : Map<char,char list>) : char list =
    let rec inner (graph : Map<char,char list>) (acc : char list) =
        if graph.IsEmpty
        then acc
        else
            let current =
                graph
                |> Map.filter (fun _ vs -> List.isEmpty vs)
                |> Map.toList
                |> List.minBy fst
                |> fst
            let withoutCurrent =
                graph
                |> Map.filter (fun k _ -> k <> current)
                |> Map.map (fun _ v -> List.filter ((<>) current) v)
            inner withoutCurrent (current :: acc)
    inner graph []
    |> List.rev

let theGraph =
    let mutable m : Map<char, char list> = Map.empty

    for i in (input |> Array.map parseLine) do
        let depsOrEmpty k =
            match Map.tryFind k m with
            | Some v -> v
            | None -> []

        m <- Map.add i.Step (i.DependsOn :: (depsOrEmpty i.Step)) m
        m <- Map.add i.DependsOn (depsOrEmpty i.DependsOn) m
    m

let answerA = order theGraph |> List.toArray |> String

let charToSpecificTime (c : char) = (int c) - 64 + 60
// let charToSpecificTime (c : char) = (int c) - 64
let times = order theGraph |> List.map int

type WorkInfo = {Deps : char list; WorkRemaining : int}
with
    static member Done wi = wi.WorkRemaining = 0




let availableWork (g : Map<char,_>) =
    g
    |> Map.filter (fun _ vs -> List.isEmpty vs.Deps)
    |> Map.toList
    |> List.sortBy fst

let work (aw : (char * WorkInfo) list) availableWorkers =
    aw
    |> List.mapi (
        fun i (c,wi) ->
        if i < availableWorkers
        then (c,{wi with WorkRemaining = wi.WorkRemaining - 1})
        else (c,wi))

let completed (aw : (char * WorkInfo) list) =
    aw
    |> List.filter (fun c -> WorkInfo.Done (snd c))
    |> List.map fst

let workedMap (aw : (char * WorkInfo) list) (g : Map<char,_>) =
    aw
    |> List.fold (fun state (k,v) -> Map.add k v state) g

let updatedWork (wm : Map<_,_>) (completed)=
    wm
    |> Map.filter (fun k _ -> not <| List.contains k completed)
    |> Map.map (fun _ v ->
        {v with
            Deps = List.filter (fun k -> not <| List.contains k completed) v.Deps})

let iterStep avWo (g : Map<_,_>) =
    let aw = availableWork g
    let w = work aw avWo
    let c = completed w
    let newWm = workedMap w g
    let upWork = updatedWork newWm c
    upWork

let workTime ( graphIn : Map<char,WorkInfo>) (availableWorkers : int) : int =
    
    let rec step (graph : Map<char,WorkInfo>) (acc : int) =
        if (Map.fold (fun s k v -> s+v.WorkRemaining) 0 graph) = 0
        then acc-1
        else
            let availableWork =
                graph
                |> Map.filter (fun _ vs -> List.isEmpty vs.Deps)
                |> Map.toList
                |> List.sortBy fst
            
            let worked =
                availableWork
                |> List.mapi (
                    fun i (c,wi) ->
                    if i < availableWorkers
                    then (c,{wi with WorkRemaining = wi.WorkRemaining - 1})
                    else (c,wi))
            let completed =
                worked
                |> List.filter (fun (c,wi) -> WorkInfo.Done wi)
                |> List.map fst
            let workedMap =
                worked
                |> List.fold (fun state (k,v) -> Map.add k v state) graph

            let remainingWork =
                workedMap
                |> Map.filter (fun k v -> v.WorkRemaining <> 0)
                |> Map.map (fun _ v ->
                    {v with
                        Deps = List.filter (fun k -> not (List.contains k completed)) v.Deps})
            step remainingWork (acc+1)
    step graphIn 0

let theWorkGraph = theGraph |> Map.map (fun k v -> {Deps = v; WorkRemaining = charToSpecificTime k})
// let aw = availableWork theWorkGraph
// let w = work aw 3
// let wm = workedMap w theWorkGraph
// let c = completed w

let w0 = iterStep 2 theWorkGraph
let w1 = iterStep 2 w0
let w2 = iterStep 2 w1
let w3 = iterStep 2 w2
let w4 = iterStep 2 w3
let w5 = iterStep 2 w4
let w6 = iterStep 2 w5
let w7 = iterStep 2 w6
let w8 = iterStep 2 w7
let w9 = iterStep 2 w8
let w10 = iterStep 2 w9
let w11 = iterStep 2 w10
let w12 = iterStep 2 w11
let w13 = iterStep 2 w12
let w14 = iterStep 2 w13
let w15= iterStep 2 w14
let t = workTime theWorkGraph 2