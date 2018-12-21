
// Input
open System.IO

let inputFile fileName = Path.Combine(__SOURCE_DIRECTORY__,@"./../input/", fileName)

let smallExample = "2 3 0 3 10 11 12 1 1 0 1 99 2 1 1 2"
let input =
    let s = File.ReadAllText(inputFile "8.txt")
    s.Split(' ')
    // smallExample.Split(' ')
    |> Array.map int
    |> Array.toList

type Header = { ChildrenCount : int ; MetadataCount : int}
type Metadata = int list


type Node (chCount : int, meCount : int) =
    member val Children : Node list = [] with get, set
    member val Metadata : Metadata = [] with get, set
    member this.Header = {ChildrenCount = chCount; MetadataCount = meCount}


let buildTree (input : int list) =
    // Not tail-call optimized, how would i do that?
    let rec iterInput (currNode : Node) (input : int list) : (Node * int list) =
        match currNode.Header.ChildrenCount, currNode.Header.MetadataCount with
        | cc, mc when cc = 0 ->
            // Consume metadata and move forward
            let metadata = input.[0..mc-1]
            currNode.Metadata <- metadata
            currNode, input.[mc..]
        | cc, mc ->
            let mutable children : Node list = []
            let mutable remainingInput = input
            while children.Length < cc do
                match remainingInput with
                | ccN :: mcN :: r ->
                    let n = Node(ccN,mcN)
                    let (completeNode, rem) = iterInput n r
                    children <- List.append children [completeNode]
                    remainingInput <- rem
                    ()
                | [_]
                | [_;_]
                | [] -> failwithf "Should not happen %d" remainingInput.Length
            currNode.Children <- children
            let metadata = remainingInput.[0..mc-1]
            currNode.Metadata <- metadata
            currNode, remainingInput.[mc..]

    match input with
    | cc :: mc :: r ->
        let n = Node(cc,mc)
        let (node, rem) = iterInput n r
        Some node
    | [] ->
        None


let tree = buildTree input
    

let rec traverse (t : Node) =
    [
        yield t.Metadata |> List.sum
        for i in t.Children do
            yield! traverse i
    ]

let rec traverse2 (t : Node) =
    [
        match t.Header.ChildrenCount with
        | 0 ->
            yield t.Metadata |> List.sum
        | _ ->
            for i in t.Metadata do
                match List.tryItem (i-1) t.Children with
                | Some n -> yield! traverse2 n
                | None -> ()
    ]

let answerA = tree |> Option.map (traverse >> List.sum)

let answerB = tree |> Option.map (traverse2 >> List.sum)