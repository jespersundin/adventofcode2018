open System.IO
open System

let inputFile fileName = Path.Combine(__SOURCE_DIRECTORY__,@"./../input/", fileName)
let input =
    File.ReadAllLines(inputFile "4a.txt")

type GuardId = int

type MsgType =
    | ShiftBegin of GuardId
    | FallAsleep
    | WakeUp

type Msg = {Timestamp : DateTime; MsgType : MsgType}
let parse (s : string) =
    // Format
    // [1518-09-08 23:59] Guard #1709 begins shift
    // [1518-08-23 00:35] falls asleep
    // [1518-06-03 00:46] wakes up
    let brackets = s.Split(']')
    let date = brackets.[0].Replace("[","") |> DateTime.Parse
    let msgType =
        match brackets.[1] with
        | m when m.Contains("#") ->
            m.Split('#').[1].Split(' ').[0] |> int |> ShiftBegin
        | m when m.Contains("up") ->
            WakeUp
        | m when m.Contains("asleep") ->
            FallAsleep
        | _ -> failwithf "Unexpected message"
    {Timestamp = date; MsgType = msgType}

module rec ShiftBlock =
    type T = private ShiftBlock of int list
    with
        // Enable the use of ex. List.sum
        static member (+) (l,r) = combine l r
        static member Zero = createEmpty()

    type Duration = {Start : int; End : int}

    let createEmpty () = List.init 60 (fun i -> 0) |> ShiftBlock
    let addDuration (ShiftBlock a) (dur : Duration) =
        a
        |> List.mapi (fun i v -> if i >= dur.Start && i < dur.End then v+1 else v)
        |> ShiftBlock
    
    let createWithDurations (durs : Duration list) = durs |> List.fold addDuration (createEmpty())
    
    let combine (ShiftBlock l) (ShiftBlock r) = List.map2 (+) l r |> ShiftBlock

    let minutesAsleep (ShiftBlock s) = List.sum s

    let minuteMostSlept (ShiftBlock s) =
        s
        |> List.mapi (fun i v -> (i,v))
        |> List.maxBy snd
        |> fst
    let mostTimesSleptInASingleMinute (ShiftBlock s) = s |> List.max

open ShiftBlock
type Shift = {Guard : GuardId ; Timestamp : DateTime ; SleepDurations : ShiftBlock.Duration list}

let orderedMessagesToShifts (msgs : Msg list) =
    let initShift (id : GuardId) (c : DateTime) =
        {Guard = id; Timestamp = c; SleepDurations = []}

    let setDurations (s : Shift) (durs : int list) =
        // Assume awake/asleep messages are always alternating and come in pairs within each relevant shift
        let sleepDurs = durs |> List.chunkBySize 2 |> List.map (fun l -> {Start = l.[0]; End = l.[1]})
        {s with SleepDurations = sleepDurs}

    let rec inner (msgs : Msg list) currentShift currentDurations shifts =
        match msgs with
        | x :: xs ->
            match x.MsgType with
            | FallAsleep
            | WakeUp ->
                let newDurs = List.append currentDurations [(x.Timestamp.Minute)]
                inner xs currentShift newDurs shifts
            | ShiftBegin gid ->
                // Previous shift complete
                let currentShift = setDurations currentShift currentDurations
                let newShifts = currentShift :: shifts
                // Continue with new shift
                let newShift = initShift gid x.Timestamp
                inner xs newShift [] newShifts
        | [] ->
            // Last record
            let currentShift = setDurations currentShift currentDurations
            currentShift :: shifts
    
    // Assume first item is always ShiftBegin
    let l = List.head msgs
    match l.MsgType with
    | ShiftBegin gid ->
        inner (List.tail msgs) (initShift gid l.Timestamp) [] []
    | _ -> failwithf "ShiftBegin must be first msg"
    |> List.rev
    
type ShiftAgg = {Shift : Shift ; ShiftBlock : ShiftBlock.T}
let toAggregate (shifts : Shift list) =
    shifts
    |> List.map (fun s ->
        {
            Shift = s;
            ShiftBlock = (ShiftBlock.createWithDurations s.SleepDurations)
        })

let shifts =
    input
    |> Array.map parse
    |> Array.toList
    |> List.sortBy (fun m -> m.Timestamp)
    |> orderedMessagesToShifts
    |> toAggregate
let answerA =   
    shifts
    |> List.groupBy (fun s -> s.Shift.Guard)
    |> List.map (fun (gid, s) ->
        let summedBlock = s |> List.sumBy (fun l -> l.ShiftBlock)
        let timeAsleep = summedBlock |> ShiftBlock.minutesAsleep
        let minuteMostSlept = summedBlock |> ShiftBlock.minuteMostSlept
        (gid, timeAsleep, minuteMostSlept))
    |> List.maxBy (fun (_,t,_) -> t)
    |> (fun (gid,_,m) -> gid * m)
    |> int

let answerB =
    shifts
    |> List.groupBy (fun s -> s.Shift.Guard)
    |> List.map (fun (gid, s) ->
        let summedBlock = s |> List.sumBy (fun l -> l.ShiftBlock)
        let timeAsleep = summedBlock |> ShiftBlock.minutesAsleep
        let minuteMostSlept = summedBlock |> ShiftBlock.minuteMostSlept
        let maxMin = summedBlock |> ShiftBlock.mostTimesSleptInASingleMinute

        (gid, timeAsleep, minuteMostSlept, maxMin))
    |> List.maxBy (fun (_,_,_,t) -> t)
    |> (fun (gid,_,m,_) -> gid * m)
    |> int


