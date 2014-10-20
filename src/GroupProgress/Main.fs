module GroupProgress.Main

open GroupProgress.DomainPrimitiveTypes

module Dtos = 

    type User =
        {RealName: string;
        DisplayName: string}

    type ProblemInfo =
        {Description: string;
        Link: string}

    type ContestUserResults =
        {Solved: int;
        RejectedAttempts: int;
        Problems: System.Nullable<int> []}

    type Contest =
        {Name: string;
        Problems: ProblemInfo [];
        Results: ContestUserResults []}

    type OverallUserResults =
        {Solved: int;
        RejectedAttempts: int;
        Contests: int []}

    type OverallResults = 
        {TotalProblems: int;
        Results: OverallUserResults []}

    type GroupProgress = 
        {Name: string;
        Users: User array;
        Contests: Contest array;
        Overall: OverallResults}

    type StandingsEntry =
        {UserIndex: int;
        Place: int}

open Dtos

// Shortcuts to unwrap values.
let private uS50 s50 = String50.value s50
let private uS100 s100 = String100.value s100
let private uPI positiveInt = PositiveInt.value positiveInt
let private uNNI nonNegativeInt = NonNegativeInt.value nonNegativeInt

/// Extracts from sequence of problems information about problems in the specified contest.
let private exportContestProblemsInfo (group : Config.Group) index (problems : seq<Providers.ProblemInfo>) =
    let links = Array.create (uPI group.Contests.[index].NumberOfProblems) None
    problems
    |> Seq.filter (fun p -> index = uNNI p.ContestIndex)
    |> Seq.iter (fun p ->
        let desc = uS100 p.Description
        let link = uS100 p.Link
        links.[uNNI p.ProblemIndex] <- Some {Description = desc; Link = link})
    match links |> Array.tryFindIndex (fun x -> Option.isNone x) with
    | Some i -> failwithf "Problem %d from contest %d doesn't have description" i index
    | _ -> ()
    links |> Array.map (Option.get)

/// Extracts results of the specified contest.
let private exportContestResults (group : Config.Group) index (progress : seq<Providers.ProblemProgress>) =
    let users = group.Users |> Array.length
    let problems = uPI group.Contests.[index].NumberOfProblems    

    let results = Array.init users (fun _ -> Array.create problems (System.Nullable()))
    let solved = Array.create users 0
    let rejectedAttempts = Array.create users 0
    progress
    |> Seq.filter (fun p -> index = uNNI p.ContestIndex)
    |> Seq.iter (fun p ->
        let i = uNNI p.UserIndex
        let j = uNNI p.ProblemIndex
        match p.Status with
        | Providers.Accepted v ->
            let rejected = uNNI v
            solved.[i] <- solved.[i] + 1
            rejectedAttempts.[i] <- rejectedAttempts.[i] + rejected
            results.[i].[j] <- System.Nullable(rejected)
        | Providers.Rejected v ->
            let rejected = uPI v
            rejectedAttempts.[i] <- rejectedAttempts.[i] + rejected
            results.[i].[j] <- System.Nullable(-rejected))  
    group.Users
    |> Array.mapi (fun i _ -> {Solved = solved.[i]; RejectedAttempts = rejectedAttempts.[i]; Problems = results.[i]})

let private exportContest (group : Config.Group) index problemsInfo progress =
    let name = uS50 group.Contests.[index].Name
    let links = exportContestProblemsInfo group index problemsInfo
    let results = exportContestResults group index progress
    {Name = name; Problems = links; Results = results}

let private exportOverallResults (group : Config.Group) (progress : seq<Providers.ProblemProgress>) =
    let users = group.Users |> Array.length
    let contests = group.Contests |> Array.length
    let results = Array.init users (fun _ -> Array.create contests 0)
    let solved = Array.create users 0
    let rejectedAttempts = Array.create users 0
    progress
    |> Seq.iter (fun p ->
        let i = uNNI p.UserIndex
        let j = uNNI p.ContestIndex
        match p.Status with
        | Providers.Accepted v ->
            let rejected = uNNI v
            solved.[i] <- solved.[i] + 1
            rejectedAttempts.[i] <- rejectedAttempts.[i] + rejected
            results.[i].[j] <- results.[i].[j] + 1
        | Providers.Rejected v ->
            let rejected = uPI v
            rejectedAttempts.[i] <- rejectedAttempts.[i] + rejected)
    let totalProblems = group.Contests |> Array.map (fun c -> uPI c.NumberOfProblems) |> Array.sum
    let overallResults = 
        group.Users
        |> Array.mapi (fun i _ -> {Solved = solved.[i]; RejectedAttempts = rejectedAttempts.[i]; Contests = results.[i]})
    {TotalProblems = totalProblems; Results = overallResults}

let private exportGroupProgress (group : Config.Group) problems progress =
    let groupName = uS50 group.Name
    let groupUsers = group.Users |> Array.map (fun u -> {User.RealName = uS50 u.RealName; DisplayName = uS50 u.DisplayName })
    let groupContests = group.Contests |> Array.mapi (fun i _ -> exportContest group i problems progress)
    let overall = exportOverallResults group progress
    {Name = groupName; Users = groupUsers; Contests = groupContests; Overall = overall}

let sortOverallUsersByProblems (users: User []) (results : OverallUserResults []) =
    let p = Array.init (results |> Array.length) (fun i -> i)
    Array.sortInPlaceBy (fun i -> (-results.[i].Solved, results.[i].RejectedAttempts, users.[i].DisplayName)) p
    p
    |> Seq.scan (fun state i ->
        match state with
        | (0, _, _) -> (1, i, 1)
        | (place, j, o) when results.[i].Solved = results.[j].Solved -> (place, i, o + 1)
        | (_, _, o) -> (o + 1, i, o + 1)) (0, 0, 0)
    |> Seq.skip 1
    |> Seq.map (fun (place, i, _) -> {UserIndex = i; Place = place})
    |> Seq.toArray

let sortContestUsersByProblems (users: User []) (results : ContestUserResults []) =
    let p = Array.init (results |> Array.length) (fun i -> i)
    Array.sortInPlaceBy (fun i -> (-results.[i].Solved, results.[i].RejectedAttempts, users.[i].DisplayName)) p
    p
    |> Seq.scan (fun state i ->
        match state with
        | (0, _, _) -> (1, i, 1)
        | (place, j, o) when results.[i].Solved = results.[j].Solved -> (place, i, o + 1)
        | (_, _, o) -> (o + 1, i, o + 1)) (0, 0, 0)
    |> Seq.skip 1
    |> Seq.map (fun (place, i, _) -> {UserIndex = i; Place = place})
    |> Seq.toArray

open System.IO
open System.Text

type Judge =
    private 
        | T of (string -> seq<Providers.ProblemProgress>) * (string -> seq<Providers.ProblemInfo>)

    static member CreateTimus() = T(Timus.getProgress, Timus.getProblemsInfo)    

    // dir/3.xml; dir/7.xml; dir/43.xml
    static member CreateLocalEjudge judgeName dir =
        let readXml (id : int) =
            File.ReadAllText(Path.Combine(dir, (string) id + ".xml"), Encoding.UTF8)
        T(Ejudge.getProgress judgeName readXml, Ejudge.getProblemsInfo judgeName)

    // dir/000043/var/status/dir/external.xml
    static member CreateEjudge judgeName dir =
        let readXml (id : int) =
            File.ReadAllText(Path.Combine([|dir; sprintf "%06d" id; "var"; "status"; "dir"; "external.xml" |]), System.Text.Encoding.UTF8)
        T(Ejudge.getProgress judgeName readXml, Ejudge.getProblemsInfo judgeName)

let gather (judges: Judge seq) (configPath: string) =
    let group = Config.readGroup configPath
    let problems = judges |> Seq.map (fun (T (_, g)) -> g configPath) |> Seq.concat
    let progress = judges |> Seq.map (fun (T (f, _)) -> f configPath) |> Seq.concat
    exportGroupProgress group problems progress
