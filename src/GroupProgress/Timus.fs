module internal GroupProgress.Timus

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open GroupProgress.DomainPrimitiveTypes
open GroupProgress.Config.Common
open GroupProgress.Providers
open FSharp.Data

/// Information about local and global user identifiers.
type private User = 
    {LocalIndex: NonNegativeInt.T;
     TimusAuthor: PositiveInt.T}

/// Information about local and global problem identifiers.
type private Problem = 
    {ContestIndex: NonNegativeInt.T;
     ProblemIndex: NonNegativeInt.T;
     TimusNum: PositiveInt.T}

/// Maximum number of attempts a user can made for problem.
/// This assumption is checked against actual data.
/// Program will fail with descriptional exception if it is not true.
let private maxAttempts = 100

/// Creates record with information about user identifiers.
let private readUser index json =
    let messageCtor e = sprintf "Unable to read timus id of user %d: %s." index e
    let author = tryGetRopProperty convertToPositiveInt messageCtor "timus" json
    author |> Option.map (fun x -> {LocalIndex = NonNegativeInt.wrap index; TimusAuthor = x})

/// Returns sequence of users defined in the passed JSON array.
let private readUsers (users : JsonValue []) = 
    users
    |> Seq.mapi readUser
    |> Seq.choose id

/// Creates record with information about problem identifiers.
let private readProblem contestIndex problemIndex json = 
    let messageCtor e = sprintf "Timus handler could not read information about problem %d from contest %d: %s." problemIndex contestIndex e
    let site = getRopProperty convertToString messageCtor "site" json
    match site with
    | "timus" ->
        let num = getRopProperty convertToPositiveInt messageCtor "num" json
        Some {ContestIndex = NonNegativeInt.wrap contestIndex;
                      ProblemIndex = NonNegativeInt.wrap problemIndex; TimusNum = num}        
    | _ -> None

/// Returns sequence of problems defined in the passed JSON array.
let private readProblems (problems : JsonValue [] []) =
    problems
    |> Seq.mapi (fun i c -> c |> Seq.mapi (fun j p -> readProblem i j p))
    |> Seq.concat
    |> Seq.choose id

/// Utility method that loads web page.
let private loadPage createMessage url parameters = 
    try 
        Http.RequestString(url, query = parameters)
    with e -> failwith (createMessage <| sprintf "could not load web page %s with query %A: %s" url query e.Message)

/// Parses summary page and returns sorted array of tried problems.
let private extractSortedArrayOfTriedProblems rawHtml = 
    let re = "<A HREF=\"status.aspx\?space=1&amp;num=(\d+)"
    let problems = 
        [for m in Regex.Matches(rawHtml, re) -> m.Groups.Item(1).Value]
        |> List.map int
        |> List.rev
        |> List.toArray
    Array.sortInPlace problems
    problems

/// Returns progress of user on specific problem.
let private getProblemProgress (user: User) (problem: Problem) = 
    let author = user.TimusAuthor |> PositiveInt.value    
    let num = problem.TimusNum |> PositiveInt.value

    let query = 
        [("space", "1");
         ("num", string num);
         ("author", string author);
         ("count", string maxAttempts)]    
    
    let re = "<TD class=\"id\">.*?<TD class=\"verdict_(\w\w)\">(\w+)"    
    let mapVerdict = function
        | "ac", _ -> AC
        | "rj", "Compilation" -> CE
        | "rj", _ -> RJ
        | v -> failwithf "not expecting verdict %A for author %d on problem %d" v author num

    let messageCtor e = sprintf "Unable to get progress of timus author %d on problem %d: %s." author num e
    let raw = loadPage messageCtor "http://acm.timus.ru/status.aspx" query
    let verdicts = 
        [for m in Regex.Matches(raw, re) -> 
             mapVerdict (m.Groups.Item(1).Value, m.Groups.Item(2).Value)]
        |> List.rev
    match verdicts with
    | _ when List.length verdicts = maxAttempts -> failwith (messageCtor "the user reached maximum number of allowed attempts")
    | _ -> List.fold progressAcc None verdicts |> Option.map (fun s -> {UserIndex = user.LocalIndex; ContestIndex = problem.ContestIndex; ProblemIndex = problem.ProblemIndex; Status = s})

/// Returns user results on passed problems.
let private getUserProgress (user: User) (problems: Problem seq) = 
    let author = user.TimusAuthor |> PositiveInt.value    
    let messageCtor e = sprintf "Unable to get progress of timus author %d: %s." author e
    let raw = loadPage messageCtor "http://acm.timus.ru/author.aspx" [("id", string author)]
    let tried = extractSortedArrayOfTriedProblems raw
    problems
    |> Seq.filter (fun p -> Array.BinarySearch (tried, p.TimusNum |> PositiveInt.value) >= 0)
    |> Seq.map (getProblemProgress user)
    |> Seq.choose id

/// Returns progress of users on acm.timus.ru
let getProgress configPath = 
    let jsonUsers, jsonProblems = getUsersAndProblems configPath
    let users = readUsers jsonUsers
    match readProblems jsonProblems with
    | s when Seq.isEmpty s -> Seq.empty
    | problems ->
        users
        |> Seq.map (fun u -> getUserProgress u problems)
        |> Seq.concat

/// Returns information about timus problem.
/// It's based on site, num and desc properties of JSON object.
let private getProblemInfo contestIndex problemIndex json = 
    let messageCtor e = sprintf "Timus handler could not read information about problem %d from contest %d: %s." problemIndex contestIndex e
    let site = getRopProperty convertToString messageCtor "site" json
    match site with
    | "timus" ->
        let num = getRopProperty convertToPositiveInt messageCtor "num" json |> PositiveInt.value
        let desc =
            match tryGetRopProperty convertToString100 messageCtor "desc" json with
            | Some v -> v
            | None -> sprintf "Задача %d" num |> String100.wrap
        let link = sprintf "http://acm.timus.ru/problem.aspx?space=1&num=%d&locale=ru" num |> String100.wrap
        Some {ContestIndex = NonNegativeInt.wrap contestIndex; ProblemIndex = NonNegativeInt.wrap problemIndex; Description = desc; Link = link}
    | _ -> None

/// Returns information about timus problems
let getProblemsInfo configPath = 
    let _, jsonProblems = getUsersAndProblems configPath
    jsonProblems
    |> Seq.mapi (fun i c -> c |> Seq.mapi (getProblemInfo i))
    |> Seq.concat
    |> Seq.choose id

