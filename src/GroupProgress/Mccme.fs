module internal GroupProgress.Mccme

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
     MccmeUserId: PositiveInt.T}
/// Information about local and global problem identifiers.
type private Problem = 
    {ContestIndex: NonNegativeInt.T;
     ProblemIndex: NonNegativeInt.T;
     MccmeProblemId: PositiveInt.T}

/// Creates record with information about user identifiers.
let private readUser index json =
    let messageCtor e = sprintf "Unable to read mccme id of user %d: %s." index e
    let uid = tryGetRopProperty convertToPositiveInt messageCtor "mccme" json
    uid |> Option.map (fun x -> {LocalIndex = NonNegativeInt.wrap index; MccmeUserId = x})

/// Returns sequence of users defined in the passed JSON array.
let private readUsers (users : JsonValue []) = 
    users
    |> Seq.mapi readUser
    |> Seq.choose id

/// Creates record with information about problem identifiers.
let private readProblem contestIndex problemIndex json = 
    let messageCtor e = sprintf "Mccme handler could not read information about problem %d from contest %d: %s." problemIndex contestIndex e
    let site = getRopProperty convertToString messageCtor "site" json
    match site with
    | "mccme" ->
        let pid = getRopProperty convertToPositiveInt messageCtor "problem" json
        Some {ContestIndex = NonNegativeInt.wrap contestIndex;
                      ProblemIndex = NonNegativeInt.wrap problemIndex; MccmeProblemId = pid}
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

/// Queries pages and returns result property of the returned JSON object.
let private getJsonResult createMessage url parameters =
    let raw = loadPage createMessage url parameters
    let root = JsonValue.Parse(raw)
    getRopProperty Some createMessage "result" root

/// Returns user results on passed problems.
let private getUserProgress (user: User) (problems: Problem seq) = 
    let uid = user.MccmeUserId |> PositiveInt.value    
    let messageCtor e = sprintf "Unable to get progress of mccme user %d: %s." uid e

    // get number of attempts
    let jsonPageCount = getJsonResult messageCtor "http://informatics.mccme.ru/moodle/ajax/ajax.php"  [("group_id", "0"); ("problem_id", "0"); ("user_id", string uid); ("objectName", "submits"); ("lang_id", "-1"); ("status_id", "-1"); ("action", "getPageCount"); ("count", "1")]
    let count = getRopProperty convertToInt messageCtor "page_count" jsonPageCount
    // get all attempts
    let jsonHTMLTable = getJsonResult messageCtor "http://informatics.mccme.ru/moodle/ajax/ajax.php"  [("group_id", "0"); ("problem_id", "0"); ("user_id", string uid); ("objectName", "submits"); ("lang_id", "-1"); ("status_id", "-1"); ("action", "getHTMLTable"); ("count", string count)]
    let html = getRopProperty convertToString messageCtor "text" jsonHTMLTable
    
    let problemStatus = new Dictionary<int, Status option>()
    problems |> Seq.iter (fun p -> problemStatus.Add(PositiveInt.value p.MccmeProblemId, None))

    // parse attempts from past to present
    let mutable tr = html.LastIndexOf("<tr>")
    while tr <> -1 do
        let td = Array.scan (fun p _ -> html.IndexOf("<td>", p + 1)) (html.IndexOf("<td>", tr)) [|0; 0; 0; 0; 0; 0|]
        let p =
            let m = Regex.Match(html.Substring(td.[2], td.[3] - td.[2]), "chapterid=(\d+)")
            System.Int32.Parse(m.Groups.Item(1).Value)
        let v =
            match html.Substring(td.[5], td.[6] - td.[5]) with
            | s when s.StartsWith("<td>OK") -> AC
            | s when s.StartsWith("<td>Ошибка компиляции") -> CE
            | _ -> RJ
        if problemStatus.ContainsKey(p) then
            problemStatus.[p] <- progressAcc problemStatus.[p] v
        tr <- html.LastIndexOf("<tr>", tr - 1)

    // return progress for attempted problems
    problems
    |> Seq.map (fun p ->
        match problemStatus.[PositiveInt.value p.MccmeProblemId] with
        | Some status -> Some  {UserIndex = user.LocalIndex; ContestIndex = p.ContestIndex; ProblemIndex = p.ProblemIndex; Status = status}
        | None -> None)
    |> Seq.choose id    

/// Returns progress of users on informatics.mccme.ru
let getProgress configPath = 
    let jsonUsers, jsonProblems = getUsersAndProblems configPath
    let users = readUsers jsonUsers
    match readProblems jsonProblems with
    | s when Seq.isEmpty s -> Seq.empty
    | problems ->
        users
        |> Seq.map (fun u -> getUserProgress u problems)
        |> Seq.concat

/// Returns information about mccme problem.
/// It's based on site, problem and desc properties of JSON object.
let private getProblemInfo contestIndex problemIndex json = 
    let messageCtor e = sprintf "Mccme handler could not read information about problem %d from contest %d: %s." problemIndex contestIndex e
    let site = getRopProperty convertToString messageCtor "site" json
    match site with
    | "mccme" ->
        let pid = getRopProperty convertToPositiveInt messageCtor "problem" json |> PositiveInt.value
        let desc =
            match tryGetRopProperty convertToString100 messageCtor "desc" json with
            | Some v -> v
            | None -> sprintf "Задача %d" pid |> String100.wrap
        let link = sprintf "http://informatics.mccme.ru/mod/statements/view3.php?chapterid=%d" pid |> String100.wrap
        Some {ContestIndex = NonNegativeInt.wrap contestIndex; ProblemIndex = NonNegativeInt.wrap problemIndex; Description = desc; Link = link}
    | _ -> None

/// Returns information about mccme problems
let getProblemsInfo configPath = 
    let _, jsonProblems = getUsersAndProblems configPath
    jsonProblems
    |> Seq.mapi (fun i c -> c |> Seq.mapi (getProblemInfo i))
    |> Seq.concat
    |> Seq.choose id

