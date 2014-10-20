module internal GroupProgress.Ejudge

open System
open System.Collections.Generic
open System.Text.RegularExpressions
open GroupProgress.DomainPrimitiveTypes
open GroupProgress.Config.Common
open GroupProgress.Providers
open FSharp.Data

/// Example of external.xml file
[<Literal>]
let private externalXml = """
<runlog contest_id="3" start_time="2012/11/08 08:51:48" current_time="2014/08/15 10:00:15">
  <name>Арифметические операции</name>
  <users>
    <user id="698" name="m70"/>
    <user id="699" name="m71"/>
  </users>
  <problems>
    <problem id="1" short_name="01" long_name=""/>
    <problem id="2" short_name="02" long_name=""/>
  </problems>
  <languages>
    <language id="1" short_name="fpc" long_name="Free Pascal 2.6.0-9"/>
    <language id="2" short_name="gcc" long_name="GNU C 4.7.2"/>
  </languages>
  <runs>
    <run run_id="92" time="1140860" status="OK" user_id="728" prob_id="1" lang_id="3" test="1" nsec="655620000"/>
    <run run_id="93" time="1141357" status="WA" user_id="728" prob_id="2" lang_id="3" test="1" nsec="457866000"/>
    <run run_id="94" time="1141441" status="CE" user_id="728" prob_id="2" lang_id="3" nsec="241373000"/>
  </runs>
</runlog>"""

/// Type provider for reading external.xml
type private Contest = XmlProvider<externalXml>

/// Information about local and global user identifiers.
type private User = 
    {LocalIndex: NonNegativeInt.T;
     EjudgeId: PositiveInt.T}

/// Information about local and global problem descriptions.
type private Problem = 
    {ContestIndex: NonNegativeInt.T;
     ProblemIndex: NonNegativeInt.T;
     EjudgeContest: PositiveInt.T;
     EjudgeProblem: PositiveInt.T}

/// Creates record with information about user identifiers.
let private readUser judgeName index json =
    let messageCtor e = sprintf "Unable to read ejudge id of user %d: %s." index e
    let id = tryGetRopProperty convertToPositiveInt messageCtor judgeName json
    id |> Option.map (fun x -> {LocalIndex = NonNegativeInt.wrap index; EjudgeId = x})

/// Returns sequence of users defined in the passed JSON array.
let private readUsers judgeName (users : JsonValue []) = 
    users
    |> Seq.mapi (readUser judgeName)
    |> Seq.choose id

/// Creates record with information about problem identifiers.
let private readProblem judgeName contestIndex problemIndex json = 
    let messageCtor e = sprintf "Ejudge handler could not read information about problem %d from contest %d: %s." problemIndex contestIndex e
    match getRopProperty convertToString messageCtor "site" json with
    | v when judgeName = v ->
        let c = getRopProperty convertToPositiveInt messageCtor "contest" json
        let p = getRopProperty convertToPositiveInt messageCtor "problem" json
        Some {ContestIndex = NonNegativeInt.wrap contestIndex;
                      ProblemIndex = NonNegativeInt.wrap problemIndex;
                      EjudgeContest = c;
                      EjudgeProblem = p}
    | _ -> None

/// Returns sequence of problems defined in the passed JSON array.
let private readProblems judgeName (problems : JsonValue [] []) =
    problems
    |> Seq.mapi (fun i c -> c |> Seq.mapi (fun j p -> readProblem judgeName i j p))
    |> Seq.concat
    |> Seq.choose id

/// Utility method that parses external.xml file.
let private parseContest createMessage rawXml = 
    try 
        Contest.Parse(rawXml)
    with e -> failwith (createMessage <| sprintf "could not read ejudge xml: %s." e.Message)

/// Returns progress of users in the specific ejudge contest.
let private getContestProgress index rawXml users problems = 
    let udict = 
        users
        |> Seq.map (fun u -> u.EjudgeId |> PositiveInt.value, u)
        |> dict
    
    let pdict = 
        problems
        |> Seq.filter (fun p ->  index = (p.EjudgeContest |> PositiveInt.value))
        |> Seq.map (fun p -> p.EjudgeProblem |> PositiveInt.value, p)
        |> dict

    let messageCtor e = sprintf "Unable to get results from ejudge contest %d: %s." index e
    let contest = parseContest messageCtor rawXml
    let verdicts = new Dictionary<int * int, ResizeArray<JudgementVerdict>>()
    contest.Runs
    |> Seq.map (fun r -> 
           match r.Status with
           | "OK" -> (r.UserId, r.ProbId, AC)
           | "CE" -> (r.UserId, r.ProbId, CE)
           | _ -> (r.UserId, r.ProbId, RJ))
    |> Seq.filter 
           (fun (u, p, _) -> udict.ContainsKey(u) && pdict.ContainsKey(p))
    |> Seq.iter (fun (u, p, s) -> 
           match verdicts.ContainsKey((u, p)) with
           | false -> 
               verdicts.Add((u, p), new ResizeArray<JudgementVerdict>())
           | true -> ()
           verdicts.[(u, p)].Add(s))
    verdicts.Keys
    |> Seq.map (fun (u, p) -> 
          match verdicts.[(u, p)] |> Seq.fold progressAcc None with
          | Some status -> 
              Some {UserIndex = udict.[u].LocalIndex;
                    ContestIndex = pdict.[p].ContestIndex;
                    ProblemIndex = pdict.[p].ProblemIndex;
                    Status = status}
          | None -> None)
    |> Seq.choose id
    |> Seq.toList

/// Returns progress of users.
let getProgress judgeName getXml configPath = 
    let jsonUsers, jsonProblems = getUsersAndProblems configPath
    let users = readUsers judgeName jsonUsers
    let problems = readProblems judgeName jsonProblems
    problems
    |> Seq.map (fun p -> p.EjudgeContest |> PositiveInt.value)
    |> set        
    |> Seq.map (fun c ->
        let xml =
            try
                getXml c
            with e -> failwith "Couldn't read external.xml for contest %d: %s." c e.Message
        getContestProgress c xml users problems)
    |> Seq.concat

/// Returns information about ejudge problem.
/// This information depends on ejudge installation and we have to
/// call external functions getInfo.
let private getProblemInfo judgeName contestIndex problemIndex json = 
    let messageCtor e = sprintf "Ejudge handler could not get information about problem %d from contest %d: %s." problemIndex contestIndex e
    match getRopProperty convertToString messageCtor "site" json with
    | v when judgeName = v ->
        let c = getRopProperty convertToPositiveInt messageCtor "contest" json |> PositiveInt.value
        let p = getRopProperty convertToPositiveInt messageCtor "problem" json |> PositiveInt.value
        let d =
            match  tryGetRopProperty convertToString100 messageCtor "desc" json with
            | Some v -> v
            | None -> String100.wrap (sprintf "Задача %d из контеста %d" p c)
        let l =
            match tryGetRopProperty convertToString100 messageCtor "link" json with
            | Some v -> v
            | None -> String100.wrap "#"
        Some {ContestIndex = NonNegativeInt.wrap contestIndex; ProblemIndex = NonNegativeInt.wrap problemIndex; Description = d; Link = l}
    | _ -> None

/// Returns information about ejudge problems
let getProblemsInfo judgeName configPath = 
    let _, jsonProblems = getUsersAndProblems configPath
    jsonProblems
    |> Seq.mapi (fun i c -> c |> Seq.mapi (getProblemInfo judgeName i))
    |> Seq.concat
    |> Seq.choose id

