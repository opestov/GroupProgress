module internal GroupProgress.Config

open GroupProgress.DomainPrimitiveTypes
open FSharp.Data

type User =
    {RealName: String50.T;
     DisplayName: String50.T}

type Contest =
    {Name: String50.T;
    NumberOfProblems: PositiveInt.T}

type Group =
    {Name: String50.T;
     Users: User array;
     Contests: Contest array}

module Common =

    let convertToString (obj: JsonValue) =
        match obj with
        | JsonValue.String s -> Some s
        | _ -> None

    let convertToInt (obj: JsonValue) =
        match obj with
        | JsonValue.Number n ->
            try
                Some(System.Decimal.ToInt32(n))
            with _ -> None
        | _ -> None

    let convertToArray (obj: JsonValue) =
        match obj with
        | JsonValue.Array a -> Some a
        | _ -> None

    let convertAndCreate convert create obj =
        match convert obj with
        | Some s -> create s
        | None -> None

    let convertToString50 = convertAndCreate convertToString String50.create
    let convertToString100 = convertAndCreate convertToString String100.create
    let convertToNonNegativeInt = convertAndCreate convertToInt NonNegativeInt.create
    let convertToPositiveInt = convertAndCreate convertToInt PositiveInt.create

    let getProperty convert okCont missedCont badCont propName (obj: JsonValue) =
        match obj.TryGetProperty(propName) with
        | Some x ->
            match convert x with
            | Some v -> okCont v
            | None -> badCont()
        | None -> missedCont()

    let getRopProperty convert createMessage propName obj =
        let missedCont() = failwith (createMessage <| sprintf "missed property [%s]" propName)
        let badCont() = failwith (createMessage <| sprintf "property [%s] has wrong type" propName)
        getProperty convert id missedCont badCont propName obj

    let tryGetRopProperty convert createMessage propName obj =
        let okCont x = Some x
        let missedCont() = None
        let badCont() = failwith (createMessage <| sprintf "property [%s] has wrong type" propName)
        getProperty convert okCont missedCont badCont propName obj

    let getUsersAndProblems (path : string) =
        let groupMessage e = sprintf "Unable to read group info: %s." e
        let contestMessage i e = sprintf "Unable to read contest #%d: %s." i e

        let root = JsonValue.Load(path)
        let jsonUsers = getRopProperty convertToArray groupMessage "users" root
        let jsonContests = getRopProperty convertToArray groupMessage "contests" root
        let problems =
            jsonContests
            |> Array.mapi (fun i o -> getRopProperty convertToArray (contestMessage i) "problems" o)
        jsonUsers, problems

open Common

let private parseUser index user =
    let messageCtor e = sprintf "Unable to read user #%d: %s." index e
    let name = getRopProperty convertToString50 messageCtor "name" user
    let display = getRopProperty convertToString50 messageCtor "display" user
    {RealName = name; DisplayName = display}

let private parseContest index contest =
    let messageCtor e = sprintf "Unable to read contest #%d: %s." index e
    let name = getRopProperty convertToString50 messageCtor "name" contest
    let size = getRopProperty convertToArray messageCtor "problems" contest |> Array.length
    {Contest.Name = name; NumberOfProblems = PositiveInt.wrap size}

let readGroup (configPath : string) =
    let root = JsonValue.Load(configPath)

    let messageCtor e = sprintf "Unable to read group: %s." e
    let name = getRopProperty convertToString50 messageCtor "name" root
    let users = getRopProperty convertToArray messageCtor "users" root |> Array.mapi(fun i u -> parseUser i u)
    let contests = getRopProperty convertToArray messageCtor "contests" root |> Array.mapi (fun i c -> parseContest i c)
    {Name = name; Users = users; Contests = contests}
