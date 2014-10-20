module internal GroupProgress.DomainPrimitiveTypes

module String50 =
    open System.Text.RegularExpressions

    [<NoEquality; NoComparison>]
    type T =
        | String50 of string

    let create (s: string) =
        match s with
        | null -> None
        | _ when s.Length > 50 -> None
        | _ when Regex.IsMatch(s, @"^[\p{L}\p{M}\p{N}\p{P}\p{S}\p{Zs}]+$") -> Some(String50 s)
        | _ -> Some(String50 s)

    let value (String50 s) = s

module String100 =
    open System.Text.RegularExpressions

    [<NoEquality; NoComparison>]
    type T =
        | String100 of string

    let create (s: string) =
        match s with
        | null -> None
        | _ when s.Length > 100 -> None
        | _ when Regex.IsMatch(s, @"^[\p{L}\p{M}\p{N}\p{P}\p{S}\p{Zs}]+$") -> Some(String100 s)
        | _ -> Some(String100 s)

    /// Use this method internally only if you are sure that argument is string100
    let internal wrap s =
        match create s with
        | Some v -> v
        | None -> failwithf "Not expecting invalid string here."


    let value (String100 s) = s

module PositiveInt =
    [<NoEquality; NoComparison>]
    type T =
        | PositiveInt of int
    let ONE = PositiveInt 1

    let create i =
        if i < 1 then None
        else Some(PositiveInt i)

    /// Use this method internally only if you are sure that argument is >= 1
    let internal wrap i =
        match create i with
        | Some v -> v
        | None -> failwithf "Not expecting non-positive int here."

    let value (PositiveInt i) = i

module NonNegativeInt =
    [<NoEquality; NoComparison>]
    type T =
        | NonNegativeInt of int
    let ZERO = NonNegativeInt 0

    let create i =
        if i < 0 then None
        else Some(NonNegativeInt i)

    /// Use this method internally only if you are sure that argument is >= 0
    let internal wrap i =
        match create i with
        | Some v -> v
        | None -> failwithf "Not expecting negative int here."

    let value (NonNegativeInt i) = i


