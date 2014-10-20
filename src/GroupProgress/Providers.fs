module internal GroupProgress.Providers

open GroupProgress.DomainPrimitiveTypes

type JudgementVerdict =
    | CE
    | AC
    | RJ

type Status =
    | Accepted of NonNegativeInt.T
    | Rejected of PositiveInt.T

type ProblemProgress =
    {UserIndex: NonNegativeInt.T;
    ContestIndex: NonNegativeInt.T;
    ProblemIndex: NonNegativeInt.T;
    Status: Status}

type ProblemInfo =
    {ContestIndex: NonNegativeInt.T;
    ProblemIndex: NonNegativeInt.T;
    Description: String100.T;
    Link: String100.T}

/// Accumulator that will be used to calculate problem progress
let progressAcc state item = 
    match state, item with
    | _, CE -> state
    | None, AC -> Some(Accepted NonNegativeInt.ZERO)
    | None, RJ -> Some(Rejected PositiveInt.ONE)
    | Some(Accepted _), _ -> state
    | Some(Rejected x), AC ->
        let y = x |> PositiveInt.value |> NonNegativeInt.wrap
        Some(Accepted y)
    | Some(Rejected x), RJ -> 
        let y = x |> PositiveInt.value |> (+) 1 |> PositiveInt.wrap            
        Some(Rejected y)

