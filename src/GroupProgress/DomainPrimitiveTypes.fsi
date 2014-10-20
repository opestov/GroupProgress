module internal GroupProgress.DomainPrimitiveTypes

module String50 =
    [<NoEquality; NoComparison>]
    type T

    val create: string -> Option<T>
    val value: T -> string

module String100 =
    [<NoEquality; NoComparison>]
    type T

    val create: string -> Option<T>
    val internal wrap : s:string -> T
    val value: T -> string


module PositiveInt =
    [<NoEquality; NoComparison>]
    type T

    val ONE : T

    val create : i:int -> T option
    val internal wrap : i:int -> T
    val value : T -> int


module NonNegativeInt =
    [<NoEquality; NoComparison>]
    type T

    val ZERO : T

    val create : i:int -> T option
    val internal wrap : i:int -> T
    val value : T -> int


