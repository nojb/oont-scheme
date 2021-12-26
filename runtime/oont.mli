type t

exception Error of t

val sym : string -> t
val print : t -> unit
