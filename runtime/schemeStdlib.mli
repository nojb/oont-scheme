type t

exception Error of string * t list

val sym : string -> t
val print : t -> unit
