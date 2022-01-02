type t

exception Error of t

val get_sym : string -> t
val print : t -> unit
val append : t list -> t
val list_to_vector : t -> t
val vector_append : t list -> t
val apply : t -> t -> t
