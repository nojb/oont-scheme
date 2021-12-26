type t = Lambda.lambda

val makeblock : int -> t list -> t
val makemutable : int -> t list -> t
val ifthenelse : t -> t -> t -> t
val eq : t -> t -> t
val isint : t -> t
val not : t -> t
val addint : t -> t -> t
val andint : t -> t -> t
val lslint : t -> t -> t
val lsrint : t -> t -> t
val int : int -> t
val extension_constructor : string -> string -> t
val value : string -> string -> t
val letin : t -> (Ident.t -> t) -> t
val raise : t -> t
val string : ?loc:Location.t -> string -> t
val seq : t -> t -> t
val apply : t -> t list -> t
val var : Ident.t -> t
val sequand : t -> t -> t
val sequor : t -> t -> t
