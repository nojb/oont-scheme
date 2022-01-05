type t

exception Error of t

type kind =
  | True
  | False
  | Empty_list
  | Undefined
  | Eof
  | Char
  | Int
  | Pair
  | Vector
  | String
  | Symbol
  | Bytevector
  | Procedure
  | Error_object

type closure

val emptylist : t
val undefined : t
val false_ : t
val true_ : t
val int : int -> t

(* *)

val classify : t -> kind

(* *)

val unsafe_to_int : t -> int
val unsafe_to_uchar : t -> Uchar.t
val unsafe_car : t -> t
val unsafe_cdr : t -> t
val unsafe_set_car : t -> t -> unit
val unsafe_set_cdr : t -> t -> unit
val unsafe_vector_array : t -> t array
val unsafe_string_data : t -> bytes
val unsafe_symbol_name : t -> string
val unsafe_bytevector_bytes : t -> bytes
val unsafe_procedure_arity : t -> int
val unsafe_procedure_name : t -> string
val unsafe_procedure_closure : t -> closure
val unsafe_error_msg : t -> string
val unsafe_error_irritants : t -> t array

(* *)

val mksym : string -> t
val mkpair : t -> t -> t
val mkvector : t array -> t
val mkstring : bytes -> t
val mkbytevector : bytes -> t
val mkerror : string -> t array -> t
val mkprocedure : int -> string -> closure -> t

(* *)

val print : t -> unit
