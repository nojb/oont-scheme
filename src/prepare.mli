type primitive =
  | Pcons
  | Psym of string
  | Paddint
  | Papply
  | Pzerop
  | Pappend
  | Peq
  | Pvectorappend
  | Pvector
  | Plist
  | Pvectoroflist

type constant =
  | Const_bool of bool
  | Const_int of int
  | Const_emptylist
  | Const_undefined
  | Const_string of string

type expr_desc =
  | Const of constant
  | Apply of expr * expr list
  | Var of Ident.t Location.loc
  | If of expr * expr * expr
  | Prim of primitive * expr list
  | Lambda of Ident.t Location.loc list * Ident.t Location.loc option * expr
  | Cseq of expr * expr
  | Assign of Ident.t Location.loc * expr
  | Let of Ident.t * expr * expr

and expr = { desc : expr_desc; loc : Location.t }

type env

val initial_env : env
val parse_expr_list : env -> Parser.sexp list -> expr
