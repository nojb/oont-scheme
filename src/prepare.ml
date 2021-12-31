open Location

type primitive = Pcons | Psym of string | Paddint | Papply | Pzerop

module Env = Map.Make (String)

type constant = Const_bool of bool | Const_int of int | Const_emptylist

type binding =
  | Evar of Ident.t
  | Esyntax of (loc:Location.t -> binding Env.t -> Parser.sexp list -> expr)
  | Eprim of primitive

and expr_desc =
  | Const of constant
  | Apply of expr * expr list
  | Var of Ident.t loc
  | If of expr * expr * expr option
  | Prim of primitive * expr list
  | Lambda of Ident.t loc list * Ident.t loc option * expr
  | Begin of expr list

and expr = { desc : expr_desc; loc : Location.t }

type env = binding Env.t

let prim ~loc p args = { desc = Prim (p, args); loc }
let cons ~loc car cdr = prim ~loc Pcons [ car; cdr ]
let const ~loc c = { desc = Const c; loc }
let sym ~loc s = prim ~loc (Psym s) []

let merge_loc { Location.loc_start; _ } { Location.loc_end; _ } =
  { Location.loc_start; loc_end; loc_ghost = false }

type arity = Fixed of int | Variadic of int

let arity_of_primitive = function
  | Pcons -> Fixed 2
  | Psym _ -> Fixed 0
  | Paddint -> Variadic 0
  | Papply -> Fixed 2
  | Pzerop -> Fixed 1

let rec parse_expr env { Parser.desc; loc } =
  match desc with
  | Bool b -> const ~loc (Const_bool b)
  | Int n -> const ~loc (Const_int n)
  | Symbol s -> (
      match Env.find_opt s env with
      | Some (Evar txt) -> { desc = Var { txt; loc }; loc }
      | Some (Esyntax _) -> failwith "bad syntax"
      | Some (Eprim p) -> (
          match arity_of_primitive p with
          | Fixed n ->
              let args =
                List.init n (fun _ ->
                    Location.mknoloc (Ident.create_local "arg"))
              in
              {
                desc =
                  Lambda
                    ( args,
                      None,
                      prim ~loc p
                        (List.map
                           (fun id -> { desc = Var id; loc = Location.none })
                           args) );
                loc;
              }
          | Variadic _ -> assert false)
      | None -> failwith "not found")
  | List ({ desc = Symbol s; loc = loc' } :: args) -> (
      match Env.find_opt s env with
      | Some (Evar id) ->
          {
            desc =
              Apply
                ( { desc = Var (Location.mkloc id loc'); loc = loc' },
                  List.map (parse_expr env) args );
            loc;
          }
      | Some (Esyntax f) -> f ~loc env args
      | Some (Eprim p) -> prim ~loc p (List.map (parse_expr env) args)
      | _ -> failwith "parse_expr")
  | List [] -> failwith "(): bad syntax"
  | List (f :: args) ->
      { desc = Apply (parse_expr env f, List.map (parse_expr env) args); loc }

and parse_expr_list env xs =
  { desc = Begin (List.map (parse_expr env) xs); loc = Location.none }

let if_syntax ~loc env = function
  | [ x1; x2 ] ->
      { desc = If (parse_expr env x1, parse_expr env x2, None); loc }
  | [ x1; x2; x3 ] ->
      {
        desc =
          If (parse_expr env x1, parse_expr env x2, Some (parse_expr env x3));
        loc;
      }
  | _ -> failwith "if: bad syntax"

let quote_syntax ~loc:_ _env = function
  | [ x ] ->
      let rec quote { Parser.desc; loc } =
        match desc with
        | List xs ->
            List.fold_left
              (fun cdr x ->
                let loc = merge_loc x.Parser.loc cdr.loc in
                cons ~loc (quote x) cdr)
              (const ~loc:Location.none Const_emptylist)
              xs
        | Int n -> const ~loc (Const_int n)
        | Symbol s -> sym ~loc s
        | Bool b -> const ~loc (Const_bool b)
      in
      quote x
  | _ -> failwith "quote: bad syntax"

let lambda_syntax ~loc env = function
  | { Parser.desc = List args; loc = _ } :: body ->
      let args =
        List.map
          (function
            | { Parser.desc = Symbol arg; loc } ->
                (arg, Ident.create_local arg, loc)
            | _ -> assert false)
          args
      in
      let env =
        List.fold_left
          (fun env (arg, id, _) -> Env.add arg (Evar id) env)
          env args
      in
      let args = List.map (fun (_, id, loc) -> Location.mkloc id loc) args in
      { desc = Lambda (args, None, parse_expr_list env body); loc }
  | _ -> failwith "lambda: bad syntax"

let initial_env =
  Env.add "quote" (Esyntax quote_syntax)
    (Env.add "if" (Esyntax if_syntax)
       (Env.add "lambda" (Esyntax lambda_syntax)
          (Env.add "+" (Eprim Paddint)
             (Env.add "zero?" (Eprim Pzerop) Env.empty))))
