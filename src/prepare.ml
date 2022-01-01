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
  | Assign of Ident.t loc * expr
  | Let of Ident.t * expr * expr

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
  | Atom s -> (
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
      | None -> failwith (Printf.sprintf "not found: %s" s))
  | List ({ desc = Atom s; loc = loc' } :: args) -> (
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
      | None -> failwith (Printf.sprintf "not_found: %s" s))
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
        | Atom s -> sym ~loc s
        | Bool b -> const ~loc (Const_bool b)
      in
      quote x
  | _ -> failwith "quote: bad syntax"

let lambda_syntax ~loc env = function
  | { Parser.desc = List args; loc = _ } :: body ->
      let args =
        List.map
          (function
            | { Parser.desc = Atom arg; loc } ->
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
  | { Parser.desc = Atom args; loc = loc_args } :: body ->
      let id = Ident.create_local args in
      let env = Env.add args (Evar id) env in
      {
        desc =
          Lambda
            ([], Some (Location.mkloc id loc_args), parse_expr_list env body);
        loc;
      }
  | _ -> failwith "lambda: bad syntax"

let set_syntax ~loc env = function
  | [ { Parser.desc = Atom s; loc = loc_sym }; e ] -> (
      match Env.find_opt s env with
      | None -> failwith (Printf.sprintf "set!: not found: %s" s)
      | Some (Evar id) ->
          { desc = Assign (Location.mkloc id loc_sym, parse_expr env e); loc }
      | Some (Esyntax _) -> failwith "set!: cannot modify syntax"
      | Some (Eprim _) -> failwith "set!: cannot modify primitive")
  | _ -> failwith "set!: bad syntax"

let let_syntax ~loc env = function
  | { Parser.desc = List bindings; loc = _ } :: body ->
      let bindings =
        List.map
          (function
            | {
                Parser.desc = List [ { Parser.desc = Atom var; loc = _ }; e ];
                loc = _;
              } ->
                (var, Ident.create_local var, parse_expr env e)
            | _ -> failwith "let: bad syntax")
          bindings
      in
      let env =
        List.fold_left
          (fun env (var, id, _) -> Env.add var (Evar id) env)
          env bindings
      in
      List.fold_right
        (fun (_, id, e) body -> { desc = Let (id, e, body); loc })
        (* FIXME loc *)
        bindings (parse_expr_list env body)
  | _ -> failwith "let: bad syntax"

let initial_env =
  Env.add "quote" (Esyntax quote_syntax)
    (Env.add "set!" (Esyntax set_syntax)
       (Env.add "let" (Esyntax let_syntax)
          (Env.add "if" (Esyntax if_syntax)
             (Env.add "lambda" (Esyntax lambda_syntax)
                (Env.add "+" (Eprim Paddint)
                   (Env.add "zero?" (Eprim Pzerop) Env.empty))))))
