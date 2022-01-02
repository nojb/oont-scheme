open Location
open Parser

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
  | Pcall of int * string

module Env = Map.Make (String)

type constant =
  | Const_bool of bool
  | Const_int of int
  | Const_emptylist
  | Const_undefined
  | Const_string of string

type binding =
  | Evar of Ident.t
  | Esyntax of (loc:Location.t -> binding Env.t -> Parser.sexp list -> expr)
  | Eprim of primitive

and expr_desc =
  | Const of constant
  | Apply of expr * expr list
  | Var of Ident.t loc
  | If of expr * expr * expr
  | Prim of primitive * expr list
  | Lambda of Ident.t loc list * Ident.t loc option * expr
  | Cseq of expr * expr
  | Assign of Ident.t loc * expr
  | Let of Ident.t * expr * expr

and expr = { desc : expr_desc; loc : Location.t }

type env = binding Env.t

let mk desc loc = { desc; loc }
let prim ~loc p args = mk (Prim (p, args)) loc
let cons ~loc car cdr = prim ~loc Pcons [ car; cdr ]
let const ~loc c = mk (Const c) loc
let sym ~loc s = prim ~loc (Psym s) []

let check_unique f l =
  let l = List.map f l in
  List.compare_lengths (List.sort_uniq compare l) l = 0

let merge_loc { Location.loc_start; _ } { Location.loc_end; _ } =
  { Location.loc_start; loc_end; loc_ghost = false }

type arity = Fixed of int | Variadic of int

let arity_of_primitive = function
  | Pcons -> Fixed 2
  | Psym _ -> Fixed 0
  | Paddint -> Variadic 0
  | Papply -> Fixed 2
  | Pzerop -> Fixed 1
  | Pappend -> Variadic 0
  | Peq -> Fixed 2
  | Pvectorappend -> Variadic 0
  | Pvector -> Variadic 0
  | Plist -> Variadic 0
  | Pvectoroflist -> Fixed 1
  | Pcall (n, _) -> if n >= 0 then Fixed n else Variadic (-(n + 1))

let undefined = const ~loc:Location.none Const_undefined

let rec parse_sexp env { sexp_desc; sexp_loc = loc } =
  match sexp_desc with
  | Bool b -> const ~loc (Const_bool b)
  | Int n -> const ~loc (Const_int n)
  | String s -> const ~loc (Const_string s)
  | Vector sexpl -> prim ~loc Pvector (List.map (parse_sexp env) sexpl)
  | Atom s -> (
      match Env.find_opt s env with
      | Some (Evar txt) -> mk (Var { txt; loc }) loc
      | Some (Esyntax _) -> failwith "bad syntax"
      | Some (Eprim p) -> (
          match arity_of_primitive p with
          | Fixed n ->
              let args =
                List.init n (fun _ ->
                    Location.mknoloc (Ident.create_local "arg"))
              in
              mk
                (Lambda
                   ( args,
                     None,
                     prim ~loc p
                       (List.map (fun id -> mk (Var id) Location.none) args) ))
                loc
          | Variadic _ -> assert false)
      | None -> failwith (Printf.sprintf "not found: %s" s))
  | List ({ sexp_desc = Atom s; sexp_loc = loc' } :: args) -> (
      match Env.find_opt s env with
      | Some (Evar id) ->
          mk
            (Apply
               ( mk (Var (Location.mkloc id loc')) loc',
                 List.map (parse_sexp env) args ))
            loc
      | Some (Esyntax f) -> f ~loc env args
      | Some (Eprim p) ->
          (match arity_of_primitive p with
          | Fixed n ->
              if List.compare_length_with args n <> 0 then
                failwith "arity mismatch"
          | Variadic n ->
              let n = -n - 1 in
              if List.compare_length_with args n < 0 then
                failwith "arity mismatch");
          prim ~loc p (List.map (parse_sexp env) args)
      | None -> failwith (Printf.sprintf "not_found: %s" s))
  | List [] -> failwith "(): bad syntax"
  | List (f :: args) ->
      mk (Apply (parse_sexp env f, List.map (parse_sexp env) args)) loc

and parse_sexp_list env = function
  | [] -> const ~loc:Location.none Const_undefined
  | [ sexp ] -> parse_sexp env sexp
  | sexp :: sexpl ->
      mk (Cseq (parse_sexp env sexp, parse_sexp_list env sexpl)) Location.none

let begin_syntax ~loc:_ env sexpl = parse_sexp_list env sexpl
let define_syntax ~loc:_ _env _sexpl = failwith "define: not allowed here"

let rec expand_begin env sexpl =
  let expand = function
    | { sexp_desc = List ({ sexp_desc = Atom s; _ } :: sexpl); _ } as sexp -> (
        match Env.find_opt s env with
        | Some (Esyntax f) when f == begin_syntax -> expand_begin env sexpl
        | _ -> [ sexp ])
    | sexp -> [ sexp ]
  in
  List.flatten (List.map expand sexpl)

let is_define env = function
  | { sexp_desc = List ({ sexp_desc = Atom s; _ } :: sexpl); _ } -> (
      match Env.find_opt s env with
      | Some (Esyntax f) when f == define_syntax -> (
          match sexpl with
          | { sexp_desc = List ({ sexp_desc = Atom f; _ } :: args); _ } :: body
            -> (
              let rec doargs accu = function
                | { sexp_desc = Atom s; sexp_loc = loc } :: args ->
                    doargs (Location.mkloc s loc :: accu) args
                | [] -> Some (List.rev accu)
                | _ -> None
              in
              match doargs [] args with
              | None -> None
              | Some args -> Some (f, `Lambda (args, body)))
          | [ { sexp_desc = Atom s; _ }; e ] -> Some (s, `Var e)
          | _ -> failwith "define: bad syntax")
      | _ -> None)
  | _ -> None

let parse_toplevel env sexpl =
  let sexpl = expand_begin env sexpl in
  let rec aux env = function
    | sexp :: sexpl -> (
        match is_define env sexp with
        | Some (s, def) -> (
            let def env =
              match def with
              | `Var e -> parse_sexp env e
              | `Lambda (_args, _body) -> assert false
            in
            match Env.find_opt s env with
            | None | Some (Esyntax _) | Some (Eprim _) ->
                let id = Ident.create_local s in
                let env = Env.add s (Evar id) env in
                mk (Let (id, def env, aux env sexpl)) Location.none
            | Some (Evar id) ->
                mk
                  (Cseq
                     ( mk (Assign (Location.mknoloc id, def env)) Location.none,
                       aux env sexpl ))
                  Location.none)
        | None ->
            let e = parse_sexp env sexp in
            if sexpl = [] then e else mk (Cseq (e, aux env sexpl)) Location.none
        )
    | [] -> const ~loc:Location.none Const_undefined
  in
  aux env sexpl

let parse_body env sexpl =
  let rec aux = function
    | [] -> ([], [])
    | sexp :: sexpl as sexpl' -> (
        match is_define env sexp with
        | Some def ->
            let defs, sexpl = aux sexpl in
            (def :: defs, sexpl)
        | None -> ([], sexpl'))
  in
  let defs, sexpl = aux (expand_begin env sexpl) in
  if not (check_unique (fun (s, _) -> s) defs) then
    failwith "body: multiple define";
  let defs = List.map (fun (s, def) -> (s, Ident.create_local s, def)) defs in
  let env =
    List.fold_left (fun env (s, id, _) -> Env.add s (Evar id) env) env defs
  in
  let body =
    List.fold_left
      (fun cont (_, id, def) ->
        let def =
          match def with
          | `Var e -> parse_sexp env e
          | `Lambda (_args, _body) -> assert false
        in
        mk
          (Cseq (mk (Assign (Location.mknoloc id, def)) Location.none, cont))
          Location.none)
      (parse_sexp_list env sexpl)
      (List.rev defs)
  in
  List.fold_left
    (fun cont (_, id, _) ->
      mk
        (Let (id, const ~loc:Location.none Const_undefined, cont))
        Location.none)
    body (List.rev defs)

let if_syntax ~loc env = function
  | [ x1; x2 ] -> mk (If (parse_sexp env x1, parse_sexp env x2, undefined)) loc
  | [ x1; x2; x3 ] ->
      mk (If (parse_sexp env x1, parse_sexp env x2, parse_sexp env x3)) loc
  | _ -> failwith "if: bad syntax"

let quote_syntax ~loc:_ _env = function
  | [ sexp ] ->
      let rec quote { sexp_desc; sexp_loc = loc } =
        match sexp_desc with
        | List sexpl ->
            List.fold_left
              (fun cdr sexp ->
                let loc = merge_loc sexp.sexp_loc cdr.loc in
                cons ~loc (quote sexp) cdr)
              (const ~loc:Location.none Const_emptylist)
              (List.rev sexpl)
        | Int n -> const ~loc (Const_int n)
        | Atom s -> sym ~loc s
        | Bool b -> const ~loc (Const_bool b)
        | Vector sexpl -> prim ~loc Pvector (List.map quote sexpl)
        | String s -> const ~loc (Const_string s)
      in
      quote sexp
  | _ -> failwith "quote: bad syntax"

let lambda_syntax ~loc env = function
  | { sexp_desc = List args; sexp_loc = _ } :: body ->
      let args =
        List.map
          (function
            | { sexp_desc = Atom arg; sexp_loc = loc } ->
                (arg, Ident.create_local arg, loc)
            | _ -> failwith "lambda: bad syntax")
          args
      in
      let env =
        List.fold_left
          (fun env (arg, id, _) -> Env.add arg (Evar id) env)
          env args
      in
      let args = List.map (fun (_, id, loc) -> Location.mkloc id loc) args in
      mk (Lambda (args, None, parse_body env body)) loc
  | { sexp_desc = Atom args; sexp_loc = loc_args } :: body ->
      let id = Ident.create_local args in
      let env = Env.add args (Evar id) env in
      mk
        (Lambda ([], Some (Location.mkloc id loc_args), parse_body env body))
        loc
  | _ -> failwith "lambda: bad syntax"

let set_syntax ~loc env = function
  | [ { sexp_desc = Atom s; sexp_loc = loc_sym }; e ] -> (
      match Env.find_opt s env with
      | None -> failwith (Printf.sprintf "set!: not found: %s" s)
      | Some (Evar id) ->
          mk (Assign (Location.mkloc id loc_sym, parse_sexp env e)) loc
      | Some (Esyntax _) -> failwith "set!: cannot modify syntax"
      | Some (Eprim _) -> failwith "set!: cannot modify primitive")
  | _ -> failwith "set!: bad syntax"

let let_syntax ~loc env = function
  | { sexp_desc = List bindings; sexp_loc = _ } :: body ->
      let bindings =
        List.map
          (function
            | {
                sexp_desc = List [ { sexp_desc = Atom var; sexp_loc = _ }; e ];
                sexp_loc = _;
              } ->
                (var, Ident.create_local var, parse_sexp env e)
            | _ -> failwith "let: bad syntax")
          bindings
      in
      let env =
        List.fold_left
          (fun env (var, id, _) -> Env.add var (Evar id) env)
          env bindings
      in
      List.fold_right
        (fun (_, id, e) body -> mk (Let (id, e, body)) loc)
        (* FIXME loc *)
        bindings (parse_body env body)
  | _ -> failwith "let: bad syntax"

let and_syntax ~loc env el =
  match List.rev el with
  | [] -> const ~loc (Const_bool true)
  | e :: el ->
      List.fold_left
        (fun accu e ->
          mk
            (If
               ( parse_sexp env e,
                 accu,
                 const ~loc:Location.none (Const_bool false) ))
            Location.none)
        (parse_sexp env e) el

let or_syntax ~loc env el =
  match List.rev el with
  | [] -> const ~loc (Const_bool false)
  | e :: el ->
      List.fold_left
        (fun accu e ->
          let id = Ident.create_local "or" in
          let var = mk (Var (Location.mknoloc id)) Location.none in
          mk
            (Let (id, parse_sexp env e, mk (If (var, var, accu)) Location.none))
            Location.none)
        (parse_sexp env e) el

let when_syntax ~loc env = function
  | e :: el -> mk (If (parse_sexp env e, parse_sexp_list env el, undefined)) loc
  | [] -> failwith "when: bad syntax"

let unless_syntax ~loc env = function
  | e :: el -> mk (If (parse_sexp env e, undefined, parse_sexp_list env el)) loc
  | [] -> failwith "unless: bad syntax"

let cond_syntax ~loc:_ env clauses =
  let rec aux = function
    | [
        {
          sexp_desc = List ({ sexp_desc = Atom "else"; sexp_loc = _ } :: rest);
          sexp_loc = _;
        };
      ] ->
        parse_sexp_list env rest
    | { sexp_desc = List (test :: body); sexp_loc = _ } :: clauses -> (
        let test = parse_sexp env test in
        let else_ = aux clauses in
        match body with
        | [] ->
            let id = Ident.create_local "cond" in
            let var = mk (Var (Location.mknoloc id)) Location.none in
            mk
              (Let (id, test, mk (If (var, var, else_)) Location.none))
              Location.none
        | [ { sexp_desc = Atom "=>"; sexp_loc = _ }; body ] ->
            let id = Ident.create_local "cond" in
            let var = mk (Var (Location.mknoloc id)) Location.none in
            mk
              (Let
                 ( id,
                   test,
                   mk
                     (If
                        ( var,
                          mk
                            (Apply (parse_sexp env body, [ var ]))
                            Location.none,
                          else_ ))
                     Location.none ))
              Location.none
        | body -> mk (If (test, parse_sexp_list env body, else_)) Location.none)
    | [] -> undefined
    | _ -> failwith "cond: bad syntax"
  in
  aux clauses

let quasiquote_syntax ~loc:_ env = function
  | [ x ] ->
      let rec qq n x =
        let loc = x.sexp_loc in
        match x.sexp_desc with
        | List [ { sexp_desc = Atom "quasiquote"; sexp_loc = loc_sym }; x ] ->
            prim ~loc Pcons [ sym ~loc:loc_sym "quasiquote"; qq (n + 1) x ]
        | List [ { sexp_desc = Atom "unquote"; sexp_loc = loc_comma }; x ] ->
            if n = 0 then parse_sexp env x
            else prim ~loc Pcons [ sym ~loc:loc_comma "unquote"; qq (n - 1) x ]
        | List sexpl ->
            List.fold_left
              (fun cdr x ->
                let loc = merge_loc x.sexp_loc cdr.loc in
                match x.sexp_desc with
                | List [ { sexp_desc = Atom "unquote-splicing"; _ }; x ] ->
                    prim ~loc Pappend [ parse_sexp env x; cdr ]
                | _ -> cons ~loc (qq n x) cdr)
              (const ~loc:Location.none Const_emptylist)
              (List.rev sexpl)
        | Vector sexpl ->
            let vectors =
              let rec aux accu = function
                | [] -> [ prim ~loc Pvector (List.rev accu) ]
                | {
                    sexp_desc =
                      List [ { sexp_desc = Atom "unquote-splicing"; _ }; sexp ];
                    _;
                  }
                  :: sexpl ->
                    prim ~loc Pvector (List.rev accu)
                    :: prim ~loc Pvectoroflist [ parse_sexp env sexp ]
                    :: aux [] sexpl
                | sexp :: sexpl -> aux (parse_sexp env sexp :: accu) sexpl
              in
              aux [] sexpl
            in
            prim ~loc Pvectorappend vectors
        | Atom s -> sym ~loc s
        | Bool b -> const ~loc (Const_bool b)
        | Int n -> const ~loc (Const_int n)
        | String s -> const ~loc (Const_string s)
      in
      qq 0 x
  | _ -> failwith "quasiquote: bad syntax"

let include_syntax ~loc:_ _env args =
  let _filenames =
    List.map
      (function
        | { sexp_desc = String s; sexp_loc } -> Location.mkloc s sexp_loc
        | _ -> failwith "include: bad syntax")
      args
  in
  assert false

let initial_env =
  let bindings =
    [
      ("quote", Esyntax quote_syntax);
      ("quasiquote", Esyntax quasiquote_syntax);
      ("and", Esyntax and_syntax);
      ("or", Esyntax or_syntax);
      ("when", Esyntax when_syntax);
      ("unless", Esyntax unless_syntax);
      ("cond", Esyntax cond_syntax);
      ("set!", Esyntax set_syntax);
      ("let", Esyntax let_syntax);
      ("if", Esyntax if_syntax);
      ("lambda", Esyntax lambda_syntax);
      ("+", Eprim Paddint);
      ("zero?", Eprim Pzerop);
      ("eq?", Eprim Peq);
      ("eqv?", Eprim Peq);
      ("include", Esyntax include_syntax);
      ("define", Esyntax define_syntax);
      ("begin", Esyntax begin_syntax);
    ]
  in
  List.fold_left
    (fun env (name, binding) -> Env.add name binding env)
    Env.empty bindings
