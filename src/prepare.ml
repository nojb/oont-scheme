open Location

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
  | Pappend -> Variadic 0
  | Peq -> Fixed 2
  | Pvectorappend -> Variadic 0
  | Pvector -> Variadic 0
  | Plist -> Variadic 0
  | Pvectoroflist -> Fixed 1

let undefined = { desc = Const Const_undefined; loc = Location.none }

let rec parse_expr env { Parser.desc; loc } =
  match desc with
  | Bool b -> const ~loc (Const_bool b)
  | Int n -> const ~loc (Const_int n)
  | String s -> const ~loc (Const_string s)
  | Vector sexpl -> prim ~loc Pvector (List.map (parse_expr env) sexpl)
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
      | Some (Eprim p) ->
          (match arity_of_primitive p with
          | Fixed n ->
              if List.compare_length_with args n <> 0 then
                failwith "arity mismatch"
          | Variadic n ->
              let n = -n - 1 in
              if List.compare_length_with args n < 0 then
                failwith "arity mismatch");
          prim ~loc p (List.map (parse_expr env) args)
      | None -> failwith (Printf.sprintf "not_found: %s" s))
  | List [] -> failwith "(): bad syntax"
  | List (f :: args) ->
      { desc = Apply (parse_expr env f, List.map (parse_expr env) args); loc }

and parse_expr_list env xs =
  { desc = Begin (List.map (parse_expr env) xs); loc = Location.none }

let if_syntax ~loc env = function
  | [ x1; x2 ] ->
      { desc = If (parse_expr env x1, parse_expr env x2, undefined); loc }
  | [ x1; x2; x3 ] ->
      {
        desc = If (parse_expr env x1, parse_expr env x2, parse_expr env x3);
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
              (List.rev xs)
        | Int n -> const ~loc (Const_int n)
        | Atom s -> sym ~loc s
        | Bool b -> const ~loc (Const_bool b)
        | Vector sexpl -> prim ~loc Pvector (List.map quote sexpl)
        | String s -> const ~loc (Const_string s)
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

let and_syntax ~loc env el =
  match List.rev el with
  | [] -> { desc = Const (Const_bool true); loc }
  | e :: el ->
      List.fold_left
        (fun accu e ->
          {
            desc =
              If
                ( parse_expr env e,
                  accu,
                  { desc = Const (Const_bool false); loc = Location.none } );
            loc = Location.none;
          })
        (parse_expr env e) el

let or_syntax ~loc env el =
  match List.rev el with
  | [] -> { desc = Const (Const_bool false); loc }
  | e :: el ->
      List.fold_left
        (fun accu e ->
          let id = Ident.create_local "or" in
          let var = { desc = Var (Location.mknoloc id); loc = Location.none } in
          {
            desc =
              Let
                ( id,
                  parse_expr env e,
                  { desc = If (var, var, accu); loc = Location.none } );
            loc = Location.none;
          })
        (parse_expr env e) el

let when_syntax ~loc env = function
  | e :: el ->
      { desc = If (parse_expr env e, parse_expr_list env el, undefined); loc }
  | [] -> failwith "when: bad syntax"

let unless_syntax ~loc env = function
  | e :: el ->
      { desc = If (parse_expr env e, undefined, parse_expr_list env el); loc }
  | [] -> failwith "unless: bad syntax"

let cond_syntax ~loc:_ env clauses =
  let rec aux = function
    | [
        {
          Parser.desc = List ({ desc = Atom "else"; loc = _ } :: rest);
          loc = _;
        };
      ] ->
        parse_expr_list env rest
    | { desc = List (test :: body); loc = _ } :: clauses -> (
        let test = parse_expr env test in
        let else_ = aux clauses in
        match body with
        | [] ->
            let id = Ident.create_local "cond" in
            let var =
              { desc = Var (Location.mknoloc id); loc = Location.none }
            in
            {
              desc =
                Let
                  ( id,
                    test,
                    { desc = If (var, var, else_); loc = Location.none } );
              loc = Location.none;
            }
        | [ { desc = Atom "=>"; loc = _ }; body ] ->
            let id = Ident.create_local "cond" in
            let var =
              { desc = Var (Location.mknoloc id); loc = Location.none }
            in
            {
              desc =
                Let
                  ( id,
                    test,
                    {
                      desc =
                        If
                          ( var,
                            {
                              desc = Apply (parse_expr env body, [ var ]);
                              loc = Location.none;
                            },
                            else_ );
                      loc = Location.none;
                    } );
              loc = Location.none;
            }
        | body ->
            {
              desc = If (test, parse_expr_list env body, else_);
              loc = Location.none;
            })
    | [] -> undefined
    | _ -> failwith "cond: bad syntax"
  in
  aux clauses

let quasiquote_syntax ~loc:_ env = function
  | [ x ] ->
      let rec qq n x =
        let loc = x.Parser.loc in
        match x.Parser.desc with
        | List [ { desc = Atom "quasiquote"; loc = loc_sym }; x ] ->
            prim ~loc Pcons [ sym ~loc:loc_sym "quasiquote"; qq (n + 1) x ]
        | List [ { desc = Atom "unquote"; loc = loc_comma }; x ] ->
            if n = 0 then parse_expr env x
            else prim ~loc Pcons [ sym ~loc:loc_comma "unquote"; qq (n - 1) x ]
        | List sexpl ->
            List.fold_left
              (fun cdr x ->
                let loc = merge_loc x.Parser.loc cdr.loc in
                match x.Parser.desc with
                | List [ { desc = Atom "unquote-splicing"; _ }; x ] ->
                    prim ~loc Pappend [ parse_expr env x; cdr ]
                | _ -> cons ~loc (qq n x) cdr)
              (const ~loc:Location.none Const_emptylist)
              (List.rev sexpl)
        | Vector sexpl ->
            let vectors =
              let rec aux accu = function
                | [] -> [ prim ~loc Pvector (List.rev accu) ]
                | {
                    Parser.desc =
                      List
                        [ { Parser.desc = Atom "unquote-splicing"; _ }; sexp ];
                    _;
                  }
                  :: sexpl ->
                    prim ~loc Pvector (List.rev accu)
                    :: prim ~loc Pvectoroflist [ parse_expr env sexp ]
                    :: aux [] sexpl
                | sexp :: sexpl -> aux (parse_expr env sexp :: accu) sexpl
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
        | { Parser.desc = String s; loc } -> Location.mkloc s loc
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
    ]
  in
  List.fold_left
    (fun env (name, binding) -> Env.add name binding env)
    Env.empty bindings
