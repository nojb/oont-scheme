module P = Prepare
module L = Lambda_helper

let msb = 1 lsl (Sys.word_size - 2)
let falsev = L.int msb
let truev = L.int (msb lor 1)
let intv n = L.int (n land lnot msb)
let untag_int n = n
let tag_int n = L.andint n (L.int (lnot msb))

(* let stringv ~loc s = L.string ~loc s *)
let emptylist = L.int (msb lor 0b100)
let undefined = L.int (msb lor 0b110)
let prim name = L.value "Oont" name
let boolv b = if b then truev else falsev
let tag_bool x = L.ifthenelse x truev falsev
let error_exn = lazy (L.extension_constructor "Oont" "Error")
let cons car cdr = L.makemutable 0 [ car; cdr ]
let errorv ~loc s objs = L.makeblock 5 (L.string ~loc s :: objs)

let type_error obj =
  L.raise
    (L.makeblock 0
       [ Lazy.force error_exn; errorv ~loc:Location.none "Type error" [ obj ] ])

let checkint n =
  L.letin n (fun id ->
      let v = L.var id in
      L.ifthenelse
        (L.sequor (L.not (L.isint v)) (L.lsrint v (L.int (Sys.word_size - 2))))
        (type_error v) (L.int 0))

(* let listv xs = List.fold_left (fun cdr x -> cons x cdr) emptylist (List.rev xs) *)
let if_ x1 x2 x3 = L.ifthenelse (L.eq x1 falsev) x2 x3

let toint x =
  L.letin x (fun id ->
      let v = L.var id in
      L.seq (checkint v) (untag_int v))

let apply f args =
  L.letin f (fun id ->
      let f = L.var id in
      let doit =
        let arity = L.field 0 f in
        let clos = L.field 2 f in
        let nargs = if args = [] then 1 else List.length args in
        L.ifthenelse
          (L.eq (L.int nargs) arity)
          (L.apply clos (if args = [] then [ L.int 0 ] else args))
          (type_error f)
      in
      L.seq
        (L.ifthenelse (L.isint f) (type_error f) (L.int 0))
        (L.block_switch f [ (4, doit) ] (Some (type_error f))))

let get_sym s = L.apply (prim "sym") [ L.string s ]

let comp_primitive p args =
  match (p, args) with
  | P.Pzerop, [ x ] ->
      L.letin x (fun id ->
          let v = L.var id in
          L.seq (checkint v) (tag_bool (L.eq v (L.int 0))))
  | Paddint, [] -> intv 0
  | Paddint, x :: xs ->
      tag_int
        (List.fold_left
           (fun accu x ->
             let n = toint x in
             L.addint accu n)
           (toint x) xs)
  | Pcons, [ car; cdr ] -> cons car cdr
  | Psym s, [] -> get_sym s
  | _ -> invalid_arg "comp_primitive"

module Map = Ident.Map
module Set = Ident.Set

let rec assigned_vars e =
  match e.P.desc with
  | Const _ | Var _ -> Set.empty
  | Apply (f, args) ->
      List.fold_left
        (fun accu e -> Set.union accu (assigned_vars e))
        (assigned_vars f) args
  | If (e1, e2, None) -> Set.union (assigned_vars e1) (assigned_vars e2)
  | If (e1, e2, Some e3) ->
      Set.union (assigned_vars e1)
        (Set.union (assigned_vars e2) (assigned_vars e3))
  | Prim (_, el) | Begin el ->
      List.fold_left
        (fun accu e -> Set.union accu (assigned_vars e))
        Set.empty el
  | Lambda (_, _, body) -> assigned_vars body
  | Assign ({ txt = id; _ }, e) -> Set.add id (assigned_vars e)
  | Let (_, e1, e2) -> Set.union (assigned_vars e1) (assigned_vars e2)

type env = { vars : Ident.t Map.t; assigned_vars : Set.t }

let add_var id id1 env = { env with vars = Map.add id id1 env.vars }

let rec comp_expr env { P.desc; loc = _ } =
  match desc with
  | Const (Const_int n) -> intv n
  | Const (Const_bool b) -> boolv b
  | Const Const_emptylist -> emptylist
  | Const Const_undefined -> undefined
  | Apply (f, args) -> apply (comp_expr env f) (List.map (comp_expr env) args)
  | Var id ->
      let var = L.var (Map.find id.txt env.vars) in
      if Set.mem id.txt env.assigned_vars then L.field 0 var else var
  | If (e1, e2, e3) ->
      let e3 =
        match e3 with None -> undefined | Some e3 -> comp_expr env e3
      in
      if_ (comp_expr env e1) (comp_expr env e2) e3
  | Prim (p, args) -> comp_primitive p (List.map (comp_expr env) args)
  | Lambda (args, _extra, body) ->
      let args, env =
        if args = [] then ([ Ident.create_local "dummy" ], env)
        else
          let args =
            List.map
              (fun { Location.txt; _ } -> (txt, Ident.create_local "arg"))
              args
          in
          ( List.map snd args,
            List.fold_left (fun env (id, id1) -> add_var id id1 env) env args )
      in
      L.makeblock 4
        [
          L.int (List.length args);
          L.string "";
          L.func args (comp_expr env body);
        ]
  | Begin [] -> undefined
  | Begin (e :: es) ->
      List.fold_left
        (fun accu e -> L.seq accu (comp_expr env e))
        (comp_expr env e) es
  | Assign (id, e) ->
      L.setfield 0 (L.var (Map.find id.txt env.vars)) (comp_expr env e)
  | Let (id, e1, e2) ->
      let var = comp_expr env e1 in
      let mut = Set.mem id env.assigned_vars in
      let var = if mut then L.makeblock 0 [ var ] else var in
      L.letin var (fun id1 -> comp_expr (add_var id id1 env) e2)

let comp_expr e =
  comp_expr { assigned_vars = assigned_vars e; vars = Map.empty } e
