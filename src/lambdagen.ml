module P = Prepare
module L = Lambda_helper

let falsev = L.int 0b01
let truev = L.int 0b11
let intv n = L.int (n lsl 1)
let untag_int n = L.lsrint n (L.int 1)
let tag_int n = L.lslint n (L.int 1)
let stringv ~loc s = L.string ~loc s
let emptylist = L.int 0b111
let undefined = L.int 0b1111
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
        (L.sequor (L.not (L.isint v)) (L.eq (L.andint v (L.int 1)) (L.int 1)))
        (type_error v) (L.int 0))

let listv xs = List.fold_left (fun cdr x -> cons x cdr) emptylist (List.rev xs)
let if_ x1 x2 x3 = L.ifthenelse (L.eq x1 falsev) x2 x3

let toint x =
  L.letin x (fun id ->
      let v = L.var id in
      L.seq (checkint v) (untag_int v))

let apply f args =
  L.letin f (fun id ->
      let f = L.var id in
      let doit =
        let arity = L.field f 0 in
        let clos = L.field f 2 in
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

let rec comp_expr { P.desc; loc = _ } =
  match desc with
  | Const (Const_int n) -> intv n
  | Const (Const_bool b) -> boolv b
  | Const Const_emptylist -> emptylist
  | Apply (f, args) -> apply (comp_expr f) (List.map comp_expr args)
  | Var { txt; _ } -> Lvar txt
  | If (e1, e2, e3) ->
      let e3 = match e3 with None -> undefined | Some e3 -> comp_expr e3 in
      if_ (comp_expr e1) (comp_expr e2) e3
  | Prim (p, args) -> comp_primitive p (List.map comp_expr args)
  | Lambda (args, _extra, body) ->
      let args =
        if args = [] then [ Ident.create_local "dummy" ]
        else List.map (fun { Location.txt; _ } -> txt) args
      in
      L.makeblock 4
        [ L.int (List.length args); L.string ""; L.func args (comp_expr body) ]
  | Begin [] -> undefined
  | Begin (e :: es) ->
      List.fold_left (fun accu e -> L.seq accu (comp_expr e)) (comp_expr e) es
