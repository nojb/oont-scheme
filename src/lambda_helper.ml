open Lambda

type t = lambda

let prim p ts = Lprim (p, ts, Loc_unknown)
let makeblock tag ts = prim (Pmakeblock (tag, Immutable, None)) ts
let makemutable tag ts = prim (Pmakeblock (tag, Mutable, None)) ts
let ifthenelse t1 t2 t3 = Lifthenelse (t1, t2, t3)
let eq t1 t2 = prim (Pintcomp Ceq) [ t1; t2 ]
let isint t = prim Pisint [ t ]
let addint t1 t2 = prim Paddint [ t1; t2 ]
let andint t1 t2 = prim Pandint [ t1; t2 ]
let lslint t1 t2 = prim Plslint [ t1; t2 ]
let lsrint t1 t2 = prim Plsrint [ t1; t2 ]
let int n = Lconst (const_int n)

let extension_constructor modname name =
  let env =
    Env.add_persistent_structure (Ident.create_persistent modname) Env.empty
  in
  let lid = Longident.Ldot (Longident.Lident modname, name) in
  match Env.find_constructor_by_name lid env with
  | { cstr_tag = Cstr_extension (path, _); _ } ->
      transl_extension_path Loc_unknown env path
  | _ | (exception Not_found) ->
      Misc.fatal_error
        (Printf.sprintf "Extension constructor %s.%s not found." modname name)

let value modname name = transl_prim modname name

let letin name lam f =
  match lam with
  | Lvar id -> f id
  | _ ->
      let id = Ident.create_local name in
      Llet (Strict, Pgenval, id, lam, f id)

let raise exn = prim (Praise Raise_regular) [ exn ]

let string ?(loc = Location.none) s =
  Lconst (Const_base (Const_string (s, loc, None)))

let seq t1 t2 = Lsequence (t1, t2)

let apply t ts =
  Lapply
    {
      ap_func = t;
      ap_args = ts;
      ap_loc = Loc_unknown;
      ap_tailcall = Default_tailcall;
      ap_inlined = Default_inline;
      ap_specialised = Default_specialise;
    }
