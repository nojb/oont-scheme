open Lambda

type t = lambda

let rec const x =
  if Obj.is_int x then Lambda.const_int (Obj.magic x : int)
  else
    let tag = Obj.tag x in
    if tag = Obj.string_tag then Const_immstring (Obj.magic x : string)
    else if tag <= Obj.last_non_constant_constructor_tag then (
      let l = ref [] in
      for i = Obj.size x - 1 downto 0 do
        l := const (Obj.field x i) :: !l
      done;
      Const_block (tag, !l))
    else Misc.fatal_errorf "%s tag=%i" __FUNCTION__ tag

let const x = Lconst (const (Obj.repr x))
let prim p ts = Lprim (p, ts, Loc_unknown)
let makeblock tag ts = prim (Pmakeblock (tag, Immutable, None)) ts
let makemutable tag ts = prim (Pmakeblock (tag, Mutable, None)) ts
let ifthenelse t1 t2 t3 = Lifthenelse (t1, t2, t3)
let eq t1 t2 = prim (Pintcomp Ceq) [ t1; t2 ]
let isint t = prim Pisint [ t ]
let not t = prim Pnot [ t ]
let addint t1 t2 = prim Paddint [ t1; t2 ]
let andint t1 t2 = prim Pandint [ t1; t2 ]
let lslint t1 t2 = prim Plslint [ t1; t2 ]
let lsrint t1 t2 = prim Plsrint [ t1; t2 ]
let field n t = prim (Pfield n) [ t ]
let setfield n t1 t2 = prim (Psetfield (n, Pointer, Assignment)) [ t1; t2 ]
let int n = Lconst (const_int n)
let assign id t = Lassign (id, t)

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

let letin lam f =
  match lam with
  | Lvar id -> f id
  | _ ->
      let id = Ident.create_local "let" in
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

let var id = Lvar id
let sequand t1 t2 = prim Psequand [ t1; t2 ]
let sequor t1 t2 = prim Psequor [ t1; t2 ]

let func ids t =
  Lfunction
    {
      kind = Curried;
      params = List.map (fun id -> (id, Pgenval)) ids;
      return = Pgenval;
      body = t;
      attr = default_function_attribute;
      loc = Loc_unknown;
    }

let block_switch scrutinee blocks fail =
  let sw_numblocks =
    match fail with
    | Some _ -> Obj.last_non_constant_constructor_tag
    | None -> List.fold_left (fun accu (tag, _) -> max accu tag) 0 blocks
  in
  Lswitch
    ( scrutinee,
      {
        sw_numconsts = 0;
        sw_consts = [];
        sw_numblocks;
        sw_blocks = blocks;
        sw_failaction = fail;
      },
      Loc_unknown )
