exception Error of string

module H = Weak.Make (struct
  type t = Obj.t

  let hash = Hashtbl.hash

  let equal x1 x2 =
    String.equal (Obj.obj (Obj.field x1 0)) (Obj.obj (Obj.field x2 0))
end)

let symbols = H.create 0

let sym name =
  Obj.repr (H.merge symbols (Obj.with_tag 2 (Obj.repr (Some name))))

let rec print ppf x =
  if Obj.is_int x then
    let x = Obj.obj x in
    if x land 1 == 0 then (* int *)
      Format.pp_print_int ppf (x lsr 1)
    else assert false
  else
    match Obj.tag x with
    | 0 ->
        Format.fprintf ppf "@[<1>(%a .@ %a)@]" print (Obj.field x 0) print
          (Obj.field x 1)
    | 2 -> Format.pp_print_string ppf (Obj.obj (Obj.field x 0))
    | _ -> assert false

let print x = Format.printf "@[%a@]@." print x
