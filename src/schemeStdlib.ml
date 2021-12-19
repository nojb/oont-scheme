exception Error of string

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
    | _ -> assert false

let print x = Format.printf "@[%a@]@." print x
