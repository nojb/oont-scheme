type t = Obj.t

exception Error of t

module H = Weak.Make (struct
  type nonrec t = t

  let hash = Hashtbl.hash

  let equal x1 x2 =
    String.equal (Obj.obj (Obj.field x1 0)) (Obj.obj (Obj.field x2 0))
end)

let symbols = H.create 0

let sym name =
  Obj.repr (H.merge symbols (Obj.with_tag 2 (Obj.repr (Some name))))

let emptylist = 0b111
let falsev = 0b01
let truev = 0b11

let rec print ppf x =
  if Obj.is_int x then
    let x = Obj.obj x in
    if x land 1 == 0 then (* int *)
      Format.pp_print_int ppf (x lsr 1)
    else if x == emptylist then Format.pp_print_string ppf "()"
    else if x == falsev then Format.pp_print_string ppf "#f"
    else if x == truev then Format.pp_print_string ppf "#t"
    else assert false
  else
    match Obj.tag x with
    | 0 -> print_cons ppf x
    | 2 -> Format.pp_print_string ppf (Obj.obj (Obj.field x 0))
    | 4 ->
        (* Error *)
        let msg = Obj.obj (Obj.field x 0) in
        let irritants = Obj.field x 1 in
        Format.fprintf ppf "Error: %s: %a" msg print_cons irritants
    | _ -> assert false

and print_cons ppf x =
  if Obj.is_int x then print ppf x
  else
    match Obj.tag x with
    | 0 ->
        (* cons *)
        let car = Obj.field x 0 in
        let cdr = Obj.field x 1 in
        Format.fprintf ppf "@[<1>(%a .@ %a)@]" print car print cdr
    | _ -> print ppf x

let () =
  Printexc.register_printer (function
    | Error x -> Some (Format.asprintf "%a" print x)
    | _ -> None)

let print x = Format.printf "@[%a@]@." print x
