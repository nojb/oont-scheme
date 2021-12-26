(*

Runtime representation
----------------------

int                   immediate               0bXXXXXXXXXX0
true                  immediate               0b11
false                 immediate               0b01
empty list            immediate               0b111
undefined             immediate               0b1111
eof                   immediate               0b11111
char                  immediate               0bXXXXXXXX101
pair                  block                   tag 0, size 2
vector                block                   tag 1, size n
string                block                   tag 2, size 1: name
symbol                block                   tag 3, size 1: name
bytevector            block                   bytes
procedure             block                   tag 4, size 3: arity, name, closure
                                              (arity is -(1+n) if variadic with n fixed arguments)
error                 block                   tag 5, size 1 + n: name, irritants

*)

type t = Obj.t

let () = Printexc.record_backtrace true

exception Error of t

module H = Weak.Make (struct
  type nonrec t = t

  let hash = Hashtbl.hash

  let equal x1 x2 =
    String.equal (Obj.obj (Obj.field x1 0)) (Obj.obj (Obj.field x2 0))
end)

let symbols = H.create 0

let sym name =
  Obj.repr (H.merge symbols (Obj.with_tag 3 (Obj.repr (Some name))))

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
    | 0 ->
        let car = Obj.field x 0 in
        let cdr = Obj.field x 1 in
        Format.fprintf ppf "@[<1>(%a .@ %a)@]" print car print cdr
    | 3 -> Format.pp_print_string ppf (Obj.obj (Obj.field x 0))
    | 5 ->
        (* Error *)
        let n = Obj.size x - 1 in
        let msg = Obj.obj (Obj.field x 0) in
        Format.fprintf ppf "Error: %s:" msg;
        for i = 1 to n do
          Format.fprintf ppf "@ %a" print (Obj.field x i)
        done
    | _ -> assert false

let () =
  Printexc.register_printer (function
    | Error x -> Some (Format.asprintf "%a" print x)
    | _ -> None)

let print x = Format.printf "@[%a@]@." print x
