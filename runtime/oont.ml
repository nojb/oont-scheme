(*

Runtime representation
----------------------

int                   immediate               msb 0
true                  immediate               msb 1, 0b001
false                 immediate               msb 1, 0b000
empty list            immediate               msb 1, 0b100
undefined             immediate               msb 1, 0b110
eof                   immediate               msb 1, 0b101
char                  immediate               msb 1, 0b111
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

let msb = 1 lsl (Sys.word_size - 2)
let emptylist = msb lor 0b100
let falsev = msb
let truev = msb lor 1

let rec print ppf x =
  if Obj.is_int x then
    let x = Obj.obj x in
    if x land msb = 0 then (* int *)
      Format.pp_print_int ppf x
    else if x = emptylist then Format.pp_print_string ppf "()"
    else if x = falsev then Format.pp_print_string ppf "#f"
    else if x = truev then Format.pp_print_string ppf "#t"
    else assert false
  else
    match Obj.tag x with
    | 0 ->
        (* cons *)
        let car = Obj.field x 0 in
        let cdr = Obj.field x 1 in
        Format.fprintf ppf "@[<1>(%a .@ %a)@]" print car print cdr
    | 3 -> Format.pp_print_string ppf (Obj.obj (Obj.field x 0))
    | 4 ->
        (* procedure *)
        let name = Obj.obj (Obj.field x 1) in
        if name <> "" then Format.fprintf ppf "#<%s:procedure>" name
        else Format.pp_print_string ppf "#<procedure>"
    | 5 ->
        (* error *)
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
