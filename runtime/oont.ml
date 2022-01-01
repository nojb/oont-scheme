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

let rec write_simple oc x =
  if Obj.is_int x then
    let x = Obj.obj x in
    if x land msb = 0 then (* int *)
      output_string oc (string_of_int x)
    else if x = emptylist then output_string oc "()"
    else if x = falsev then output_string oc "#f"
    else if x = truev then output_string oc "#t"
    else assert false
  else
    match Obj.tag x with
    | 0 ->
        (* cons *)
        let car = Obj.field x 0 in
        let cdr = Obj.field x 1 in
        Printf.fprintf oc "(%a . %a)" write_simple car write_simple cdr
    | 1 ->
        (* vector *)
        let aux oc x =
          for i = 0 to Obj.size x - 1 do
            if i > 0 then output_char oc ' ';
            write_simple oc (Obj.field x i)
          done
        in
        Printf.fprintf oc "#(%a)" aux x
    | 3 ->
        (* symbol *)
        output_string oc (Obj.obj (Obj.field x 0))
    | 4 ->
        (* procedure *)
        let name = Obj.obj (Obj.field x 1) in
        if name <> "" then Printf.fprintf oc "#<%s:procedure>" name
        else output_string oc "#<procedure>"
    | 5 ->
        (* error *)
        let n = Obj.size x - 1 in
        let msg = Obj.obj (Obj.field x 0) in
        Printf.fprintf oc "Error: %s:" msg;
        for i = 1 to n do
          Printf.fprintf oc " %a" write_simple (Obj.field x i)
        done
    | _ -> assert false

(* let () = *)
(*   Printexc.register_printer (function *)
(*     | Error x -> Some (Format.asprintf "%a" write_simple x) *)
(*     | _ -> None) *)

let print x = write_simple stdout x
let null = Obj.repr emptylist
let is_null obj = obj = null
let is_pair obj = Obj.is_block obj && Obj.tag obj = 0

let rec append = function
  | [] -> null
  | list :: lists ->
      let rec loop list =
        if Obj.is_int list then
          if list = null then append lists else assert false (* type error *)
        else
          match Obj.tag list with
          | 0 -> Obj.repr (Obj.field list 0, loop (Obj.field list 1))
          | _ ->
              (* type error *)
              assert false
      in
      loop list

let list_to_vector list =
  let size =
    let rec aux accu list =
      if is_null list then accu
      else if is_pair list then aux (accu + 1) (Obj.field list 1)
      else assert false
      (* type error *)
    in
    aux 0 list
  in
  let vec = Obj.new_block 1 size in
  let rec loop i list =
    if i = size then vec
    else (
      Obj.set_field vec i (Obj.field list 0);
      loop (i + 1) (Obj.field list 1))
  in
  loop 0 list

let is_vector obj = Obj.is_block obj && Obj.tag obj = 1

let vector_append vectors =
  let size =
    let rec aux accu = function
      | [] -> accu
      | vector :: vectors ->
          if is_vector vector then aux (accu + Obj.size vector) vectors
          else assert false
      (* type error *)
    in
    aux 0 vectors
  in
  let vec = Obj.new_block 1 size in
  let rec loop i = function
    | [] -> vec
    | vector :: vectors ->
        let size = Obj.size vector in
        for j = 0 to size - 1 do
          Obj.set_field vec (i + j) (Obj.field vector j)
        done;
        loop (i + size) vectors
  in
  loop 0 vectors

let apply _ _ = assert false
