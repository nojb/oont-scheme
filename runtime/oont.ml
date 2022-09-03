(*

Runtime representation
----------------------

int                   immediate               lsb 0
false                 immediate               0b0001
true                  immediate               0b0011
empty list            immediate               0b0101
undefined             immediate               0b0111
eof                   immediate               0b1001
char                  immediate               0b1011
pair                  block                   tag 0, size 2
vector                block                   tag 1, size 1 (array)
string                block                   tag 2, size 1 (mutable, bytes)
symbol                block                   tag 3, size 1 (bytes)
bytevector            block                   tag 4, size 1 (bytes)
procedure             block                   tag 5, size 3 (arity, name, closure)
                                              (arity is -(1+n) if variadic with n fixed arguments)
error                 block                   tag 6, size 2 (name, array of irritants)

*)

type t = Obj.t

exception Error of t

type kind =
  | True
  | False
  | Empty_list
  | Undefined
  | Eof
  | Char
  | Int
  | Pair
  | Vector
  | String
  | Symbol
  | Bytevector
  | Procedure
  | Error_object

type closure

type input_port =
  | String of { data : string; mutable pos : int }
  | In_channel of { peek : Uchar.t option; chan : in_channel }
(* FIXME unicode *)

type output_port = Buffer of Buffer.t | Out_channel of out_channel

type block =
  | Pair of { mutable car : t; mutable cdr : t }
  | Vector of t array
  | String of { mutable data : bytes }
  | Symbol of { name : string }
  | Bytevector of bytes
  | Procedure of { arity : int; name : string; closure : closure }
  | Error_object of { msg : string; irritants : t array }
  | Input_port of input_port

let mk t : t =
  let n =
    match t with
    | False -> 0b0001
    | True -> 0b0011
    | Empty_list -> 0b0101
    | Eof -> 0b1001
    | Undefined -> 0b0111
    | Char -> 0b1011
    | Int | Pair | Vector | String | Symbol | Bytevector | Procedure
    | Error_object ->
        assert false
  in
  Obj.repr n

let emptylist = mk Empty_list
let undefined = mk Undefined
let true_ = mk True
let false_ = mk False
let int n = Obj.repr (n lsl 1)
let mkchar u = Obj.repr ((Uchar.to_int u lsl 4) lor 0b1011)

let classify t : kind =
  if Obj.is_block t then
    match (Obj.obj t : block) with
    | Pair _ -> Pair
    | Vector _ -> Vector
    | String _ -> String
    | Symbol _ -> Symbol
    | Bytevector _ -> Bytevector
    | Procedure _ -> Procedure
    | Error_object _ -> Error_object
  else
    let t : int = Obj.obj t in
    if t land 1 = 0 then Int
    else
      match t land 0b1111 with
      | 0b0001 -> False
      | 0b0011 -> True
      | 0b0101 -> Empty_list
      | 0b1001 -> Eof
      | 0b0111 -> Undefined
      | 0b1011 -> Char
      | _ -> assert false

let unsafe_to_int t : int = Obj.obj t asr 1
let unsafe_to_uchar t = Uchar.unsafe_of_int ((Obj.obj t : int) lsr 4)
let unsafe_car t = Obj.field t 0
let unsafe_cdr t = Obj.field t 1
let unsafe_set_car t obj = Obj.set_field t 0 obj
let unsafe_set_cdr t obj = Obj.set_field t 1 obj
let unsafe_symbol_name t : string = Obj.obj (Obj.field t 0)
let unsafe_string_data t : bytes = Obj.obj (Obj.field t 0)
let unsafe_vector_array t : t array = Obj.obj (Obj.field t 0)
let unsafe_bytevector_bytes t : bytes = Obj.obj (Obj.field t 0)
let unsafe_procedure_arity t : int = Obj.obj (Obj.field t 0)
let unsafe_procedure_name t : string = Obj.obj (Obj.field t 1)
let unsafe_procedure_closure t : closure = Obj.obj (Obj.field t 2)
let unsafe_error_msg t : string = Obj.obj (Obj.field t 0)
let unsafe_error_irritants t : t array = Obj.obj (Obj.field t 1)

(* *)

let mkpair car cdr = Obj.repr (Pair { car; cdr })
let mkvector arr = Obj.repr (Vector arr)
let mkstring data = Obj.repr (String { data })
let mkbytevector bytes = Obj.repr (Bytevector bytes)
let mkerror msg irritants = Obj.repr (Error_object { msg; irritants })

let mkprocedure arity name closure =
  Obj.repr (Procedure { arity; name; closure })

let mkbool = function true -> true_ | false -> false_

module H = Weak.Make (struct
  type nonrec t = t

  let hash = Hashtbl.hash

  let equal t1 t2 =
    String.equal (Obj.obj (Obj.field t1 0)) (Obj.obj (Obj.field t2 0))
end)

let symbols = H.create 0
let mksym name = H.merge symbols (Obj.repr (Symbol { name }))

let tag : kind -> int =
  let pair = mkpair emptylist emptylist in
  let vector = mkvector [||] in
  let string = mkstring (Bytes.create 0) in
  let symbol = mksym "" in
  let bytevector = mkbytevector (Bytes.create 0) in
  let procedure = mkprocedure 0 "" (Obj.magic ignore) in
  let error_object = mkerror "" [||] in
  function
  | Pair -> Obj.tag pair
  | Vector -> Obj.tag vector
  | String -> Obj.tag string
  | Symbol -> Obj.tag symbol
  | Bytevector -> Obj.tag bytevector
  | Procedure -> Obj.tag procedure
  | Error_object -> Obj.tag error_object
  | True | False | Empty_list | Undefined | Eof | Char | Int -> assert false

let () = Printexc.record_backtrace true

let rec write_simple oc obj =
  match classify obj with
  | Int -> output_string oc (string_of_int (unsafe_to_int obj))
  | Empty_list -> output_string oc "()"
  | False -> output_string oc "#f"
  | True -> output_string oc "#t"
  | Eof -> output_string oc "#<eof>"
  | Undefined -> output_string oc "#<undefined>"
  | Char -> Printf.fprintf oc "#\\%x" (Uchar.to_int (unsafe_to_uchar obj))
  | Pair ->
      Printf.fprintf oc "(%a . %a)" write_simple (unsafe_car obj) write_simple
        (unsafe_cdr obj)
  | Vector ->
      let aux oc () =
        let a = unsafe_vector_array obj in
        for i = 0 to Array.length a - 1 do
          if i > 0 then output_char oc ' ';
          write_simple oc a.(i)
        done
      in
      Printf.fprintf oc "#(%a)" aux ()
  | Symbol -> output_string oc (unsafe_symbol_name obj)
  | Procedure ->
      (* procedure *)
      let name = unsafe_procedure_name obj in
      if name <> "" then Printf.fprintf oc "#<%s:procedure>" name
      else output_string oc "#<procedure>"
  | String ->
      Printf.fprintf oc "%S" (Bytes.unsafe_to_string (unsafe_string_data obj))
  | Bytevector -> output_string oc "#<bytevector>"
  | Error_object ->
      (* error *)
      Printf.fprintf oc "Error: %s:" (unsafe_error_msg obj);
      let irritants = unsafe_error_irritants obj in
      for i = 0 to Array.length irritants - 1 do
        Printf.fprintf oc " %a" write_simple irritants.(i)
      done

(* let () = *)
(*   Printexc.register_printer (function *)
(*     | Error x -> Some (Format.asprintf "%a" write_simple x) *)
(*     | _ -> None) *)

let print x = write_simple stdout x
