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
vector                block                   tag 1, size 1 (array)
string                block                   tag 2, size 1 (mutable, bytes)
symbol                block                   tag 3, size 1 (bytes)
bytevector            block                   tag 4, size 1 (bytes)
procedure             block                   tag 5, size 3 (arity, name, closure)
                                              (arity is -(1+n) if variadic with n fixed arguments)
error                 block                   tag 6, size 2 (name, array of irritants)

*)

let msb = 1 lsl (Sys.word_size - 2)

module T : sig
  type t
  type immediate = True | False | Empty_list | Undefined | Eof | Char | Int
  type closure

  type block =
    | Pair of { mutable car : t; mutable cdr : t }
    | Vector of t array
    | String of { mutable data : bytes }
    | Symbol of { name : string }
    | Bytevector of bytes
    | Procedure of { arity : int; name : string; closure : closure }
    | Error of { msg : string; irritants : t array }

  val emptylist : t
  val undefined : t
  val false_ : t
  val true_ : t
  val int : int -> t

  (* val of_immediate : immediate -> t *)
  val is_immediate : t -> bool
  val is_block : t -> bool
  val to_immediate : t -> immediate
  val to_block : t -> block
  val of_block : block -> t
  val to_int : t -> int
  val to_char : t -> Uchar.t
  val get_sym : string -> t
  val pair : t -> t -> t
  val vector : t array -> t
end = struct
  type t = Obj.t
  type immediate = True | False | Empty_list | Undefined | Eof | Char | Int
  type closure

  type block =
    | Pair of { mutable car : t; mutable cdr : t }
    | Vector of t array
    | String of { mutable data : bytes }
    | Symbol of { name : string }
    | Bytevector of bytes
    | Procedure of { arity : int; name : string; closure : closure }
    | Error of { msg : string; irritants : t array }

  let is_immediate t = Obj.is_int t
  let is_block t = Obj.is_block t

  let of_immediate t : t =
    let n =
      match t with
      | False -> msb lor 0b000
      | True -> msb lor 0b001
      | Empty_list -> msb lor 0b100
      | Eof -> msb lor 0b101
      | Undefined -> msb lor 0b110
      | Char -> msb lor 0b111
      | Int -> 0
    in
    Obj.repr n

  let emptylist = of_immediate Empty_list
  let undefined = of_immediate Undefined
  let true_ = of_immediate True
  let false_ = of_immediate False
  let int n = Obj.repr (n land lnot msb)

  let to_immediate t =
    let t : int = Obj.obj t in
    if t land msb = 0 then Int
    else
      match t land 0b111 with
      | 0b000 -> False
      | 0b001 -> True
      | 0b100 -> Empty_list
      | 0b101 -> Eof
      | 0b110 -> Undefined
      | 0b111 -> Char
      | _ -> assert false

  let to_int t : int = Obj.obj t
  let to_char t = Uchar.unsafe_of_int ((Obj.obj t : int) lsr 3)
  let to_block t : block = Obj.obj t
  let of_block (t : block) = Obj.repr t
  let pair car cdr = Obj.repr (Pair { car; cdr })
  let vector arr = Obj.repr (Vector arr)

  module H = Weak.Make (struct
    type nonrec t = t

    let hash = Hashtbl.hash

    let equal t1 t2 =
      String.equal (Obj.obj (Obj.field t1 0)) (Obj.obj (Obj.field t2 0))
  end)

  let symbols = H.create 0
  let get_sym name = H.merge symbols (Obj.repr (Symbol { name }))
end

include T

let () = Printexc.record_backtrace true

exception Error of t

let get_sym s = T.get_sym s

let rec write_simple oc obj =
  if T.is_immediate obj then
    match T.to_immediate obj with
    | Int -> output_string oc (string_of_int (T.to_int obj))
    | Empty_list -> output_string oc "()"
    | False -> output_string oc "#f"
    | True -> output_string oc "#t"
    | Eof -> output_string oc "#<eof>"
    | Undefined -> output_string oc "#<undefined>"
    | Char -> Printf.fprintf oc "#\\%x" (Uchar.to_int (T.to_char obj))
  else
    match T.to_block obj with
    | Pair { car; cdr } ->
        Printf.fprintf oc "(%a . %a)" write_simple car write_simple cdr
    | Vector v ->
        let aux oc v =
          for i = 0 to Array.length v - 1 do
            if i > 0 then output_char oc ' ';
            write_simple oc v.(i)
          done
        in
        Printf.fprintf oc "#(%a)" aux v
    | Symbol { name } -> output_string oc name
    | Procedure { arity = _; name; closure = _ } ->
        (* procedure *)
        if name <> "" then Printf.fprintf oc "#<%s:procedure>" name
        else output_string oc "#<procedure>"
    | String { data } -> Printf.fprintf oc "%S" (Bytes.unsafe_to_string data)
    | Bytevector _ -> output_string oc "#<bytevector>"
    | Error { msg; irritants } ->
        (* error *)
        Printf.fprintf oc "Error: %s:" msg;
        for i = 0 to Array.length irritants - 1 do
          Printf.fprintf oc " %a" write_simple irritants.(i)
        done

(* let () = *)
(*   Printexc.register_printer (function *)
(*     | Error x -> Some (Format.asprintf "%a" write_simple x) *)
(*     | _ -> None) *)

let print x = write_simple stdout x

let rec append = function
  | [] -> T.emptylist
  | list :: lists ->
      let rec loop list =
        if T.is_immediate list then
          match T.to_immediate list with
          | Empty_list -> append lists
          | _ -> assert false (* type error *)
        else
          match T.to_block list with
          | Pair { car; cdr } -> T.pair car (loop cdr)
          | _ ->
              (* type error *)
              assert false
      in
      loop list

let list_to_vector list =
  let size =
    let rec aux accu list =
      if T.is_immediate list then
        match T.to_immediate list with Empty_list -> accu | _ -> assert false
      else
        match T.to_block list with
        | Pair { car = _; cdr } -> aux (accu + 1) cdr
        | _ -> assert false
    in
    aux 0 list
  in
  let arr =
    if T.is_immediate list then [||]
    else
      match T.to_block list with
      | Pair { car; cdr } ->
          let arr = Array.make size car in
          let rec loop i t =
            if T.is_immediate t then arr
            else
              match T.to_block t with
              | Pair { car; cdr } ->
                  arr.(i) <- car;
                  loop (i + 1) cdr
              | _ -> assert false
          in
          loop 1 cdr
      | _ -> assert false
  in
  T.vector arr

let vector_append vectors =
  let size =
    let rec aux accu = function
      | [] -> accu
      | vector :: vectors ->
          if T.is_block vector then
            match T.to_block vector with
            | Vector arr -> aux (accu + Array.length arr) vectors
            | _ -> assert false
          else assert false
    in
    aux 0 vectors
  in
  let dst = Array.make size T.emptylist in
  let rec loop i = function
    | [] -> T.vector dst
    | vector :: vectors -> (
        match T.to_block vector with
        | Vector src ->
            let size = Array.length src in
            Array.blit src 0 dst i size;
            loop (i + size) vectors
        | _ -> assert false)
  in
  loop 0 vectors

let apply _ _ = assert false

let error msg irritants =
  raise
    (Error (T.of_block (Error { msg; irritants = Array.of_list irritants })))

let scheme_vector_length t =
  let err () = error "vector-length: argument error" [ t ] in
  if is_block t then
    match to_block t with Vector arr -> int (Array.length arr) | _ -> err ()
  else err ()
