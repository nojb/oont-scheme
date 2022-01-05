module T = Oont

let prim_err fname expected tl =
  let msg = Printf.sprintf "%s: expected %s, got" fname expected in
  raise (T.Error (T.mkerror msg (Array.of_list tl)))

let not obj = if obj == T.false_ then T.true_ else T.false_

let booleanp obj =
  match T.classify obj with False | True -> T.true_ | _ -> T.false_

let booleaneqp objs =
  let nobjs = Array.length objs in
  if nobjs = 0 then T.true_
  else
    let rec aux accu i =
      if i >= nobjs then if accu then T.true_ else T.false_
      else
        let obj = objs.(i) in
        match T.classify obj with
        | True | False -> aux (accu && obj == objs.(i - 1)) (i + 1)
        | _ -> T.false_
    in
    let obj = objs.(0) in
    match T.classify obj with True | False -> aux true 1 | _ -> T.false_

let pairp obj = match T.classify obj with Pair -> T.true_ | _ -> T.false_
let cons obj1 obj2 = T.mkpair obj1 obj2

let car pair =
  match T.classify pair with
  | Pair -> T.unsafe_car pair
  | _ -> prim_err "car" "pair" [ pair ]

let cdr pair =
  match T.classify pair with
  | Pair -> T.unsafe_cdr pair
  | _ -> prim_err "cdr" "pair" [ pair ]

let set_car pair obj =
  match T.classify pair with
  | Pair ->
      T.unsafe_set_car pair obj;
      T.undefined
  | _ -> prim_err "set-car!" "pair" [ pair ]

let set_cdr pair obj =
  match T.classify pair with
  | Pair ->
      T.unsafe_set_cdr pair obj;
      T.undefined
  | _ -> prim_err "set-cdr!" "pair" [ pair ]

let nullp obj =
  match T.classify obj with Empty_list -> T.true_ | _ -> T.false_

let rec listp obj =
  match T.classify obj with
  | Empty_list -> T.true_
  | Pair -> listp (T.unsafe_cdr obj)
  | _ -> T.false_

let make_list k fill =
  let err () = prim_err "make-list" "non-negative integer" [ k ] in
  match T.classify k with
  | Int ->
      let n = T.unsafe_to_int k in
      if n < 0 then err ();
      let rec aux accu n =
        if n = 0 then accu else aux (cons fill accu) (n - 1)
      in
      aux T.emptylist n
  | _ -> err ()

let list objs =
  let rec aux accu i =
    if i < 0 then accu else aux (cons objs.(i) accu) (i - 1)
  in
  aux T.emptylist (Array.length objs - 1)

let length_aux fname list =
  let rec aux accu list =
    match T.classify list with
    | Empty_list -> accu
    | Pair -> aux (accu + 1) (T.unsafe_cdr list)
    | _ -> prim_err fname "list" [ list ]
  in
  aux 0 list

let length list = T.int (length_aux "length" list)

let append lists =
  let err obj = prim_err "append" "list" [ obj ] in
  let nlists = Array.length lists in
  if nlists = 0 then T.emptylist
  else
    let rec aux accu i =
      if i < 0 then accu
      else
        let rec aux' list =
          match T.classify list with
          | Empty_list -> accu
          | Pair ->
              cons (T.unsafe_car list) (cons (T.unsafe_cdr list) (aux' list))
              (* FIXME *)
          | _ -> err list
        in
        aux (aux' lists.(i)) (i - 1)
    in
    aux lists.(nlists - 1) (nlists - 2)

let reverse list =
  let rec aux accu list =
    match T.classify list with
    | Empty_list -> accu
    | Pair -> aux (cons (T.unsafe_car list) accu) (T.unsafe_cdr list)
    | _ -> prim_err "reverse" "list" [ list ]
  in
  aux T.emptylist list

let list_tail list k =
  let err expected objs = prim_err "list-tail" expected objs in
  let err_k () = err "non-negative integer" [ k ] in
  match T.classify k with
  | Int ->
      let n = T.unsafe_to_int k in
      if n < 0 then err_k ();
      let rec aux i obj =
        if i = 0 then obj
        else
          match T.classify obj with
          | Pair -> aux (i - 1) (T.unsafe_cdr obj)
          | Empty_list -> err "valid index" [ list; k ]
          | _ -> err "list" [ list ]
      in
      aux n list
  | _ -> err_k ()

let list_ref list k =
  let err expected objs = prim_err "list-ref" expected objs in
  let errk () = err "non-negative integer" [ k ] in
  match T.classify k with
  | Int ->
      let n = T.unsafe_to_int k in
      if n < 0 then errk ();
      let rec aux i obj =
        match T.classify obj with
        | Empty_list -> err "valid index" [ list; k ]
        | Pair ->
            if i = 0 then T.unsafe_car obj else aux (i - 1) (T.unsafe_cdr obj)
        | _ -> err "list" [ list ]
      in
      aux n list
  | _ -> errk ()

let list_set list k obj =
  let err expected objs = prim_err "list-set!" expected objs in
  let errk () = err "non-negative integer" [ k ] in
  match T.classify k with
  | Int ->
      let n = T.unsafe_to_int k in
      if n < 0 then errk ();
      let rec aux i obj' =
        match T.classify obj' with
        | Empty_list -> err "valid index" [ list; k ]
        | Pair ->
            if i = 0 then (
              T.unsafe_set_car obj' obj;
              T.undefined)
            else aux (i - 1) (T.unsafe_cdr obj')
        | _ -> err "list" [ list ]
      in
      aux n list
  | _ -> errk ()

(* FIXME: circular list *)
let rec list_copy obj =
  match T.classify obj with
  | Pair -> T.mkpair (T.unsafe_car obj) (list_copy (T.unsafe_cdr obj))
  | _ -> obj

let symbolp obj = match T.classify obj with Symbol -> T.true_ | _ -> T.false_

let symbol_to_string symbol =
  match T.classify symbol with
  | Symbol -> T.mkstring (Bytes.of_string (T.unsafe_symbol_name symbol))
  | _ -> prim_err "symbol->string" "symbol" [ symbol ]

let string_to_symbol string =
  match T.classify string with
  | String -> T.mksym (Bytes.to_string (T.unsafe_string_data string))
  | _ -> prim_err "string->symbol" "string" [ string ]

(* Characters *)

let charp obj = match T.classify obj with Char -> T.true_ | _ -> T.false_

let getchar name char =
  match T.classify char with
  | Char -> T.unsafe_to_uchar char
  | _ -> prim_err name "character" [ char ]

let getint name n =
  match T.classify n with
  | Int -> T.unsafe_to_int n
  | _ -> prim_err name "integer" [ n ]

let char_prop name p char =
  let u = getchar (Printf.sprintf "char-%s?" name) char in
  T.mkbool (p u)

let char_alphabeticp = char_prop "alphabetic" Uucp.Alpha.is_alphabetic
let char_numericp = char_prop "numeric" (fun u -> Uucp.Num.numeric_type u = `Di)
let char_whitespacep = char_prop "whitespace" Uucp.White.is_white_space
let char_upper_casep = char_prop "upper-case" Uucp.Case.is_upper
let char_lower_casep = char_prop "lower-case" Uucp.Case.is_lower

let digit_value char =
  let u = getchar "digit-value" char in
  match Uucp.Num.numeric_type u with
  | `Di -> (
      match Uucp.Num.numeric_value u with
      | `Num n -> T.int (Int64.to_int n)
      | _ -> T.false_)
  | _ -> T.false_

let char_to_integer char =
  let u = getchar "char->integer" char in
  T.int (Uchar.to_int u)

let integer_to_char num =
  let n = getint "integer->char" num in
  if Stdlib.not (Uchar.is_valid n) then
    prim_err "integer->char" "invalid value" [ num ];
  T.mkchar (Uchar.of_int n)

let char_upcase char =
  let u = getchar "char-upcase" char in
  match Uucp.Case.Map.to_upper u with
  | `Uchars [ u ] -> T.mkchar u
  | `Self | `Uchars _ -> char

let char_downcase char =
  let u = getchar "char-downcase" char in
  match Uucp.Case.Map.to_lower u with
  | `Uchars [ u ] -> T.mkchar u
  | `Self | `Uchars _ -> char

let char_foldcase char =
  let u = getchar "char-foldcase" char in
  match Uucp.Case.Fold.fold u with
  | `Uchars [ u ] -> T.mkchar u
  | `Self | `Uchars _ -> char

(* Strings *)

let stringp obj = match T.classify obj with String -> T.true_ | _ -> T.false_

let make_string k char =
  let k = getint "make-string" k in
  let char = getchar "make-string" char in
  if Uchar.is_char char then T.mkstring (Bytes.make k (Uchar.to_char char))
  else
    let buf = Buffer.create k in
    for _ = 1 to k do
      Buffer.add_utf_8_uchar buf char
    done;
    T.mkstring (Bytes.unsafe_of_string (Buffer.contents buf))

(* Vectors *)

let vectorp obj = match T.classify obj with Vector -> T.true_ | _ -> T.false_

let make_vector k fill =
  let err () = prim_err "make-vector" "non-negative integer" [ k ] in
  match T.classify k with
  | Int ->
      let n = T.unsafe_to_int k in
      if n < 0 then err ();
      T.mkvector (Array.make n fill)
      (* warning if floats? *)
  | _ -> err ()

let vector objs = T.mkvector (Array.copy objs)

let vector_length vector =
  match T.classify vector with
  | Vector -> T.int (Array.length (T.unsafe_vector_array vector))
  | _ -> prim_err "vector-length" "vector" [ vector ]

let vector_ref vector k =
  let err expected objs = prim_err "vector-ref" expected objs in
  match (T.classify vector, T.classify k) with
  | Vector, Int ->
      let a = T.unsafe_vector_array vector in
      let n = T.unsafe_to_int k in
      if n < 0 || n >= Array.length a then err "valid index" [ vector; k ];
      a.(n)
  | _ -> err "vector, int" [ vector; k ]

let vector_set vector k obj =
  let err expected objs = prim_err "vector-set!" expected objs in
  match (T.classify vector, T.classify k) with
  | Vector, Int ->
      let a = T.unsafe_vector_array vector in
      let n = T.unsafe_to_int k in
      if n < 0 || n >= Array.length a then err "valid index" [ vector; k ];
      a.(n) <- obj;
      T.undefined
  | _ -> err "vector, int" [ vector; k ]

let check_vector_indices fname vector start end_ =
  let err expected objs = prim_err fname expected objs in
  match (T.classify vector, T.classify start, T.classify end_) with
  | Vector, Int, Int ->
      let a = T.unsafe_vector_array vector in
      let getint k =
        let n = T.unsafe_to_int k in
        if n < 0 || n >= Array.length a then err "valid index" [ vector; k ];
        n
      in
      let start = getint start in
      let end_ = getint end_ in
      (a, start, end_)
  | _ -> err "vector, int, int" [ vector; start; end_ ]

let vector_to_list vector start end_ =
  let a, start, end_ = check_vector_indices "vector->list" vector start end_ in
  let rec aux accu end_ =
    if end_ <= start then accu else aux (T.mkpair a.(end_) accu) (end_ - 1)
  in
  aux T.emptylist end_

let list_to_vector list =
  let len = length_aux "list->vector" list in
  let arr = Array.make len T.emptylist in
  let rec aux i list =
    if i >= Array.length arr then T.mkvector arr
    else
      match T.classify list with
      | Pair ->
          arr.(i) <- T.unsafe_car list;
          aux (i + 1) (T.unsafe_cdr list)
      | _ -> assert false
  in
  aux 0 list

let vector_to_string vector start end_ =
  let a, start, end_ =
    check_vector_indices "vector->string" vector start end_
  in
  let buf = Buffer.create (end_ - start) in
  for i = start to end_ - 1 do
    let obj = a.(i) in
    match T.classify obj with
    | Char ->
        let uchar = T.unsafe_to_uchar obj in
        Buffer.add_utf_8_uchar buf uchar
    | _ -> prim_err "vector->string" "character" [ obj ]
  done;
  T.mkstring (Bytes.unsafe_of_string (Buffer.contents buf))

let vector_copy vector start end_ =
  let a, start, end_ = check_vector_indices "vector-copy" vector start end_ in
  T.mkvector (Array.sub a start (end_ - start))

let vector_blit _to_ _at _from _start _end_ = assert false

let vector_append vectors =
  let aa =
    Array.map
      (fun vector ->
        match T.classify vector with
        | Vector -> T.unsafe_vector_array vector
        | _ -> prim_err "vector-append" "vector" [ vector ])
      vectors
  in
  let len = Array.fold_left (fun accu a -> accu + Array.length a) 0 aa in
  let res = Array.make len T.emptylist in
  let rec aux ofs i =
    if i >= Array.length aa then T.mkvector res
    else
      let a = aa.(i) in
      let len = Array.length a in
      Array.blit a 0 res ofs len;
      aux (ofs + len) (i + 1)
  in
  aux 0 0

let vector_fill _vector _fill _start _end_ = assert false

(* Bytevectors *)

let bytevectorp obj =
  match T.classify obj with Bytevector -> T.true_ | _ -> T.false_

let make_bytevector k byte =
  match (T.classify k, T.classify byte) with
  | Int, Int ->
      let n = T.unsafe_to_int byte in
      if n < 0 || n >= 256 then prim_err "make-bytevector" "byte" [ byte ];
      T.mkbytevector (Bytes.make (T.unsafe_to_int k) (Char.chr n))
  | _ -> prim_err "make-bytevector" "int, byte" [ k; byte ]

let bytevector bytes =
  T.mkbytevector
    (Bytes.init (Array.length bytes) (fun i ->
         let byte = bytes.(i) in
         match T.classify byte with
         | Int ->
             let n = T.unsafe_to_int byte in
             if n < 0 || n >= 256 then prim_err "bytevector" "byte" [ byte ];
             Char.chr n
         | _ -> prim_err "bytevector" "byte" [ byte ]))

let bytevector_length bytevector =
  match T.classify bytevector with
  | Bytevector -> T.int (Bytes.length (T.unsafe_bytevector_bytes bytevector))
  | _ -> prim_err "bytevector-length" "bytevector" [ bytevector ]
