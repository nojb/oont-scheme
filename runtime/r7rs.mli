open Oont

(** Booleans *)

val not : t -> t
(** (not obj) *)

val booleanp : t -> t
(** (boolean? obj) *)

val booleaneqp : t array -> t
(** (boolean=? obj1 obj2 ...) *)

(** Pairs and lists *)

val pairp : t -> t
(** (pair? obj) *)

val cons : t -> t -> t
(** (cons obj1 obj2) *)

val car : t -> t
(** (car pair) *)

val cdr : t -> t
(** (cdr pair) *)

val set_car : t -> t -> t
(** (set-car! pair obj) *)

val set_cdr : t -> t -> t
(** (set-cdr! pair obj) *)

val nullp : t -> t
(** (null? obj) *)

val listp : t -> t
(** (list? obj) *)

val make_list : t -> t -> t
(** (make-list k fill) *)

val list : t array -> t
(** (list obj1 obj2 ...) *)

val length : t -> t
(** (length list) *)

val append : t array -> t
(** (append list1 list2 ...) *)

val reverse : t -> t
(** (reverse list) *)

val list_tail : t -> t -> t
(** (list-tail list k) *)

val list_ref : t -> t -> t
(** (list-ref list k) *)

val list_set : t -> t -> t -> t
(** (list-set! list k obj) *)

val list_copy : t -> t
(** (list-copy list) *)

(** Symbols *)

val symbolp : t -> t
(** (symbol? obj) *)

val symbol_to_string : t -> t
(** (symbol->string symbol) *)

val string_to_symbol : t -> t
(** (string->symbol string) *)

(** Characters *)

val charp : t -> t
(** (char? obj) *)

(** Vectors *)

val vectorp : t -> t
(** (vector? obj) *)

val make_vector : t -> t -> t
(** (make-vector k fill) *)

val vector : t array -> t
(** (vector obj1 obj2 ...) *)

val vector_length : t -> t
(** (vector-length vector) *)

val vector_ref : t -> t -> t
(** (vector-ref vector k) *)

val vector_set : t -> t -> t -> t
(** (vector-set! vector k obj) *)

val vector_to_list : t -> t -> t -> t
(** (vector->list vector start end) *)

val list_to_vector : t -> t
(** (list->vector list) *)

val vector_to_string : t -> t -> t -> t
(** (vector->string vector start end) *)

val vector_copy : t -> t -> t -> t
(** (vector-copy vector start end) *)

val vector_blit : t -> t -> t -> t -> t -> t
(** (vector-copy! to at from start end) *)

val vector_append : t array -> t
(** (vector-append vector1 vector2 ...) *)

val vector_fill : t -> t -> t -> t -> t
(** (vector-fill! vector fill start end) *)

(** Bytevectors *)

val bytevectorp : t -> t
(** (bytevector? obj) *)

val make_bytevector : t -> t -> t
(** (make-bytevector k byte) *)

val bytevector : t array -> t
(** (bytevector byte1 byte2 ...) *)

val bytevector_length : t -> t
(** (bytevector-length bytevector) *)
