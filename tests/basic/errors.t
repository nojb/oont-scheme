  $ cat >err1.scm <<EOF
  > (+ 1 'a)
  > EOF

  $ oont -dlambda err1.scm
  (apply (field 29 (global Oont!))
    (and
      (+
        (let (let/7 = 1)
          (seq
            (if (|| (not (isint let/7)) (lsr let/7 62))
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 6 "Type error" (makeblock 0 let/7))))
              0)
            let/7))
        (let (let/8 = (apply (field 22 (global Oont!)) "a"))
          (seq
            (if (|| (not (isint let/8)) (lsr let/8 62))
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 6 "Type error" (makeblock 0 let/8))))
              0)
            let/8)))
      4611686018427387903))

  $ ./err1.exe
  Fatal error: exception Oont.Error(_)
  Raised at ?? in file "_none_", line 0, characters -1--1
  [2]

  $ cat >err2.scm <<EOF
  > (external vector-length 1 "vector_length")
  > (vector-length #(1 2 3))
  > EOF

  $ oont -dlambda err2.scm
  (apply (field 29 (global Oont!))
    (apply (field 27 (global R7rs!)) (makeblock 1 (makeblock 0 1 2 3))))

  $ ./err2.exe
  3
