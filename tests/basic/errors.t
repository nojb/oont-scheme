  $ cat >err1.scm <<EOF
  > (+ 1 'a)
  > EOF

  $ znscheme -dlambda err1.scm
  (apply (field 2 (global SchemeStdlib!))
    (let (a/3 = (apply (field 1 (global SchemeStdlib!)) #"a"))
      (lsl
        (+
          (let (let/4 = 2)
            (if (isint let/4)
              (if (and let/4 1)
                (raise
                  (makeblock 0 (field 0 (global SchemeStdlib!))
                    (makeblock 4 "Type error" (makemutable 0 let/4 7))))
                (lsr let/4 1))
              (raise
                (makeblock 0 (field 0 (global SchemeStdlib!))
                  (makeblock 4 "Type error" (makemutable 0 let/4 7))))))
          (if (isint a/3)
            (if (and a/3 1)
              (raise
                (makeblock 0 (field 0 (global SchemeStdlib!))
                  (makeblock 4 "Type error" (makemutable 0 a/3 7))))
              (lsr a/3 1))
            (raise
              (makeblock 0 (field 0 (global SchemeStdlib!))
                (makeblock 4 "Type error" (makemutable 0 a/3 7))))))
        1)))

  $ ocamlc -o err1.exe znscheme_lib.cma err1.cmo

  $ ./err1.exe
  Fatal error: exception Error: Type error: (a . ())
  [2]
