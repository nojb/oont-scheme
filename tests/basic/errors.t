  $ cat >err1.scm <<EOF
  > (+ 1 'a)
  > EOF

  $ oont -dlambda err1.scm
  (apply (field 2 (global Oont!))
    (lsl
      (+
        (let (let/5 = 2)
          (seq
            (if (|| (not (isint let/5)) (== (and let/5 1) 1))
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 5 "Type error" let/5)))
              0)
            (lsr let/5 1)))
        (let (let/6 = (apply (field 1 (global Oont!)) "a"))
          (seq
            (if (|| (not (isint let/6)) (== (and let/6 1) 1))
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 5 "Type error" let/6)))
              0)
            (lsr let/6 1))))
      1))

  $ ./err1.exe
  Fatal error: exception Error: Type error:
  a
  Raised at ?? in file "_none_", line 0, characters -1--1
  [2]
