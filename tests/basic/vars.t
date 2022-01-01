  $ cat >vars1.scm <<EOF
  > (let ((x 1) (y 2)) (+ x y))
  > EOF

  $ oont -dlambda vars1.scm
  (apply (field 2 (global Oont!))
    (let (let/5 = 2 let/6 = 4)
      (lsl
        (+
          (seq
            (if (|| (not (isint let/5)) (== (and let/5 1) 1))
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 5 "Type error" let/5)))
              0)
            (lsr let/5 1))
          (seq
            (if (|| (not (isint let/6)) (== (and let/6 1) 1))
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 5 "Type error" let/6)))
              0)
            (lsr let/6 1)))
        1)))

  $ ./vars1.exe
  3

  $ cat >vars2.scm <<EOF
  > (let ((x 1) (y 2)) (set! x (+ y y)) x)
  > EOF

  $ oont -dlambda vars2.scm
  (apply (field 2 (global Oont!))
    (let (let/5 = (makeblock 0 2) let/6 = 4)
      (seq
        (setfield_ptr 0 let/5
          (lsl
            (+
              (seq
                (if (|| (not (isint let/6)) (== (and let/6 1) 1))
                  (raise
                    (makeblock 0 (field 0 (global Oont!))
                      (makeblock 5 "Type error" let/6)))
                  0)
                (lsr let/6 1))
              (seq
                (if (|| (not (isint let/6)) (== (and let/6 1) 1))
                  (raise
                    (makeblock 0 (field 0 (global Oont!))
                      (makeblock 5 "Type error" let/6)))
                  0)
                (lsr let/6 1)))
            1))
        (field 0 let/5))))

  $ ./vars2.exe
  4
