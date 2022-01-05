  $ cat >vars1.scm <<EOF
  > (let ((x 1) (y 2)) (+ x y))
  > EOF

  $ oont -dlambda vars1.scm
  (apply (field 29 (global Oont!))
    (let (let/5 = 1 let/6 = 2)
      (and
        (+
          (seq
            (if (|| (not (isint let/5)) (lsr let/5 62))
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 6 "Type error" (makeblock 0 let/5))))
              0)
            let/5)
          (seq
            (if (|| (not (isint let/6)) (lsr let/6 62))
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 6 "Type error" (makeblock 0 let/6))))
              0)
            let/6))
        4611686018427387903)))

  $ ./vars1.exe
  3

  $ cat >vars2.scm <<EOF
  > (let ((x 1) (y 2)) (set! x (+ y y)) x)
  > EOF

  $ oont -dlambda vars2.scm
  (apply (field 29 (global Oont!))
    (let (let/5 = (makeblock 0 1) let/6 = 2)
      (seq
        (setfield_ptr 0 let/5
          (and
            (+
              (seq
                (if (|| (not (isint let/6)) (lsr let/6 62))
                  (raise
                    (makeblock 0 (field 0 (global Oont!))
                      (makeblock 6 "Type error" (makeblock 0 let/6))))
                  0)
                let/6)
              (seq
                (if (|| (not (isint let/6)) (lsr let/6 62))
                  (raise
                    (makeblock 0 (field 0 (global Oont!))
                      (makeblock 6 "Type error" (makeblock 0 let/6))))
                  0)
                let/6))
            4611686018427387903))
        (field 0 let/5))))

  $ ./vars2.exe
  4
