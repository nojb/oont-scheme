  $ cat >arith1.scm <<EOF
  > (+ 2 3)
  > EOF

  $ oont -dlambda arith1.scm
  (apply (field 2 (global Oont!))
    (lsl
      (+
        (let (let/3 = 4)
          (seq
            (if (|| (not (isint let/3)) (== (and let/3 1) 1))
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 5 "Type error" let/3)))
              0)
            (lsr let/3 1)))
        (let (let/6 = 6)
          (seq
            (if (|| (not (isint let/6)) (== (and let/6 1) 1))
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 5 "Type error" let/6)))
              0)
            (lsr let/6 1))))
      1))

  $ ./arith1.exe
  5

  $ cat >arith2.scm <<EOF
  > (zero? 34)
  > EOF

  $ oont -dlambda arith2.scm
  (apply (field 2 (global Oont!))
    (let (let/3 = 68)
      (seq
        (if (|| (not (isint let/3)) (== (and let/3 1) 1))
          (raise
            (makeblock 0 (field 0 (global Oont!))
              (makeblock 5 "Type error" let/3)))
          0)
        (if (== let/3 0) 3 1))))

  $ ./arith2.exe
  #f

  $ cat >arith3.scm <<EOF
  > (zero? (+ 1 2))
  > EOF

  $ oont -dlambda arith3.scm
  (apply (field 2 (global Oont!))
    (let
      (let/7 =
         (lsl
           (+
             (let (let/3 = 2)
               (seq
                 (if (|| (not (isint let/3)) (== (and let/3 1) 1))
                   (raise
                     (makeblock 0 (field 0 (global Oont!))
                       (makeblock 5 "Type error" let/3)))
                   0)
                 (lsr let/3 1)))
             (let (let/6 = 4)
               (seq
                 (if (|| (not (isint let/6)) (== (and let/6 1) 1))
                   (raise
                     (makeblock 0 (field 0 (global Oont!))
                       (makeblock 5 "Type error" let/6)))
                   0)
                 (lsr let/6 1))))
           1))
      (seq
        (if (|| (not (isint let/7)) (== (and let/7 1) 1))
          (raise
            (makeblock 0 (field 0 (global Oont!))
              (makeblock 5 "Type error" let/7)))
          0)
        (if (== let/7 0) 3 1))))

  $ ./arith3.exe
  #f

  $ cat >arith4.scm <<EOF
  > (zero? 0)
  > EOF

  $ oont -dlambda arith4.scm
  (apply (field 2 (global Oont!))
    (let (let/3 = 0)
      (seq
        (if (|| (not (isint let/3)) (== (and let/3 1) 1))
          (raise
            (makeblock 0 (field 0 (global Oont!))
              (makeblock 5 "Type error" let/3)))
          0)
        (if (== let/3 0) 3 1))))

  $ ./arith4.exe
  #t
