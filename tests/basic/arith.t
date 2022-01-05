  $ cat >arith1.scm <<EOF
  > (+ 2 3)
  > EOF

  $ oont -dlambda arith1.scm
  (apply (field 29 (global Oont!))
    (and
      (+
        (let (let/3 = 2)
          (seq
            (if (|| (not (isint let/3)) (lsr let/3 62))
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 6 "Type error" (makeblock 0 let/3))))
              0)
            let/3))
        (let (let/8 = 3)
          (seq
            (if (|| (not (isint let/8)) (lsr let/8 62))
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 6 "Type error" (makeblock 0 let/8))))
              0)
            let/8)))
      4611686018427387903))

  $ ./arith1.exe
  5

  $ cat >arith2.scm <<EOF
  > (zero? 34)
  > EOF

  $ oont -dlambda arith2.scm
  (apply (field 29 (global Oont!))
    (let (let/3 = 34)
      (seq
        (if (|| (not (isint let/3)) (lsr let/3 62))
          (raise
            (makeblock 0 (field 0 (global Oont!))
              (makeblock 6 "Type error" (makeblock 0 let/3))))
          0)
        (if (== let/3 0) -4611686018427387903 -4611686018427387904))))

  $ ./arith2.exe
  #f

  $ cat >arith3.scm <<EOF
  > (zero? (+ 1 2))
  > EOF

  $ oont -dlambda arith3.scm
  (apply (field 29 (global Oont!))
    (let
      (let/9 =
         (and
           (+
             (let (let/3 = 1)
               (seq
                 (if (|| (not (isint let/3)) (lsr let/3 62))
                   (raise
                     (makeblock 0 (field 0 (global Oont!))
                       (makeblock 6 "Type error" (makeblock 0 let/3))))
                   0)
                 let/3))
             (let (let/8 = 2)
               (seq
                 (if (|| (not (isint let/8)) (lsr let/8 62))
                   (raise
                     (makeblock 0 (field 0 (global Oont!))
                       (makeblock 6 "Type error" (makeblock 0 let/8))))
                   0)
                 let/8)))
           4611686018427387903))
      (seq
        (if (|| (not (isint let/9)) (lsr let/9 62))
          (raise
            (makeblock 0 (field 0 (global Oont!))
              (makeblock 6 "Type error" (makeblock 0 let/9))))
          0)
        (if (== let/9 0) -4611686018427387903 -4611686018427387904))))

  $ ./arith3.exe
  #f

  $ cat >arith4.scm <<EOF
  > (zero? 0)
  > EOF

  $ oont -dlambda arith4.scm
  (apply (field 29 (global Oont!))
    (let (let/3 = 0)
      (seq
        (if (|| (not (isint let/3)) (lsr let/3 62))
          (raise
            (makeblock 0 (field 0 (global Oont!))
              (makeblock 6 "Type error" (makeblock 0 let/3))))
          0)
        (if (== let/3 0) -4611686018427387903 -4611686018427387904))))

  $ ./arith4.exe
  #t
