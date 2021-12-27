  $ cat >lambda1.scm <<EOF
  > (lambda () 12)
  > ((lambda () 12))
  > EOF

  $ oont -dlambda lambda1.scm
  (apply (field 2 (global Oont!))
    (seq (makeblock 4 1 "" (function arg/7 24))
      (let (let/4 = (makeblock 4 1 "" (function arg/3 24)))
        (seq
          (if (isint let/4)
            (raise
              (makeblock 0 (field 0 (global Oont!))
                (makeblock 5 "Type error" let/4)))
            0)
          (switch let/4
           case tag 4:
            (if (== 1 (field 0 let/4)) (apply (field 2 let/4) 0)
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 5 "Type error" let/4))))
           default:
            (raise
              (makeblock 0 (field 0 (global Oont!))
                (makeblock 5 "Type error" let/4))))))))

  $ ./lambda1.exe
  12

  $ cat >lambda2.scm <<EOF
  > ((lambda (f a) (+ (f a) a)) (lambda (x) (+ x 1)) 12)
  > EOF

  $ oont -dlambda lambda2.scm
  (apply (field 2 (global Oont!))
    (let
      (let/10 =
         (makeblock 4 2 ""
           (function f/7 a/8
             (lsl
               (+
                 (let
                   (let/9 =
                      (seq
                        (if (isint f/7)
                          (raise
                            (makeblock 0 (field 0 (global Oont!))
                              (makeblock 5 "Type error" f/7)))
                          0)
                        (switch f/7
                         case tag 4:
                          (if (== 1 (field 0 f/7)) (apply (field 2 f/7) a/8)
                            (raise
                              (makeblock 0 (field 0 (global Oont!))
                                (makeblock 5 "Type error" f/7))))
                         default:
                          (raise
                            (makeblock 0 (field 0 (global Oont!))
                              (makeblock 5 "Type error" f/7))))))
                   (if (&& (isint let/9) (== (and let/9 1) 0)) (lsr let/9 1)
                     (raise
                       (makeblock 0 (field 0 (global Oont!))
                         (makeblock 5 "Type error" let/9)))))
                 (if (&& (isint a/8) (== (and a/8 1) 0)) (lsr a/8 1)
                   (raise
                     (makeblock 0 (field 0 (global Oont!))
                       (makeblock 5 "Type error" a/8)))))
               1))))
      (seq
        (if (isint let/10)
          (raise
            (makeblock 0 (field 0 (global Oont!))
              (makeblock 5 "Type error" let/10)))
          0)
        (switch let/10
         case tag 4:
          (if (== 2 (field 0 let/10))
            (apply (field 2 let/10)
              (makeblock 4 1 ""
                (function x/3
                  (lsl
                    (+
                      (if (&& (isint x/3) (== (and x/3 1) 0)) (lsr x/3 1)
                        (raise
                          (makeblock 0 (field 0 (global Oont!))
                            (makeblock 5 "Type error" x/3))))
                      (let (let/6 = 2)
                        (if (&& (isint let/6) (== (and let/6 1) 0))
                          (lsr let/6 1)
                          (raise
                            (makeblock 0 (field 0 (global Oont!))
                              (makeblock 5 "Type error" let/6))))))
                    1)))
              24)
            (raise
              (makeblock 0 (field 0 (global Oont!))
                (makeblock 5 "Type error" let/10))))
         default:
          (raise
            (makeblock 0 (field 0 (global Oont!))
              (makeblock 5 "Type error" let/10)))))))

  $ ./lambda2.exe
  25
