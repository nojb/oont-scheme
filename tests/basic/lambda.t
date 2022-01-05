  $ cat >lambda1.scm <<EOF
  > (lambda () 12)
  > ((lambda () 12))
  > EOF

  $ oont -dlambda lambda1.scm
  (apply (field 29 (global Oont!))
    (seq (makeblock 5 1 "" (function dummy/9 24))
      (let (let/4 = (makeblock 5 1 "" (function dummy/3 24)))
        (seq
          (if (isint let/4)
            (raise
              (makeblock 0 (field 0 (global Oont!))
                (makeblock 6 "Type error" (makeblock 0 let/4))))
            0)
          (switch let/4
           case tag 5:
            (if (== 1 (field 0 let/4)) (apply (field 2 let/4) 0)
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 6 "Type error" (makeblock 0 let/4)))))
           default:
            (raise
              (makeblock 0 (field 0 (global Oont!))
                (makeblock 6 "Type error" (makeblock 0 let/4)))))))))

  $ ./lambda1.exe
  12

  $ cat >lambda2.scm <<EOF
  > ((lambda (f a) (+ (f a) a)) (lambda (x) (+ x 1)) 12)
  > EOF

  $ oont -dlambda lambda2.scm
  (apply (field 29 (global Oont!))
    (let
      (let/15 =
         (makeblock 5 2 ""
           (function arg/12 arg/13
             (and
               (+
                 (let
                   (let/14 =
                      (seq
                        (if (isint arg/12)
                          (raise
                            (makeblock 0 (field 0 (global Oont!))
                              (makeblock 6 "Type error" (makeblock 0 arg/12))))
                          0)
                        (switch arg/12
                         case tag 5:
                          (if (== 1 (field 0 arg/12))
                            (apply (field 2 arg/12) arg/13)
                            (raise
                              (makeblock 0 (field 0 (global Oont!))
                                (makeblock 6 "Type error" (makeblock 0 arg/12)))))
                         default:
                          (raise
                            (makeblock 0 (field 0 (global Oont!))
                              (makeblock 6 "Type error" (makeblock 0 arg/12)))))))
                   (seq
                     (if (|| (not (isint let/14)) (lsr let/14 62))
                       (raise
                         (makeblock 0 (field 0 (global Oont!))
                           (makeblock 6 "Type error" (makeblock 0 let/14))))
                       0)
                     let/14))
                 (seq
                   (if (|| (not (isint arg/13)) (lsr arg/13 62))
                     (raise
                       (makeblock 0 (field 0 (global Oont!))
                         (makeblock 6 "Type error" (makeblock 0 arg/13))))
                     0)
                   arg/13))
               4611686018427387903))))
      (seq
        (if (isint let/15)
          (raise
            (makeblock 0 (field 0 (global Oont!))
              (makeblock 6 "Type error" (makeblock 0 let/15))))
          0)
        (switch let/15
         case tag 5:
          (if (== 2 (field 0 let/15))
            (apply (field 2 let/15)
              (makeblock 5 1 ""
                (function arg/6
                  (and
                    (+
                      (seq
                        (if (|| (not (isint arg/6)) (lsr arg/6 62))
                          (raise
                            (makeblock 0 (field 0 (global Oont!))
                              (makeblock 6 "Type error" (makeblock 0 arg/6))))
                          0)
                        arg/6)
                      (let (let/11 = 2)
                        (seq
                          (if (|| (not (isint let/11)) (lsr let/11 62))
                            (raise
                              (makeblock 0 (field 0 (global Oont!))
                                (makeblock 6 "Type error" (makeblock 0 let/11))))
                            0)
                          let/11)))
                    4611686018427387903)))
              24)
            (raise
              (makeblock 0 (field 0 (global Oont!))
                (makeblock 6 "Type error" (makeblock 0 let/15)))))
         default:
          (raise
            (makeblock 0 (field 0 (global Oont!))
              (makeblock 6 "Type error" (makeblock 0 let/15))))))))

  $ ./lambda2.exe
  25

  $ cat >lambda3.scm <<EOF
  > zero?
  > EOF

  $ oont -dlambda lambda3.scm
  (apply (field 29 (global Oont!))
    (makeblock 5 1 ""
      (function arg/4
        (seq
          (if (|| (not (isint arg/4)) (lsr arg/4 62))
            (raise
              (makeblock 0 (field 0 (global Oont!))
                (makeblock 6 "Type error" (makeblock 0 arg/4))))
            0)
          (if (== arg/4 0) 3 1)))))

  $ ./lambda3.exe
  #<procedure>
