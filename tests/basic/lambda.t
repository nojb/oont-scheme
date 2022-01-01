  $ cat >lambda1.scm <<EOF
  > (lambda () 12)
  > ((lambda () 12))
  > EOF

  $ oont -dlambda lambda1.scm
  (apply (field 2 (global Oont!))
    (seq (makeblock 4 1 "" (function dummy/3 12))
      (let (let/5 = (makeblock 4 1 "" (function dummy/4 12)))
        (seq
          (if (isint let/5)
            (raise
              (makeblock 0 (field 0 (global Oont!))
                (makeblock 5 "Type error" let/5)))
            0)
          (switch let/5
           case tag 4:
            (if (== 1 (field 0 let/5)) (apply (field 2 let/5) 0)
              (raise
                (makeblock 0 (field 0 (global Oont!))
                  (makeblock 5 "Type error" let/5))))
           default:
            (raise
              (makeblock 0 (field 0 (global Oont!))
                (makeblock 5 "Type error" let/5))))))))

  $ ./lambda1.exe
  12

  $ cat >lambda2.scm <<EOF
  > ((lambda (f a) (+ (f a) a)) (lambda (x) (+ x 1)) 12)
  > EOF

  $ oont -dlambda lambda2.scm
  (apply (field 2 (global Oont!))
    (let
      (let/13 =
         (makeblock 4 2 ""
           (function arg/10 arg/11
             (and
               (+
                 (let
                   (let/12 =
                      (seq
                        (if (isint arg/10)
                          (raise
                            (makeblock 0 (field 0 (global Oont!))
                              (makeblock 5 "Type error" arg/10)))
                          0)
                        (switch arg/10
                         case tag 4:
                          (if (== 1 (field 0 arg/10))
                            (apply (field 2 arg/10) arg/11)
                            (raise
                              (makeblock 0 (field 0 (global Oont!))
                                (makeblock 5 "Type error" arg/10))))
                         default:
                          (raise
                            (makeblock 0 (field 0 (global Oont!))
                              (makeblock 5 "Type error" arg/10))))))
                   (seq
                     (if (|| (not (isint let/12)) (lsr let/12 62))
                       (raise
                         (makeblock 0 (field 0 (global Oont!))
                           (makeblock 5 "Type error" let/12)))
                       0)
                     let/12))
                 (seq
                   (if (|| (not (isint arg/11)) (lsr arg/11 62))
                     (raise
                       (makeblock 0 (field 0 (global Oont!))
                         (makeblock 5 "Type error" arg/11)))
                     0)
                   arg/11))
               4611686018427387903))))
      (seq
        (if (isint let/13)
          (raise
            (makeblock 0 (field 0 (global Oont!))
              (makeblock 5 "Type error" let/13)))
          0)
        (switch let/13
         case tag 4:
          (if (== 2 (field 0 let/13))
            (apply (field 2 let/13)
              (makeblock 4 1 ""
                (function arg/6
                  (and
                    (+
                      (seq
                        (if (|| (not (isint arg/6)) (lsr arg/6 62))
                          (raise
                            (makeblock 0 (field 0 (global Oont!))
                              (makeblock 5 "Type error" arg/6)))
                          0)
                        arg/6)
                      (let (let/9 = 1)
                        (seq
                          (if (|| (not (isint let/9)) (lsr let/9 62))
                            (raise
                              (makeblock 0 (field 0 (global Oont!))
                                (makeblock 5 "Type error" let/9)))
                            0)
                          let/9)))
                    4611686018427387903)))
              12)
            (raise
              (makeblock 0 (field 0 (global Oont!))
                (makeblock 5 "Type error" let/13))))
         default:
          (raise
            (makeblock 0 (field 0 (global Oont!))
              (makeblock 5 "Type error" let/13)))))))

  $ ./lambda2.exe
  25

  $ cat >lambda3.scm <<EOF
  > zero?
  > EOF

  $ oont -dlambda lambda3.scm
  (apply (field 2 (global Oont!))
    (makeblock 4 1 ""
      (function arg/4
        (seq
          (if (|| (not (isint arg/4)) (lsr arg/4 62))
            (raise
              (makeblock 0 (field 0 (global Oont!))
                (makeblock 5 "Type error" arg/4)))
            0)
          (if (== arg/4 0) -4611686018427387903 -4611686018427387904)))))

  $ ./lambda3.exe
  #<procedure>
