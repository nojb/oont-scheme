  $ cat >lambda.scm <<EOF
  > (lambda () 12)
  > ((lambda () 12))
  > EOF

  $ oont -dlambda lambda.scm
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

  $ ./lambda.exe
  12
