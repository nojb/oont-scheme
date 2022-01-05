  $ cat >quote1.scm <<EOF
  > '(a b c)
  > EOF

  $ oont -dlambda quote1.scm
  (apply (field 29 (global Oont!))
    (makemutable 0 (apply (field 22 (global Oont!)) "a")
      (makemutable 0 (apply (field 22 (global Oont!)) "b")
        (makemutable 0 (apply (field 22 (global Oont!)) "c")
          -4611686018427387900))))

  $ ./quote1.exe
  (a . (b . (c . ())))

  $ cat >quote2.scm <<EOF
  > '()
  > EOF

  $ oont -dlambda quote2.scm
  (apply (field 29 (global Oont!)) -4611686018427387900)
  $ ./quote2.exe
  ()

  $ cat >quote3.scm <<EOF
  > #|
  >   This is a test.
  > |#
  > '(#f #t #f)
  > EOF

  $ oont -dlambda quote3.scm
  (apply (field 29 (global Oont!))
    (makemutable 0 -4611686018427387904
      (makemutable 0 -4611686018427387903
        (makemutable 0 -4611686018427387904 -4611686018427387900))))
  $ ./quote3.exe
  (#f . (#t . (#f . ())))
