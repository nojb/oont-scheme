  $ cat >quote1.scm <<EOF
  > '(a b c)
  > EOF

  $ oont -dlambda quote1.scm
  (apply (field 2 (global Oont!))
    (makemutable 0 (apply (field 1 (global Oont!)) "c")
      (makemutable 0 (apply (field 1 (global Oont!)) "b")
        (makemutable 0 (apply (field 1 (global Oont!)) "a")
          -4611686018427387900))))

  $ ./quote1.exe
  (c . (b . (a . ())))

  $ cat >quote2.scm <<EOF
  > '()
  > EOF

  $ oont -dlambda quote2.scm
  (apply (field 2 (global Oont!)) -4611686018427387900)
  $ ./quote2.exe
  ()

  $ cat >quote3.scm <<EOF
  > #|
  >   This is a test.
  > |#
  > '(#f #t #f)
  > EOF

  $ oont -dlambda quote3.scm
  (apply (field 2 (global Oont!))
    (makemutable 0 -4611686018427387904
      (makemutable 0 -4611686018427387903
        (makemutable 0 -4611686018427387904 -4611686018427387900))))
  $ ./quote3.exe
  (#f . (#t . (#f . ())))
