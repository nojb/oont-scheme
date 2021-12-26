  $ cat >quote1.scm <<EOF
  > '(a b c)
  > EOF

  $ oont -dlambda quote1.scm
  (apply (field 2 (global Oont!))
    (let
      (c/5 = (apply (field 1 (global Oont!)) #"c")
       b/4 = (apply (field 1 (global Oont!)) #"b")
       a/3 = (apply (field 1 (global Oont!)) #"a"))
      (makemutable 0 a/3 (makemutable 0 b/4 (makemutable 0 c/5 7)))))

  $ ./quote1.exe
  (a . (b . (c . ())))

  $ cat >quote2.scm <<EOF
  > '()
  > EOF

  $ oont -dlambda quote2.scm
  (apply (field 2 (global Oont!)) 7)
  $ ./quote2.exe
  ()

  $ cat >quote3.scm <<EOF
  > '(#f #t #f)
  > EOF

  $ oont -dlambda quote3.scm
  (apply (field 2 (global Oont!))
    (makemutable 0 1 (makemutable 0 3 (makemutable 0 1 7))))
  $ ./quote3.exe
  (#f . (#t . (#f . ())))
