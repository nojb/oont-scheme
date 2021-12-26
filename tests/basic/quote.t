  $ cat >quote1.scm <<EOF
  > '(a b c)
  > EOF

  $ oont -dlambda quote1.scm
  (apply (field 2 (global SchemeStdlib!))
    (let
      (c/5 = (apply (field 1 (global SchemeStdlib!)) #"c")
       b/4 = (apply (field 1 (global SchemeStdlib!)) #"b")
       a/3 = (apply (field 1 (global SchemeStdlib!)) #"a"))
      (makemutable 0 a/3 (makemutable 0 b/4 (makemutable 0 c/5 7)))))

  $ ./quote1.exe
  (a . (b . (c . ())))

  $ cat >quote2.scm <<EOF
  > '()
  > EOF

  $ oont -dlambda quote2.scm
  (apply (field 2 (global SchemeStdlib!)) 7)
  $ ./quote2.exe
  ()
