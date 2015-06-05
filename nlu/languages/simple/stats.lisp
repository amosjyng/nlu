
#.(SB-IMPL::%STUFF-HASH-TABLE
   (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5 :REHASH-THRESHOLD
                    '1.0 :WEAKNESS 'NIL)
   '((("a")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {a (article)}) . 2))))
     (("Kick")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {kick}) . 3))))
     (("the")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {the (article)}) . 5))))
     (("bolt")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {bolt}) . 7))))
     (("kick")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {kick}) . 6)))))) 
#.(SB-IMPL::%STUFF-HASH-TABLE
   (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5 :REHASH-THRESHOLD
                    '1.0 :WEAKNESS 'NIL)
   '((#.(LOOKUP-ELEMENT {bolt 0-2618})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {a (article)})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {bolt 0-2611})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {the (article)})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {bolt})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("NP" ENTITY) . 7) (("VP" THEME) . 6))))
     (#.(LOOKUP-ELEMENT {article (grammar)})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("NP" ARTICLE) . 7))))
     (#.(LOOKUP-ELEMENT {kick})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("VP" ACTION) . 6))))
     (NIL
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL)))) 