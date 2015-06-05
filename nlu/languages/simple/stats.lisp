
#.(SB-IMPL::%STUFF-HASH-TABLE
   (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5 :REHASH-THRESHOLD
                    '1.0 :WEAKNESS 'NIL)
   '((("kick")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {kick}) . 2))))
     (("bolt")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {bolt}) . 3))))
     (("the")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {the (article)}) . 3))))
     (("Kick")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {kick}) . 3)))))) 
#.(SB-IMPL::%STUFF-HASH-TABLE
   (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5 :REHASH-THRESHOLD
                    '1.0 :WEAKNESS 'NIL)
   '((#.(LOOKUP-ELEMENT {bolt 0-2638})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {bolt 0-2631})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {kick})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("VP" ACTION) . 2))))
     (#.(LOOKUP-ELEMENT {article (grammar)})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("NP" ARTICLE) . 3))))
     (#.(LOOKUP-ELEMENT {bolt})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("VP" THEME) . 2) (("NP" ENTITY) . 3))))
     (#.(LOOKUP-ELEMENT {the (article)})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL)))) 