
#.(SB-IMPL::%STUFF-HASH-TABLE
   (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5 :REHASH-THRESHOLD
                    '1.0 :WEAKNESS 'NIL)
   '((("bolt")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {bolt}) . 1))))
     (("the")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {the (article)}) . 1))))
     (("Kick")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {kick}) . 3)))))) 
#.(SB-IMPL::%STUFF-HASH-TABLE
   (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5 :REHASH-THRESHOLD
                    '1.0 :WEAKNESS 'NIL)
   '((#.(LOOKUP-ELEMENT {article (grammar)})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("NP" ARTICLE) . 1))))
     (#.(LOOKUP-ELEMENT {bolt})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("NP" ENTITY) . 1))))
     (#.(LOOKUP-ELEMENT {the (article)})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL)))) 