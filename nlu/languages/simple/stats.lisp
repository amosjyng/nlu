
#.(SB-IMPL::%STUFF-HASH-TABLE
   (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5 :REHASH-THRESHOLD
                    '1.0 :WEAKNESS 'NIL)
   '((("Kick")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {kick}) . 3)))))) 