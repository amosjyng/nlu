
#.(SB-IMPL::%STUFF-HASH-TABLE
   (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5 :REHASH-THRESHOLD
                    '1.0 :WEAKNESS 'NIL)
   '((("red")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {red thing}) . 24))))
     (("big")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {big thing}) . 25))))
     (("kick")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {kick}) . 18))))
     (("bolt")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {bolt}) . 50))))
     (("the")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {the (article)}) . 24))))
     (("Kick")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {kick}) . 3))))
     (("a")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {a (article)}) . 17))))
     (("screw")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {screw}) . 3)
             (#.(LOOKUP-ELEMENT {syntactic thing}) . 12))))
     (("in")
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((#.(LOOKUP-ELEMENT {syntactic thing}) . 12)))))) 
#.(SB-IMPL::%STUFF-HASH-TABLE
   (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5 :REHASH-THRESHOLD
                    '1.0 :WEAKNESS 'NIL)
   '((#.(LOOKUP-ELEMENT {bolt 0-2703})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {bolt 0-2691})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {bolt 0-2669})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {screw (fastener)})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {red thing})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("NP" MODIFIERS) . 24))))
     (#.(LOOKUP-ELEMENT {big thing})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("NP" MODIFIERS) . 25))))
     (#.(LOOKUP-ELEMENT {kick})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("VP" ACTION) . 18))))
     (#.(LOOKUP-ELEMENT {article (grammar)})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("NP" ARTICLE) . 41))))
     (#.(LOOKUP-ELEMENT {bolt})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("VP" THEME) . 21) (("NP" ENTITY) . 50)
             (("SCREW-IN-X" THEME) . 12))))
     (#.(LOOKUP-ELEMENT {the (article)})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {a (article)})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     ({syntactic thing}
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {screw})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("VP" ACTION) . 3))))
     (#.(LOOKUP-ELEMENT {syntactic thing})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL))
     (#.(LOOKUP-ELEMENT {thing})
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           '((("SCREW-IN-X" DISCARD) . 24))))
     (NIL
      . #.(SB-IMPL::%STUFF-HASH-TABLE
           (MAKE-HASH-TABLE :TEST 'EQUAL :SIZE '16 :REHASH-SIZE '1.5
                            :REHASH-THRESHOLD '1.0 :WEAKNESS 'NIL)
           'NIL)))) 