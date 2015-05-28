(defconstruction NP
    ((? ({article (grammar)}) article)
     (* ({adjective (grammar)}) modifiers)
     (= ({noun (grammar)}) entity))
    
    ;; TODO: add paoyload
    (let ((new-node (new-indv nil (first entity))))
      ;; it is each of the modifiers that describe it
      (mapcar (lambda (modifier) (new-is-a new-node (scone-element modifier)))
              modifiers)
      ;; but it itself is not a modifier
      (new-is-not-a new-node {modifier})
      ;; return the newly created node
      new-node))