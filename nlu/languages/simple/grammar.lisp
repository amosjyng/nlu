(defun ensure-indv-exists (element)
  "Either returns the INDV-NODE ELEMENT itself or an INDV-NODE of type ELEMENT."
  (declare (element element))
  (cond ((type-node? element) (new-indv nil element))
        (t element)))

(defconstruction NP {entity}
  ((? ({article (grammar)}) article)
   (* ({modifier}) modifiers)
   (= (:type {entity}) entity))
  
  (let ((new-node (ensure-indv-exists (scone-element (first entity)))))
    ;; it is each of the modifiers that describe it
    (mapcar (lambda (modifier) (new-is-a new-node (scone-element modifier)))
            modifiers)
    ;; but it itself is not a modifier
    (new-is-not-a new-node {modifier})
    ;; return the newly created node
    new-node))

(defconstruction VP {action}
  ((= (:type {action}) action) (? ({entity}) theme))
  
  (let ((action-element (ensure-indv-exists (scone-element (first action)))))
    (unless (null theme)
      (x-is-the-y-of-z (scone-element (first theme)) *action-object*
                       action-element))
    action-element))

(defmacro defconstructions (ntps &rest payload)
  "Create multiple pairs of forms with the same meaning."
  `(progn ,@(mapcar (lambda (ntp)
                      (destructuring-bind (name type pattern) ntp
                        `(defconstruction ,name ,type ,pattern ,@payload)))
                    ntps)))


