(defun reload-grammar ()
  "Reload this grammar file

   Used only for development"
  (load "nlu/grammar"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           ;;;
;;;   GRAMMATICAL ENTITIES    ;;;
;;;                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; allow this file to be loaded multiple times
(unless (lookup-element {grammatical entity})
  (new-type {grammatical entity} {thing})
  (new-eq {entity} {entity.n.01})
  (new-type {specific entity} {entity})
  (new-type {pronoun} {entity})
  (new-type {entity modifier} {thing})
  (new-type {article (grammatical entity)} {grammatical entity})
  (new-type {clause (grammatical entity)} {grammatical entity})
  (new-type {action clause (grammatical entity)} {grammatical entity})
  (new-type {definition clause (grammatical entity)} {grammatical entity})
  (new-type {sentence (grammatical entity)} {grammatical entity})
  
  (new-is-a {tangible} {entity})
  (new-is-a {legal entity} {entity})
  (new-type {I} {pronoun})
  (new-type {me} {pronoun})
  (new-type {you} {pronoun})
  (new-type {he} {pronoun})
  (new-type {she} {pronoun})
  (new-type {we} {pronoun})
  (new-eq {first person} {first_person.n.01})
  (new-eq {second person} {second_person.n.01})
  (new-eq {I}   {first person})
  (new-eq {me}  {first person})
  (new-eq {you} {second person})

  (new-eq {physical_entity.n.01} {tangible})

  (new-type {table leg} {leg.n.03} :english "leg")
  (x-is-a-y-of-z {table leg} {part} {table.n.02})
  
  (new-type {indefinite article (grammatical entity)}
            {article (grammatical entity)})
  (new-type {definite article (grammatical entity)}
            {article (grammatical entity)})
  (new-indv {a (article)}
            {indefinite article (grammatical entity)}
            :english "a")
  (new-indv {an (article)}
            {indefinite article (grammatical entity)}
            :english "an")
  (new-indv {the (article)}
            {definite article (grammatical entity)}
            :english "the")

  (new-type {to entity (grammatical entity)}
            {grammatical entity})

  (new-indv {generic entity} {thing})
  
  (new-is-a {large.a.01} {entity modifier})
  (new-is-a {color.n.01} {entity modifier})
  ;;(new-is-a {red.s.01} {entity modifier})
  ;;(new-is-a {blue.s.01} {entity modifier})
  (new-is-a {short.a.03} {entity modifier})

  (english {pick_up.v.01} :verb "pick")

  (new-relation {command}
                :a-inst-of {entity}
                :b-inst-of {entity}
                :c-inst-of {action})
  (new-is-a {command} {clause (grammatical entity)})
  
  (new-type {query} {thing})
  (new-type {is-a query} {query})
  (new-type {why query} {query})
  (new-type {how query} {query}))

;;; utility functions

(defun mse-f (meanings)
  "Get the first meaning's scone element out of a list of meanings"
  (meaning-scone-element (first meanings)))

;;;;;;;;;;;;;;;;;;;;;;;
;;;                 ;;;
;;;   ATTRIBUTES    ;;;
;;;                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *attribute-detection* nil)
(defparameter *attribute-removal* nil)
(defparameter *attribute-combination* nil)

(defun str-ends-in-charp (str char)
  "See if a string ends in a certain character"
  (eq char (aref str (1- (length str)))))

(defun str-ends-in (str suffix)
  "See if a string ends (in a case-sensitive manner) in another string. If so,
   returns the string without the suffix"
  (let* ((end-pos (length str))
         (base-end (- end-pos (length suffix))))
    (when (and (> base-end 0) (equal suffix (subseq str base-end end-pos)))
      (subseq str 0 base-end))))

(defun remove-char (str char)
  "If STR has CHAR at the end, remove it"
  (if (str-ends-in-charp str char)
      (subseq str 0 (1- (length str)))
    str))

(defmacro defpunctuation-attr (attr punctuation-symbol)
  "Define a punctuation mark as an attribute"
  `(defattr ,attr
       ;; how do we detect it?
       (lambda (word) (str-ends-in-charp word ,punctuation-symbol))
     ;; how do we remove it?
     (lambda (word) (remove-char word ,punctuation-symbol))
     ;; how do we detect it in a newly finished match?
     (lambda (match) (attr? ,attr (get-last-match-component match)))))

(defpunctuation-attr :ends-in-comma #\,)
(defpunctuation-attr :ends-in-. #\.)
(defpunctuation-attr :ends-in-? #\?)

(defmacro inherit-from (attr match-component)
  "Inherit an attribute from one component of a match"
  `(lambda (match)
     (let ((component (first (gethash ',match-component (match-so-far match)))))
       (when component (attr? ,attr component)))))

(defmacro defsuffix (attr suffixes match-component)
  "Define a suffix ending (e.g. -ed or -ing or -es) as an attribute"
  `(let ((detection-func
          (lambda (word)
            (or ,@(mapcar (lambda (suffix) `(str-ends-in word ,suffix))
                          suffixes)))))
     (defattr ,attr
       ;; how do we detect it?
       detection-func
     ;; how do we remove it?
     detection-func
     ;; how do we detect it in a newly finished match?
     (inherit-from ,attr ,match-component))))

(defsuffix :plural ("es" "s") entity)
(defsuffix :past ("ed") action)

(defattr :present
    (lambda (word)
      (every #'null
             (mapcar (lambda (suffix) (str-ends-in word suffix)) '("ed"))))
  (lambda (word) word)
  (inherit-from :present action))

(defun get-string-meanings (word position)
  "Retrieve a list of all meanings associated with all morphological forms of a
   given string (i.e. given the string 'deer' it will return both the singular
   and plural meanings of 'deer')"
  (let ((attrs nil))
    (funcall #'append
             (exact-meanings word position attrs)
             (with-attrs (:ends-in-comma :ends-in-. :ends-in-?)
               (funcall #'append
                        (when-attr :present
                          (exact-meanings word position attrs :verb))
                        (exact-meanings word position attrs)
                        (when-attr :plural
                          (exact-meanings word position attrs :noun))
                        (when-attr :past
                                   (exact-meanings word position attrs :verb)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    ;;;
;;;   CONSTRUCTIONS    ;;;
;;;                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

;;; user is first person, computer is second person
(new-eq {cccc} {second person})
(new-eq {uuuu} {first person})
(defparameter *computer* {cccc})
(defparameter *user* {uuuu})

(defparameter *constructions* nil)

(defun find-element-with (article modifiers entity)
  "Given an article, modifiers, and a Scone element representing an
   entity, find the one Scone individual (if any) that matches all
   modifiers and is of type ENTITY"
  (when 
      (and (not (null article))
           (simple-is-x-a-y? (meaning-scone-element (first article))
                             {definite article (grammatical entity)}))
    (with-markers (m um)
                  (progn
                    (mark-instances (meaning-scone-element (first entity))
                                    m)
                    (mapcar (lambda (modifier)
                              (do-marked (instance m)
                                         (unless (simple-is-x-a-y? instance
                                                                   (meaning-scone-element modifier))
                                           (mark instance um))))
                            modifiers)
                    (do-marked (instance um)
                               (unmark instance m))
                    (let ((instances (list-marked m)))
                      (when (eq (length instances) 1)
                        (first instances)))))))

(defconstruction noun-phrase
    ((? {article (grammatical entity)} article)
     (* {entity modifier} modifiers)
     (= {entity} entity))

    (let* ((existing-node (find-element-with article modifiers entity))
           (new-node
            (or existing-node
                (ensure-indv-exists
                 (meaning-scone-element (first entity))))))
      (progn
        (unless existing-node ; then create it
          (mapcar (lambda (modifier)
                    (new-is-a new-node (meaning-scone-element modifier)))
                  modifiers)
          ;; always make sure it's not treated as an adjective
          ;; otherwise it'll be an {entity modifier} because it IS-A various
          ;; entity modifiers
          (new-is-not-a new-node {entity modifier})
          (when ; we are referring to "an <object>"
              (and (not (null article))
                   (simple-is-x-a-y? (meaning-scone-element (first article))
                                     {indefinite article (grammatical entity)}))
            ;; then make this newly define object generic
            (new-is-a new-node {generic entity})))
        new-node)))

(defconstruction subentity
  ((= (:unstructured {entity}) bigger-entity)
   (= (:unstructured {entity}) entity))
  
  (unless (simple-is-x-a-y-of-z? (mse-f entity) {part} (mse-f bigger-entity))
    (error "Subentity not part of entity"))
  
  (mse-f entity))

(defconstruction entities
  ((= (:structured {entity}) items)
   (= "and" discard)
   (= (:structured {entity}) items))
  
  (mapcar #'meaning-scone-element items))

(defmacro defaction (action-name action-in-between-name
                     action-scone-element extra)
  `(defconstructions ,(list action-name action-in-between-name)
     (((= ,action-scone-element action)
       (= ,extra discard)
       (? (:structured {entity}) theme))
      
      ((= ,action-scone-element action)
       (= (:structured {entity}) theme)
       (= ,extra discard)))
     
     (let ((new-node (ensure-indv-exists ,action-scone-element))
           (action-object
            (when (not (null theme))
              (meaning-scone-element (first theme)))))
       (when (not (null action-object))
         (x-is-the-y-of-z action-object *action-object* new-node))
       new-node)))

(defaction hammer-in-x hammer-x-in {hammer.v.01} "in")
(defaction hammer-down-x hammer-x-down {hammer.v.01} "down")
(defaction nail-down-x nail-x-down {nail.v.01} "down")
(defaction pick-up-x pick-x-up {pick_up.v.01} "up")
(defaction screw-in-x screw-x-in {screw.v.03} "in")

(defmacro deft-action (v-r-o v-o-to-r action)
  `(defconstructions ,(list v-r-o v-o-to-r)
     (((= (:unstructured ,action) action)
       (= (:structured {entity}) recipient)
       (= (:structured {entity}) theme))
      
      ((= (:unstructured ,action) action)
       (= (:structured {entity}) theme)
       (= "to" discard)
       (= (:structured {entity}) recipient)))

     (let ((new-node (ensure-indv-exists ,action))
           (action-object
            (meaning-scone-element (first theme))))
       (x-is-the-y-of-z action-object *action-object* new-node)
       (when recipient
         (x-is-the-y-of-z
          (meaning-scone-element (first recipient))
          *action-recipient* new-node))
       new-node)))

(deft-action hand-me-x hand-x-to-me {pass.v.05})

(defconstruction verb-phrase
    ((= (:unstructured {action}) action)
     (? (:structured {entity}) theme))
    (let ((new-node (ensure-indv-exists (meaning-scone-element (first action))))
          (action-object
           (when (not (null theme))
             (meaning-scone-element (first theme)))))
      (when (not (null action-object))
        (x-is-the-y-of-z action-object *action-object* new-node))
      new-node))

(defconstruction v-entities
  ((= (:unstructured {action}) action)
   (= (:list {entity}) entities))
  
  (new-indv nil (first action)))

(defconstruction action-clause
  ((= (:structured {entity}) agent) (= (:structured {action}) action))

    (x-is-the-y-of-z (meaning-scone-element (first agent))
                     *action-agent*
                     (meaning-scone-element (first action)))
    (new-indv nil {action clause (grammatical entity)}))

(defconstruction sentence
  ((= (:ends-in-. :atom {clause (grammatical entity)}) clause))
  
  (new-indv nil {sentence (grammatical entity)}))
(setf (construction-score-multiplier sentence) 1.1)

(defconstruction s-actions
  ((= (:structured {entity}) agent) (= (:list {action}) actions))
  
  (mapcar (lambda (action)
            (x-is-the-y-of-z (meaning-scone-element (first agent))
                             *action-agent*
                             (meaning-scone-element action)))
          (gethash 'actions
                   (get-matched-construction-components
                    (first actions))))
  (new-indv nil {action clause (grammatical entity)}))

(defconstruction clauses
  ((= {clause (grammatical entity)} clauses)
   (= "and" discard)
   (= {clause (grammatical entity)} clauses))
  
  (mapcar #'meaning-scone-element clauses))
(setf (construction-score-multiplier clauses) 0.8)

(defconstruction definition
  ((= {indefinite article (grammatical entity)} discard)
   (= :unstructured unknown)
   (= "is" discard)
   (= (:structured {entity}) something-existing))

  (let* ((unknown-thing (meaning-scone-element (first unknown)))
	 (singular-form
	  (when (stringp unknown-thing) unknown-thing))
	 (possibly-new-thing (or singular-form unknown-thing))
	 (existing-meaning
	  (simple-get-meaning 'entity something-existing)))
    (if (and (not (null singular-form))
	     (null (lookup-definitions singular-form)))
	(convert-parent-wire-to-link
	 (new-type singular-form existing-meaning))
      (new-is-a possibly-new-thing existing-meaning)))
  (new-indv nil {definition clause (grammatical entity)}))

(defconstruction command
 ((? "please" discard) (= (:structured {action}) action))

 (new-statement {first person} {command} {second person}
		:c (meaning-scone-element (first action))))
(setf (construction-score-multiplier command) 1.1)

(defconstruction ordered-command
  ((? "please" discard) (= (:structured {before}) ordered-action))
  
  (new-statement {first person} {command} {second person}
		 :c (meaning-scone-element (first ordered-action))))
(setf (construction-score-multiplier ordered-command) 1.3)

(defmacro defordered-action (construction-name pattern)
  `(defconstruction ,construction-name
     ,pattern
     
     (new-statement (meaning-scone-element (first first-action))
		    {before}
		    (meaning-scone-element (first second-action)))))

(defordered-action action-after
  ((= (:structured {action}) first-action)
   (= "and" discard)
   (? "then" discard)
   (= (:structured {action}) second-action)))

(defordered-action action-after2
  ((= (:structured {action}) second-action)
   (= "after" discard)
   (= (:structured {action}) first-action)))

(defordered-action action-after3
  ((= "after" discard) ; note: can be problematic as {action}
   (= (:ends-in-comma :structured {action}) first-action)
   (= (:structured {action}) second-action)))

(defordered-action action-before
  ((= "before" discard)
   (= (:ends-in-comma :structured {action}) second-action)
   (= (:structured {action}) first-action)))

(defordered-action action-before2
  ((= (:structured {action}) first-action)
   (= "before" discard)
   (= (:structured {action}) second-action)))

(defun singularize (plural-form)
  "Obtain the singular form of a plural word"
  ;; simply assume it ends in 's' for now
  (subseq plural-form 0 (1- (length plural-form))))

(defconstruction definition2
  ((= :unstructured unknown) (= "are" discard)
   (= {entity} something-existing))

  (let* ((unknown-thing (meaning-scone-element (first unknown)))
	 (singular-form
	  (when (stringp unknown-thing)
	    (singularize unknown-thing)))
	 (possibly-new-thing (or singular-form unknown-thing))
	 (existing-meaning
	  (simple-get-meaning 'entity something-existing)))
    (if (and (not (null singular-form))
	     (null (lookup-definitions singular-form)))
	(convert-parent-wire-to-link
	 (new-type singular-form existing-meaning))
      (new-is-a possibly-new-thing existing-meaning)))
  (new-indv nil {definition clause (grammatical entity)})) ; don't combine

(defconstruction is-a-query
  ((= "is" discard)
   (= {article (grammatical entity)} discard)
   (= nil first-thing)
   (= {indefinite article (grammatical entity)} discard)
   (= :ends-in-? second-thing))
  (new-indv nil {is-a query}))

(defconstruction why-query
  ((= "why" discard) (? "exactly" discard) (= "was" discard)
   (= (:structured {entity}) object)
   (= (:structured :ends-in-? {action}) action))
  (new-indv nil {why query}))

(defconstruction how-query
  ((= "how" discard) (? "exactly" discard) (= "was" discard)
   (= (:structured {entity}) object)
   (= (:structured :ends-in-? {action}) action))
  (new-indv nil {how query}))

(setup-new-parse)

;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  ;;;
;;;    DICTIONARY    ;;;
;;;                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;; set custom weights for words
(load "nlu/weights")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       ;;;
;;;     LISPIFICATION     ;;;
;;;                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun lispify-pronoun (meaning)
  "Replace a pronoun meaning with first/second person (or actual object)"
  (let ((mse (meaning-scone-element meaning)))
    (cond ((simple-is-x-a-y? mse {me})
	   {first person}))))

(defun lispify-before (matched-construction)
  "Turn a matched construction representing a {before} relation into a
   list"
  (list :in-order
	(lispify (get-first-component 'first-action matched-construction))
	(lispify (get-first-component 'second-action matched-construction))))

(defun lispify-command (matched-construction)
  "Turn a matched construction representing a {command} into a list"
  (list :command
	(lispify
	 (or (get-first-component 'action matched-construction)
	     (get-first-component 'ordered-action matched-construction)))))

(defun lispify-action (matched-construction)
  "Turn a matched constructino representing an {action} into a list"
  (let* ((object (get-first-component 'theme matched-construction))
	 (recipient (get-first-component 'recipient matched-construction))
	 (o-list (when object
		   (list :object (lispify object))))
	 (r-list (when recipient
		   (list :recipient (lispify recipient)))))
    (cons :action
	  (cons (parent-element (meaning-scone-element matched-construction))
		(cond ((and o-list r-list)
		       (list o-list r-list))
		      (o-list (list o-list))
		      (r-list (list r-list)))))))

(defun lispify-entity (meaning)
  "Turn a meaning representing an {entity} into a list"
  (let* ((article (and (matched-constructionp meaning)
                       (get-mse 'article meaning)))
	 (modifiers (and (matched-constructionp meaning)
                         (get-mses 'modifiers meaning)))
	 (entity (if (matched-constructionp meaning)
                      (get-first-component 'entity meaning)
                      meaning))
	 (article-keyword
	  (if (and article
		   (simple-is-x-a-y? article
				     {definite article (grammatical entity)}))
	      :specific
	    :generic))
	 (entity-list
	  (if modifiers
	      (list (meaning-scone-element entity)
		    (cons :attributes modifiers))
	    (list (meaning-scone-element entity)))))
    (if (attr? :plural entity)
	(list article-keyword
	      (cons :multiple entity-list))
      (cons article-keyword
	    entity-list))))

(defun lispify-action-clause (matched-construction)
  "Turn a matched construction representing an action clause into a list"
  (list :event
	(lispify (get-first-component 'agent matched-construction))
	(lispify (get-first-component 'action matched-construction))))

(defun lispify-why-query (matched-construction)
  "Turn a matched construction representing a why-query into a list"
  (list :why-query
	(list :action
	      (parent-element (get-mse 'action matched-construction))
	      (lispify (get-first-component 'object matched-construction)))))

(defun lispify-sentence (matched-construction)
  "Turn a matched construction representing a sentence into a list"
  ;; I mean a "sentence" is a purely grammatical construct... it is
  ;; really just an arbitrary division of semantic information, so just
  ;; lispify whatever the sentence is made of for now.
  (lispify (get-first-component 'clause matched-construction)))

(defun lispify (goal-value)
  "Turn a matched construction into a Lisp list"
  (when goal-value
    (cond ((listp (meaning-scone-element goal-value))
	   (mapcar #'lispify (meaning-scone-element goal-value)))
	  ((simple-is-x-a-y? (meaning-scone-element goal-value)
			     {pronoun})
	   (lispify-pronoun goal-value))
	  ((simple-is-x-a-y? (meaning-scone-element goal-value)
			     {before})
	   (lispify-before goal-value))
	  ((simple-is-x-a-y? (meaning-scone-element goal-value)
			     {command})
	   (lispify-command goal-value))
	  ((simple-is-x-a-y? (meaning-scone-element goal-value)
			     {action})
	   (lispify-action goal-value))
	  ((simple-is-x-a-y? (meaning-scone-element goal-value)
			     {entity})
	   (lispify-entity goal-value))
	  ((simple-is-x-a-y? (meaning-scone-element goal-value)
			     {action clause (grammatical entity)})
	   (lispify-action-clause goal-value))
	  ((simple-is-x-a-y? (meaning-scone-element goal-value)
			     {why query})
	   (lispify-why-query goal-value))
	  ((simple-is-x-a-y? (meaning-scone-element goal-value)
			     {sentence (grammatical entity)})
	   (lispify-sentence goal-value))
	  (t goal-value))))
