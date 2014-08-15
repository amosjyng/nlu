;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           ;;;
;;;   GRAMMATICAL ENTITIES    ;;;
;;;                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

;; allow this file to be loaded multiple times
(when (zerop (hash-table-count *strings-to-concepts-hashmap*))
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
  (new-type {you} {pronoun})
  (new-type {he} {pronoun})
  (new-type {she} {pronoun})
  (new-type {we} {pronoun})
  (new-indv {first person} {first_person.n.01})
  (new-indv {second person} {second_person.n.01})
  
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

  (new-indv {generic entity} {thing})
  
  (new-is-a {large.a.01} {entity modifier})
  (new-is-a {color.n.01} {entity modifier})
  ;;(new-is-a {red.s.01} {entity modifier})
  ;;(new-is-a {blue.s.01} {entity modifier})
  (new-is-a {short.a.03} {entity modifier})

  (english {kick.v.01} :verb "kicks")
  (english {kick.v.01} :verb "kicked")
  (english {kick.v.01} :verb "kicking")
  (english {animal} :noun "animals")
  (english {bolt.n.06} :noun "bolts")
  (english {hammer.v.01} :verb "hammered")

  (new-relation {command}
		:a-inst-of {entity}
		:b-inst-of {entity}
		:c-inst-of {action})
  (new-is-a {command} {clause (grammatical entity)})
  
  (new-type {query} {thing})
  (new-type {is-a query} {query})
  (new-type {why query} {query})
  (new-type {how query} {query}))

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

(defconstruction noun-phrase
    ((? {article (grammatical entity)} article)
     (* {entity modifier} modifiers)
     (= {entity} entity))

    (let ((new-node
	   (ensure-indv-exists
	    (meaning-scone-element
	     (first entity)))))
      (progn
	(mapcar (lambda (modifier)
		  (new-is-a new-node (meaning-scone-element modifier)))
		modifiers)
	(when
	    (and (not (null article))
		 (simple-is-x-a-y? (meaning-scone-element (first article))
				   {indefinite article (grammatical entity)}))
	  (new-is-a new-node {generic entity}))
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
			    (when (eq (length instances) 2)
			      (mapcar (lambda (instance)
					(unless (eq instance new-node)
					  (new-eq instance new-node)))
				      instances))))))
	new-node)))

(defconstruction entities
  ((= (:structured {entity}) items)
   (= "and" discard)
   (= (:structured {entity}) items))
  
  (mapcar #'meaning-scone-element items))

(defmacro defaction (action-name action-in-between-name
		     action-scone-element word1 word2)
  `(progn
     (defconstruction ,action-name
       ((= ,word1 discard)
	(= ,word2 discard)
	(? (:structured {entity}) theme))
       
       (let ((new-node (ensure-indv-exists ,action-scone-element))
	     (action-object
	      (when (not (null theme))
		(meaning-scone-element (first theme)))))
	 (when (not (null action-object))
	   (x-is-the-y-of-z action-object *action-object* new-node))
	 new-node))
     (defconstruction ,action-in-between-name
       ((= ,word1 discard)
	(? (:structured {entity}) theme)
	(= ,word2 discard))
       
       (let ((new-node (ensure-indv-exists ,action-scone-element))
	     (action-object
	      (when (not (null theme))
		(meaning-scone-element (first theme)))))
	 (when (not (null action-object))
	   (x-is-the-y-of-z action-object *action-object* new-node))
	 new-node))))

(defaction hammered-in-x hammered-x-in {hammer.v.01} "hammer" "in")
(defaction hammered-down-x hammered-x-down {hammer.v.01} "hammer" "down")
(defaction nailed-down-x nailed-x-down {nail.v.01} "nail" "down")
(defaction pick-up-x pick-x-up {pick_up.v.01} "pick" "up")
(defaction screw-in-x screw-x-in {screw.v.03} "screw" "in")
;;; now for the gerunds
(defaction hammered-in-x hammered-x-in {hammer.v.01} "hammering" "in")
(defaction hammered-down-x hammered-x-down {hammer.v.01} "hammering" "down")
(defaction nailed-down-x nailed-x-down {nail.v.01} "nailing" "down")
(defaction pick-up-x pick-x-up {pick_up.v.01} "picking" "up")
(defaction screw-in-x screw-x-in {screw.v.03} "screwing" "in")

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
  ((= (:ends-in-period :atom {clause (grammatical entity)}) clause))
  
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
   (= :ends-in-question-mark second-thing))
  (new-indv nil {is-a query}))

(defconstruction why-query
  ((= "why" discard) (? "exactly" discard) (= "was" discard)
   (= (:structured {entity}) object)
   (= (:structured :ends-in-question-mark {action}) action))
  (new-indv nil {why query}))

(defconstruction how-query
  ((= "how" discard) (? "exactly" discard) (= "was" discard)
   (= (:structured {entity}) object)
   (= (:structured :ends-in-question-mark {action}) action))
  (new-indv nil {how query}))

(defparameter *new-matches* (start-match-against-constructions *constructions*)
  "List of new matches started from all constructions")

;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  ;;;
;;;    DICTIONARY    ;;;
;;;                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;

;;; set custom weights for words
(load "nlu/weights")

;;; Create a hashmap from strings to Scone concepts
(maphash (lambda (k v)
	   (setf (gethash k *strings-to-concepts-hashmap*) (mapcar #'car v)))
	 *english-dictionary*)

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       ;;;
;;;     LISPIFICATION     ;;;
;;;                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *sentence-position* 0
  "The position at which to start a new sentence")

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
  (list :action (parent-element (meaning-scone-element matched-construction))
	(list :object
	      (lispify (get-first-component 'theme matched-construction)))))

(defun lispify-entity (matched-construction)
  "Turn a matched construction representing an {entity} into a list"
  (let* ((article (get-mse 'article matched-construction))
	 (modifiers (get-mses 'modifiers matched-construction))
	 (entity (get-first-component 'entity matched-construction))
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
    (if (pluralp entity)
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
  (when (and goal-value (matched-constructionp goal-value))
    (cond ((listp (meaning-scone-element goal-value))
	   (mapcar #'lispify (meaning-scone-element goal-value)))
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
	   (lispify-sentence goal-value)))))

(defun nlu (sentence)
  "Create a Lisp representation of a natural language sentence"
  (when (zerop (hash-table-count *chart*))
    (setup-new-parse))
  (let* ((word-list (split-sequence:split-sequence #\Space sentence))
	 (parse-result (semiring-parse word-list *sentence-position*)))
    (incf *sentence-position* (length word-list))
    (lispify parse-result)))
