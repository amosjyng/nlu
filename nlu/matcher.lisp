;;; For maximum compatibility, use the "metal" version of Scone. You need to
;;; have Quicklisp installed.
;;;
;; ===== SAMPLE RUN =====
;; * (load "matcher/matcher")
;;
;; T
;; * (load "matcher/grammar")
;;
;; T
;; * (nlu "Please pick up a large screwdriver and screw in the bolts.")
;;
;; (:COMMAND
;;  (:IN-ORDER
;;   (:ACTION {pick_up.v.01}
;;    (:GENERIC {screwdriver.n.02} (:ATTRIBUTES {large.a.01})))
;;   (:ACTION {screw.v.03} (:SPECIFIC (:MULTIPLE {bolt.n.06})))))
;; * (nlu "Why was the nail hammered in?")
;;
;; (:WHY-QUERY (:ACTION {hammer.v.01} (:SPECIFIC {nail.n.02})))

;;; NOTE: This is only the CxG matcher code! It does not come with a grammar.
;;; You must define your own. The default grammar is defined in grammar.lisp
;;;
;;; To profile on SBCL, run
;;; (require :sb-sprof)
;;; (sb-sprof:with-profiling (:show-progress t :loop t :report :graph)
;;;  (nlu "Please pick up a large red table leg and screw in the bolts."))

;;; TODO:
;;; Allow for parsing CSG's.


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    ;;;
;;;     PARAMETERS     ;;;
;;;                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;

(defparameter *pattern-skip-penalty* 0.05
  "Percentage penalty applied to score when skipping the next part of a pattern.
   For example, a skip penality of 0.1 would mean that the match retains 90% of
   its original score when it wants to skip the next optional part of a
   pattern")

;;; TODO!!!!! FIX THIS SO THAT PENALTY IS ACTUALLY APPLIED!!!
(defparameter *word-skip-penalty* 0.2
  "Percentage penalty applied to score when skipping the next word in a
   sentence.")

(defparameter *string-as-concept-penalty* 0.01
  "Penalty for using a string as-is (e.g. as an unknown word) in place
   of a Scone element for a MEANING object")

;;; DEBUGGING PARAMETERS
(defvar *debug-ht-update* nil
  "Print successful updates to the chart/agenda")

(defvar *debug-add-failures* nil
  "Print failed adds to the chart/agenda")

(defvar *debug-con-creation* nil
  "Print errors from completed matches that can't become matched constructions")

(defvar *debug-payload* nil
  "Print relevant information about the payload while it is being created")

(defvar *debug-goal-discovery* nil
  "Print all newly discovered potential goal values")

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                   ;;;
;;;     GENERAL UTILITY FUNCTIONS     ;;;
;;;                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(base-context-marker-setup)

;; should probably be in Scone instead
(defun simple-is-x-a-y-of-z? (x y z)
  "See if element X is a role Y of element Z"
  (let ((x (lookup-element-test x))
        (y (lookup-element-test y))
        (z (lookup-element-test z)))
    (with-markers (m)
      (progn
        (mark-role y z m :downscan t)
        (marker-on? x m)))))

(defmacro print-debug (statement &rest args)
  "Prints debugging STATEMENT out to user"
  `(progn
     (format *error-output* ,statement ,@args)
     (format *error-output* "~%")))

(defun reload-matcher ()
  "Relaod this file

   Used for development"
  (load "nlu/matcher")
  (setup-new-parse))

(defun debug-all ()
  "Set all debug global variables to true, and make element names human-readable

   Only used for debugging"
  (setf *generate-long-element-names* t)
  
  (setf *debug-ht-update* t)
  (setf *debug-add-failures* t)
  (setf *debug-con-creation* t)
  (setf *debug-payload* t)
  (setf *debug-goal-discovery* t))

(defun round-decimal (number shift)
  "Rounds a number to something"
  (/ (round (* number shift)) shift))

(defun insert-at (item list index)
  "Return a new copy of LIST with ITEM inserted at the position specified by
   the INDEX. The new list may share a tail with the original LIST.

   Used only in printing the progress of a match to the screen, so performance
   shouldn't be much of an issue.

   TODO: Use a loop instead of recursion"
  (cond
    ((< index 1) (error "Index too small ~A" index))
    ((= index 1) (cons item list))
    ((endp list) (list "Index too big!"))
    (t (cons (first list) (insert-at item (rest list) (1- index))))))

(defun ensure-indv-exists (scone-element)
  "If SCONE-ELEMENT is a type node, then returns a new indv node that is one of
   SCONE-ELEMENT. Otherwise, if it's already an indv, just returns itself."
  (if (type-node? scone-element)
      (new-indv nil scone-element)
      scone-element))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         ;;;
;;;     DATA STRUCTURES     ;;;
;;;                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defclass range ()
  ((start-position
    :documentation "The inclusive start position of this range"
    :type integer
    :initarg :start
    :initform nil
    :accessor start-of)
   (end-position
    :documentation "The exclusive end position of this range"
    :type integer
    :initarg :end
    :initform nil
    :accessor end-of))
  (:documentation
   "A class that, along with functions designed for it, keep
    track of what spans what ranges, which ranges overlap, etc. This class and
    its functions are reusable across different other classes."))

(defclass meaning (range)
  ((scone-element
    :documentation "The Scone concept that this meaning is all about"
    :type structure-object ; Scone element
    :initarg :scone-element
    :initform (error
               "You MUST provide the Scone element for this meaning")
    :accessor meaning-scone-element)
   (score
    :documentation
    "The probability of this meaning being the right meaning. For
    example, the speaker will most likely intend to communicate a modern meaning
    of a word rather than an archaic meaning, so this score captures such
    information."
    :type number
    :initarg :score
    :initform 1
    :reader get-meaning-score)
   (plural ; todo: make constructions plural too
    :documentation
    "See whether this word matches a plural word. Might be true for verbs as well,
     but if it's a verb just ignore this field as it is meaningless."
    :type boolean
    :initarg :plural
    :initform nil
    :reader pluralp)
   (ends-in-comma
    :documentation
    "Whether or not the text for this meaning ends in a comma"
    :type boolean
    :initarg :ends-in-comma
    :initform nil
    :reader ends-in-commap)
   (ends-in-period
    :documentation
    "Whether or not the text for this meaning ends in a period"
    :type boolean
    :initarg :ends-in-period
    :initform nil
    :reader ends-in-periodp)
   (ends-in-question-mark
    :documentation
    "Whether or not the text for this meaning ends in a question mark"
    :type boolean
    :initarg :ends-in-question-mark
    :initform nil
    :reader ends-in-question-markp))
  (:documentation
   "Wrapper for a Scone element. Eventually this is intended to
    store extra information about this meaning, such as the language or part of
    speech."))

(defclass construction ()
  ((elements
    :documentation
    "The actual list of elements that make up this construction pattern.

     Each element in this list can either be:
     - a string, in which case only string tokens *exactly* matching that string
       will be considered
     - a Scone element, in which case only matched-constructions that are in the
       is-a hierarchy of this Scone element (including the element iteslf) are
       matched
     - a list of the form (OPERATOR TOKEN BINDING-VARIABLE). The OPERATOR,
       inspired by regex orepators, should be one of these symbols:
       - = : simply match this token as usual, but bind the match to the binding
             variable
       - ? : this token can be optionally matched
       - * : this token can be matched any number of times (including zero)
       - + : this token must be matched at least once
      Meanwhile, TOKEN should be either a string or a Scone concept, which will 
      be matched as described above.

      OPTIONAL-BINDING-VARIABLE is an optional third element in the list. This
      is a symbol that the actual match will be bound to when calling the
      payload function of a matched-construction using this pattern."
    :type list
    :initarg :elements
    :initform (error "You MUST provide the list of elements that forms this
     pattern")
    :reader get-construction-pattern)
   (count
    :documentation
    "The number of times this pattern was successfully matched in
     its entirety. Currently unused."
    :type integer
    :initform 0
    :accessor pattern-count)
   (payload
    :documentation
    "The optional code which does something with the bindings
     supplied by a successful match. This code, when optionally supplied, should
     return a primary node representing the bulk of the information in this
     CONSTRUCTION (e.g. if this were a noun phrase CONSTRUCTION, the
     payload may return a new INDV node that contains
     is-a links to each of the adjectives in the noun phrase)."
    :type function
    :initarg :payload
    :reader get-construction-payload)
   (score-multiplier
    :documentation
    "What the score of the matched construction will be multiplied by, after
     multiplying together the scores of all the constituent components"
    :type number
    :initform 1
    :accessor construction-score-multiplier))
  (:documentation
   "A class that holds the patterns defining a particular
    CONSTRUCTION, the Scone type that this CONSTRUCTION represents, and a
    payload which is executed once such a CONSTRUCTION is matched."))

(defclass matched-construction (meaning)
  ((rule
    :documentation
    "The CONSTRUCTION that this matched-construction was created from"
    :type construction
    :initarg :rule
    :initform (error "You MUST specify which CONSTRUCTION this
                      MATCHED-CONSTRUCTION is created from")
    :reader get-construction)
   (components
    :documentation
    "The components of this matched-construction, obtained from the actual
     matches made. This should be a hash table with the keys being Lisp symbols
     representing the components, and values being a list of actual tokens that
     were matched."
    :type hash-table
    :initarg :components
    :initform nil
    :reader get-matched-construction-components)
   (context
    :documentation
    "The context in which this matched construction was created"
    :type structure-object
    :initarg :context
    :accessor context-of))
  (:documentation
   "A class which represents a successfully matched MATCHED-CONSTRUCTION, as
    well as various information about it, such as how it was created."))

(defclass match (range)
  ((construction
    :documentation
    "The CONSTRUCTION that has one of the patterns this is 
     trying to match"
    :type construction
    :initarg :construction
    :initform (error "You MUST provide the CONSTRUCTION whose pattern this 
                      MATCH class is supposed to match")
    :reader get-match-construction)
   (match-so-far
    :documentation
    "A hash table of bindings from successful matches. The car 
     of each element should be the symbol that the match is bound to, while the 
     cdr is the list of tokens that were matched to that symbol in the pattern."
    :type hash-table
    :initarg :match-so-far
    :initform (make-hash-table)
    :accessor match-so-far)
   (match-progress
    :documentation
    "Which part of the CONSTRUCTION this MATCH is currently at."
    :type integer
    :initarg :match-progress
    :initform 0
    :accessor match-progress)
   (score
    :documentation
    "The likelihood of this match being the correct parse"
    :type number
    :initarg :score
    :initform 1
    :accessor match-score))
  (:documentation
   "A partial match of a CONSTRUCTION specified by a MATCHED-CONSTRUCTION. Once
    a MATCH is completed, a MATCHED-CONSTRUCTION object is created that stores
    all the useful information in the completed match."))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                  ;;;
;;;     DATA STRUCTURE FUNCTIONS     ;;;
;;;                                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload "alexandria")

(defun change-operator (new-operator element)
  "Change the operator of an element of a pattern. ELEMENT is assumed to be a
   list"
  (cons new-operator (rest element)))

(defun operator (expected-token)
  "Return the operator symbol of an expected token. If EXPECTED-TOKEN is not a
   list, returns NIL."
  (when (listp expected-token)
    (first expected-token)))

(defmethod initialize-instance :after ((new-construction construction) &key)
  "Expand all (+ TOKEN) elements into TOKEN (* TOKEN) elements for simpler
   downstream processing.

   This is a pre-processing step before the actual parsing, so effiency here
   shouldn't be quite as much of a concern."
  (setf (slot-value new-construction 'elements) ; Replace original pattern
        (apply #'nconc ; with expanded pattern
               (loop for element in (get-construction-pattern new-construction)
                  if  (equal '+ (operator element)) ;if x+
                  collect (list (change-operator '= element) ; change to xx*
                                (change-operator '* element))
                  else collect (list element)))))

(defun get-length (range)
  "Get the length spanned by a RANGE"
  (if (null (start-of range))
      0 ; don't want to make it throw an error
      (- (end-of range) (start-of range))))

(defmethod initialize-instance :after ((new-range range) &key)
  "Check if end-of NEW-RANGE is after or same as start-of NEW-RANGE"
  (when (> 0 (get-length new-range))
    (error "Provided range MUST have non-negative length!")))

(defgeneric data-equalp (data1 data2)
  (:documentation
   "Compares the equality of two data structures using custom comparison
    logic"))

(defmethod data-equalp ((data1 structure-object) (data2 structure-object))
  "Check if two Scone concepts are equal. Just a simple wrapper call to 
   SIMPLE-IS-X-EQ-Y? for consistency."
  (simple-is-x-eq-y? data1 data2))

(defmethod data-equalp ((data1 range) (data2 range))
  "See if two ranges are equivalent.

   TODO: refactor and combine with the :around method if possible?"
  t)

(defmethod data-equalp :around ((data1 range) (data2 range))
  "See if two ranges are equivalent.

   This is an :around method so that it runs for all derived classes"
  (if (and (equal (start-of data1) (start-of data2))
           (equal (end-of data1) (end-of data2)))
      (call-next-method)))

(defmethod data-equalp ((data1 meaning) (data2 meaning))
  "Comparison function for two MEANING objects"
  (data-equalp (meaning-scone-element data1) (meaning-scone-element data2)))

(defmethod data-equalp ((data1 match) (data2 match))
  "See if two matches are equal"
  (and (eq (get-match-construction data1)
           (get-match-construction data2))
       (equal (match-progress data1) (match-progress data2))
       (equal (match-score data1) (match-score data2))
       (data-equalp (match-so-far data1) (match-so-far data2))))

(defmethod data-equalp ((data1 list) (data2 list))
  "See if two lists are equal using DATA-EQUALP for comparison of list elements

   Assumes that if the first argument is a dotted pair, the second list is one
   as well."
  (if (listp (cdr data1))
      (and (eq (length data1) (length data2))
           (loop for x in data1 for y in data2 ; check all elements the same
              always (data-equalp x y)))
      (and (data-equalp (car data1) (car data2)) ; check two elements the same
           (data-equalp (cdr data1) (cdr data2)))))

(defun hash-keys (hash-table)
  "Get hash-keys from a hash-table

   TODO: remove after installing Alexandria library"
  (loop for key being the hash-keys of hash-table collect key))

(defmethod data-equalp ((data1 hash-table) (data2 hash-table))
  "See if keys and values of two hash tables are equal

   TODO: replace with call to Alexandria library hash-table comparison function"
  (and (equalp (hash-keys data1) (hash-keys data2))
       (data-equalp (alexandria:hash-table-keys data1)
                    (alexandria:hash-table-keys data2))
       (loop for key being the hash-keys in data1
          always (data-equalp (gethash key data1) (gethash key data2)))))

(defmethod data-equalp ((data1 matched-construction)
                        (data2 matched-construction))
  "See if two matched-constructions are equal"
  (and (eq (get-construction data1) (get-construction data2))
       ;; don't care about actual Scone node because two newly created INDV's
       ;; are never gonna be the same
       (data-equalp (get-matched-construction-components data1)
                    (get-matched-construction-components data2))))

(defmethod data-equalp ((data1 t) (data2 t))
  "If DATA-EQUALP is not define for types of DATA1 and DATA2, simply use normal
   EQUALP function"
  ;; use UNWIND-PROTECT to catch errors when calling EQUALP on two different
  ;; datatypes that EQUALP hasn't been designed for
  (unwind-protect (equalp data1 data2) nil))

(defgeneric get-score (data)
  (:documentation
   "Returns the likelihood of this data being the intended
    meaning of the speaker"))

(defmethod get-score ((data meaning))
  (get-meaning-score data))

(defmethod get-score ((data matched-construction))
  (get-meaning-score data))

(defmethod get-score ((data match))
  (match-score data))

(defmethod get-score ((data string))
  1) ; just return 1 for all strings for now

(defmethod get-score ((data t))
  (if (null data)
      0
      (error (format nil "Unrecognized data ~S with no score" data))))

(defun define-range (start-pos length)
  "Utility function for quickly defining a new range object, using its start
   position and length."
  (make-instance 'range :start start-pos :end (+ start-pos length)))

(defun define-meaning (scone-element start end
                                     plural
                                     ends-in-comma
                                     ends-in-period
                                     ends-in-question-mark
                                     &optional (score 1))
  "Utility function for creating a MEANING object from a SCONE-ELEMENT spanning
   a certain RANGE in a sentence"
  (make-instance 'meaning :scone-element scone-element
                 :start start :end end
                 :plural plural
                 :ends-in-comma ends-in-comma
                 :ends-in-period ends-in-period
                 :ends-in-question-mark ends-in-question-mark
                 :score score))

(defmacro defconstruction (construction-name elements &rest payload)
  "Utility macro for defining new constructions and adding them to the global
   list of constructions"
  `(progn
     (defparameter ,construction-name
       (make-instance 'construction
                      :elements ',elements
                      :payload
                      (lambda (&key ,@(remove-duplicates
                                       (mapcar #'third elements)))
                        ,@payload)))
     (setf *constructions* (cons ,construction-name *constructions*))
     ,construction-name))

(defmacro defconstructions (names patterns &rest payload)
  "Define multiple constructions with the same payload but different
   names and patterns"
  `(progn
     ,@(mapcar
        (lambda (name pattern)
          `(defconstruction ,name
             ,pattern
             ,@payload))
        names patterns)))

(defun increment-range (range &optional (length 1))
  "Destructively increment the length of the range (by a default of 1)"
  (setf (end-of range) (+ (end-of range) length)))

(defun empty-range? (range)
  "Check if a range doesn't span anything (its start and end positions are the
   same)"
  (equal (start-of range) (end-of range)))

(defun subsume-range (range1 range2)
  "Subsume second range into first one (first one gets destructively modified).
   Returns first range after it's been destructively modified."
  (setf (start-of range1)
        (min (or (start-of range1) 99999999) (or (start-of range2) 99999999)))
  (setf (end-of range1)
        (max (or (end-of range1) -1) (or (end-of range2) -1)))
  range1)

(defgeneric increment-match-range (match matched-token)
  (:documentation
   "After matching a new token, this function destructively
    increments the range of a match object"))

(defmethod increment-match-range ((match match) (matched-token string))
  "Given a newly matched string token, destructively increases the range of the
   match by one (because a string only has a range of 1)"
  (increment-range match))

(defmethod increment-match-range ((match match) (matched-token meaning))
  "Given a newly matched MEANING, destructively increases the range of the match
   by the range of the MATCHED-TOKEN. Returns original MATCH after modification"
  (subsume-range match matched-token))

(defun matchp (potential-match)
  "Determines whether something is a match object or not"
  (typep potential-match 'match))

(defun matched-constructionp (potential-matched-construction)
  "Determines whether something is a matched-construction object or not"
  (typep potential-matched-construction 'matched-construction))

(defun meaningp (potential-meaning)
  "Determines whether something is a meaning object or not"
  (typep potential-meaning 'meaning))

(defun get-match-pattern (match)
  "Directly obtains the pattern list from a MATCH object

   NOTE: This returns a list, not a MATCH object!"
  (get-construction-pattern (get-match-construction match)))

(defun build-parse-tree (thing)
  "Build a parse tree from a matched-construction/string"
  (cond ((matched-constructionp thing)
         ;; if it's a matched-construction, build a parse tree by making each 
         ;; of the keys in the bindings a child subtree of this entire parse 
         ;; tree
         (cons
          (meaning-scone-element thing)
          (loop for key being the hash-keys in
                (get-matched-construction-components thing)
                using (hash-value value)
                collect (cons key (build-parse-tree value)))))
        ((matchp thing)
         (loop for key being the hash-keys in
              (match-so-far thing)
            using (hash-value value)
            collect (cons key (build-parse-tree value))))
        ;; otherwise if it's a list, return a list of parse trees for each item
        ((listp thing) (loop for item in thing collect (build-parse-tree item)))
        ;; finally if anything else, just returned the matched token itself
        (t thing)))

(defun get-range-string (range)
  "Return a string showing the range"
  (if (null (start-of range))
      "."
      (format nil "[~d -> ~d]" (start-of range) (end-of range))))

(defmethod print-object ((object construction) stream)
  "Show the pattern of a construction"
  (print-unreadable-object (object stream :type t)
    (format stream "<~S>" (get-construction-pattern object))))

(defmethod print-object ((object match) stream)
  "Show the current progress of a MATCH when printing it to a stream"
  (format stream "|~S ~a ~%            ~S SCORE=~,2f|"
          (insert-at #\. (get-match-pattern object)
                     (1+ (match-progress object)))
          (get-range-string object)
          (build-parse-tree object)
          (match-score object)))

(defmethod print-object ((object matched-construction) stream)
  "Show the parse tree of a MATCHED-CONSTRUCTION when printing it to a stream"
  (format stream "<~S=~S ~a SCORE=~,2f>"
          (meaning-scone-element object) (lispify object)
          (get-range-string object) (get-meaning-score object)))

(defmethod print-object ((object range) stream)
  "Show the start and end positions of a RANGE when printing it to a stream"
  (format stream (get-range-string object)))

(defmethod print-object ((object meaning) stream)
  "Shows which Scone node a MEANING is attached to when printing to a stream"
  (format stream "<~S ~a SCORE=~,2f>"
          (meaning-scone-element object) (get-range-string object)
          (get-meaning-score object)))

(defun copy-range (range)
  "Create a copy of a RANGE object."
  (unless (null range) ; if range is null then this will return NIL anyways
    (define-range (start-of range) (get-length range))))

(defun get-range-only (range)
  "Create a new base RANGE object with the same start and end positions"
  (make-instance 'range :start (start-of range) :end (end-of range)))

(defun copy-hash-table (old-table)
  "Create a new hash table with the same keys and values of OLD-TABLE.

   NOTE: Since in this program, hash tables pretty much only stores lists in
   the values, this function assumes that the values contained in OLD-TABLE are
   all lists."
  (let ((new-table (make-hash-table)))
    (loop for key being the hash-keys in old-table using (hash-value value)
       do (setf (gethash key new-table) (copy-list (gethash key old-table))))
    new-table))

(defun copy-match (match)
  "Create a copy of a MATCH object. Except for the matched-construction rule
   and pattern, no pointers are reused, so this copy can be safely modified 
   without affecting the original."
  (make-instance 'match
                 :construction (get-match-construction match)
                 :match-so-far (copy-hash-table (match-so-far match))
                 :match-progress (match-progress match) ; ints don't change
                 :start (start-of match)
                 :end (end-of match)
                 :score (match-score match)))

(defun overlaps? (range1 range2)
  "Checks whether two ranges overlap"
  ;; If both starts come before both ends, it's an overlap
  ;; range1-start guaranteed to come before range1-end, and same for range2
  ;; So we only need to check range2-start < range1-end and vice versa
  (and (< (start-of range1) (end-of range2))
       (< (start-of range2) (end-of range1))))

(defun right-after? (range1 range2)
  "See if RANGE2 is immediately after RANGE1"
  (or (null (start-of range1))
      (null (start-of range2))
      (= (end-of range1) (start-of range2))))

(defun adjacent? (range1 range2)
  "Check if two ranges are adjacent to each other"
  ;; They're adjacent if the start position of one is the same as the end
  ;; position of another
  (or (equal (start-of range1) (end-of range2))
      (equal (start-of range2) (end-of range1))))

(defun get-next-expected-token (match)
  "Obtains the next element to be matched in the pattern"
  (nth (match-progress match) (get-match-pattern match)))

(defun is-completed? (match)
  "Checks whether the pattern a MATCH object is trying to match, has been 
   successfully matched in its entirety."
  (equal (match-progress match) (length (get-match-pattern match))))

(defun is-almost-completed? (match)
  "Checks whether a match object is one element away from being matched"
  (equal (1+ (match-progress match)) (length (get-match-pattern match))))

(defun can-be-completed? (match)
  "See if a match object can be turned into a MATCHED-CONSTRUCTION

   If so, returns the version of the match with the progress fast-forwarded to
   the end"
  (or (and (is-completed? match) match) ; return completed match
      (let ((next-expected-token (get-next-expected-token match))
            (match-copy (copy-match match)))
        (and (can-be-skipped? next-expected-token)
             (progn
               (skip-element match-copy next-expected-token)
               (can-be-completed? match-copy))))))

(defun construct-let-value (key value)
  "Returns an expression that creates the bindings for one variable in a LET
   expression"
  ;; A LET expression is of the form (VAR VAL) or
  ;; (VAR (LIST-VAL1 LIST-VAL2 ...)) -- depending on whether the latter is a
  ;; list or not, the LET expression is a bit different
  `(,key ,(if (listp value) `(list ,@value) value)))

(defun component-contexts (match)
  "Get a list of contexts for the components of a match"
  (apply #'append
         (loop for v being the hash-values in (match-so-far match)
            collect (loop for c in v
                       if (matched-constructionp c) collect (context-of c)))))

(defun payload-argument-list (components)
  "Get the payload function arguments for components of a finished match"
  (apply #'append
         (loop for k being the hash-key using (hash-value v) of components
            collect (list (intern (string-upcase k) "KEYWORD") v))))

(defun run-payload (new-match)
  "If the matched-construction rule's payload is not NIL, run the payload
   against the matched elements.

   NOTE: Creates the new Scone node, if any, under an entirely new context
   NOTE: Does not return to the original context upon completion of this code

   The payload function should return a primary node representing the
   information in this new match."
  (let ((construction (get-match-construction new-match))
        (components (match-so-far new-match)))
    ;; execute payload only when payload function is specified
    (when (slot-boundp construction 'payload)
      ;; bind variables in matched-construction components to their values, and
      ;; then evaluate payload code
      (progn
        (let ((parent-contexts (cons *context* (component-contexts new-match)))
              (new-context (new-context nil parent-contexts)))
          (change-context new-context)
          (when *debug-payload*
            (print-debug "[PAYLOAD] Creating construction for match ~S"
                         new-match)
            (print-debug "[PAYLOAD] Creating new context ~S from contexts ~S"
                         new-context parent-contexts)))
        ;; let the calling function restore the previous context
        (when *debug-payload*
          (print-debug "[PAYLOAD] Calling payload with arguments ~S"
                       (payload-argument-list components)))
        (apply (get-construction-payload construction)
               (payload-argument-list components))))))

(defun increment-pattern-count (construction)
  "If a construction has been successfully matched, increment its count"
  (incf (pattern-count construction)))

(defun lossless-add-to-hash-table (key entry hash-table)
  "If ENTRY is non-NULL, adds ENTRY to the list of values associated with KEY
   in HASH-TABLE. Otherwise, simply makes sure that KEY exists in the table."
  ;; GETHASH returns NIl if the entry doesn't exist yet. Thus, if no entry
  ;; exists yet, this simply puts (ENTRY) as the value of KEY. If an entry
  ;; (A B ...) already exists, this appends ENTRY to the front, as such:
  ;; (ENTRY A B ...)
  ;;
  ;; todo: test that this doesn't remove existing information when ENTRY is null
  (cond (entry (setf (gethash key hash-table)
                     (cons entry (gethash key hash-table))))
        ((null (gethash key hash-table))
         (setf (gethash key hash-table) nil))))

(defun add-to-match-so-far (returned-match match-so-far)
  "If necessary, destructively add the next successful match to the hash table
   of bindings and matches. Return value is meaningless -- do not store it."
  (lossless-add-to-hash-table (car returned-match) (cdr returned-match)
                              match-so-far))

(defun get-components (component-name construction)
  "Get the values for that component in a construction"
  (gethash component-name (get-matched-construction-components construction)))

(defun components-list (construction)
  "Get an association list of the components inside a construction

   Used only for debugging"
  (loop for k being the hash-keys of
       (get-matched-construction-components construction)
       using (hash-value v)
       collect (cons k v)))

(defun get-first-component (component-name construction)
  "Extract component from hash map of construction"
  (first (get-components component-name construction)))

(defun get-mse (component-name construction)
  "Extract Scone element representing the first element in a certain
   component of a construction"
  (let ((first-component (get-first-component component-name construction)))
    (when first-component
      (meaning-scone-element first-component))))

(defun get-mses (component-name construction)
  "Extract Scone elements from a component of a construction"
  (mapcar #'meaning-scone-element
          (get-components component-name construction)))

(defun simple-get-meaning (component-name constructions-list)
  "Get the first meaning associated with a component from the first
   construction in a list of constructions (for situations where the
   list only consists of one element)"
  (let ((first-thing (first constructions-list)))
    (if (typep first-thing 'matched-construction)
        (get-mse component-name first-thing)
      (meaning-scone-element first-thing))))

(defun get-last-match-component (match)
  "Get the component of a match that is last in a sentence"
  (loop named outer
        for comp-list being the hash-values of (match-so-far match)
        do (loop for comp in comp-list
                 when (eq (end-of match) (end-of comp))
                 do (return-from outer comp))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                    ;;;
;;;     PATTERN-MATCHING FUNCTIONS     ;;;
;;;                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric matches? (actual-token expected-token)
  (:documentation
   "Sees if the ACTUAL-TOKEN satisfies the specification of the 
    EXPECTED-TOKEN in a PATTERN"))

(defmethod matches? ((actual-token meaning) (expected-token string))
  "Returns whether or not the string part of ACTUAL-TOKEN is an exact 
   string match of EXPECTED-TOKEN"
  (equalp (meaning-scone-element actual-token) expected-token))

(defmethod matches? ((actual-token structure-object)
                     (expected-token structure-object))
  "See if ACTUAL-TOKEN IS-A EXPECTED-TOKEN"
  (simple-is-x-a-y? actual-token expected-token))

(defmethod matches? ((actual-token matched-construction)
                     (expected-token structure-object))
  "See if the Scone node associated with ACTUAL-TOKEN IS-A EXPECTED-TOKEN"
  (let ((original-context *context*)
        (meaning (meaning-scone-element actual-token)))
    (change-context (context-of actual-token))
    (let ((result (matches? meaning expected-token)))
      (change-context original-context)
      result)))

(defmethod matches? ((actual-token matched-construction)
                     (expected-token symbol))
  "See if the MATCHED-CONSTRUCTION satisfies some particular condition"
  (cond ((eq expected-token :atom)
         ;; meaning should never be NIL
         (atom (meaning-scone-element actual-token)))
        ((eq expected-token :list)
         (listp (meaning-scone-element actual-token)))
        ((eq expected-token :plural)
         (pluralp actual-token))
        ((eq expected-token :ends-in-comma)
         (ends-in-commap actual-token))
        ((eq expected-token :ends-in-period)
         (ends-in-periodp actual-token))
        ((eq expected-token :ends-in-question-mark)
         (ends-in-question-markp actual-token))
        ((eq expected-token :structured)
         t)))

(defmethod matches? ((actual-token meaning) (expected-token structure-object))
  "See if the Scone node associated with ACTUAL-TOKEN IS-A EXPECTED-TOKEN"
  (matches? (meaning-scone-element actual-token) expected-token))

(defmethod matches? ((actual-token meaning) (expected-token symbol))
  "See if the meaning object matches some condition specified by
   EXPECTED-TOKEN"
  (cond ((eq expected-token :plural)
         (pluralp actual-token))
        ((eq expected-token :ends-in-comma)
         (ends-in-commap actual-token))
        ((eq expected-token :ends-in-period)
         (ends-in-periodp actual-token))
        ((eq expected-token :ends-in-question-mark)
         (ends-in-question-markp actual-token))
        ((eq expected-token :unstructured)
         t)))

(defmethod matches? ((actual-token t) (expected-token symbol))
  "See if ACTUAL-TOKEN matches some condition specified by
   EXPECTED-TOKEN (e.g. :unstructured)"
  (eq expected-token :unstructured)) ; only if it's unstructured would this be called

(defmethod matches? ((actual-token list) (expected-token symbol))
  "See whether everything in ACTUAL-TOKEN satisfies a certain attribute"
  (loop for token in actual-token
        always (matches? actual-token expected-token)))

(defmethod matches? ((actual-token list) (expected-token structure-object))
  "See whether everything in ACTUAL-TOKEN matches EXPECTED-TOKEN"
  (when (loop for token in actual-token
              always (matches? token expected-token))
    t))

(defmethod matches? ((actual-token t) (expected-token list))
  "EXPECTED-TOKEN must be either of the form (OPERATOR TOKEN) or (OPERATOR TOKEN
   BINDING-VARIABLE), as specified in the documentation for the MATCH class.
   This function returns a dotted pair (BINDING-VARIABLE . ACTUAL-TOKEN) if
   matched."
  (cond ((and (not (null expected-token))
              (listp (second expected-token))
              (loop for token in (second expected-token)
                    always (matches? actual-token token)))
         (cons (third expected-token) actual-token))
        ((and (not (null expected-token))
              (not (listp (second expected-token)))
              (matches? actual-token (second expected-token)))
         (cons (third expected-token) actual-token))
        ((and (not (null expected-token))
              (null (second expected-token)))
         (cons (third expected-token) actual-token))))

(defmethod matches? ((actual-token t) (expected-token t))
  "If unknown data-type, simply see if the two tokens are DATA-EQUALP."
  (data-equalp actual-token expected-token))

(defun increment-match-progress (match)
  "Increment the progress counter on a match"
  (setf (match-progress match) (1+ (match-progress match))))

(defun update-match-score (match new-score)
  "Round a match's score before saving it"
  (setf (match-score match) (round-decimal new-score 1000)))

(defun handle-matched-token (match next-expected-token match-result
                                   matched-token)
  "When a NEXT-TOKEN is successfully matched, call this function so that:
    - match range will be incremented
    - match progress will be increased if appropriate
    - if BINDING-VARIABLE is specified (see MATCH class documentation), then
      NEXT-TOKEN will be added to the list of bound variables

   This is not destructive. It will return a modified copy of the original
   MATCH

   NOTE: MATCHED-TOKEN is needed even if MATCH-RESULT is supplied, because
   MATCH-RESULT may simply be T and not contain the actual token"
  (let ((new-match (copy-match match)))
    ;; it should always increase the span that it covers in the sentence
    (increment-match-range new-match matched-token)
    ;; add new token to bindings
    (add-to-match-so-far match-result (match-so-far new-match))
    ;; now see if it's appropriate to increase the progress of this match
    ;; only situation in which we don't increase the progress is if it's a *
    ;; operator (i.e. you match as many times as needed)
    (unless (equal '* (operator next-expected-token))
      (increment-match-progress new-match))
    ;; take matched token score into account when updating new score
    (unless (stringp matched-token) ; no score for strings
      (update-match-score new-match
                          (* (get-score matched-token) (match-score new-match))))
    new-match)) ; return modified match

(defun skip-element (match next-expected-token)
  "Destructively skip the next part of the PATTERN in a MATCH by incrementing
   the MATCH-PROGRESS and ensuring that the variable binding (if any) of the
   NEXT-EXPECTED-TOKEN exists in the hash-table (so that later on the payload
   is able to access it and see that it's NIL)"
  ;; first, make sure element exists in bindings, because we want to bind NIL
  ;; to this variable when it comes time to run the payload code
  (when (and (listp next-expected-token)
             (not (null (third next-expected-token))))
    (unless (gethash (third next-expected-token) (match-so-far match))
      ;; penalize for skipping something that's never been matched
      (update-match-score match (* (match-score match) (- 1 *pattern-skip-penalty*))))
    (add-to-match-so-far (cons (third next-expected-token) nil)
                         (match-so-far match)))
  (increment-match-progress match)) ; skip this token

(defun can-be-skipped? (next-expected-token)
  "Returns whether or not the next expected token in the pattern can be skipped"
  ;; OPERATOR returns NIL when NEXT-EXPECTED-TOKEN is not a list, so no need to
  ;; check for that
  (member (operator next-expected-token) '(? *)))

(defun handle-unmatched-token (match next-token next-expected-token)
  "Handle a next-token that was unsuccessfully matched. If it has an appropriate
   operator (e.g. the token is an optional match), then it may return a new
   match object that skips over some elements.

   Not destructive. Returns a copy of the original match."
  (when (can-be-skipped? next-expected-token)
    (let ((new-match (copy-match match)))
      (skip-element new-match next-expected-token)
      ;; STYLE warning here because CONTINUE-MATCH won't be defined yet, but
      ;; that's unavoidable since these two functions recursively call each
      ;; other
      (continue-match new-match next-token))))

(defun continue-match (match next-token)
  "Continue a previous partial match. If the NEXT-TOKEN satisfies the next part 
   of the pattern, then a copy of the original MATCH will be returned with the
   following changes:
    - match range will be incremented
    - match progress will be increased if appropriate
    - if BINDING-VARIABLE is specified (see MATCH class documentation), then
      NEXT-TOKEN will be added to the list of bound variables
   Othewise, see if the next expected token is optional and if we can match the
   NEXT-TOKEN to something after skipping the optional expected token. If so,
   then the above steps apply. If not, then nothing is done."
  (let* ((next-expected-token (get-next-expected-token match))
         (match-result (matches? next-token next-expected-token)))
    (if match-result
        (handle-matched-token match next-expected-token match-result
                              next-token)
        (handle-unmatched-token match next-token next-expected-token))))

(defun make-matched-construction (new-match)
  "Returns a matched-construction from the completed match, and handles other
  side effects as well. Current, that means running the matched-construction
  rule payload, as well as incrementing the pattern count."
  ;; increment the number of times this particular pattern was matched
  (increment-pattern-count (get-match-construction new-match)) 
  ;; create a new matched-construction from this match
  (let ((matched-construction
         (make-instance 'matched-construction 
                        ;; same matched-construction rule
                        :rule (get-match-construction new-match) 
                        :start (start-of new-match)
                        :end (end-of new-match)
                        :components (match-so-far new-match)
                        :score (* (match-score new-match)
                                  (construction-score-multiplier
                                   (get-match-construction new-match)))
                        :ends-in-comma
                        (ends-in-commap
                         (get-last-match-component new-match))
                        :ends-in-period
                        (ends-in-periodp
                         (get-last-match-component new-match))
                        :ends-in-question-mark
                        (ends-in-question-markp
                         (get-last-match-component new-match))
                        :scone-element (run-payload new-match))))
    (setf (context-of matched-construction) *context*)
    (change-context *last-parse-context*)
    matched-construction))

(defun start-match-against-construction (construction)
  "Start an empty match against a single construction"
  (make-instance 'match :construction construction))

(defun start-match-against-constructions (constructions)
  "Create a list of new MATCH objects, one for each CONSTRUCTION in the list"
  (loop for construction in constructions
     collect (start-match-against-construction construction)))

(defun continue-matches (matches token)
  "Continue a list of unfinished matches with the next token, returning only
   successful matches."
  (remove-if #'null
             (loop for match in matches collect (continue-match match token))))

(defvar *strings-to-concepts-hashmap* (make-hash-table :test #'equal)
  "A hashmap with string tokens as keys and a list of associated Scone concepts
     as values")

(defun str-ends-in-charp (str char)
  "See if a string ends in a certain character"
  (eq char (aref str (1- (length str)))))

(defun str-ends-in-commap (str)
  "See if a string ends in a comma"
  (str-ends-in-charp str #\,))

(defun str-ends-in-periodp (str)
  "See if a string ends in a period"
  (str-ends-in-charp str #\.))

(defun str-ends-in-question-markp (str)
  "See if a string ends in a question mark"
  (str-ends-in-charp str #\?))

(defun remove-char (str char)
  "If STR has CHAR at the end, remove it"
  (if (str-ends-in-charp str char)
      (subseq str 0 (1- (length str)))
    str))

(defun remove-comma (str)
  "If STR has a comma at the end, remove it"
  (remove-char str #\,))

(defun remove-period (str)
  "If STR has a period at the end, remove it"
  (remove-char str #\.))

(defun remove-question-mark (str)
  "If STR has a question mark at the end, remove it"
  (remove-char str #\?))

(defun remove-punctuation (str)
  "Remove question marks and commas from end of STR"
  (remove-period (remove-question-mark (remove-comma str))))

(defun str-pluralp (str)
  "See if a string (possibly) represents a plural noun"
  (str-ends-in-charp (remove-punctuation str) #\s))

(defun get-string-concepts (str)
  "Get all Scone concepts associated with a particular string"
  (when str ; if STR is NIL, return NIL as well
    (gethash (string-upcase (remove-punctuation str))
             *strings-to-concepts-hashmap*)))

(defvar *constructions* nil
    "List of all defined constructions")

(defparameter *new-matches* (start-match-against-constructions *constructions*)
  "List of new matches started from all constructions")

(defparameter *last-parse-context* *context*
  "The context of the last successful parse")

(defparameter *question* nil
  "The last question asked by the user that was successfully parsed")

(defparameter *answer* nil
  "Answer to the *QUESTION* last asked, if any")

(defparameter *sentence-position* nil
  "Where we are right now in the global sentence stream")

(defun answer-question ()
  "Set the *ANSWER* global variable to the appropriate response to the
   question posed by *QUESTION*. *ANSWER* global variable may be set
   to NIL if no answer exists or is unknown."
  (let ((se (meaning-scone-element *question*)))
    (change-context *last-parse-context*)
    (cond ((simple-is-x-a-y? se {is-a query})
           (setf *answer*
                 (is-x-a-y? (get-mse 'first-thing  *question*)
                            (get-mse 'second-thing *question*)))))))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                        ;;;
;;;     SEMIRING LOGIC     ;;;
;;;                        ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload "cl-custom-hash-table")
(ql:quickload "split-sequence")

;;; currently a great deal of redundancy here

(defclass span ()
  ((range
    :documentation "The range in the sentence that this SPAN covers"
    :type range
    :initarg :range
    :initform (error "You MUST provide the range for this span")
    :reader get-span-range))
  (:documentation
   "Keeps track of information about the different things that
    span a sentence"))

(defclass meaning-span (span)
  ((meaning
    :documentation
    "The Scone element that represents a certain part of a sentence"
    :type meaning
    :initarg :meaning
    :initform (error "You MUST provide the meaning this is associated with!")
    :reader get-meaning-span-meaning))
  (:documentation
   "Keeps track of information about a construction/meaning/str that are
    essential to matching it with a right-hook"))

(defclass right-hook (span)
  ((pattern
    :documentation
    "The pattern the right-hook that contains the match is trying to complete"
    :type list
    :initarg :pattern
    :initform (error "You MUST provide the pattern that this right hook is
     trying to match")
    :reader get-right-hook-pattern))
  (:documentation
   "Keeps track of information about a match that is essential to finding a
    suitable next token for it"))

(defparameter *goal-span* nil
  "The SPAN object that determines which part of the sentence the GOAL should
  span")

(defun define-goal-span (range)
  "Given the range of a sentence, return a SPAN that will end up defining the
  *GOAL-SPAN*"
  (make-instance 'span :range range))


(defmethod print-object ((object span) stream)
  "Shows information about a generic SPAN (should never happen) when printing
   to a stream"
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (get-span-range object))))

(defmethod print-object ((object meaning-span) stream)
  "Shows information about a MEANING-SPAN when printing to a stream"
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~S"
            (get-span-range object) (get-meaning-span-meaning object))))

(defmethod print-object ((object right-hook) stream)
  "Shows information about a RIGHT-HOOK when printing to a stream"
  (print-unreadable-object (object stream :type t)
    (format stream "~S ~S"
            (get-span-range object)
            (get-right-hook-pattern object))))

(defun combine-spans (span1 span2)
  "Combine two ranges (assumes span1 begins before span2)"
  (if (typep span2 'right-hook)
      (make-instance 'right-hook
                     :range (subsume-range (copy-range (get-span-range span1))
                                           (get-span-range span2))
                     :pattern (get-right-hook-pattern span2))
      (combine-spans span2 span1)))

(defun get-type (element)
  "Return ELEMENT if it's a type, and the parent of ELEMENT otherwise"
  (cond ((listp element) (mapcar #'get-type element))
        ((type-node? element) element)
        (t (parent-element element))))

(defun semi-equal (span1 span2)
  "See if two spans are equivalent in the semiring"
  (and (data-equalp (get-span-range span1) (get-span-range span2))
       (eq (class-of span1) (class-of span2))
       (if (typep span1 'meaning-span)
           (let* ((span1-mse
                   (get-meaning-span-meaning span1))
                  (span2-mse
                   (get-meaning-span-meaning span2))
                  (span1-type
                   (if (stringp span1-mse)
                       span1-mse
                     (get-type span1-mse)))
                  (span2-type
                   (if (stringp span2-mse)
                       span2-mse
                     (get-type span2-mse))))
             (data-equalp span1-type span2-type))
           t)
       (if (typep span1 'right-hook)
           (eq (get-right-hook-pattern span1) (get-right-hook-pattern span2))
           t)))

(defun semi-hash (data)
  "Hash function that simply returns the sum of the range start and end in a
   semiring object that spans a certain range"
  ;; todo: make start and end different and add in type as well
  (let ((range (get-span-range data)))
    (if (null (start-of range))
        999999999
        (+ (start-of range) (* 1000 (end-of range))))))

(cl-custom-hash-table:define-custom-hash-table-constructor make-semiring-ht
    :test semi-equal :hash-function semi-hash)

(defparameter *agenda* (make-semiring-ht)
  "Create a hash-table for the agenda using SEMI-EQUAP as the comparison
   function and SEMI-HASH for the hash function")

(defparameter *chart* (make-semiring-ht)
  "Create a hash-table for the chart using SEMI-EQUAL as the comparison
   function and SEMI-HASH for the hash function")

(defun both-antecedents-satisfied? (item1 item2)
  "See if item1 in the agenda completes a rule with item2 in the chart"
  (and (typep item1 'meaning-span) (typep item2 'right-hook)
       (or (null (start-of (get-span-range item2)))
           (right-after? (get-span-range item2) (get-span-range item1)))))

(defparameter *semiring-zero* nil)
(defparameter *semiring-one* nil)

(defun seminull (value)
  "Check if a value is equal to semizing zero"
  ;; for now just check if it's null, since semiring zero is NIL
  (null value))

(defun semiplus (value1 value2)
  "Define how to combine chart elements together."
  (cond ((listp value1) ; then value2 should be a list of matches too
         (append value1 value2)) ; just add list of matches together
        (t ; otherwise they're meanings. pick the meaning w/ highest score
         (if (> (get-score value1) (get-score value2))
             value1
             value2))))

(defun semitimes (value1 value2)
  "Define how to create the value of a new chart element from the values of the
   two antecedent elements."
  (cond ((null value1) value2)
        ;; value1 should always be the value instead of the match
        ((not (listp value2)) (semitimes value2 value1))
        (t
         (let ((results
                (remove-if (lambda (match) ; keep only valid matches
                             (or (null match) (zerop (get-score match))))
                           (mapcar (lambda (match)
                                     (continue-match match value1))
                                   value2))))
           (if (null results)
               *semiring-zero* ; keep it general
               results)))))

(defun get-match-key (match)
  "Get the hash table key for a newly created match object"
  (make-instance 'right-hook
                 :range (get-range-only match)
                 :pattern (get-match-pattern match)))

(defun get-match-value (match)
  "Get the value of a match to put in the hash-table"
  (list match))

(defun get-meaning-key (meaning)
  "Get the hash table key for a newly created meaning object"
  (make-instance 'meaning-span
                 :range (get-range-only meaning)
                 :meaning (meaning-scone-element meaning)))

(defun get-meaning-value (meaning)
  "Get the hash table value of some object that is to be used as a meaning"
  meaning)

(defun get-ht-value (ht item)
  "Get the value of ITEM in the hash-table HT. Returns *SEMIRING-ZERO* if ITEM
   is not found in HT"
  (cl-custom-hash-table:with-custom-hash-table
    (alexandria:ensure-gethash item ht *semiring-zero*)))

(defun add-to-ht (ht new-item item-value)
  "Set the value in HT associated with NEW-ITEM to (semiplus ITEM-VALUE
  existing-value"
  (when (null item-value)
    ;; mostly for debugging
    (error "null item value"))
  (let* ((old-value (get-ht-value ht new-item))
         (result (semiplus old-value item-value))
         (ht-name (if (eq ht *chart*) "CHART" "AGENDA")))
    (if (data-equalp old-value result)
        (when *debug-add-failures*
          (print-debug "[~A] Failed to replace ~S~%       with ~S"
                       ht-name old-value item-value))
        (progn
          (when *debug-ht-update*
            (if (seminull old-value)
                (print-debug "[~A] Adding ~S" ht-name item-value)
                (print-debug "[~A] Replacing ~S~%       with ~S"
                             ht-name old-value item-value)))
          (cl-custom-hash-table:with-custom-hash-table
            (setf (gethash new-item ht) result))))))

(defun combine-antecedents (item1 item2)
  "Combine two antecedents and add consequent to agenda"
  (let ((new-span (combine-spans item1 item2))
        (result (semitimes (get-ht-value *chart* item1)
                           (get-ht-value *chart* item2))))
    (unless (seminull result)
      (add-to-ht *agenda* new-span result))))

(defun complete-and-add (match)
  "When possible, turn a match into a matched-construction, and add it to the
   agenda"
  (let ((completion (can-be-completed? match)))
    (when completion
      (handler-case
          (let ((new-cons (make-matched-construction match)))
            (add-to-ht *agenda* (get-meaning-key new-cons) new-cons))
        (simple-error (se)
          (when *debug-con-creation*
              (print-debug "~S" se)))))))

(defun single-antecedent-satisfied? (span)
  "Turn into a meaning span with a finished construction if possible (i.e. a
  consequent formed from only one antecedent."
  (cond ((typep span 'right-hook)
         (let* ((matches (get-ht-value *chart* span))
                (incomplete (remove-if #'is-completed? matches)))
           (mapcar #'complete-and-add matches)
           (when (and *debug-ht-update* (equal matches incomplete))
             (print-debug "[CHART] Removing complete ~S~%     incomplete: ~S"
                          matches incomplete))
           (cl-custom-hash-table:with-custom-hash-table
             (setf (gethash span *chart*)
                   incomplete))))
        ((and (typep span 'meaning-span)
              (data-equalp (get-span-range span) (get-span-range *goal-span*)))
         (progn
           (when *debug-goal-discovery*
             (print-debug "[GOAL] Found new valid parse ~S"
                          (get-ht-value *chart* span)))
           (add-to-ht *chart* *goal-span* (get-ht-value *chart* span))))))

(defun process-agenda-item (agenda-item)
  "Put agenda item into chart and see if it can produce any new consequents with
  existing items in the chart"
  (let ((old-val (get-ht-value *chart* agenda-item)))
    (add-to-ht *chart* agenda-item (get-ht-value *agenda* agenda-item))
    (cl-custom-hash-table:with-custom-hash-table
      (remhash agenda-item *agenda*))
    ;; for performance reasons that one function also handles putting things
    ;; into the chart
    ;; check now if antecedent satisfied, in case this doesn't beat out
    ;; the old score but will do so as a finished construction (of
    ;; course now the problem of this not beating out another item
    ;; further on in the match is a problem)
    (single-antecedent-satisfied? agenda-item)
    ;; have we found a better parse?
    (unless (data-equalp old-val (get-ht-value *chart* agenda-item))
        ;; yes we have, so now check with everything in the chart to see if
        ;; this can be combined with anything else to produce new parses
        (cl-custom-hash-table:with-custom-hash-table
          (loop for chart-item being the hash-keys in *chart*
             do
               (when (both-antecedents-satisfied? agenda-item chart-item)
                 (combine-antecedents agenda-item chart-item)))))))

(defun get-string-meanings (word position)
  "Retrieve a list of all meanings associated with a particular string"
  (loop for concept in (get-string-concepts word)
       collect (define-meaning concept position (1+ position)
                 (str-pluralp word)
                 (str-ends-in-commap word)
                 (str-ends-in-periodp word)
                 (str-ends-in-question-markp word)
                 (or (get-element-property concept :score) 1))))

(defun setup-new-parse ()
  "Reset everything for a fresh parse"
  (change-context {general})

  (setf *sentence-position* 0)
  (setf *goal-span* nil)
  (setf *agenda* (make-semiring-ht))
  (setf *chart* (make-semiring-ht))
  
  (setf *new-matches* (start-match-against-constructions *constructions*))
  
  (let ((*debug-ht-update* nil))
    (loop for new-match in *new-matches*
       do (add-to-ht *chart*
                     (get-match-key new-match)
                     (get-match-value new-match)))))

(defun add-word-meanings-to-agenda (new-word position)
  "Add all meanings of a given string (including the raw string itself) to the
   agenda"
  (loop for concept in
       (nconc (list (define-meaning (remove-punctuation new-word)
                      position
                      (1+ position)
                      (str-pluralp new-word)
                      (str-ends-in-commap new-word)
                      (str-ends-in-periodp new-word)
                      (str-ends-in-question-markp new-word)
                      (- 1 *string-as-concept-penalty*)))
              (get-string-meanings new-word position))
     do (add-to-ht *agenda*
                   (get-meaning-key concept)
                   (get-meaning-value concept))))

(defun parse-word (new-word position)
  "Given a new word at the next position in the sentence, keep on evaluating
   items in the agenda until the agenda is empty"
  (add-word-meanings-to-agenda new-word position)
  (loop while (> (hash-table-count *agenda*) 0)
     ;; may be doing repeated work here
     do (loop for key being the hash-keys of *agenda*
           do (process-agenda-item key))))

(defun semiring-parse (word-list start-position)
  "Run the semiring parsing algorithm on a sentence until the agenda is empty"
  ;; set what we want to get as a parse result
  (setf *goal-span*
        (define-goal-span
          (define-range start-position (length word-list))))
  ;; parse one word at a time
  (loop for word in word-list
        do (progn
             (parse-word word start-position)
             (setf start-position (1+ start-position))))
  ;; parse completed, now perform actions using parse result
  (let ((goal-value (get-ht-value *chart* *goal-span*)))
    (when goal-value
      (let ((se (meaning-scone-element goal-value)))
        (if (matches? se {query})
            (progn
              (setf *question* goal-value)
              (answer-question))
          (progn
            (setf *last-parse-context* (context-of goal-value))
            (setf *question* nil)
            (setf *answer* nil)))))
    (change-context *last-parse-context*)
    goal-value))

(defun get-goal-value ()
  "Get the result of the latest parse

   Used only for debugging"
  (get-ht-value *chart* *goal-span*))

(defun get-agenda-values ()
  "Get all values from the agenda

   Used only for debugging"
  (loop for v being the hash-values of *agenda* collect v))

(defun get-chart-values ()
  "Get all values from the chart

   Used only for debugging"
  (loop for v being the hash-values of *chart* collect v))

(defun cons-that-mean (meaning)
  "Get all matched constructions whose meanings are MEANING

   Used only for debugging"
  (remove-if-not
   (lambda (value)
     (and (matched-constructionp value)
          (simple-is-x-eq-y? (meaning-scone-element value) meaning)))
   (get-chart-values)))

(defun con-that-means (meaning)
  "Gets only the first matched construction whose meaning is MEANING

   Useful when there's only one such matched construction. Used only for
   debugging"
  (first (cons-that-mean meaning)))

;; allow for loading this file without warnings
(unless (fboundp 'lispify)
  (defun lispify (goal-value)
    "Turn a matched construction into a Lisp list"
    (list goal-value) ; just to get rid of warning
    (error "LISPIFY function must be redefined inside grammar file!")))

(defun nlu (sentence)
  "Create a Lisp representation of a natural language sentence"
  (when (zerop (hash-table-count *chart*))
    (setup-new-parse))
  (let* ((word-list (split-sequence:split-sequence #\Space sentence))
	 (parse-result (semiring-parse word-list *sentence-position*)))
    (incf *sentence-position* (length word-list))
    (lispify parse-result)))
