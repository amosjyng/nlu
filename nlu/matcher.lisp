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
;;; (sb-sprof:with-profiling (:show-progress t :loop nil :report :graph)
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

(defparameter *final-mc-modifier* 0.1
  "How much to increase successes/decrease failures of a component construction
   inside a final, finished parse by")

(defvar *stats-filename* ""
  "The filename for grammar statistics (e.g. how often each meaning is invoked,
   how often each construction is used, etc.).")

(defvar *learning* nil
  "Dynamically from failures and successes in matching constructions")

(defparameter *search-queue-size* 10000
  "How big of a search queue to keep")

;;; DEBUGGING PARAMETERS
(defvar *debug-isa-caching* nil
  "Print reads and updates to the *ISA-CACHE*")

(defvar *debug-con-creation* nil
  "Print errors from completed matches that can't become matched constructions")

(defvar *debug-payload* nil
  "Print relevant information about the payload while it is being created")

(defvar *debug-goal-discovery* nil
  "Print all newly discovered potential goal values")

(defvar *debug-nodes* nil
  "Print all current nodes for beam search")

(defvar *debug-verify* nil
  "Verify the correctness of a matched construction before continuing with the
   parse")

(defparameter *n-searched* 0
  "How many nodes we've searched through")

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

(defun get-type (element)
  "Return ELEMENT if it's a type, and the parent of ELEMENT otherwise"
  (cond ((listp element) (mapcar #'get-type element))
        ((type-node? element) element)
        (t (parent-element element))))

(defmacro print-debug (statement &rest args)
  "Prints debugging STATEMENT out to user"
  `(progn
     (format *error-output* ,statement ,@args)
     (format *error-output* "~%")))

(defun setup-new-parse ()
  "Reset everything for a fresh parse"
  (change-context {general})

  (setf *sentence-position* 0)
  
  (setf *isa-cache* (make-hash-table))
  (setf *isa-cache-writes* 0)
  (setf *isa-cache-reads* 0))

(defun reload-matcher ()
  "Relaod this file

   Used for development"
  (load "nlu/matcher")
  (setup-new-parse))

(defun debug-all ()
  "Set all debug global variables to true, and make element names human-readable

   Only used for debugging"
  (setf *generate-long-element-names* t)
  
  (setf *debug-isa-caching* t)
  (setf *debug-con-creation* t)
  (setf *debug-payload* t)
  (setf *debug-goal-discovery* t)
  (setf *debug-nodes* t))

(defun debug-none ()
  "Set all debug global variables to NIL, and make element names normal

   Only used for debugging"
  (setf *generate-long-element-names* nil)
  
  (setf *debug-isa-caching* nil)
  (setf *debug-con-creation* nil)
  (setf *debug-payload* nil)
  (setf *debug-goal-discovery* nil)
  (setf *debug-nodes* nil))

(defun average (list)
  "Get the average of a list of numbers"
  (/ (reduce #'+ list) (length list)))

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

(defun take (n list)
  "Take the first N elements of a list. Returns entire list if list is less than
   N elements long"
  (if (> n (length list))
      list
      (subseq list 0 n)))

(defun make-appendable (a)
  "If NIL, then return NIL. Otherwise return (a)"
  (if (null a)
      nil
      (list a)))

(defun replace-head (list new-head)
  "Replace head of a list with something else"
  (cons new-head (rest list)))

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
   (attributes
    :documentation
    "A list of various semantic (such as whether or not this meaning is plural)
     and syntactic (such as whether the string for this meaning ends with a
     period) attributes for this meaning"
    :type list
    :initarg :attributes
    :initform (error "You MUST provide attributes for this meaning, even if it's
                      just NIL!")
    :accessor meaning-attributes))
  (:documentation
   "Wrapper for a Scone element. Eventually this is intended to
    store extra information about this meaning, such as the language or part of
    speech."))

(defclass construction ()
  ((name
    :documentation
    "The symbolic Lisp variable name associated with this construction"
    :type string
    :initarg :name
    :initform (error "Every construction must have a name!")
    :reader c-name)
   (elements
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
   (success-count
    :documentation
    "The number of times this construction was correctly matched"
    :type integer
    :initform 0
    :accessor c-successes)
   (failure-count
    :documentation
    "The number of times this construction was incorrectly matched"
    :type integer
    :initform 0
    :accessor c-failures)
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
    :reader get-construction-payload))
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

(defun construction-score-multiplier (c)
  "Return the relative frequency of a construction"
  (let ((s (1+ (c-successes c)))
        (f (1+ (c-failures c))))
    (+ 0.5 (/ s (+ s f)))))

(defun load-stats ()
  (when (probe-file *stats-filename*)
    (load *stats-filename*)))

(defun save-stats ()
  (let ((stats (open *stats-filename*
                     :direction :output
                     :if-exists :supersede)))
    (mapcar (lambda (c)
              (format stats "~S~%"
                      `(setf (c-successes ,(c-name c)) ,(c-successes c)))
              (format stats "~S~%"
                      `(setf (c-failures ,(c-name c)) ,(c-failures c))))
            *constructions*)
    (close stats)))

(defvar *attribute-detection* nil
  "Assoc list of functions to check whether a string contains certain
   morphological attributes")

(defvar *attribute-removal* nil
  "Assoc list of functions to obtain the base form of a string without the
   particular morphological attribute")

(defvar *attribute-combination* nil
  "Assoc list of functions to return whether or not combination of elements with
   and without this attribute also results in a matched construction with this
   attribute")

(defmacro defattr (attr detection-func removal-func combination-func)
  "Macro that adds the detection and removal functions for an attribute to the
   corresponding assoc lists"
  `(progn
     (setf *attribute-detection*
           (acons ,attr ,detection-func *attribute-detection*))
     (setf *attribute-removal*
           (acons ,attr ,removal-func *attribute-removal*))
     (setf *attribute-combination*
           (acons ,attr ,combination-func *attribute-combination*))))

(defgeneric attr? (attr token)
  (:documentation "Check if a token contains a certain attribute"))

(defmethod attr? (attr (token string))
  "Check if a word contains a certain attribute"
  (funcall (cdr (assoc attr *attribute-detection*)) token))

(defmethod attr? (attr (token match))
  "Check if a match contains a certain attribute"
  (funcall (cdr (assoc attr *attribute-combination*)) token))

(defmethod attr? (attr (token meaning))
  "Check if a meaning contains a certain attribute"
  (member attr (meaning-attributes token)))

(defun remove-attr (attr word)
  "Remove an attribute from a word"
  (funcall (cdr (assoc attr *attribute-removal*)) word))

(defmacro if-attr (attr true-statement false-statement)
  "If WORD has ATTR, redefine ATTRS to include ATTR and redefine WORD to have
   ATTR removed, and execute TRUE-STATEMENT. Otherwise leave ATTRS and WORD
   as-is and execute FALSE-STATEMENT"
  `(if (attr? ,attr word)
       (let ((word (remove-attr ,attr word))
             (attrs (cons ,attr attrs)))
         ,true-statement)
       ,false-statement))

(defmacro when-attr (attr &body body)
  "If WORD has ATTR, refedine ATTRS to include ATTR and redefine WORD to have
   ATTR removed, and execute BODY. Otherwise do nothing."
  `(if-attr ,attr
            ,(cons 'progn body)
            nil))

(defun collect-attrs (attrs remaining-attrs word)
  "Returns the word after adding all attrs in REMAINING-ATTRS that the word
   contains"
  (if (null remaining-attrs)
      (list attrs word)
      (let ((next-attr (first remaining-attrs)))
        (if-attr next-attr
                 ;; ATTRS and WORD will be redefined, so don't worry
                 (collect-attrs attrs (rest remaining-attrs) word)
                 (collect-attrs attrs (rest remaining-attrs) word)))))

(defun collect-match-attrs (word)
  "See which attributes a match contains"
  (loop for attr-pair in *attribute-combination*
     if (attr? (first attr-pair) word)
     collect (first attr-pair)))

(defmacro with-attrs (new-attrs &body body)
  "Macro for adding a new list of attributes (if they exist) to the ATTRS list
   and removing said attributes from the word"
  `(let* ((attr-results (collect-attrs attrs ',new-attrs word))
          (attrs (first attr-results))
          (word (second attr-results)))
     ,@body))

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
  (and (data-equalp (meaning-scone-element data1) (meaning-scone-element data2))
       (equal (meaning-attributes data1) (meaning-attributes data2))))

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

(defmethod get-score ((data construction))
  (construction-score-multiplier data))

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

(defun define-meaning (scone-element start end attributes score)
  "Utility function for creating a MEANING object from a SCONE-ELEMENT spanning
   a certain RANGE in a sentence"
  (make-instance 'meaning :scone-element scone-element
                 :start start :end end
                 :attributes attributes :score score))

(defmacro defconstruction (construction-name elements &rest payload)
  "Utility macro for defining new constructions and adding them to the global
   list of constructions"
  `(progn
     (defparameter ,construction-name
       (make-instance 'construction
                      :name ',construction-name
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
  (format stream "<~S=~S ~a~%            ~S SCORE=~,2f>"
          (meaning-scone-element object) (lispify object)
          (get-range-string object) (meaning-attributes object)
          (get-meaning-score object)))

(defmethod print-object ((object range) stream)
  "Show the start and end positions of a RANGE when printing it to a stream"
  (format stream (get-range-string object)))

(defmethod print-object ((object meaning) stream)
  "Shows which Scone node a MEANING is attached to when printing to a stream"
  (format stream "<~S ~a ~S SCORE=~,2f>"
          (meaning-scone-element object) (get-range-string object)
          (meaning-attributes object) (get-meaning-score object)))

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

(defun can-be-continued? (match)
  "Checks if a pattern can be continued (even if it's finished, it may be
   continued if the last token to be matched can be matched any number of
   times)"
  ;; only way it can be continued is if we're at the end and the end is not an
  ;; operator that can allow any number of tokens to be matched
  (let ((last-token (first (last (get-match-pattern match)))))
    (not (and (is-completed? match) (not (can-be-skipped? last-token))))))

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
        (let* ((parent-contexts (cons *context* (component-contexts new-match)))
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

(defun get-first-match-component (component-name match)
  "Extract first component from list of components in match"
  (first (gethash component-name (match-so-far match))))

(defun get-mse (component-name construction)
  "Extract Scone element representing the first element in a certain
   component of a matched construction"
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

(defun get-attr (attr meaning)
  "Get an attribute (if it exists) from the attributes of a meaning"
  (member attr (meaning-attributes meaning)))

(defun set-attr (attr meaning new-val)
  "set an attribute to a certain value for a meaning"
  (when new-val ; no need to remove attributes for now
    (setf (meaning-attributes meaning)
          (cons attr (meaning-attributes meaning)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                    ;;;
;;;     PATTERN-MATCHING FUNCTIONS     ;;;
;;;                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defun get-hash-str (e1 e2 c)
  "Return a hashable string for an IS-A query in a certain context"
  (format nil "~A{~A}~A|" (iname e1) (iname e2) (iname c)))

(defparameter *isa-cache* (make-hash-table)
  "Because changing contexts is expensive, cache repeated IS-A queries in the
   same context")

(defparameter *isa-cache-writes* 0)
(defparameter *isa-cache-reads* 0)

(defun isa-cache-stats ()
  "Print how often the *ISA-CACHE* is actually used, to give a better idea of
   whether a change to caching helps or not"
  (format t "~A writes to cache, ~A reads from cache"
          *isa-cache-writes* *isa-cache-reads*))

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
  (let* ((original-context *context*)
         (meaning (meaning-scone-element actual-token))
         (str
          (if (listp meaning)
              (apply #'concatenate 'string
                     (mapcar (lambda (mse)
                               (get-hash-str mse expected-token
                                             (context-of actual-token)))
                             meaning))
              (get-hash-str meaning expected-token (context-of actual-token))))
         (key (sxhash str)))
    (multiple-value-bind (result present) (gethash key *isa-cache*)
      (if present
          (progn
            (when *debug-isa-caching*
              (incf *isa-cache-reads*)
              (print-debug "Using cached result for ~S" str))
            result) ; return result without doing expensive context change
          (progn
            (change-context (context-of actual-token))
            (let ((result (matches? meaning expected-token)))
              (change-context original-context)
              (when *debug-isa-caching*
                (incf *isa-cache-writes*)
                (print-debug "Adding cached result for ~S to be ~S" str result))
              (setf (gethash key *isa-cache*) result)
              result))))))

(defmethod matches? ((actual-token matched-construction)
                     (expected-token symbol))
  "See if the MATCHED-CONSTRUCTION satisfies some particular condition"
  (cond ((eq expected-token :atom)
         ;; meaning should never be NIL
         (atom (meaning-scone-element actual-token)))
        ((eq expected-token :list)
         (listp (meaning-scone-element actual-token)))
        ((eq expected-token :structured)
         t)
        ;; e.g. EXPECTED-TOKEN is :PLURAL and ACTUAL-TOKEN represents a plural
        ;; entity
        (t (get-attr expected-token actual-token))))

(defmethod matches? ((actual-token meaning) (expected-token structure-object))
  "See if the Scone node associated with ACTUAL-TOKEN IS-A EXPECTED-TOKEN"
  (matches? (meaning-scone-element actual-token) expected-token))

(defmethod matches? ((actual-token meaning) (expected-token symbol))
  "See if the meaning object matches some condition specified by
   EXPECTED-TOKEN"
  (cond ((eq expected-token :unstructured)
         t)
        ;; e.g. EXPECTED-TOKEN is :PLURAL and ACTUAL-TOKEN represents a plural
        ;; entity
        (t (get-attr expected-token actual-token))))

(defmethod matches? ((actual-token list) (expected-token symbol))
  "See whether everything in ACTUAL-TOKEN satisfies a certain attribute"
  (loop for token in actual-token
        always (matches? token expected-token)))

(defmethod matches? ((actual-token t) (expected-token symbol))
  "See if ACTUAL-TOKEN matches some condition specified by
   EXPECTED-TOKEN (e.g. :unstructured)"
  (cond ((eq expected-token :unstructured)
         t)
        ((eq expected-token :string)
         (stringp actual-token))
        ((eq expected-token :meaning)
         (meaningp actual-token))))

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

(defun skippable? (op)
  "See if an operator is a skippable one"
  (member op '(? *)))

(defun can-be-skipped? (next-expected-token)
  "Returns whether or not the next expected token in the pattern can be skipped"
  ;; OPERATOR returns NIL when NEXT-EXPECTED-TOKEN is not a list, so no need to
  ;; check for that
  (skippable? (operator next-expected-token)))

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
   Otherwise, see if the next expected token is optional and if we can match the
   NEXT-TOKEN to something after skipping the optional expected token. If so,
   then the above steps apply. If not, then nothing is done.

   If there is nothing more to match, throws an error."
  (if (can-be-continued? match)
    (let* ((next-expected-token (get-next-expected-token match))
           (match-result (matches? next-token next-expected-token)))
      (if match-result
          (handle-matched-token match next-expected-token match-result
                                next-token)
          (handle-unmatched-token match next-token next-expected-token)))
    (error (format nil "Match ~S is already finished" match))))

(defun verify (mc)
  "Ask the user if the matched construction is the correct one"
  (if (and mc *debug-verify*)
      (progn
        (format t "Is this construction correct? ~S" mc)
        (y-or-n-p))
      t))

(defun update-c-stats (c outcome)
  "Update the stats of a construction given the ultimate outcome of a match"
  (when *learning*
    (if outcome
      (incf (c-successes c))
      (incf (c-failures c)))))

(defun learn-from-finished (mc)
  "Give more weight to things inside a finished construction"
  (when (and *learning* (matched-constructionp mc))
    (let ((c (get-construction mc)))
      (setf (c-successes c) (* (c-successes c) (+ 1 *final-mc-modifier*)))
      (setf (c-failures c)  (* (c-failures c)  (- 1 *final-mc-modifier*))))
    (loop for mc-list being the hash-values in
         (get-matched-construction-components mc)
       do (mapcar #'learn-from-finished mc-list))))

(defun make-matched-construction (new-match)
  "Returns a matched-construction from the completed match, and handles other
  side effects as well. Current, that means running the matched-construction
  rule payload, as well as incrementing the pattern count."
  ;; check that match is actually completed
  (unless (can-be-completed? new-match)
    (error (format nil "Trying to create construction from incomplete match ~S!"
                   new-match)))
  ;; create a new matched-construction from this match
  (let ((mc
         (handler-case
             (make-instance 'matched-construction 
                            ;; same matched-construction rule
                            :rule (get-match-construction new-match) 
                            :start (start-of new-match)
                            :end (end-of new-match)
                            :components (match-so-far new-match)
                            :attributes (collect-match-attrs new-match)
                            :score (match-score new-match)
                            :scone-element (run-payload new-match))
             (simple-error (se)
               (when *debug-con-creation*
                 (print-debug "~S" se)
                 nil)))))
    (when mc
      (setf (context-of mc) *context*))
    (change-context *last-parse-context*)
    (let ((outcome (verify mc)))
      (update-c-stats (get-match-construction new-match) outcome)
      mc)))

(defun start-match-against-construction (c)
  "Start an empty match against a single construction"
  (make-instance 'match
                 :construction c
                 :score (get-score c)))

(defun start-match-against-constructions (constructions)
  "Create a list of new MATCH objects, one for each CONSTRUCTION in the list"
  (loop for construction in constructions
     collect (start-match-against-construction construction)))

(defun continue-matches (matches token)
  "Continue a list of unfinished matches with the next token, returning only
   successful matches."
  (remove-if #'null
             (loop for match in matches collect (continue-match match token))))

(defun cross (matches meanings)
  "Form every possible continuation of a match by every single meaning"
  (let* ((crossed (mapcar (lambda (meaning) (continue-matches matches meaning))
                          meanings))
         (non-null (remove-if #'null (apply #'append crossed))))
    non-null))

(defun start-new-matches-with (token)
  "Start a completely new match with the given token"
  (continue-matches (new-matches) token))

(defun continue-tokens (match tokens)
  "Continue a single match with a list of tokens to continue with"
  (remove-if #'null
             (loop for token in tokens collect (continue-match match token))))

(defun get-string-concepts (str &optional (syntax-tag :other))
  "Get all Scone concepts associated with a particular string"
  (when str ; if STR is NIL, return NIL as well
    (mapcar #'first
            (lookup-definitions str (list syntax-tag)))))

(defun exact-meanings (word position attributes &optional (syntax-tag :other))
  "Retrieve a list of all meanings associated with this exact string. The same
   set of attributes (e.g. plural, or ends in a period) will be assigned to each
   meaning in this list"
  ;; get the exact string itself as a "meaning"
  (nconc (list (define-meaning word position (1+ position)
                               attributes (- 1 *string-as-concept-penalty*)))
         ;; as well as each concept of that syntax tag
         (loop for concept in (get-string-concepts word syntax-tag)
            collect (funcall #'define-meaning concept position (1+ position)
                             attributes
                             (or (get-element-property concept :score) 1)))))

(defvar *constructions* nil
    "List of all defined constructions")

(defun new-matches ()
    "List of new matches started from all constructions"
    (start-match-against-constructions *constructions*))

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

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                    ;;;
;;;     GRAMMAR-SPECIFIC FUNCTIONS     ;;;
;;;                                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(unless (fboundp 'get-string-meanings)
  (defun get-string-meanings (word position)
    "Retrieve a list of all meanings associated with all morphological forms of
     a given string (i.e. given the string 'deer' it will return both the
     singular and plural meanings of 'deer')

     Due to morphology being grammar-dependent, force user to redefine this
     inside the grammar file"
    (error "Must be redefined inside grammar file!")))

(unless (fboundp 'lispify)
  (defun lispify (goal-value)
    "Turn a matched construction into a Lisp list"
    (list goal-value) ; just to get rid of warning
    (error "LISPIFY function must be redefined inside grammar file!")))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           ;;;
;;;     BEAM SEARCH LOGIC     ;;;
;;;                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(ql:quickload "split-sequence")

(defparameter *branches-count* nil
  "Keep track of how many branches there are")

(defclass node ()
  ((stack
    :documentation "A particular interpretation of the sentence"
    :type list
    :initarg :stack
    :initform nil
    :accessor node-stack)
   (level
    :documentation "How deep down we are"
    :type integer
    :initarg :level
    :initform 0
    :accessor node-level))
  (:documentation
   "A node in the search tree for the correct parse"))

(defmethod print-object ((object node) stream)
  "Show the pattern of a construction"
  (print-unreadable-object (object stream :type t)
    (format stream "~S" (node-stack object))))

(defun make-node-from (m)
  "Create a new Node from either a match or a matched construction"
  (make-instance 'node :stack (list m) :level 0))

(defun make-child (node new-stack)
  "Create a new child Node with a custom stack"
  (make-instance 'node :stack new-stack :level (1+ (node-level node))))

;;; A state is defined as a list of matches-in-progress, or a single finished
;;; construction (a potential final state)

(defun current-top (node)
  "Given a node, returns the current top of that node"
  (first (node-stack node)))

(defun rest-stack (node)
  "Get the rest of the stack of a node"
  (rest (node-stack node)))

(defun stack-length (node)
  "Get the length of a node's stack"
  (length (node-stack node)))

(defun replace-current-top (node replacement)
  "Create new node with new top"
  (make-child node (replace-head (node-stack node) replacement)))

(defun add-new-tops (node new-matches)
  "Put new matches on top of the current node"
  (mapcar (lambda (new-match)
            (make-child node (cons new-match (node-stack node))))
          new-matches))

(defun collapse (node)
  "Collapse one layer of the node by feeding this mc to the match in the layer
   before it"
  (when (> 2 (stack-length node))
    (error (format nil "Nowhere to collapse to: ~S!" (stack-length node))))
  (let* ((rest-stack (rest-stack node))
         (result (continue-match (first rest-stack) (current-top node))))
    (when result
      (make-child node (replace-head rest-stack result)))))

(defun continue-from (node continued-matches)
  "Given the current node and a list of matches from which the last match would
   be continued from, returns a list of all nodes from here"
  (mapcar (lambda (match) (replace-current-top node match))
          continued-matches))

(defun expand-as-mc (node)
  "If possible, expand a node by finishing the current match"
  (let* ((m (current-top node))
         (mc (and (can-be-completed? m) (make-matched-construction m))))
    (when mc
      (replace-current-top node mc))))

(defun expand-tokens (node adj-meanings)
  "Expand the specified node by continuing the previous match with the adjacent
   meanings"
  (let ((m (current-top node)))
    (when (can-be-continued? m)
      (continue-from node (continue-tokens m adj-meanings)))))

(defun is-match-node? (node)
  "Check to see if a node is a match node"
  (matchp (current-top node)))

(defun is-mc-node? (node)
  "Check to see if a node is a matched construction node"
  (matched-constructionp (current-top node)))

(defun is-final-node? (node goal-start goal-end)
  "Checks to see if a node is a final one"
  (and (eq (stack-length node) 1) (is-mc-node? node)
       (let ((m (current-top node)))
         (and (eq (start-of m) goal-start)
              (eq (end-of m) goal-end)))))

(defun from-mc (node)
  "Given an mc node, find all the matches that can be continued with the top"
  (continue-from node (start-new-matches-with (current-top node))))

(defun expand-from-match (node adj-meanings)
  "Given a match node, find all ways of expanding it"
  (append (make-appendable (expand-as-mc node))
          (expand-tokens node adj-meanings)
          (when (can-be-continued? (current-top node))
            (add-new-tops node (cross (new-matches) adj-meanings)))))

(defun expand-from-mc (node)
  "Given an mc node, find all ways of expanding it"
  (append (when (> (stack-length node) 1)
            (make-appendable (collapse node)))
          (from-mc node)))

(defun branches-of (node adj-meanings)
  "Returns a list of states to go from here"
  (cond ((is-match-node? node) (expand-from-match node adj-meanings))
        ((is-mc-node? node) (expand-from-mc node))
        (t (error "Unknown node type"))))

(defun get-adj-meanings (node meanings-list)
  "Get all meanings that begin at the same point the current node ends"
  (nth (- (end-of (current-top node)) *sentence-position*) meanings-list))

(defun node-score (node)
  "Get the score of a node"
  (get-score (current-top node)))

(defun compare-nodes (n1 n2)
  "See which node should go first in a list"
  (> (node-score n1) (node-score n2)))

(defun sort-nodes (nodes)
  "Sort nodes by their confidence scores"
  (sort nodes #'compare-nodes))

(defun add-to-fringe (fringe new-nodes)
  "Add some new nodes to the sorted fringe. Returns new sorted fringe"
  (sort-nodes (append new-nodes (rest fringe))))

(defun beam-search (fringe meanings-list)
  "Given a list of meanings, tries to find a structured representation of
   the entire list"
  (incf *n-searched*)
  (when *debug-nodes*
    (print-debug "~%~%~%Fringe is:")
    (mapcar (lambda (node) (print-debug "==========~%~S" node))
            fringe))
  (unless (null fringe)
    (let* ((best-node (first fringe))
           (neighbors (branches-of best-node
                                   (get-adj-meanings best-node meanings-list)))
           (new-fringe
            (add-to-fringe fringe neighbors))
           (new-best (first new-fringe)))
      (unless (zerop (length neighbors))
        (setf *branches-count* (cons (length neighbors) *branches-count*)))
      (when *debug-nodes*
        (print-debug "~~~~~~~~~~~%~~~~~~~~~~~%New neighbors are:")
        (mapcar (lambda (node) (print-debug "----------~%~S" node))
                neighbors))
      (if (or (null new-best)
              (is-final-node? new-best
                              (start-of (first (first meanings-list)))
                              (end-of (first (first (last meanings-list))))))
          (progn
            (print-debug "Searched through ~S nodes. Solution is ~S levels deep. Average branching factor of ~2$"
                         *n-searched*
                         (unless (null new-best) (node-level new-best))
                         (if (null *branches-count*) 'UNDEFINED
                             (average *branches-count*)))
            (when new-best (current-top new-best)))
          (beam-search (take *search-queue-size* new-fringe)
                       meanings-list)))))

(defun get-initial-states (meanings)
  "Get the initial set of states to do beam search with. Each node is a state"
  (sort-nodes (mapcar #'make-node-from (cross (new-matches) meanings))))

(defun get-meanings-list (words)
  "From a list of string words, get a list of possible meanings for each word"
  (let ((word-pos *sentence-position*))
    (mapcar (lambda (word)
              (incf word-pos)
              (get-string-meanings word (1- word-pos)))
            words)))

(defun setup-next-parse (words)
  "Given previous parse, ready globals for next parse"
  (setf *n-searched* 0)
  (setf *branches-count* nil)
  (incf *sentence-position* (length words)))

(defun print-parse-result (parse-result)
  "Display the parse result in a natural way to the human user"
  (if parse-result
      (progn
        (learn-from-finished parse-result)
        (lispify parse-result))
      (format t "Unable to understand.")))

(defun nlu (sentence)
  "Create a Lisp representation of a natural language sentence"
  (when (zerop *sentence-position*)
    (setup-new-parse))
  (let* ((words (split-sequence:split-sequence #\Space sentence))
         (meanings-list (get-meanings-list words))
	 (parse-result
          (progn
            (when *debug-nodes*
              (print-debug "MEANINGS-LIST: ~S" meanings-list))
            (beam-search (get-initial-states (first meanings-list))
                         meanings-list))))
    (setup-next-parse words)
    (print-parse-result parse-result)))
