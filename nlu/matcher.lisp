;;;
;;;
;;;          RAW TEXT
;;;             |
;;;             |
;;;             V
;;;     ,---------------.
;;;     |               |
;;;     | PRE-PROCESSOR |
;;;     |               |
;;;     '---------------'
;;;             |
;;;             |
;;;             V
;;;          N-GRAMS
;;;             |
;;;             |
;;;             V
;;;     ,---------------.
;;;     |               |
;;;     | MORPHOLOGICAL |<--- GRAMMAR MORPHOLOGY
;;;     |    ENGINE     |
;;;     |               |
;;;     '---------------'
;;;             |
;;;             |
;;;             V
;;;         ROOT WORD
;;;             +
;;;       MORPHOLOGICAL
;;;           INFO
;;;             |
;;;             |
;;;             V
;;;        ,---------.    --- SCONE KB ELEMENTS (DICTIONARY)
;;;        |         |    |
;;;        | MEANING | <--+
;;;        | CREATOR |    |
;;;        |         |    --- P(MEANING | N-GRAM) STATS
;;;        '---------'
;;;             |
;;;             |
;;;             V
;;;          MEANINGS
;;;       /~~~~~~~~~~~\
;;;      (SCONE ELEMENT)
;;;       \~~~~~~~~~~~/
;;;             |
;;;       --->--+
;;;      /      |
;;;     |       === PARSING ALGORITHM
;;;     |       |
;;;     ^       V
;;;     |  ,---------.    --- GRAMMAR CONSTRUCTION PATTERNS
;;;     |  |         |    |
;;;     |  | MATCHER | <--+-- P(CONSTRUCTION) STATS
;;;     |  |         |    |
;;;     |  '---------'    --- P(CONSTRUCTION | MEANING HISTORY) STATS
;;;     |       |
;;;     |       |
;;;     |       V
;;;     |   (PARTIAL)
;;;     ^    MATCHES
;;;    / \      |
;;;   /   \--<--+
;;;  /          |
;;; |           V
;;; |  ,------------------.    --- GRAMMER CONSTRUCTION PAYLOADS
;;; |  |                  |    |
;;; |  | PAYLOAD EXECUTOR | <--+
;;; |  |                  |    |
;;; |  '------------------'    --- SCONE KB
;;; ^           |                    ^
;;; |           +--------------------|
;;; |           |
;;; |           V
;;; |        MATCHED
;;; |     CONSTRUCTIONS
;;; |           |
;;; \-----<-----/
;;;
;;;
;;; A brief explanation of the diagram
;;; ----------------------------------
;;;
;;; Raw text goes to the pre-processor, which segments it into n-grams.
;;;
;;; N-grams go to the morphological engine, which uses the language grammar to
;;; find all plausible derivations of root forms into the current word. These
;;; root forms, along with the extra morphological information contained in the
;;; original word, are output.
;;; 
;;; The root forms are fed to finds the meaning creator, which uses a dictionary
;;; of some sort (in our case, the Scone KB's English tags serve as the source
;;; for the dictionary) to find corresponding Scone elements and produce meaning
;;; wrappers around each element. The meaning wrapper contains extra
;;; information, such as the sentence location, morphological information from
;;; the previous step, likelihood of this being the meaning referred to by the
;;; string, etc.
;;;
;;; Meanings go to the matcher, which tries to match them to the grammar's
;;; construction patterns. Additional statistical information is embedded into
;;; each partial match to denote the current likelihood of this match.
;;;
;;; Partial matches go back to the matcher so that future meanings can be
;;; matched against the rest of the pattern.
;;;
;;; When a pattern is fully matched, or a partial match can be made into a
;;; matched construction, the payload executor runs the payload (defined
;;; individually for each construction) on the matched meanings. Execution of
;;; the payload can involve querying and/or updating the Scone KB. A new Scone
;;; element is output from the payload.
;;;
;;; The output Scone element is added to the list of meanings waiting to be fed
;;; into the matcher. In this way, it is possible to nest constructions within
;;; each other.
;;;
;;; The parsing algorithm controls which meanings get fed to which partial
;;; matches in oder to produce the correct final interpretation of the sentence.
;;; A good algorithm should know which meanings generall pair with which
;;; constructions, so as to explore fewer futile searches.


;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                      ;;;
;;;     DEPENDENCIES     ;;;
;;;                      ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;


;; For splitting strings by delimiters
(ql:quickload "split-sequence")


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    ;;;
;;;     PARAMETERS     ;;;
;;;                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


(defvar *constructions* nil
  "List of currently-known constructions.")

(defvar *stats-filename* nil
  "File to save and load statistics to/from.")

(defvar *beam-search-width* 5
  "How many values to keep in a beam search at once.")

(defvar *default-interpretation-confidence* 1
  "Default confidence value for new interpretations.")

(defvar *default-stats-count* 1
  "Default count for non-existent stats")

(defvar *elements-given-text* (make-hash-table :test 'equal)
  "Statistics for how often each n-gram is used to refer to each Scone
   element.")

(defvar *constructions-given-elements* (make-hash-table :test 'equal)
  "Statistics for how often each Scone element is used in each construction.")


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                   ;;;
;;;     GENERAL UTILITY FUNCTIONS     ;;;
;;;                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defmethod print-object :around ((object element) stream)
  "Allow a Scone element to be printed readably"
  (declare (stream stream))
  (if *print-readably*
      (format stream "#.~A" `(lookup-element ,(element-name object)))
      (call-next-method object stream)))

(defun type-node (element)
  "Get either the ELEMENT itself if it's a type, or its parent if it's an
   INDV-NODE."
  (declare (element element))
  (cond ((type-node? element) element)
        (t (parent-element element))))

(defun load-language (language)
  "Load the dictionary, morphology, and constructions of a specified language."
  (declare (string language))
  (load (format nil "nlu/languages/~A.lisp" language)))

(defun replace-head (new-head list)
  "Replace head of a list with something else"
  (declare (list list))
  (cons new-head (rest list)))

(defun sethash (key value hash-table)
  "A convenience function for overwriting the value of a key in a hash-table."
  (declare (hash-table hash-table))
  (setf (gethash key hash-table) value))

(defun addhash (key value hash-table &key (aggregate #'cons) (default nil))
  "If VALUE is true, AGGREGATE it with the existing entry of KEY in HASH-TABLE
   (or with DEFAULT if no existing entry)."
  (declare (hash-table hash-table) (function aggregate))
  (when value
    (sethash key (funcall aggregate value (gethash key hash-table default))
             hash-table)))

(defun get-values (hash-table &optional default)
  "Get a list of all the values of a hash-table."
  (declare (hash-table hash-table))
  (or (loop for v being the hash-values of hash-table collect v) default))

(defun make-keyword (symbol)
  "Convert a symbol to a keyword."
  (declare (symbol symbol))
  (intern (symbol-name symbol) "KEYWORD"))

(defgeneric copy-instance (object &rest initargs)
  (:documentation "Shallow copy OBJECT, with changes specified by INITARGS")
  (:method ((object standard-object) &rest initargs)
    (let* ((class (class-of object))
           (copy (allocate-instance class)))
      (dolist (slot-name (mapcar #'sb-mop:slot-definition-name
                                 (sb-mop:class-slots class)))
        (when (slot-boundp object slot-name)
          (setf (slot-value copy slot-name) (slot-value object slot-name))))
      (apply #'reinitialize-instance copy initargs))))

(defmacro mapcar-append (func list)
  "Call MAPCAR on a FUNC that produces a separate list for each element, and
   append everything together."
  `(apply #'append (mapcar ,func ,list)))

(defun take (n list)
  "Return first N elements of a list."
  (cond ((> n (length list)) list)
        (t (subseq list 0 n))))


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    ;;;
;;;     STATISTICS     ;;;
;;;                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun load-stats ()
  "Load learned statistics from disk."
  (when (probe-file *stats-filename*)
    (with-open-file (stats *stats-filename* :direction :input)
      (setf *elements-given-text* (read stats))
      (setf *constructions-given-elements* (read stats)))))

(defun save-stats ()
  "Save learned statistics to disk."
  (with-open-file (stats *stats-filename*
                         :direction :output :if-exists :supersede)
    (let ((*print-readably* t))
      (print *elements-given-text* stats)
      (print *constructions-given-elements* stats))))

(defun get-universe (givens stats)
  "Get the universe of possibilities from a list of givens. If it or its path
   does not exist, it is created."
  (declare (list givens) (hash-table stats))
  (if (null givens)
      stats
      (let ((key (first givens)))
        (when (null (gethash key stats))
          (sethash key (make-hash-table :test 'equal) stats))
        (get-universe (rest givens) (gethash key stats)))))

(defun p (stats var &optional givens)
  "Get P(var | givens) from the object containing the statistics."
  (declare (hash-table stats) (list givens))
  (let* ((universe (get-universe givens stats))
         (total (reduce #'+ (get-values universe '(1))))
         (this (gethash var universe *default-stats-count*)))
    (/ this total)))

(defun 1+p (stats var &optional givens)
  "Increment the count of a var in stats by 1."
  (declare (hash-table stats) (list givens))
  (let* ((universe (get-universe givens stats)))
    (sethash var (1+ (gethash var universe 0)) universe)))

(defmacro defp (stats query-name update-name params)
  "Macro that creates convenience functions for querying and updating
   statistics."
  (let ((p-args `(,stats var (list ,@params))))
    `(progn
       (defun ,query-name (var ,@params)
         (p ,@p-args))
       (defun ,update-name (var ,@params)
         (1+p ,@p-args)))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       ;;;
;;;     PRE-PROCESSOR     ;;;
;;;                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass span ()
  ((start :type integer :initarg :start :initform (error "No start")
          :reader start-of :documentation "Start position of a span")
   (end :type integer :initarg :end :initform (error "No end")
        :reader end-of :documentation "End position of a span"))
  (:documentation "Class that denotes a span across a sentence."))

(defmethod shared-initialize :after ((object span) slot-names &key)
  "Sanity check to see if span has greater-than-zero length"
  (when (<= (end-of object) (start-of object)) (error "Start not after end!")))

(defun span-arrow (span)
  "Returns a string representation of the span using an arrow"
  (declare (span span))
  (format nil "~S -> ~S" (start-of span) (end-of span)))

(defmethod print-object ((object span) stream)
  "Print spans readably"
  (declare (stream stream))
  (print-unreadable-object (object stream) (format stream (span-arrow object))))

(defgeneric combine (span1 span2)
  (:documentation "Combine two spans in some way to produce a third one. Second
   one should be after first, and neither should overlap."))

(defmethod combine ((span1 span) (span2 span))
  "By default just combine the ranges of the two spans."
  (if (> (end-of span1) (start-of span2))
      (error "Span 1 is after 2")
      (make-instance 'span :start (start-of span1) :end (end-of span2))))

(defun span-length (span)
  "Return the length of this span."
  (declare (span span))
  (- (end-of span) (start-of span)))

(defun span-lessp (span1 span2)
  "Return true if one span is strictly before another."
  (declare (span span1 span2))
  (< (start-of span1) (start-of span2)))

(defun sort-spans (spans)
  "Sort a list of spans."
  (declare (list spans))
  (stable-sort spans #'span-lessp))

(defclass n-gram (span)
  ((text :type list :initarg :text :initform (error "0-gram") :reader text
         :documentation "List of natural language tokens for this n-gram")
   (base :type string :initarg :base-form :reader base-form
         :documentation "The base form of this n-gram")
   (attrs :type list :initarg :attrs :reader attrs
          :documentation "List of morphological attributes of this n-gram"))
  (:documentation "Class denoting a particular n-gram in a sentence."))

(defmethod print-object ((object n-gram) stream)
  "Print n-grams readably"
  (declare (stream stream))
  (print-unreadable-object (object stream)
    (format stream "~A ~A" (span-arrow object) (text object))))

(defun make-n-gram (text start &rest rest)
  "Constructor for an n-gram given the text and start"
  (declare (list text) (integer start))
  (apply #'make-instance 'n-gram
         :text text :start start :end (+ start (length text)) rest))

(defun pre-processor (sentence &optional (start 0))
  "Chop a sentence up into n-grams.

   Currently only returns a list of unigrams.
   TODO: return a list of n-grams (how do we define n?)"
  (declare (string sentence) (integer start))
  (let ((unigrams (split-sequence:split-sequence #\Space sentence)))
    (loop for unigram in unigrams
         counting unigram into pos
         collecting (make-n-gram (list unigram) (1- (+ start pos))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              ;;;
;;;     MORPHOLOGICAL ENGINE     ;;;
;;;                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun morphological-engine (an-n-gram)
  "Given an n-gram, separate the plaintext into a list of base words and
   additional morphological information.

   TODO: actually do something about morphology"
  (declare (n-gram an-n-gram))
  (let* ((downcased-text (mapcar #'string-downcase (text an-n-gram)))
         (formatted-text (format nil "~{~A~^ ~}" downcased-text)))
    (list (copy-instance an-n-gram :base-form formatted-text :attrs nil))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                         ;;;
;;;     MEANING CREATOR     ;;;
;;;                         ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass interpretation ()
  ((confidence :type number :initarg :confidence :reader confidence
               :initform *default-interpretation-confidence*
               :documentation
               "Confidence that this is the interpretation of some text."))
  (:documentation "Base case for anything to do with interpretation."))

(defun interpretation-greaterp (interp1 interp2)
  "Return true if one interpretation is strictly more confident than another."
  (declare (interpretation interp1 interp2))
  (> (confidence interp1) (confidence interp2)))

(defun sort-interpretations (interpretations)
  "Sort a list of interpretations according to decreasing confidence."
  (declare (list interpretations))
  (stable-sort interpretations #'interpretation-greaterp))

(defclass meaning (n-gram interpretation)
  ((element :type element :initarg :element :initform (error "No element")
            :reader scone-element
            :documentation "Scone element represented by this base n-gram")
   (confidence :initform *default-interpretation-confidence*))
  (:documentation
   "Wrapper around a Scone element providing meta-information about its role in
    the sentence."))

(defun meaningp (object)
  "Is an object of the class MEANING?"
  (typep object 'meaning))

(defmethod print-object ((object meaning) stream)
  "Print meanings readably"
  (print-unreadable-object (object stream)
    (format stream "~A ~A ~,2f" (span-arrow object) (scone-element object)
            (confidence object))))

(defun make-meaning (n-gram &rest rest)
  "Create a meaning from an n-gram."
  (apply #'change-class (copy-instance n-gram) 'meaning rest))

(defp *elements-given-text* p-e_t 1+p-e_t (text))

(defun meaning-creator (n-gram)
  "Given an n-gram, return a list of possible meanings that this n-gram refers
   to."
  (declare (n-gram n-gram))
  (mapcar (lambda (result)
            (let ((entry (first result)))
              (make-meaning n-gram
                            :element entry
                            :confidence (p-e_t entry (text n-gram)))))
          (lookup-definitions (base-form n-gram))))


;;;;;;;;;;;;;;;;;;;;;;;
;;;                 ;;;
;;;     GRAMMAR     ;;;
;;;                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;


(defclass construction ()
  ((name :type string :initarg :name :reader construction-name
         :documentation "The string name for this construction.")
   (element-type :type element :initarg :type :reader element-type
                 :documentation
                 "What kind of Scone element this construction produces.")
   (pattern :type list :initarg :pattern :initform "No pattern"
            :reader pattern :documentation "A natural-language form.")
   (payload :type function :initarg :payload :reader payload
            :documentation "Lisp code to put the payload into Scone"))
  (:documentation
   "A class that represents meaning-form pairings (aka constructions) in a
    particular language."))

(defmethod print-object ((object construction) stream)
  "Print constructions in a readable way"
  (print-unreadable-object (object stream :type t)
    (format stream "~A" (construction-name object))))

(defun conditions (part)
  "Get the list of conditions from one part of a pattern."
  (declare (list part))
  (second part))

(defun operator (part)
  "Get the operator from one part of a pattern."
  (declare (list part))
  (third part))

(defun expand-pattern (pattern)
  "Pre-process a construction pattern."
  (declare (list pattern))
  (loop for element in pattern
       when (equal '+ (first element))
         collect (replace-head '= element) and collect (replace-head '* element)
       else collect element))

(defun make-construction (name element-type pattern payload)
  "Create a new constrution object."
  (declare (list pattern) (element element-type) (function payload))
  (make-instance 'construction :name name :type element-type
                 :pattern (expand-pattern pattern) :payload payload))

(defun payload-arguments (pattern)
  "Get a list of all possible payload arguments from a construction pattern."
  (declare (list pattern))
  (remove-duplicates (mapcar #'operator pattern)))

(defmacro make-payload (arguments payload-body)
  "Create the payload function for a new construction."
  (declare (list arguments payload-body))
  `(lambda (&key ,@arguments)
     (declare ,@(loop for arg in arguments collect `(ignorable ,arg)))
     ,@payload-body))

(defmacro defconstruction (name element-type-iname pattern &rest body)
  "Macro for conveniently creating a new construction."
  (declare (symbol name) (element-iname element-type-iname) (list pattern))
  (let ((element-type (lookup-element element-type-iname))
        (arguments (payload-arguments pattern))
        (payload (gensym))
        (new-construction (gensym)))
    `(let* ((,payload (make-payload ,arguments ,body))
            (,new-construction
             (make-construction ,(symbol-name name) ,element-type ',pattern
                                ,payload)))
       (defparameter ,name ,new-construction)
       (setf *constructions* (cons ,new-construction *constructions*)))))


;;;;;;;;;;;;;;;;;;;;;;;
;;;                 ;;;
;;;     MATCHER     ;;;
;;;                 ;;;
;;;;;;;;;;;;;;;;;;;;;;;


(defclass match (span interpretation)
  ((construction :type construction :initarg :construction
                 :reader construction-being-matched
                 :initform (error "No construction")
                 :documentation "Construction being matched against.")
   (bindings :type list :initarg :bindings :reader bindings
             :initform (error "No bindings set")
             :documentation "Matched meanings so far.")
   (progress :type integer :initarg :progress :reader progress :initform 0
             :documentation "Progress on matching othe construction's pattern.")
   (confidence :initform *default-interpretation-confidence*))
  (:documentation "An in-progress match of a construction's patterns."))

(defmethod print-object ((object match) stream)
  (print-unreadable-object (object stream)
    (format stream "~A progress=~A ~A ~,2f" (span-arrow object)
            (progress object) (bindings object) (confidence object))))

(defmethod pattern ((match match))
  "The natural language form of the construction being matched."
  (pattern (construction-being-matched match)))

(defmethod bindings ((construction construction))
  "Get the initial bindings for a match that is to be created froom a
   construction."
  (mapcar #'list (payload-arguments (pattern construction))))

(defmethod payload ((match match))
  "Get the payload function of the construction of the match."
  (payload (construction-being-matched match)))

(defmethod text ((match match))
  "Get the matched text of a partial MATCH."
  (let* ((meanings (mapcar-append #'cdr (bindings match)))
         (sorted-meanings (sort-spans meanings))
         (texts (mapcar #'text sorted-meanings)))
    (apply #'append texts)))

(defgeneric matches? (actual expected)
  (:documentation
   "See if an actual item matches up with the expected one in a pattern."))

(defmethod matches? (actual expected)
  "By default, just check if they're equal. This is the case for strings."
  (equal actual expected))

(defmethod matches? (actual (expected list))
  "If EXPECTED is a list, check that all elements are similarly matched."
  (every #'matches? (make-list (length expected) :initial-element actual)
         expected))

(defmethod matches? (actual (expected element-iname))
  "Convert EXPECTED to an ELEMENT first before comparing."
  (let ((expected (lookup-element expected)))
    (when expected (matches? actual expected))))

(defmethod matches? ((actual element-iname) expected)
  "Convert ACTUAL to an ELEMENT first before comparing."
  (let ((actual (lookup-element actual)))
    (when actual (matches? actual expected))))

(defmethod matches? ((actual element) (expected element))
  (simple-is-x-a-y? actual expected))

(defmethod matches? ((actual meaning) expected)
  (matches? (scone-element actual) expected))

(defmethod matches? ((actual element) (expected (eql :type)))
  "Ensure that it is a type node we're matching against."
  (type-node? actual))

(defp *constructions-given-elements* p-c_e 1+p-c_e (element))

(defun matchp (object)
  "Is an object of the class MATCH?"
  (typep object 'match))

(defun completed? (match)
  "Check to see if a match is fully completed."
  (equal (progress match) (length (pattern match))))

(defun 1+progress (progress operator)
  "Depending on what the operator is, decides whether or not to increment the
   progress."
  (case operator
    (* progress)
    (otherwise (1+ progress))))

(defun is-optional? (operator)
  "Returns whether or not an operator is optional."
  (member operator '(? *)))

(defun next-match-part (match)
  "Get the next part of the pattern to be matched."
  (declare (match match))
  (nth (progress match) (pattern match)))

(defun match-helper (pattern progress actual)
  "Continue the current pattern being matched, and return an update to the
   bindings and update to progress.

  TODO: refactor so that values returned in same order as called"
  (declare (meaning actual) (number progress) (meaning actual))
  (if (>= progress (length pattern))
      (list nil nil nil)
      (destructuring-bind (operator expected binding) (nth progress pattern)
        (cond ((matches? actual expected)
               (list binding actual (1+progress progress operator)))
              ((is-optional? operator)
               (match-helper pattern (1+ progress) actual))
              (t (list nil nil nil))))))

(defun add-binding (binding actual bindings)
  "Add a new binding to a copied list of existing ones."
  (declare (symbol binding) (list bindings) (meaning actual))
  (let* ((new-bindings (copy-alist bindings))
         (result (assoc binding new-bindings)))
    (setf (cdr result) (cons actual (cdr result)))
    new-bindings))

(defun make-match (construction actual)
  "Create a match from a construction"
  (declare (construction construction) (meaning actual))
  (destructuring-bind (binding actual progress)
      (match-helper (pattern construction) 0 actual)
    (when binding
      (make-instance 'match
                     :start (start-of actual) :end (end-of actual)
                     :construction construction :progress progress
                     :bindings (add-binding binding actual
                                            (bindings construction))
                     :confidence (* (confidence actual)
                                    (p-c_e (list (construction-name
                                                  construction)
                                                 binding)
                                           (scone-element actual)))))))

(defmethod combine :around ((match match) (meaning meaning))
  "Combine a partial match with a meaning to produce another partial match.

   TODO: Code reuse with MAKE-MATCH."
  (when (completed? match) (error "Match is already completed!"))
  (destructuring-bind (binding meaning new-progress)
      (match-helper (pattern match) (progress match) meaning)
    (when binding
      (let ((new-span (call-next-method))
            (construction (construction-being-matched match)))
        (change-class new-span 'match
                      :construction construction
                      :bindings (add-binding binding meaning (bindings match))
                      :progress new-progress
                      :confidence (* (confidence match) (confidence meaning)
                                     (p-c_e (list (construction-name
                                                   construction)
                                                  binding)
                                            (scone-element meaning))))))))

(defmethod combine ((match match) (meanings list))
  "Try to combine a match with a variety of meanings."
  (mapcar (lambda (meaning) (combine match meaning)) meanings))

(defmethod combine ((matches list) (meanings list))
  "Try to combine a list of matches with a list of meanings."
  (mapcar-append (lambda (match) (combine match meanings))
                 meanings))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                          ;;;
;;;     PAYLOAD EXEoCUTOR     ;;;
;;;                          ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass matched-construction (meaning match)
  () ; no extra slots
  (:documentation "A fully completed match of a construction."))

(defun payload-parameters (bindings)
  "Given a list of matched bindings, create a list of argument parameters to
   call the constructor with."
  (declare (list bindings))
  (mapcar-append (lambda (binding)
                   (list (make-keyword (car binding)) (cdr binding)))
                 bindings))

(defun execute-payload (match)
  "Run the payload function defined by the construction of this match."
  (declare (match match))
  (apply (payload match) (payload-parameters (bindings match))))

(defun complete (match)
  "Complete a match, if possible."
  (declare (match match))
  (cond ((completed? match)
         (change-class (copy-instance match) 'matched-construction
                       :text (text match) :element (execute-payload match)))
        ((is-optional? (operator (next-match-part match)))
         (complete (copy-instance match :progress (1+ (progress match)))))))


;;;;;;;;;;;;;;;;;;;;;;;;
;;;                  ;;;
;;;     LEARNING     ;;;
;;;                  ;;;
;;;;;;;;;;;;;;;;;;;;;;;;


(defgeneric update-stats (interpretation)
  (:documentation
   "Update learned statistics for an interpretation of something."))

(defmethod update-stats ((meaning meaning))
  (1+p-e_t (scone-element meaning) (text meaning)))

(defmethod update-stats ((mc matched-construction))
  (loop for (binding . values) in (bindings mc)
       when values
       do (loop for value in values
               do (1+p-c_e (list (construction-name
                                  (construction-being-matched mc))
                                 binding)
                           (type-node (scone-element value)))
               do (update-stats value))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                           ;;;
;;;     PARSING ALGORITHM     ;;;
;;;                           ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defstruct node stack level understandings)

(defun make-node-from (m)
  "Create a node from either a match or a matched construction"
  (make-node :stack (list m) :level 0))

(defun make-child (node new-stack)
  "Create a new child node with a custom stack"
  (declare (node node) (list new-stack))
  (make-node :stack new-stack :level (1+ (node-level node))))

(defun add-node-stack (node addition)
  "Add something to a node's stack."
  (declare (node node))
  (make-child node (cons addition (node-stack node))))

(defun replace-node-stack (node replacement)
  "Replace the head of a node's stack with something else."
  (declare (node node) (interpretation replacement))
  (make-node :stack (replace-head replacement (node-stack node))
             :level (1+ (node-level node))))

(defun node-length (node)
  "Get the length of a node's stack."
  (length (node-stack node)))

(defun collapse-node (node completion meanings-ht constructions)
  "Feed the completion into the previous pattern."
  (declare (node node) (matched-construction completion))
  (addhash (start-of completion) completion meanings-ht)
  (when (> (node-length node) 1)
    (continue-node (make-node :stack (rest (node-stack node))
                              :level (node-level node))
                   meanings-ht constructions)))

(defun node-current (node)
  "Get the current element at the top of a node's stack."
  (first (node-stack node)))

(defun outer-product (fn list1 list2)
  "Take the outer product of two lists to produce a list of lists."
  (declare (function fn) (list list1 list2))
  (mapcar (lambda (e1) (mapcar (lambda (e2) (funcall fn e1 e2)) list2))
          list1))

(defun score (node)
  "Get a confidence score for an entire node."
  (reduce #'* (mapcar (lambda (si) (* (span-length si) (confidence si)))
                      (node-stack node))))

(defun node-greaterp (node1 node2)
  "Returns whether one node should be before another or not."
  (declare (node node1 node2))
  (> (score node1) (score node2)))

(defun sort-nodes (nodes)
  "Sort a list of nodes according to their overall span and confidence."
  (declare (list nodes))
  (stable-sort nodes #'node-greaterp))

(defun cull (nodes)
  "Return only the most likely nodes."
  (declare (list nodes))
  (take *beam-search-width* (sort-nodes (remove nil nodes))))

(defun start-matches (constructions meanings)
  "Produce all possible starts to a match."
  (declare (list constructions meanings))
  (let* ((cm-list (outer-product #'make-match constructions meanings)))
    (remove nil (apply #'append cm-list))))

(defun start-nodes (constructions meanings &key from on)
  "Create search nodes for all possible matches."
  (declare (list constructions meanings))
  (let* ((new-matches (start-matches constructions meanings)))
    (cond (from
           (mapcar (lambda (new-match) (replace-node-stack from new-match))
                   new-matches))
          (on
           (mapcar (lambda (new-match) (add-node-stack on new-match))
                   new-matches))
          (t (mapcar #'make-node-from new-matches)))))

(defun continue-node (node meanings-ht
                      &optional (constructions *constructions*))
  "Find all continuations for a search node."
  (declare (node node) (hash-table meanings-ht)
           (list constructions))
  (let* ((match (first (node-stack node)))
         (meanings (gethash (end-of match) meanings-ht))
         (ontop-nodes (start-nodes constructions meanings :on node))
         (continued-matches (remove nil (combine match meanings)))
         (completions (remove nil (mapcar #'complete continued-matches)))
         (new-start-nodes (start-nodes constructions completions :from node))
         (collapsed-nodes
          (mapcar-append
           (lambda (completion)
             (collapse-node node completion meanings-ht constructions))
           completions))
         (next-matches (remove-if #'completed? continued-matches))
         (new-match-nodes
          (mapcar (lambda (next-match) (replace-node-stack node next-match))
                  next-matches)))
    (append ontop-nodes new-start-nodes collapsed-nodes new-match-nodes)))

(defun continue-nodes (nodes &rest rest)
  "Find the best continuations for a set of search nodes."
  (cull (mapcar-append (lambda (node) (apply #'continue-node node rest))
                       nodes)))

(defun goal-meaning? (meaning &key (start 0) until)
  "See if a parsed meaning is the goal."
  (declare (meaning meaning))
  (and (= (start-of meaning) start) (= (end-of meaning) until)))

(defun parsing-algorithm (meanings-ht nodes until)
  (when nodes
    (or (loop for meaning in (gethash 0 meanings-ht)
           when (goal-meaning? meaning :start 0 :until until)
           return meaning)
        (let ((result
               (parsing-algorithm meanings-ht (continue-nodes nodes meanings-ht)
                                  until)))
          (when result
            (update-stats result)
            result)))))

(defun understand (text &key (meanings-ht (make-hash-table :test 'equal)))
  "Try to truly understand a piece of text."
  (declare (string text))
  (let* ((n-grams (pre-processor text))
         (morph-n-grams (mapcar-append #'morphological-engine n-grams))
         (meanings (mapcar-append #'meaning-creator morph-n-grams)))
    (loop for meaning in meanings
       do (addhash (start-of meaning) meaning meanings-ht))
    (let ((result
           (parsing-algorithm meanings-ht
                              (start-nodes *constructions*
                                           (gethash 0 meanings-ht))
                              (end-of (first (last n-grams))))))
      (or result
          (format t "Only understood: ~A"
                  (loop for meanings being the hash-values of meanings-ht
                       collect meanings))))))
