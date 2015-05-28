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
;;; N-grams go to the morphological engine, which uses the langauge grammar to
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
;;; the payload can involve querying and/or updating the Scone KB. A matched
;;; construction is output.
;;;
;;; A matched construction object inherits from meaning, and is added to the
;;; list of meanings to be fed into the matcher. In this way, it is possible to
;;; nest constructions within each other.
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

(defvar *default-interpretation-confidence* 1
  "Default confidence value for new interpretations.")

(defvar *default-stats-count* 1
  "Default count for non-existent stats")

(defvar *elements-given-text* (make-hash-table :test 'equal)
  "Statistics for how often each n-gram is used to refer to each Scone
   element.")


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

(defun get-values (hash-table &optional default)
  "Get a list of all the values of a hash-table."
  (declare (hash-table hash-table))
  (or (loop for v being the hash-values of hash-table collect v) default))

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


;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                    ;;;
;;;     STATISTICS     ;;;
;;;                    ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;


(defun load-stats ()
  "Load learned statistics from disk."
  (when (probe-file *stats-filename*)
    (with-open-file (stats *stats-filename* :direction :input)
      (setf *elements-given-text* (read stats)))))

(defun save-stats ()
  "Save learned statistics to disk."
  (with-open-file (stats *stats-filename*
                         :direction :output :if-exists :supersede)
    (let ((*print-readably* t))
      (print *elements-given-text* stats))))

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

(defclass meaning (n-gram interpretation)
  ((element :type element :initarg :element :initform (error "No element")
            :reader scone-element
            :documentation "Scone element represented by this base n-gram")
   (confidence :initform *default-interpretation-confidence*))
  (:documentation
   "Wrapper around a Scone element providing meta-information about its role in
    the sentence."))

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
   to"
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

(defun expand-pattern (pattern)
  "Pre-process a construction pattern."
  (declare (list pattern))
  (loop for element in pattern
       when (equal '+ (first element))
         collect (replace-head '= element) and collect (replace-head '* element)
       else collect element))

(defun make-construction (name pattern payload)
  "Create a new constrution object."
  (declare (list pattern) (function payload))
  (make-instance 'construction 
                 :name name :pattern (expand-pattern pattern) :payload payload))

(defun payload-arguments (pattern)
  "Get a list of all possible payload arguments from a construction pattern."
  (declare (list pattern))
  (remove-duplicates (mapcar #'third pattern)))

(defmacro make-payload (arguments payload-body)
  "Create the payload function for a new construction."
  (declare (list arguments payload-body))
  `(lambda (&key ,@arguments)
     (declare ,@(loop for arg in arguments collect `(ignorable ,arg)))
     ,@payload-body))

(defmacro defconstruction (name pattern &rest body)
  "Macro for conveniently creating a new construction."
  (declare (symbol name) (list pattern))
  (let ((arguments (payload-arguments pattern))
        (payload (gensym))
        (new-construction (gensym)))
    `(let* ((,payload (make-payload ,arguments ,body))
            (,new-construction
             (make-construction ,(symbol-name name) ',pattern ,payload)))
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
   (bindings :type hashtable :initarg :bindings :reader bindings
             :initform (make-hash-table :test 'equal)
             :documentation "Matched meanings so far.")
   (progress :type list :initarg :progress :reader progress :documentation
             "Progress on matching each pattern of the construction.")
   (confidence :initform *default-interpretation-confidence*))
  (:documentation "An in-progress match of a construction's patterns."))