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
;;;        ,---------.    --- SCONE KB CONCEPTS (DICTIONARY)
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
;;;      (SCONE CONCEPT)
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
;;; for the dictionary) to find corresponding Scone concepts and produce meaning
;;; wrappers around each concept. The meaning wrapper contains extra
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



;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                                   ;;;
;;;     GENERAL UTILITY FUNCTIONS     ;;;
;;;                                   ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass span ()
  ((start :type integer :initarg :start :initform (error "No start")
          :reader start-of :documentation "Start position of a span")
   (end :type integer :initarg :end :initform (error "No end")
        :reader end-of :documentation "End position of a span"))
  (:documentation
   "Class that denotes a span across a sentence."))

(defun span-arrow (span)
  "Returns a string representation of the span using an arrow"
  (format nil "~S -> ~S" (start-of span) (end-of span)))

(defmethod print-object ((object span) stream)
  "Print spans readably

   TODO: check for *print-readably*"
  (print-unreadable-object (object stream) (format stream (span-arrow object))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                       ;;;
;;;     PRE-PROCESSOR     ;;;
;;;                       ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


(defclass n-gram (span)
  ((text :type list :initarg :text :initform (error "0-gram") :reader text
         :documentation "List of natural language tokens for this n-gram"))
  (:documentation
   "Class denoting a particular n-gram in a sentence."))

(defmethod print-object ((object n-gram) stream)
  "Print n-grams readably"
  (print-unreadable-object (object stream)
    (format stream "~A ~A" (span-arrow object) (text object))))

(defun make-n-gram (text start)
  "Constructor for an n-gram given the text and start"
  (declare (list text) (integer start))
  (make-instance 'n-gram :text text :start start :end (+ start (length text))))

(defun pre-process (sentence &optional (start 0))
  "Chop a sentence up into n-grams.

   Currently only returns a list of unigrams.
   TODO: return a list of n-grams (how do we define n?)"
  (let ((unigrams (split-sequence:split-sequence #\Space sentence)))
    (loop for unigram in unigrams
         counting unigram into pos
         collecting (make-n-gram (list unigram) (1- (+ start pos))))))


;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;;                              ;;;
;;;     MORPHOLOGICAL ENGINE     ;;;
;;;                              ;;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;


