;;; Quick script to load all necessary files

(setf *comment-on-element-creation* nil)
(scone "scone")
(load-kb "core")
(load-kb "lexical-components/wordnet-concepts")
(load-kb "lexical-components/wordnet-english-names")
(load "nlu/matcher")
(setf *comment-on-element-creation* t)
(load "nlu/grammar")
