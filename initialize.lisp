;;; Quick script to load all necessary files

(setf *comment-on-element-creation* nil)
(scone "scone")
(load-kb "core")
(load "nlu/matcher")
(setf *comment-on-element-creation* t)
(load "nlu/grammars/simple/grammar")


