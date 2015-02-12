(defun prefer (concept)
  "Choose this concept over others in the same situation"
  (set-element-property (lookup-element concept) :score 1.1))

(defun prefer-not (concept)
  "Don't choose this concept over others in the same situation"
  (set-element-property (lookup-element concept) :score 0.9))

(setf (construction-score-multiplier noun-phrase) 1.5)
(setf (construction-score-multiplier sentence) 1.1)
(setf (construction-score-multiplier clauses) 0.8)
(setf (construction-score-multiplier command) 1.1)
(setf (construction-score-multiplier ordered-command) 1.3)
(setf (construction-score-multiplier definition) 0.8)
(setf (construction-score-multiplier v-entities) 0.8)
(setf (construction-score-multiplier action-after) 0.9)
(setf (construction-score-multiplier action-after2) 0.8)
(setf (construction-score-multiplier action-after3) 0.8)
(setf (construction-score-multiplier action-before) 0.8)
(setf (construction-score-multiplier action-before2) 0.8)
