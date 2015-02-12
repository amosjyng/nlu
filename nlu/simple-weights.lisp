(defun prefer (concept)
  "Choose this concept over others in the same situation"
  (set-element-property (lookup-element concept) :score 1.1))

(defun prefer-not (concept)
  "Don't choose this concept over others in the same situation"
  (set-element-property (lookup-element concept) :score 0.9))


