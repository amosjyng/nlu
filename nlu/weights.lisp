(defun prefer (concept)
  "Choose this concept over others in the same situation"
  (set-element-property (lookup-element concept) :score 1.1))

(defun prefer-not (concept)
  "Don't choose this concept over others in the same situation"
  (set-element-property (lookup-element concept) :score 0.9))

(prefer {push.v.01})
(prefer {ball.n.01})
(prefer {nail.n.02})
(prefer {screwdriver.n.01})
(prefer {table.n.02})

(prefer-not {up.v.01})
;;; this shouldn't be an {action}
;;; just make impossible for now
(set-element-property (lookup-element {ball.n.11}) :score 0)
;;; also temporary
(set-element-property (lookup-element {ball.v.01}) :score 0)
