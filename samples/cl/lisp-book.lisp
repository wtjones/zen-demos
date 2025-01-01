; Lisp 2nd edition

;; Ch 3

(defun rotate-left (a)
  (append (cdr a) (list (car a))))
