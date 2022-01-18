(defun fibonacci (n)
  (if (< n 2)
    n
    (+ (fibonacci (- n 2)) (fibonacci (- n 1)))))

(print (fibonacci 7))
