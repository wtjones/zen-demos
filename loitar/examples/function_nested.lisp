(defun inner (y)
    (print "inner1")
    (+ y y))

(defun outer (x)
    (print "outer1")
    (inner (inner x)))

(print (outer 1))
