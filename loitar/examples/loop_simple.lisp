(setq a 10)
(loop
    (setq a (+ a 1))
    (print a)
    (if (eq a 20) (return a))
)

