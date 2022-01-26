; based on https://rosettacode.org/wiki/FizzBuzz#Common_Lisp

(defun fizzbuzz ()
  (loop for x from 1 to 15 do
    (print (cond ((zerop (mod x 15)) "FizzBuzz") ;blah
                 ((zerop (mod x 3))  "Fizz")
                 ((zerop (mod x 5))  "Buzz")
                 (t                  x)))
    ))

(fizzbuzz)
