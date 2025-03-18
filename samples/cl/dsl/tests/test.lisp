(defpackage dsl/tests
  (:use :cl :parachute))
(in-package :dsl/tests)

(REMOVE-ALL-TESTS-IN-PACKAGE)

(define-test eval-equates-scalar
  (is equal "DEF BAD 123"
      (dsl::eval-dsl '((equ :bad 123)))))


(define-test eval-equates-operator
  (is equal "DEF MAX (4 + 5)" (dsl::eval-dsl '((equ max (+ 4 5))))))

(define-test eval-sub
  (let ((result (dsl::eval-dsl '((defsub mysub
                                         (setf foo 1)
                                         (setf baz 3))))))
    (format t "~a~%" result)
    (true result)))


(define-test eval-whenf
  (let ((result (dsl::eval-dsl '((when (<= MAP_WIDTH pos_x)
                                       (setf my_result EMPTY_TILE)
                                       (return))))))
    (format t "~a~%" result)
    (true result)))


(test :dsl/tests)
