(defpackage dsl/tests
  (:use :cl :parachute))
(in-package :dsl/tests)

(REMOVE-ALL-TESTS-IN-PACKAGE)

(defun join-elements (vec)
  (format nil "~{~A~^ ~A~}" vec))

(defun compare-asm-line (line1 line2)

  (format t "comp elem: ~a~%~a~%~a~%"
    (dsl::asm-line-elements line1) (dsl::asm-line-elements line2)
    (equalp (dsl::asm-line-elements line1)
            (dsl::asm-line-elements line2)))

  (and (equal (dsl::asm-line-line-type line1)
              (dsl::asm-line-line-type line2))
       (equalp (dsl::asm-line-elements line1)
               (dsl::asm-line-elements line2))))


(define-test sanity1
  (let ((line1
         (dsl::make-line 'dsl::label "foo::"))

        (line2
         (dsl::make-line 'dsl::label "foo::")))
    (true (compare-asm-line line1 line2))))


(define-test eval-equates-scalar
  (true
      (compare-asm-line
        (dsl::make-line 'dsl::directive "DEF" "BAD" "EQU" "123")
        (nth 0 (dsl::eval-dsl '((equ BAD "123")))))))

(define-test eval-equates-operator
  (true (compare-asm-line
          (dsl::make-line 'dsl::directive "DEF" "MAX" "EQU" "(4 + 5)")
          (nth 0 (dsl::eval-dsl '((equ MAX (+ 4 5))))))))


(define-test eval-setf
  (true
      (compare-asm-line
        (dsl::make-line 'dsl::instruction "ld a, 123")
        (nth 0 (dsl::eval-dsl '((setf baz 123)))))))

(define-test eval-sub
  (let ((result (dsl::eval-dsl '((defsub mysub
                                         (setf fob 10)
                                         (setf baz 3))))))
    (format t "*********testing sub!!!! ~a~%" (nth 1 result))
    (true
        (compare-asm-line
          (dsl::make-line 'dsl::instruction "mysub")
          (nth 0 result)))

    (true
        (compare-asm-line
          (dsl::make-line 'dsl::instruction "ld a, 10")
          (nth 1 result)))))


(define-test eval-whenf
  (let ((result (dsl::eval-dsl '((when (<= MAP_WIDTH pos_x)
                                       (setf my_result EMPTY_TILE)
                                       (return))))))
    (format t "~a~%" result)
    (true result)))


(test :dsl/tests)
