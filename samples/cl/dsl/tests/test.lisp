(defpackage dsl/tests
  (:use :cl :parachute))
(in-package :dsl/tests)

(REMOVE-ALL-TESTS-IN-PACKAGE)
(defvar *dummystate* (dsl::make-asm-state))

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

(defun compare-asm-lines (lines1 lines2)
  (when (/= (length lines1) (length lines2))
        (return-from compare-asm-lines nil))
  (every #'compare-asm-line lines1 lines2))

(define-test sanity1
  (let ((line1
         (dsl::make-line 'dsl::label "foo::"))

        (line2
         (dsl::make-line 'dsl::label "foo::")))
    (true (compare-asm-line line1 line2))))


(define-test eval-equates-scalar
  (let ((state (dsl::make-asm-state)))
    (true
        (compare-asm-line
          (dsl::make-line 'dsl::directive "DEF" "BAD" "EQU" "123")
          (nth 0 (dsl::eval-dsl state '((equ BAD "123"))))))
    (true (dsl::asm-constant-p state 'BAD))))

(define-test eval-equates-operator
  (true (compare-asm-line
          (dsl::make-line 'dsl::directive "DEF" "MAX" "EQU" "(4 + 5)")
          (nth 0 (dsl::eval-dsl *dummystate* '((equ MAX (+ 4 5))))))))


(define-test eval-defvar
  (let ((state (dsl::make-asm-state)))
    (true
        (compare-asm-line
          (dsl::make-line 'dsl::directive "my_var:" "DS" "1")
          (nth 0 (dsl::eval-dsl state '((defvar my_var))))))
    (true (dsl::asm-variable-p state 'my_var))))


(define-test eval-setf
  (let ((state (dsl::make-asm-state)))
    (dsl::asm-set-variable state 'baz)
    (true
        (compare-asm-line
          (dsl::make-line 'dsl::instruction "ld a, 123")
          (nth 0 (dsl::eval-dsl
                   state
                   '((setf baz 123))))))))

(define-test eval-sub
  (let ((state (dsl::make-asm-state)))
    (dsl::asm-set-variable state 'fob)

    (let ((result (dsl::eval-dsl
                    state
                    '((defsub (:global mysub)
                              (setf fob 10))))))
      (true
          (compare-asm-lines
            result
            (list
             (dsl::make-line 'dsl::label "mysub::")
             (dsl::make-line 'dsl::instruction "ld a, 10")
             (dsl::make-line 'dsl::instruction "[fob], a")
             (dsl::make-line 'dsl::instruction "ret")))))))

(define-test eval-whenf
  (let ((state (dsl::make-asm-state)))
    (dsl::asm-set-variable state 'my_result)
    (let ((result
           (dsl::eval-dsl
             state
             '((when (<= MAP_WIDTH pos_x)
                     (setf my_result EMPTY_TILE)
                     (return))))))
      (format t "~a~%" result)
      (true result))))

(test :dsl/tests)
