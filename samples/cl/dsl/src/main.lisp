(uiop:define-package dsl
  (:use #:cl)
  (:export #:eval-dsl
           #:main
           #:.&equ
           #:.&defsub
           #:.&setf
           #:.&return))
(in-package #:dsl)

(defun to-infix (expr)
  (cond ((not (listp expr)) (princ-to-string expr))
        ((equal expr '()) nil)
        ((not (symbolp (nth 0 expr))) '())
        (t (format nil "(~a ~a ~a)"
             (to-infix (nth 1 expr))
             (nth 0 expr)
             (to-infix (nth 2 expr))))))

(defun eval-dsl (expr)
  (let ((result '()))
    (loop for e in expr collect
            (destructuring-bind (name &rest properties) e
              (let* ((zfunc (intern (format nil ".&~a" name) :dsl))
                     (struct-def `(,zfunc ,@properties)))
                (format t "eval: ~a~%" struct-def)
                (format t "evala: ~a~%" (apply zfunc properties))

                (setf result (append result
                               (apply zfunc properties))))))
    result))


(defun .&equ (name expr)
  (list (make-line 'directive "DEF" name "EQU" (to-infix expr))))

(defun .&defsub (name &rest expr)
  (append (list (make-line 'instruction name))
    (eval-dsl expr)
    (list (make-line 'instruction "ret"))))

(defun .&setf (name val)
  (list
   (make-line 'instruction (format nil "ld a, ~a" val))
   (make-line 'instruction (format nil "[~(~A~)], a" name))))

(defun .&return ()
  (list (make-line 'instruction "ret")))

; here: convert to new structure
(defun .&when (cond-expr &rest body)
  (with-output-to-string (s)
    (let ((cond-asm
           (cond
            ((eq (nth 0 cond-expr) '<=)
              "baz!" ; here: need to know if either is var vs const 
                    )
            (t "foo!"))))

      (format s "~a~%~a~%~%"
        cond-asm
        (eval-dsl body)))))


(defun main (&optional (args (uiop:command-line-arguments)))

  (format t "test~%"))
