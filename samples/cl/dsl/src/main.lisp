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


(defun eval-dsl (expr &key (indent-level 0))
  (with-output-to-string (s)
    (loop for e in expr collect

            (destructuring-bind (name &rest properties) e
              (let* ((zfunc (intern (format nil ".&~a" name) :dsl))
                     (struct-def `(,zfunc ,@properties)))
                (format t "eval: ~a~%" struct-def)
                (format t "len: ~a~%" (length expr))
                (princ (make-string indent-level :initial-element #\Space))
                (princ (apply zfunc properties) s)

                (when (> (length expr) 1)
                      (terpri s)))))))


(defun .&equ (name expr)
  (format nil "DEF ~a ~a" name (to-infix expr)))

(defun .&defsub (name &rest expr)
  (with-output-to-string (s)
    (format s "~a::~%~a~%    ret~%"
      name
      (eval-dsl expr))))

(defun .&setf (name val)
  (with-output-to-string (s)
    (format s "    ld a, ~a~%" val)
    (format s "    [~(~A~)], a" name)))

(defun .&return ()
  (with-output-to-string (s)
    (format s "    ret~%")))


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
