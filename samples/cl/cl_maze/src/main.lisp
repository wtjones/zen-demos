(uiop:define-package maze
  (:use #:cl)
  (:export #:main #:command-maze-export))
(in-package #:maze)

(defvar *log-file*)

(defun command-maze-export (&rest args)
  (format t "Command: export with args: ~a~%" args))


(defun main ()
  (let ((args (uiop:command-line-arguments)))
    (if args
        (progn
         (format t "the arg: ~a~%" (first args))
         (if (string= (string-downcase (string (first args))) "export")
             (command-maze-export (cdr args))
             (format t "Command ~a not valid~%" (first args))))
        (command-maze-tui (cdr args)))))
