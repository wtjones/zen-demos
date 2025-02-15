(uiop:define-package maze
  (:use #:cl)
  (:export #:main #:command-maze-export))
(in-package #:maze)

(defun command-maze-export (args)
  (format t "Command: export with args: ~a~%" args)

  (let ((board (generate-maze
                 (parse-integer (nth 0 args))
                 (parse-integer (nth 1 args)))))
    (format t "~a~%" (print-maze board))))


(defun main (&optional (args (uiop:command-line-arguments)))

  (setup-logging)
  (log:info "startup...")

  (if args
      (progn
       (log:info "parsing command: ~a~%" (first args))
       (if (string= (string-downcase (string (first args))) "export")
           (command-maze-export (cdr args))
           (format *error-output* "Command ~a not valid~%" (first args))))
      t
      ;disable until functional
      ; (command-maze-tui (cdr args))
      ;      )
  ))
