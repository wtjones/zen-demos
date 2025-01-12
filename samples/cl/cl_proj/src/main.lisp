(defpackage sample
  (:use :cl)
  (:export :main))

(in-package :sample)


(defun main (&rest args)
  (format t "~&Starting up...~%")
  (if args
      (format t "Received arguments: ~a~%" args)
      (format t "No arguments provided.~%")):w
  )
