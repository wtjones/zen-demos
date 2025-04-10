(in-package #:dsl)

(deftype
    asm-line-type () '(member directive instruction label))

(defstruct asm-line
  (line-type nil :type asm-line-type)
  (elements nil :type list))

(defun make-line (line-type &rest elements)
  (make-asm-line :line-type line-type
                 :elements (mapcar #'string elements)))
