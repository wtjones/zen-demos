(in-package #:dsl)

(deftype
    asm-line-type () '(member directive instruction label))

(defstruct asm-line
  (line-type nil :type asm-line-type)
  (elements nil :type list))

(defun make-line (line-type &rest elements)
  (make-asm-line :line-type line-type
                 :elements (mapcar #'string elements)))

(defstruct asm-state
  (constants (make-hash-table))
  (variables (make-hash-table)))

(defun asm-constant-p (state sym)
  (gethash sym (asm-state-constants state)))

(defun asm-set-constant (state sym)
  (setf (gethash sym (asm-state-constants state)) t))

(defun asm-variable-p (state sym)
  (gethash sym (asm-state-variables state)))

(defun asm-set-variable (state sym)
  (setf (gethash sym (asm-state-variables state)) t))
