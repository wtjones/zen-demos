(ql:quickload "cl-project")
(cl-project:make-project #p"."
  :name "cl-maze"
  :long-name "common-lisp-sample"
  :version "0.1.1"
  :description "Sample library"
  :long-description "Common Lisp sample library"
  :depends-on '(:cl-charms)  ; Corrected
  :use '(:cl)    
  :export '(main)
  :code '((defun main ()
           (format t "Welcome to cl-maze!~%")))
  :load-system t)

