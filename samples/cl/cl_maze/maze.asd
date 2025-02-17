(asdf:defsystem "maze"
  :long-name "maze-in-common-lisp"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (#:cl-charms
               #:alexandria
               #:log4cl-extras)
  :components ((:module "src"
                        :components
                        ((:file "main")
                         (:file "log")
                         (:file "board")
                         (:file "solve")
                         (:file "tiles")
                         (:file "repr")
                         (:file "tui"))))
  :description "Sample library"
  :long-description "Common Lisp sample library")
