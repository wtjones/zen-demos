(asdf:defsystem "maze"
  :long-name "maze-in-common-lisp"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on (:cl-charms)
  :components ((:module "src"
                        :components
                        ((:file "main")
                         (:file "tui"))))
  :description "Sample library"
  :long-description "Common Lisp sample library")
