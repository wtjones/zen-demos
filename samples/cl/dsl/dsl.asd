(asdf:defsystem "dsl"
  :long-name "dsl-in-common-lisp"
  :version "0.0.1"
  :author ""
  :license ""
  :depends-on ()
  :components ((:module "src"
                        :components
                        ((:file "main"))))
  :description "Sample dsl"
  :long-description "Common Lisp sample dsl")


(asdf:defsystem "dsl/tests"
  :description "Test suite for my-project"
  :depends-on ("dsl" "parachute")
  :components ((:module "tests"
                        :components
                        ((:file "test"))))
  :perform (asdf:test-op (op c)
                         (uiop:symbol-call :parachute :test :dsl/tests)))
