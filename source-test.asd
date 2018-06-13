(defsystem "source-test"
  :defsystem-depends-on ("prove-asdf")
  :author "John Mercouris"
  :license ""
  :depends-on ("source"
               "prove")
  :components ((:module "tests"
                :components
                ((:test-file "source"))))
  :description "Test system for source"
  :perform (test-op (op c) (symbol-call :prove-asdf :run-test-system c)))
