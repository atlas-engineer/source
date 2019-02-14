(defsystem "source-test"
  :defsystem-depends-on ("prove-asdf")
  :author "John Mercouris"
  :license ""
  :depends-on ("source"
               "fiveam"
               "log4cl")
  :components ((:module "tests"
                        :components
                        ((:test-file "test-db"))))
  :description "Test system for source"
  :perform (test-op (op c) (call-function #'source-test:test-all)))
