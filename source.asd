(defsystem "source"
  :version "0.1.0"
  :author "Atlas Engineer"
  :license ""
  :depends-on (:clack
               :lack
               :caveman2
               :envy
               :cl-ppcre
               :uiop
               :ironclad
               :cl-markup
               :s-xml
               :datafly
               :sxql
               :cl-ppcre
               :str
               :alexandria
               :flexi-streams
               :mito)
  :components ((:module "src"
                :components
                ((:file "main" :depends-on ("config" "view" "db"))
                 (:file "web" :depends-on ("view"))
                 (:file "repository" :depends-on ("web"))
                 (:file "user" :depends-on ("web"))
                 (:file "view" :depends-on ("config"))
                 (:file "db" :depends-on ("web" "config"))
                 (:file "config"))))
  :description ""
  :in-order-to ((test-op (test-op "source-test"))))
