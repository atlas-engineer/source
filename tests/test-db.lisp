(in-package :cl-user)
(defpackage source-test
  (:use :cl
        :source
        :source.web
        :mito
        :sxql
        :fiveam)
  (:export :test-db
           :test-all
           :db-tests))
(in-package :source-test)

#|

To run tests:

(asdf:test-system 'source-test)

or

(in-package :source-test)
(run! 'easytest) ;; one test
(run! 'db-tests) ;; or (test-db): DB test suite
(test-all) ;; all tests.

|#

(defun random-string (length)
  ;; thanks 40ants/hacrm, and the Cookbook.
  (let ((chars "ABCDEFGHIJKLMNOPQRSTUVWXYZabcdefghijklmnopqrstuvwxyz0123456789"))
    (coerce (loop repeat length
                  collect (aref chars (random (length chars))))
            'string)))

(defmacro with-empty-db (&body body)
  "Run `body` with a new temporary DB."
  `(let* ((*random-state* (make-random-state t))
          (prefix (concatenate 'string
                               (random-string 8)
                               "/"))
          (connection mito:*connection*))
     (uiop:with-temporary-file (:pathname db-name :prefix prefix)
       (let* ((source.web::*db-name* db-name))
         (log:info db-name)
         ;; Catch anything to always re-connect to our real DB.
         (unwind-protect
              (progn
                (source.web::connect-and-migrate)
                ,@body)

           (setf mito:*connection* connection))))))

(def-suite db-tests
    :description "All DB tests.")

(in-suite db-tests)

(test create-repository
  (with-empty-db
    (is (= 0
           (length (select-repositories))))
    (let ((repository (make-repository "repo-test")))
      (log:info repository)
      (is (= 1
             (length (select-repositories :name "repo-test")))))))

(test create-user
  (with-empty-db
    (is (= 0
           (count-dao 'user)))
    (let ((user (create-account "usertest" "pwd" "test@test.test")))
      (is (= 1
             (count-dao 'user)))
      (is (not (equal "pwd"
                      (password user)))
          "The password is hashed"))))

(defun test-db ()
  (run! 'db-tests))

(defun test-all ()
  (run! 'db-tests))
