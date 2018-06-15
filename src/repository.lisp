;;; repository.lisp --- classes and functions for creation and
;;; management of repositories. Security considerations should be made
;;; for where the user can read and write to to protect the system

(in-package :source.web)

(defun create-repository (path)
  (uiop:run-program
   (list "git" "init" "--bare"
         (uiop:unix-namestring
          path))))

(defun delete-repository (path)
  (uiop:run-program
   (list "rm" "-rf"
         (uiop:unix-namestring
          path))))
