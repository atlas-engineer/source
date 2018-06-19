;;; repository.lisp --- classes and functions for creation and
;;; management of repositories. Security considerations should be made
;;; for where the user can read and write to to protect the system

(in-package :source.web)

(defun create-repository (name public)
  (create-repository-folder
   (merge-pathnames name source.config::*repository-directory*))
  (with-connection (db)
    (execute
     (insert-into :repository
       (set=
        :name name
        :public (if public 1 0))))))

(defun create-repository-folder (path)
  (uiop:run-program
   (list "git" "init" "--bare"
         (uiop:unix-namestring
          path))))

(defun delete-repository-folder (path)
  (uiop:run-program
   (list "rm" "-rf"
         (uiop:unix-namestring
          path))))
