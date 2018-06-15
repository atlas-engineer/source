;;; repository.lisp --- classes and functions for creation and
;;; management of repositories

(in-package :source.web)

(defun create-repository (name)
  (uiop:run-program
   (list "git" "init" "--bare"
         (uiop:unix-namestring
          (merge-pathnames
           name
           source.config::*repository-directory*)))))

(defun delete-repository (name))
