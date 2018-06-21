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

(defun delete-repository (name)
  (delete-repository-folder
   (merge-pathnames name source.config::*repository-directory*))
  (with-connection (db)
    (execute (delete-from :repository
               (where (:= :name name))))))

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

(defun list-repositories (public)
  (with-connection (db)
    (let* ((public-int (if public 1 0))
           (repositories
            (retrieve-all
             (select :name
               (from :repository)
               (where (:= :public public-int))))))
      (loop for repository in repositories
            collect (cadr repository)))))
