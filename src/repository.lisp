;;; repository.lisp --- classes and functions for creation and
;;; management of repositories. Security considerations should be made
;;; for where the user can read and write to to protect the system

(in-package :source.web)

(defun create-repository (name public)
  (with-connection (db)
    (execute
     (insert-into :repository
       (set=
        :name name
        :public (if public 1 0)))))
  (create-repository-folder
   (merge-pathnames name source.config::*repository-directory*)))

(defun delete-repository (name)
  (with-connection (db)
    (execute (delete-from :repository
               (where (:= :name name)))))
  (delete-repository-folder
   (merge-pathnames name source.config::*repository-directory*)))

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

(defun get-repository-visibility (repository)
  (with-connection (db)
    (retrieve-one (select :public
                    (from :repository)
                    (where (:= :name repository))))))
