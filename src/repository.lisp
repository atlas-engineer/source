;;; repository.lisp --- classes and functions for creation and
;;; management of repositories. Security considerations should be made
;;; for where the user can read and write to to protect the system

(in-package :source.web)

(defun create-repository (name public)
  (crane:save
   (crane:create
    'repository
    :name name
    :public (if public "1" "0")))
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
          path)))
  (uiop:run-program
   (list "chown" "-R"
         (concatenate 'string source.config::*git-user* ":" source.config::*git-user*)
         (uiop:unix-namestring
          path))))

(defun delete-repository-folder (path)
  (uiop:run-program
   (list "rm" "-rf"
         (uiop:unix-namestring
          path))))

(defun list-repositories (public)
  (crane:filter
   'repository
   (:= :public (if public 1 0))))

(defun get-repository-visibility (repository)
  (when repository
    (if (equal (public repository) 1) t nil)))
