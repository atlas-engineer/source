;;; repository.lisp --- classes and functions for creation and
;;; management of repositories. Security considerations should be made
;;; for where the user can read and write to to protect the system

(in-package :source.web)

(defun create-repository (name public)
  (crane:save
   (crane:create
    'repository
    :name name
    :public (if public 1 0)))
  (let ((repository-path (merge-pathnames name source.config:*repository-directory*))
        (public-repository-path
          (concatenate 'string
                       (uiop:unix-namestring source.config:*public-repository-directory*)
                       "./" name)))
    (create-repository-folder repository-path)
    (when public (create-public-repository-symlink repository-path public-repository-path))))

(defun create-repository-folder (path)
  (uiop:run-program
   (list "git" "init" "--bare"
         (uiop:unix-namestring
          path)))
  (uiop:run-program
   (list "chown" "-R"
         (concatenate 'string source.config::*git-user* ":" source.config::*git-user*)
         (uiop:unix-namestring
          path)) :ignore-error-status t))

(defun create-public-repository-symlink (path public-path)
  (uiop:run-program
   (list "ln" "-s"
         (uiop:unix-namestring path)
         (uiop:unix-namestring public-path))))

(defun delete-repository (name)
  (let ((repository (crane:single 'repository :name name)))
    (crane:del repository))
  (delete-repository-folder
   (merge-pathnames name source.config:*repository-directory*)))

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
