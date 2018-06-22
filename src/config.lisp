(in-package :cl-user)
(defpackage source.config
  (:use :cl)
  (:import-from :envy
                :config-env-var
                :defconfig)
  (:export :config
           :*application-root*
           :*static-directory*
           :*template-directory*
           :appenv
           :developmentp
           :productionp))
(in-package :source.config)

(setf (config-env-var) "APP_ENV")

(defparameter *domain*
  "domain.com")
(defparameter *git-user*
  "git" "User created specifically for this application")
(defparameter *application-root*
  (asdf:system-source-directory :source))
(defparameter *static-directory*
  (merge-pathnames #P"static/" *application-root*))
(defparameter *repository-directory*
  (make-pathname :directory (list :absolute "home" *git-user*)))
(defparameter *git-url-base*
  (concatenate 'string
               "ssh://"
               *git-user*
               "@"
               *domain*
               (uiop:unix-namestring *repository-directory*)))
(defparameter *authorized-keys-path*
  (make-pathname :directory (list :absolute "home" *git-user* ".ssh")
                 :name "authorized_keys"))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;; Allow special user configuration via an init file ;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
(defun xdg-config-home (&optional (file-name ""))
  (merge-pathnames
   file-name
   (merge-pathnames
    (make-pathname :directory '(:relative "source"))
    (uiop:xdg-config-home))))
(defvar *init-file-path* (xdg-config-home "init.lisp"))
(load *init-file-path* :if-does-not-exist nil)

(defconfig :common
  `(:databases ((:maindb :sqlite3 :database-name ":memory:"))))

(defconfig |development|
  `(:debug T
    :databases
           ((:maindb :sqlite3 :database-name
                     ,(merge-pathnames #P"database.db"
                                       *application-root*)))))
(defconfig |production|
  '())

(defconfig |test|
  '())

(defun config (&optional key)
  (envy:config #.(package-name *package*) key))

(defun appenv ()
  (uiop:getenv (config-env-var #.(package-name *package*))))

(defun developmentp ()
  (string= (appenv) "development"))

(defun productionp ()
  (string= (appenv) "production"))
