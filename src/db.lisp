(in-package :cl-user)
(defpackage source.db
  (:use :cl)
  (:import-from :source.config
                :config)
  (:import-from :datafly
                :*connection*)
  (:import-from :cl-dbi
                :connect-cached)
  (:export :connection-settings
           :db
           :with-connection
           :hash-password))
(in-package :source.db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

(defun hash-password (password)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence 
    :sha256
    (ironclad:ascii-string-to-byte-array password))))

;; Initialize the database tables
(defun initialize-database ()
  (with-connection (db)
    (datafly:execute
     (sxql:create-table (:user :if-not-exists t)
         ((id :type 'integer
              :primary-key t
              :autoincrement t)
          (username :type 'text)
          (password :type 'text)
          (email :type 'text))))
    (datafly:execute
     (sxql:create-table (:repository :if-not-exists t)
         ((id :type 'integer
              :primary-key t
              :autoincrement t)
          (name :type 'text))))))

(defun create-administrator-account (username password email)
  (with-connection (db)
    (datafly:execute
     (sxql:insert-into :user
       (sxql:set=
        :username username
        :password (hash-password password)
        :email email)))))
