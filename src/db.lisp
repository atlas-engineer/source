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
           :with-connection))
(in-package :source.db)

(defun connection-settings (&optional (db :maindb))
  (cdr (assoc db (config :databases))))

(defun db (&optional (db :maindb))
  (apply #'connect-cached (connection-settings db)))

(defmacro with-connection (conn &body body)
  `(let ((*connection* ,conn))
     ,@body))

;; Initialize the database tables
(defun initialize-database ()
  (with-connection (db)
    (datafly:execute
     (sxql:create-table (:survey :if-not-exists t)
         ((id :type 'integer
              :primary-key t
              :autoincrement t)
          (data :type 'text))))
    (datafly:execute
     (sxql:create-table (:response :if-not-exists t)
         ((survey_id :type 'integer)
          (data :type 'text))))))
