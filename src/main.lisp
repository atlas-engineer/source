(in-package :cl-user)
(defpackage source
  (:use :cl)
  (:import-from :source.config
                :config)
  (:import-from :clack
                :clackup)
  (:export :start
           :stop))
(in-package :source)

(defvar *appfile-path*
  (asdf:system-relative-pathname :source #P"app.lisp"))

(defvar *handler* nil)

(defun start (&rest args &key server port debug &allow-other-keys)
  (declare (ignore server port debug))
  (source.web::connect-and-migrate)
  (when *handler*
    (restart-case (error "Server is already running.")
      (restart-server ()
        :report "Restart the server"
        (stop))))
  (setf *handler*
        (apply #'clackup *appfile-path* args)))

(defun stop ()
  (source.web::disconnect)
  (prog1
      (clack:stop *handler*)
    (setf *handler* nil)))
