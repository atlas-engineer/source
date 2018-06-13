(in-package :cl-user)
(defpackage source.view
  (:use :cl)
  (:import-from :source.config
                :*template-directory*)
  (:import-from :caveman2
                :*response*
                :response-headers)
  (:import-from :datafly
                :encode-json)
  (:export :render
           :render-json))
(in-package :source.view)

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))
