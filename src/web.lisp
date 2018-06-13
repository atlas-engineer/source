(in-package :cl-user)
(defpackage source.web
  (:use :cl
        :caveman2
        :source.config
        :source.view
        :source.db
        :datafly
        :sxql)
  (:export :*web*))
(in-package :source.web)

;; for @route annotation
(syntax:use-syntax :annot)

;;
;; Application

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defroute "/" ()
  (cl-markup:html
   (:body
    (:meta :name "viewport" :content "width=device-width, initial-scale=1")
    (:link :rel "stylesheet" :type "text/css" :media "screen" :href "/css/pure-min.css")
    (:link :rel "stylesheet" :type "text/css" :media "screen" :href "/css/main.css")
    (:h1 "Hello World."))))


;; Error pages
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (cl-markup:html
   (:body
    (:p "Error, page not found."))))
