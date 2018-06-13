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
  (render-page
   (cl-markup:markup (:h1 "Hello Worlds."))))

(defroute "/login" (&key _parsed)
  (render-page
   (cl-markup:markup
    (:h1 "Login")
    (:form :class "pure-form" :action "/login"
           (:p "Username")
           (:input :type "text" :name "login[username]")
           (:p "Password")
           (:input :type "text" :name "login[password]")
           (:br)
           (:br)
           (:button :type "submit" :class "pure-button" "Submit")))))

(defroute "/logout" ()
  (render-page
   (cl-markup:markup
    (:h1 "Logged Out."))))

;; Error pages
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (cl-markup:html
   (:body
    (:p "Error, page not found."))))
