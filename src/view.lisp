(in-package :cl-user)
(defpackage source.view
  (:use :cl)
  (:import-from :source.config
                :*template-directory*)
  (:import-from
   :caveman2
   :*response*
   :*response*
   :*session*
   :*request*
   :response-headers)
  (:import-from :datafly
                :encode-json)
  (:export :render-page
           :render-json))
(in-package :source.view)

(defun logged-in-p ()
  (gethash :logged-in *session*))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))

(defparameter menu-links
  (quote (("Index" . "/"))))

(defparameter menu-links-logged-out
  (quote (("Login" . "/login"))))

(defparameter menu-links-logged-in
  (quote (("Create Repository" . "/create/repository")
          ("My Account" . "/account")
          ("Logout" . "/logout"))))

(defun render-link (link &optional (selected nil))
  (cl-markup:markup
   (:li :class (if selected
                    "pure-menu-item pure-menu-selected"
                    "pure-menu-item")
        (:a :href (cdr link) :class "pure-menu-link" (car link)))))

(defun render-menu (&key (login-menu-enabled t))
  (let ((path (lack.request:request-path-info *request*))
        (links (cond
                 ((not login-menu-enabled) menu-links)
                 ((logged-in-p) (append menu-links menu-links-logged-in))
                 (t (append menu-links menu-links-logged-out)))))
    (cl-markup:markup
     (:div :class "pure-menu pure-menu-horizontal"
           (:ul :class "pure-menu-list"
                (:li :class "pure-menu-heading menu-brand"
                     (:a :href "/" "Source"))
                (loop for link in links
                      collect
                      (render-link link (equal path (cdr link)))))))))

(defun render-page (object)
  (cl-markup:html
   (:title "Source")
   (:head
    (:meta :name "viewport" :content "width=device-width, initial-scale=1")
    (:link :rel "stylesheet" :type "text/css" :media "screen" :href "/css/pure-min.css" "")
    (:link :rel "stylesheet" :type "text/css" :media "screen" :href "/css/grids-responsive-min.css" "")
    (:link :rel "stylesheet" :type "text/css" :media "screen" :href "/css/auto-complete.css" "")
    (:link :rel "stylesheet" :type "text/css" :media "screen" :href "/css/main.css" ""))
   (:body
    (:header
     (cl-markup:raw (render-menu)))
    (:main
     (cl-markup:raw object))
    (:footer
     (:script :src "/js/user.js" "")))))
