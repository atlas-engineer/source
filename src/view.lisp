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
  (:export :render-page
           :render-json))
(in-package :source.view)

(defun render-menu ()
  (cl-markup:markup
   (:div :class "pure-menu pure-menu-horizontal"
         (:ul :class "pure-menu-list"
              (:li :class "pure-menu-item"
                   (:a :href "/" :class "pure-menu-link"
                       "Index"))
              (:li :class "pure-menu-item"
                   (:a :href "/create/repository" :class "pure-menu-link"
                       "Create Repository"))
              (:li :class "pure-menu-item"
                   (:a :href "/login" :class "pure-menu-link"
                       "Login"))
              (:li :class "pure-menu-item"
                   (:a :href "/logout" :class "pure-menu-link"
                       "Logout"))))))

(defun render-json (object)
  (setf (getf (response-headers *response*) :content-type) "application/json")
  (encode-json object))

(defun render-page (object)
  (cl-markup:html
   (:body
    (:meta :name "viewport" :content "width=device-width, initial-scale=1")
    (:link :rel "stylesheet" :type "text/css" :media "screen" :href "/css/pure-min.css")
    (:link :rel "stylesheet" :type "text/css" :media "screen" :href "/css/main.css")
    (cl-markup:raw (render-menu))
    (cl-markup:raw object))))
