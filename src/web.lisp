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

(defroute "/login" ()
  (render-page
   (cl-markup:markup
    (:h1 "Login")
    (:form :class "pure-form" :action "/login_authentication"
           (:p "Username")
           (:input :type "text" :name "login[username]")
           (:p "Password")
           (:input :type "password" :name "login[password]")
           (:br)
           (:br)
           (:button :type "submit" :class "pure-button" "Submit")))))

(defroute "/login_authentication" (&key _parsed)
  (let* ((credentials (car _parsed))
             (username (cdadr credentials))
             (password (hash-password (cdaddr credentials))))
        (with-connection (db)
          (let ((login-matched
                  (retrieve-one-value
                   (select :username
                     (from :user)
                     (where (:and
                             (:= :username username)
                             (:= :password password)))))))
            (if login-matched
                (progn
                  (setf (gethash :logged-in *session*) t)
                  (print (gethash :logged-in *session*))
                  (render-page
                   (cl-markup:markup (:h1 (concatenate 'string "Login successful: " username)))))
                (render-page
                 (cl-markup:markup (:h1 "Login failed."))))))))

(defroute "/logout" ()
  (if (gethash :logged-in *session*)
      ;;;;;;;;;;;;;;;;;;;;;;
      ;; Log out the user ;;
      ;;;;;;;;;;;;;;;;;;;;;;
      (progn
        (setf (gethash :logged-in *session*) nil)
        (render-page
         (cl-markup:markup
          (:h1 "Logged out."))))
      ;;;;;;;;;;;;;;;;;;;;;;;;;
      ;; User not logged in  ;;
      ;;;;;;;;;;;;;;;;;;;;;;;;;
      (render-page
       (cl-markup:markup
        (:h1 "You are not logged in.")))))

;; Error pages
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (cl-markup:html
   (:body
    (:p "Error, page not found."))))
