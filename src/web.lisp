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

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defroute "/" ()
  (let ((repositories (if (gethash :logged-in *session*)
                          (union (list-repositories t)
                                 (list-repositories nil))
                          (list-repositories t))))
    (render-page
     (cl-markup:markup
      (:h1 "Source: The simple source repository.")
      (:h2 "Repositories:")
      (:ul
       (loop for repository in repositories
             collect
             (cl-markup:markup
              (:li (:a :href (concatenate 'string "/view/repository/" repository)
                       repository)))))))))

(defroute "/login" ()
  (if (gethash :logged-in *session*)
      (render-page
       (cl-markup:markup
        (:h1 "You are logged in.")))
      (render-page
       (cl-markup:markup
        (:h1 "Login")
        (:form :class "pure-form" :action "/login/authenticate"
               (:p "Username")
               (:input :type "text" :name "login[username]")
               (:p "Password")
               (:input :type "password" :name "login[password]")
               (:br)
               (:br)
               (:button :type "submit" :class "pure-button" "Submit"))))))

(defroute "/login/authenticate" (&key _parsed)
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
                   (cl-markup:markup
                    (:h1 (concatenate 'string
                                      "Login successful: "
                                      username)))))
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

(defroute "/create/repository" ()
  (if (gethash :logged-in *session*)
      (render-page
       (cl-markup:markup
        (:h1 "Create Repository")
        (:form :class "pure-form" :action "/create/repository/process"
           (:p "Name ([A-Za-z0-9_-]+)")
           (:input :type "text" :name "repository[name]")
           (:p "Public Visibility "
               (:input :type "checkbox" :name "repository[public]"))
           (:br)
           (:button :type "submit" :class "pure-button" "Create"))))
      (render-page
       (cl-markup:markup
        (:h1 "You must be logged in.")))))

(defroute "/create/repository/process" (&key _parsed)
  (let* ((parsed (rest (car _parsed)))
         (name (cdr (assoc "name" parsed :test #'equalp)))
         (public (cdr (assoc "public" parsed :test #'equalp))))
    (create-repository name public))
  (render-page
   (cl-markup:markup
    (:h1 "Repository created"))))

(defroute "/view/repository/:repository" (&key repository)
  (render-page
   (cl-markup:markup
    (:h1 repository)
    (:h2 "Url")
    (:p (concatenate 'string
                     source.config::*git-url-base*
                     repository))
    (:h2 "Operations")
    (:p (:a :href (concatenate 'string
                               "/delete/repository/"
                               repository)
            "Delete Repository")))))

(defroute "/delete/repository/:repository" (&key repository)
  (render-page
   (cl-markup:markup
    (:h1 (concatenate 'string "Delete repository? " repository)))))

;; Error pages
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (cl-markup:html
   (:body
    (:p "Error, page not found."))))
