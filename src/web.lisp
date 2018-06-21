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
                  (setf (gethash :username *session*) username)
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
        (setf (gethash :username *session*) nil)
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
  (let ((repository-public?
          (with-connection (db)
            (retrieve-one (select :public
                            (from :repository)
                            (where (:= :name repository))))))
        (logged-in (gethash :logged-in *session*)))
    (if (or (equal 1 (cadr repository-public?))
            (and logged-in repository-public?))
        (render-page
         (cl-markup:markup
          (:h1 repository)
          (:h2 "URL")
          (:p (concatenate 'string
                           source.config::*git-url-base*
                           repository))
          (:h2 "Operations")
          (:p (:a :href (concatenate 'string
                                     "/delete/repository/confirm/"
                                     repository) "Delete Repository"))))
        (render-page
         (cl-markup:markup
          (:h1 "Repository does not exist."))))))

(defroute "/delete/repository/confirm/:repository" (&key repository)
  (if (gethash :logged-in *session*)
      (render-page
       (cl-markup:markup
        (:h1 (concatenate 'string "Delete repository? " repository))
        (:a :href (concatenate 'string "/delete/repository/" repository)
            "Click here to confirm deletion.")))
      (render-page
       (cl-markup:markup
        (:h1 "You must be logged in.")))))

(defroute "/delete/repository/:repository" (&key repository)
  (if (gethash :logged-in *session*)
      (progn
        (delete-repository repository)
        (render-page
         (cl-markup:markup
          (:h1 "Repository deleted."))))
      (render-page
       (cl-markup:markup
        (:h1 "You must be logged in.")))))

(defroute "/configure/account" ()
  (with-connection (db)
    (let* ((username (gethash :username *session* ""))
           (public-key (retrieve-one-value
                       (select :public-key
                         (from :user)
                         (where (:= :username username))))))
      (render-page
       (cl-markup:markup
        (:h1 "Account Settings")
        (:form :class "pure-form" :action "/update/account"
               (:p "Public Key")
               (:input :type "text" :name "account[public-key]"
                       :placeholder public-key)
               (:br)
               (:br)
               (:button :type "submit" :class "pure-button" "Update")))))))

(defroute "/update/account" (&key _parsed)
  (with-connection (db)
    (let* ((username (gethash :username *session* ""))
           (parsed (rest (car _parsed)))
           (new-public-key (cdr (assoc "public-key" parsed :test #'equalp)))
           (old-public-key (retrieve-one-value
                            (select :public-key
                              (from :user)
                              (where (:= :username username))))))
      (delete-key-from-authorized-keys old-public-key)
      (add-key-to-authorized-keys new-public-key)
      (execute
       (update :user
         (set= :public-key new-public-key)
         (where (:= :username username))))
      (render-page
       (cl-markup:markup
        (:h1 "Account Updated."))))))

;; Error pages
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (cl-markup:html
   (:body
    (:p "Error, page not found."))))
