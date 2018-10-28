(in-package :cl-user)
(defpackage source.web
  (:use
   :cl
   :caveman2
   :source.config
   :source.view
   :crane)
  (:import-from
   :cl-markup
   :markup)
  (:export :*web*))
(in-package :source.web)

(defclass <web> (<app>) ())
(defvar *web* (make-instance '<web>))
(clear-routing-rules *web*)

(defmacro with-logged-in (&body body)
  `(if (gethash :logged-in *session*)
       (progn ,@body)
       (source.view::render-page
        (cl-markup:markup
         (:h1 "You are not logged in.")))))

(defroute "/" ()
  (let ((repositories
          (if (gethash :logged-in *session*)
              (union (list-repositories t)
                     (list-repositories nil))
              (list-repositories t))))
    (render-page
     (markup
      (:h1 "Source: The simple source repository.")
      (:h2 "Repositories:")
      (:ul :class "repository-list"
       (loop for repository in repositories
             collect
             (markup
              (:li (:a :href
                       (concatenate 'string "/view/repository/" (name repository))
                       (name repository))))))))))

(defroute ("/login" :method :GET) ()
  (if (gethash :logged-in *session*)
      (render-page
       (cl-markup:markup
        (:h1 "You are logged in.")))
      (render-page
       (cl-markup:markup
        (:h1 "Login")
        (:form :class "pure-form" :action "/login" :method "post"
               (:p "Username")
               (:input :type "text" :name "username")
               (:p "Password")
               (:input :type "password" :name "password")
               (:br)
               (:br)
               (:button :type "submit" :class "pure-button" "Submit"))))))

(defroute ("/login" :method :POST) (&key |username| |password|)
  (let ((user (crane:single 'user
                            :username |username|
                            :password (hash-password |password|))))
    (if user
        (progn
          (setf (gethash :logged-in *session*) t)
          (setf (gethash :email *session*) (email user))
          (render-page
           (cl-markup:markup
            (:h1 (concatenate 'string "Login successful: " (email user))))))
        (render-page
         (cl-markup:markup (:h1 "Login failed."))))))

(defroute "/logout" ()
  (with-logged-in 
    (setf (gethash :logged-in *session*) nil)
    (setf (gethash :username *session*) nil)
    (render-page
     (cl-markup:markup
      (:h1 "Logged out.")))))

(defroute ("/create/repository" :method :GET) ()
  (with-logged-in
    (render-page
     (cl-markup:markup
      (:h1 "Create Repository")
      (:form :class "pure-form" :action "/create/repository" :method "post"
             (:p "Name ([A-Za-z0-9_-]+)")
             (:input :type "text" :name "repository[name]")
             (:p "Public Visibility "
                 (:input :type "checkbox" :name "repository[public]"))
             (:br)
             (:button :type "submit" :class "pure-button" "Create"))))))

(defroute ("/create/repository" :method :POST) (&key _parsed)
  (with-logged-in
    (let* ((parsed (rest (car _parsed)))
           (name (cdr (assoc "name" parsed :test #'equalp)))
           (public (cdr (assoc "public" parsed :test #'equalp))))
      (create-repository name public))
    (render-page
     (cl-markup:markup
      (:h1 "Repository created.")))))

(defroute "/view/repository/:repository-name" (&key repository-name)
  (let* ((repository (crane:single 'repository :name repository-name))
         (repository-public? (get-repository-visibility repository))
         (logged-in (gethash :logged-in *session*)))
    (if (or repository-public? logged-in)
        (render-page
         (cl-markup:markup
          (:h1 (name repository))
          (:h2 "URL")
          (:p (concatenate 'string
                           source.config::*git-url-base*
                           (name repository)))
          (:div (when logged-in
                  (markup:raw
                   (markup
                    (:h2 "Operations")
                    (:p (:a :href (concatenate 'string
                                               "/delete/repository/confirm/"
                                               (name repository))
                            "Delete Repository"))))))))
        (render-page
         (cl-markup:markup
          (:h1 "Repository does not exist."))))))

(defroute "/delete/repository/confirm/:repository" (&key repository)
  (with-logged-in
    (render-page
     (cl-markup:markup
      (:h1 (concatenate 'string "Delete repository? " repository))
      (:a :href (concatenate 'string "/delete/repository/" repository)
          "Click here to confirm deletion.")))))

(defroute "/delete/repository/:repository" (&key repository)
  (with-logged-in
    (delete-repository repository)
    (render-page
     (cl-markup:markup
      (:h1 "Repository deleted.")))))

(defroute "/configure/account" ()
  (with-logged-in
    (with-connection (db)
      (let* ((username (gethash :username *session* ""))
             (user (car (retrieve-all (select :* (from :user)
                                        (where (:= :username username))))))
             (public-key (getf user :public-key))
             (email-address (getf user :email)))
        (render-page
         (cl-markup:markup
          (:h1 "Account Settings")
          (:p "Only enter information for updated fields. Current
          password is required to update any/all fields.")
          (:form :class "pure-form" :action "/update/account"
                 (:p "Public Key")
                 (:input :type "text" :name "account[public-key]"
                         :placeholder public-key)
                 (:p "Email Address")
                 (:input :type "text" :name "account[email]"
                         :placeholder email-address)
                 (:p "New Password")
                 (:input :type "password" :name "account[password]")
                 (:p "Current Password (required)")
                 (:input :type "password" :name "account[current-password]")
                 (:br)
                 (:br)
                 (:button :type "submit" :class "pure-button" "Update"))))))))

(defroute "/update/account" (&key _parsed)
  (with-logged-in
    (with-connection (db)
      (let* ((username (gethash :username *session* ""))
             (user (car (retrieve-all (select :* (from :user)
                                        (where (:= :username username))))))
             (current-hashed-password (getf user :password))
             (parsed (rest (car _parsed)))
             (new-public-key (cdr (assoc "public-key" parsed :test #'equalp)))
             (new-email (cdr (assoc "email" parsed :test #'equalp)))
             (new-password (cdr (assoc "password" parsed :test #'equalp)))
             (submitted-current-password (cdr (assoc "current-password" parsed :test #'equalp))))
        (if (equal (hash-password submitted-current-password)
                   current-hashed-password)
            (progn
              (update-user username :public-key new-public-key)
              (update-authorized-keys)
              (update-user username :email new-email)
              (update-user username :password
                           (if (not (equalp new-password ""))
                               (hash-password new-password)
                               nil))
              (render-page
               (cl-markup:markup
                (:h1 "Account Updated."))))
            (render-page
             (cl-markup:markup
              (:h1 "Invalid current password entered."))))))))

;; Error pages
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (cl-markup:html
   (:body
    (:p "Error, page not found."))))
