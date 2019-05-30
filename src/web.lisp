(in-package :cl-user)
(defpackage source.web
  (:use
   :cl
   :caveman2
   :source.config
   :source.view
   :mito
   :sxql)
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
  (let ((user (find-user :username |username|
                         :password (hash-password |password|))))
    (if user
        (progn
          (setf (gethash :logged-in *session*) t)
          (setf (gethash :email *session*) (email user))
          (render-page
           (cl-markup:markup
            (:h2 (concatenate 'string "Login successful: " (email user))))))
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
  (let* ((repository (find-repository :name repository-name))
         (repository-public? (get-repository-visibility repository))
         (logged-in (gethash :logged-in *session*)))
    (if (or repository-public? logged-in)
        (render-page
         (cl-markup:markup
          (:h1 (name repository))
          (:h2 "URL")
          (:p (if logged-in
                  (concatenate 'string
                               source.config:*git-url-base*
                               (name repository))
                  (concatenate 'string
                               source.config:*http-url-base*
                               (name repository))))
          (:div (when logged-in
                  (markup:raw
                   (markup
                    (:h2 "Operations")
                    (:p (:a :href (concatenate 'string
                                               "/delete/repository/confirm/"
                                               (name repository))
                            "Delete Repository"))))))
          (:hr)
          (:div (markup:raw (get-repository-readme repository-name)))
          (:div (:h2 "Contribute:")
                (:p "All contributions and patches are accepted via
                email. You can use git send-email to create patches
                and send them to patch@atlas.engineer. To find out
                more about git email patches please view the official
                documentation:")
                (:p "https://git-scm.com/docs/git-send-email"))))
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

(defroute ("/account" :method :GET) ()
  (with-logged-in
    (let ((user (find-dao 'user :email (gethash :email *session*))))
      (render-page
       (cl-markup:markup
        (:h1 "My Account")
        (:h2 "Please only add values to fields you wish to change.")
        (:form :class "pure-form" :action "/account" :method "post"
               (:p "Public Key")
               (:input :type "text" :name "public-key" :placeholder (public-key user))
               (:p "Email Address")
               (:input :type "text" :name "email" :placeholder (email user))
               (:p "New Password")
               (:input :type "password" :name "password")
               (:p "Current Password (required)")
               (:input :type "password" :name "current-password")
               (:br)
               (:br)
               (:button :type "submit" :class "pure-button" "Update")))))))

(defroute ("/account" :method :POST) (&key |public-key| |email|
                                           |password|
                                           |current-password|)
  (with-logged-in
    (let ((user (find-dao 'user :email (gethash :email *session*))))
      (if (equalp (password user) (hash-password |current-password|))
          (progn
            (unless (str:emptyp |public-key|) (setf (public-key user) |public-key|))
            (unless (str:emptyp |email|) (setf (email user) |email|))
            (unless (str:emptyp |password|) (setf (password user) (hash-password |password|)))
            (save-dao user)
            (update-authorized-keys)
            (render-page
             (cl-markup:markup
              (:h1 "User Account Information Updated."))))
          (render-page
           (cl-markup:markup
            (:h1 "Invalid current password provided, please try again.")))))))


;; Error pages
(defmethod on-exception ((app <web>) (code (eql 404)))
  (declare (ignore app))
  (cl-markup:html
   (:body
    (:p "Error, page not found."))))
