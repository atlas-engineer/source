;;; user.lisp --- classes and functions for management of user assets

(in-package :source.web)

(defun delete-key-from-authorized-keys (key)
  (let ((lines (uiop:read-file-lines source.config::*authorized-keys-path*)))
    (with-open-file (f source.config::*authorized-keys-path*
                     :direction :output
                     :if-exists :supersede
                     :if-does-not-exist :create)
      (loop for line in lines do
        (when (and (not (equalp key line))
                   (not (equalp line "")))
          (write-sequence line f)
          (write-char #\newline f)))))
  (update-authorized-keys-ownership))

(defun add-key-to-authorized-keys (key)
  (with-open-file (f source.config::*authorized-keys-path*
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (write-char #\newline f)
    (write-sequence key f))
  (update-authorized-keys-ownership))

(defun update-authorized-keys-ownership ()
  (uiop:run-program  
   (list "chown"
         (concatenate 'string source.config::*git-user* ":" source.config::*git-user*)
         source.config::*authorized-keys-path*)))

(defun update-user (username field value)
  (unless (or (not value) (equal "" value))
    (with-connection (db)
      (execute (update :user
                 (set= field value)
                 (where (:= :username username)))))))
