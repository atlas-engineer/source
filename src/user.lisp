;;; user.lisp --- classes and functions for management of user assets

(in-package :source.web)

(defun delete-key-from-authorized-keys (key)
  (print "key deleted"))

(defun add-key-to-authorized-keys (key)
  (with-open-file (f source.config::*authorized-keys-path*
                     :direction :output
                     :if-exists :append
                     :if-does-not-exist :create)
    (write-char #\return f)
    (write-char #\linefeed f)
    (write-sequence (concatenate 'string "\n" key) f)))
