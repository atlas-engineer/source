(in-package :source.web)

(deftable repository ()
  (name :type text)
  (public :type integer))

(deftable user ()
  (username :type text)
  (password :type text)
  (email :type text)
  (public-key :type text))

(defun hash-password (password)
  (ironclad:byte-array-to-hex-string 
   (ironclad:digest-sequence 
    :sha256
    (ironclad:ascii-string-to-byte-array password))))

(defun create-account (username password email)
  (crane:save
   (crane:create
    'user
    :username username
    :password (hash-password password)
    :email email)))

(defun set-password (username password)
  (let ((user (crane:single 'user :username username)))
    (setf (password user) (hash-password password))
    (crane:save user)))

(defun delete-account (username)
  (let ((user (crane:single 'user :username username)))
    (crane:del user)))

