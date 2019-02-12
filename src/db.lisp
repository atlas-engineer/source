(in-package :source.web)

(deftable repository ()
  ((name
    :col-type :text
    :accessor name)
   (public
    :col-type :integer
    :accessor public)))

(defmethod print-object ((obj repository) stream)
  (print-unreadable-object (obj stream :type t)
    (with-accessors ((id object-id)
                     (name name)
                     (public public))
        obj
      (format stream "~a ~a, public? ~a" id name public))))

(defun make-repository (name &key public)
  (create-dao 'repository :name name :public public))

(defun find-repository (&key name)
  (find-dao 'repository :name name))


(deftable user ()
  ((username
    :col-type :text
    :accessor username)
   (password
    :col-type :text
    :accessor password)
   (email
    :col-type :text
    :accessor email)
   (public-key
    :col-type (or :text :null)
    :accessor public-key)))

(defmethod print-object ((user user) stream)
  (print-unreadable-object (user stream :type t)
    (with-accessors ((id object-id)
                     (username username))
        user
      (format stream "~a ~a" id username))))

(defun find-user (&key username password)
  (find-dao 'user :username username :password password))

(defun connect ()
  "Connect to the DB.
   Get the connection with mito:*connection*."
  (mito:connect-toplevel :sqlite3
                         ;; TODO: use configuration system.
                         :database-name (merge-pathnames #p"database.db"
                                                         source.config:*application-root*)))

(defun disconnect ()
  (mito:disconnect-toplevel))

(defparameter *db-tables* nil
  "List of our DB tables (for migrations and DB management.)")

;; Can Mito tell us this ?
(setf *db-tables* '(user
                    repository))

(defun ensure-tables ()
  "Create the tables if needed.
   Then, alter them with migrations."
  (mapcar #'mito:ensure-table-exists *db-tables*))

(defun migrate-all ()
  "Alter all tables.
   Need this after a change to the table definition."
  (mapcar #'mito:migrate-table *db-tables*))

(defun connect-and-migrate ()
  "Connect, ensure tables exist, run migrations."
  (connect)
  (ensure-tables)
  (migrate-all))

(defun hash-password (password)
  (ironclad:byte-array-to-hex-string
   (ironclad:digest-sequence
    :sha256
    (ironclad:ascii-string-to-byte-array password))))

(defun create-account (username password email)
  (create-dao 'user
              :username username
              :password (hash-password password)
              :email email))

(defun set-password (username password)
  (let ((user (find-dao 'user :username username)))
    (setf (password user) (hash-password password))
    (save-dao user)))

(defun delete-account (user)
  "Delete a user by its id, username or by reference."
  ;; (facility for the REPL)
  (let ((user (typecase user
                (integer (find-dao 'user :id user))
                (string (find-dao 'user :username user))
                (t user))))             ;; class of 'user
    (delete-dao user)))

(defun list-public-keys ()
  (remove nil (mapcar #'public-key
                      (select-dao 'user))))
