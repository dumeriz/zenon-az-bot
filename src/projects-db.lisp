(defpackage :zaz-bot-projects-db
  (:use :cl)
  (:nicknames :zaz/db)
  (:import-from :zaz-bot-projects
		:project-id
		:changed)
  (:export :try-restore
	   :make-backup
	   :insert
	   :project-ids
	   :get-project))

(in-package :zaz-bot-projects-db)

(defvar *projects* (make-hash-table :test #'equal)
  "`*PROJECTS*' is the local db - a hashtable storing projects under their `ID'.")

(defvar *backup-file* "projects-backup.cls"
  "File storing the serialization of the existing local database, see `MAKE-BACKUP'.")

(defun make-backup ()
  "Generates a backup for the current local db"
  (cl-store:store *projects* *backup-file*))

(defun try-restore ()
  "Attempts to restore the database from a backup; non-NIL only when data was found."
  ;; doing python-style conditionals
  (zaz/util:when-let (data (ignore-errors
			     (cl-store:restore *backup-file*)))
    (setf *projects* data)))

(defun get-project (id)
  "Retrieves the project stored under `ID' from the local db."
  (gethash id *projects*))

(defun project-ids ()
  "Returns the ids of all known projects."
  (alexandria:hash-table-keys *projects*))
  
(defun get-project-from-db (project)
  "Extracts the entry for `PROJECT' from the local db."
  (gethash (project-id project) *projects*))

(defun project-exists (table project)
  "`T if the `PROJECT' already exists in the HASH-TABLE `TABLE'."
  (gethash (project-id project) table))

(defun store (project)
  "Stores `PROJECT' in the local db."
  (setf (gethash (project-id project) *projects*) project))

(defun compare-with-db (projects)
  "Returns a list for every new or changed entry in `PROJECTS' wrt the local db.
`PROJECTS' must be a HASH-TABLE. The returned list of CONS-cells has the
project id in the first and the status (`:NEW', `:VOTES' or `:TIMESTAMP') in the second place."
  (flet ((compare-project (project)
	   (let ((existing (get-project-from-db project))
		 (id (project-id project)))
	     (zaz/util:when-let (status (if (null existing)
					    :new
					    (changed existing project)))
	       (cons id status)))))
    (remove-if-not #'identity
		   (mapcar #'compare-project (alexandria:hash-table-values projects)))))
       
(defun insert (projects)
  "Stores `PROJECTS' in the database and returns a change set.
For every project that is new or differs from the existing copy,
a CONS is returned with the `PROJECT-ID' in the CAR and a
change type indication in the CDR', which is one of `:NEW' (project did not exist),
`:VOTES' (voting set changed) and `:TIMESTAMP' (project was updated)."
  (let ((changes (compare-with-db projects)))
    (mapc #'(lambda (c)
	      (store (gethash (car c) projects)))
	  changes)))
