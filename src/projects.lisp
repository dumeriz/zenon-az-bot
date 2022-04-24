(defpackage :zaz-bot-projects
  (:use :cl)
  (:nicknames :zaz/projects)
  (:export :project
	   :project-id
	   :project-name
	   :project-owner
	   :project-description
	   :project-url
	   :project-znn
	   :project-qsr
	   :project-created
	   :project-updated
	   :project-status
	   :project-votes
	   :votes
	   :votes-no
	   :votes-yes
	   :import-api-projects
	   :same
	   :updated))

(in-package :zaz-bot-projects)

(defstruct votes no yes)
(defstruct project id name owner description url znn qsr created updated status votes)

(defun project-from-api-type (api-project)
  "Imports the `API-PROJECT' that was returned from `CL-ZENON' into a local `PROJECT' struct.
Mainly access to some slots is simplified and the coin values are normalized to 0 decimals."
  (make-project :id (:id api-project)
		:name (:name api-project)
		:owner (:owner api-project)
		:description (:description api-project)
		:url (:url api-project)
		:znn (zaz/util:normalize-funds
		      (:znn-funds-needed api-project))
		:qsr (zaz/util:normalize-funds
		      (:qsr-funds-needed api-project))
		:created (:creation-timestamp api-project)
		:updated (:last-update-timestamp api-project)
		:votes (make-votes :no (:no (:votes api-project))
				   :yes (:yes (:votes api-project)))))

(defun project-table-from-api-result (api-projects)
  "Converts the list of api-returned data in `API-PROJECTS' into a hashtable in local representation."
  (let ((ht (make-hash-table :test #'equal))
	(projects (mapcar #'project-from-api-type api-projects)))
    (dolist (p projects) (setf (gethash (project-id p) ht) p))
    ht))

;; Wrapping PROJECT-TABLE-FROM-API-RESULT for export
(defun import-api-projects (api-projects)
  "Converts the list of api-returned data in `API-PROJECTS' into a hashtable in local representation."
  (project-table-from-api-result api-projects))

(defun same (p1 p2)
  "T if `P1' and `P2' have the same `ID'."
  (string= (project-id p1) (project-id p2)))

(defun votes-changed (p-ref p-new &key (allow-other-projects nil))
  "T if the `VOTES' field in `P-REF' differs from that in `P-NEW'.
If `ALLOW-OTHER-PROJECTS' is not T, for projects with different `ID' always NIL."
  (when (or allow-other-projects (same p-ref p-new))
    (not (and (= (votes-yes (project-votes p-ref))
		 (votes-yes (project-votes p-new)))
	      (= (votes-no (project-votes p-ref))
		 (votes-no (project-votes p-new)))))))

(defun was-updated (p-ref p-new &key (allow-other-projects nil))
  "T if the `UPDATED' field in `P-REF' differs from that in `P-NEW'.
If `ALLOW-OTHER-PROJECTS' is not T, for projects with different `ID' always `NIL'."
  (when (or allow-other-projects (same p-ref p-new))
    (not (= (project-updated p-ref)
	    (project-updated p-new)))))

(defun changed (p-ref p-new &key (allow-other-projects nil))
  "Checks if a project definition changed.
`:VOTES' if `P-NEW' has a different set of votes from `P-REF'.
`:TIMESTAMP' if the `UPDATED' field is different.
If `ALLOW-OTHER-PROJECTS' is not T, for projects with different `ID' always `NIL'."
  (cond ((votes-changed p-ref p-new :allow-other-projects allow-other-projects)
	 :votes)
	((was-updated p-ref p-new :allow-other-projects allow-other-projects)
	 :timestamp)
	(t nil)))
