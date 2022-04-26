(defpackage :zaz-bot-api
  (:use :cl)
  (:nicknames :zaz/api)
  (:export :current-pillar-names
	   :pillar-total-votes
	   :current-projects))

(in-package :zaz-bot-api)

(defun current-pillar-names (endpoint)
  (flet ((pillar-names (list)
	   (mapcar (lambda (p) (:name p)) list)))
  (let ((page-entries 50))
    (zenon/conn:with-node-at client endpoint
      (loop
	 for page from 0
	 for result = (pillar:get-all client page page-entries)
	 while (:list result)
	 nconcing (pillar-names (:list result)) into names
	 finally (return names))))))

(defun pillar-total-votes (connection pillar-name project-ids)
  (remove-if-not #'identity
		 (accelerator:get-pillar-votes connection pillar-name project-ids)))
  
(defun current-projects (endpoint)
  (let ((page-entries 50))
    (zenon/conn:with-node-at client endpoint
      (loop
	 for page from 0
	 for result = (accelerator:get-all client page page-entries)
	 while (:list result)
	 nconcing (:list result) into projects
	 finally (return projects)))))
