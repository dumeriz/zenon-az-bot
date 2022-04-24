(defpackage :zaz-bot-api
  (:use :cl)
  (:nicknames :zaz/api)
  (:export :current-projects))

(in-package :zaz-bot-api)

(defun current-projects (endpoint)
  (let ((page-entries 50))
    (zenon/conn:with-node-at client endpoint
      (loop
	 for page from 0
	 for result = (accelerator:get-all client page page-entries)
	 nconcing (:list result) into projects
	 until (< (:count result) page-entries)
	 finally (return projects)))))
