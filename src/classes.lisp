(defpackage :zaz-bot-classes
  (:use :cl)
  (:nicknames :zaz/cls)
  (:import-from :zaz/projects
		:project-name
		:project-description
		:project-owner
		:project-url
		:project-znn
		:project-qsr
		:project-votes
		:votes-yes
		:votes-no)
  (:export :project-update-for-new
	   :project-update-for-votes
	   :project-update-for-timestamp
	   :to-html))

(in-package :zaz-bot-classes)

(defclass project () ())

;;; Class creation for the different update types we support - new projects, new votes on projects
;;; and updated projects (the updateTimestamp field in the api result changed.)
;;; These macro calls return class definitions for types derived from class PROJECT,
;;; with slots named after the provided strings that have initializers by keyworded name and readers by symbolized name.
(zaz/util:create-const-class
 new-project project "name" "owner" "description" "url" "znn" "qsr")
(zaz/util:create-const-class
 votes-update project "name" "yes" "no")
(zaz/util:create-const-class
 updated-project project "name" "owner" "description" "url" "znn" "qsr" "yes" "no")

(defun project-update-for-new (project)
  "Returns an `NEW-PROJECT' from the details in `PROJECT'."
  (make-instance 'new-project
		 :name (project-name project)
		 :owner (project-owner project)
		 :description (project-description project)
		 :url (project-url project)
		 :znn (project-znn project)
		 :qsr (project-qsr project)))

(defun project-update-for-votes (project)
  "Returns a `VOTES-UPDATE' from the details in `PROJECT'."
  (make-instance 'votes-update
		 :name (project-name project)
		 :yes (votes-yes (project-votes project))
		 :no  (votes-no (project-votes project))))

(defun project-update-for-timestamp (project)
  "Returns an `UPDATED-PROJECT' from the details in `PROJECT'."
  (make-instance 'updated-project
		 :name (project-name project)
		 :owner (project-owner project)
		 :description (project-description project)
		 :url (project-url project)
		 :znn (project-znn project)
		 :qsr (project-qsr project)
		 :yes (votes-yes (project-votes project))
		 :no (votes-no (project-votes project))))

;;; It's unbelievable how poor the telegram html capabilities are.
;;; Why no <br> or <p> tags?
(defgeneric to-html (project)
  (:documentation "Creates a html-presentation of PROJECT.")
  (:method ((p new-project))
    (cl-who:with-html-output-to-string (s)
      (:b "New: " (cl-who:str (name p)))"
"
      (:i (cl-who:str (description p)))"

Requested funds:
"
      (:b (format s "~A ZNN, ~A QSR" (znn p) (qsr p)))"

"
      (:a :href (url p) "Proposal URL")))
  (:method ((p votes-update))
    (cl-who:with-html-output-to-string (s)
      (:b (cl-who:str (name p)) ":")"
"
      (format s "Yes: ~A, No: ~A" (yes p) (no p))))
  (:method ((p updated-project))
    (cl-who:with-html-output-to-string (s)
      (:b "Updated: " (cl-who:str (name p)))"
"
      (:i (cl-who:str (description p)))"

Requested funds:
"
      (format s "~A ZNN, ~A QSR" (znn p) (qsr p))"

Current votes: "
      (format s "Yes: ~A, No: ~A" (yes p) (no p))"

"
      (:a :href (url p) "Proposal URL"))))
