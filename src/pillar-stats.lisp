(defpackage :zaz-bot-pillar-stats
  (:use :cl)
  (:nicknames :zaz/p-stats)
  (:export :get-current
	   :to-html))

(in-package :zaz-bot-pillar-stats)

;; update runs once per day
(defvar *update-period* 86400
  "Minimum period in seconds between two updates")

(defvar *last-update* 0
  "Timestamp of the last successfull update")

(defun get-pillar-names (endpoint)
  (zaz/api:current-pillar-names endpoint))

(defstruct (pillar-participation-rate (:conc-name ppr-))
  pillar votes rate)

(defun update-due ()
  (< *update-period* (- (get-universal-time) *last-update*)))

(defun get-current (endpoint project-ids)
  (labels ((pillar-participation-rates (p-name-votes)
	     (make-pillar-participation-rate :pillar (car p-name-votes)
					     :votes (cdr p-name-votes)
					     :rate (/ (length (cdr p-name-votes))
						      (length project-ids))))
	   (run-update ()
	     (let ((pillars (get-pillar-names endpoint)))
	       (zenon/conn:with-node-at client endpoint
		 (loop for name in pillars
		    collecting (cons name
				     (zaz/api:pillar-total-votes client name project-ids))
		    into votes
		    finally (return (mapcar #'pillar-participation-rates votes)))))))
    (when (update-due)
      (zaz/util:when-let (update (ignore-errors (run-update)))
	(setq *last-update* (get-universal-time))
	update))))

(defun to-html (stats-list)
  (let ((cp (sort (copy-list stats-list)
		  (lambda (s1 s2) (< (ppr-rate s2) (ppr-rate s1))))))
    (cl-who:with-html-output-to-string (s)
      (:b "Pillar Participation rate (> 0)")"

"
      (loop
	 for stat in cp
	 until (null (ppr-votes stat))
	 do
	   (progn
	     (cl-who:htm (:i (cl-who:esc (ppr-pillar stat)) ": "))
	     (cl-who:esc (format nil "~$~%" (ppr-rate stat))))))))
