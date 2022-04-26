(defpackage :zaz-bot-main
  (:use :cl)
  (:nicknames :zaz)
  (:import-from :zaz/util :when-let)
  (:export :init :main :stop))

(in-package :zaz-bot-main)

(defvar *thread* nil "Stores the background update thread instance.")
(defvar *stopped* nil "When set `*THREAD*' stops. Don't set manually, use `STOP'.")

(defun get-current-projects (endpoint)
  "Retrieves the current az-projects from `ENDPOINT' and translates them into internal types."
  (let ((projects
	 (ignore-errors
	   (zaz/api:current-projects endpoint))))
    (when projects
      (zaz/projects:import-api-projects projects))))

(defun update-local-db (projects)
  "Pushes `PROJECTS' into the local db, returning a change set."
  (when projects
    (zaz/db:insert projects)))

(defun update-from (endpoint)
  "Requests the current accelerator projects and generates `PROJECT' instances.
Requests are directed to a node at `ENDPOINT'. Results are stored in the local db
and from the change set compared with its previous state, update instances are
created via `PROJECT-UPDATE-FOR-*', depending on the update type."
  (flet ((make-project-update (id-change)
	   (let ((project (zaz/db:get-project (car id-change))))
	     (ecase (cdr id-change)
	       (:new (zaz/cls:project-update-for-new project))
	       (:votes (zaz/cls:project-update-for-votes project))
	       (:timestamp (zaz/cls:project-update-for-timestamp project))))))
    (let ((changes (update-local-db
		    (get-current-projects endpoint))))
      (mapcar #'make-project-update changes))))

(defun send-updates (updates &optional chat-id)
  "Delivers the html-formatted `UPDATES' to the `ZAZ/BOT' package."
  (if chat-id ;; private bot or channel
      (loop for u in updates do (zaz/bot:send-to-chat chat-id u))
      (loop for u in updates do (zaz/bot:send-to-channel u))))

(defun maybe-send-pillar-stats (endpoint)
  (when-let (update (zaz/p-stats:get-current endpoint (zaz/db:project-ids)))
    (zaz/bot:send-to-channel (zaz/p-stats:to-html update))))

(defun run-update (endpoint &optional chat-id)
  "Initiates a backup and update of the local db from a zenon node.
The node must have the 'embedded.accelerator.getAll' api function enabled
and be reachable via `ENDPOINT'. If `CHAT-ID' is given, updates will be
delivered through configured telegram bot in the telegram chat with `CHAT-ID'.
Else, updates go through telegram-send; see the `SEND-*' methods in `ZAZ/BOT'."
  (let ((update (update-from endpoint)))
    (zaz/db:make-backup)
    (send-updates (mapcar #'zaz/cls:to-html update) chat-id)
    (maybe-send-pillar-stats endpoint)))

;; Integrated bot doesn't work with channels unfortunately
(defun init (token chat-id endpoint)
  (zaz/bot:start token)
  (zaz/db:try-restore)
  (run-update endpoint chat-id))

(defun init (endpoint)
  (zaz/db:try-restore))

(defun stop ()
  "Triggers `*THREAD*' to stop and joins it."
  (setq *stopped* t)
  (format t "Stopped; waiting for *THREAD* to join~%")
  (when *thread*
    (bt:join-thread *thread*)))

(defun wait (seconds)
  "Does nothing for `SECONDS' seconds.
Checks `*STOPPED*' every 10 seconds so that it never blocks longer if `*STOPPED' is set."
  (loop for i upto (/ seconds 10) until *stopped*
        do (sleep 10)))
        
(defun main-loop (endpoint cycle-minutes)
  "Background loop that calls `RUN-UPDATE' on the provided `ENDPOINT' every `CYCLE-MINUTES'."
  (format t "Starting background update loop~%")
  (loop
     with seconds = (* cycle-minutes 60)
     until *stopped* do
       (progn
	 (run-update endpoint)
	 (wait seconds)))
  (format t "Background update loop done~%"))

(defun main (endpoint &key (cycle-minutes 10))
  "Initializes the local db and starts `MAIN-LOOP' in a thread stored in `*THREAD*'."
  (unless *thread*
    (init endpoint)
    (setq *stopped* nil
	  *thread* (bt:make-thread
		    (lambda () (main-loop endpoint cycle-minutes))
		    :name "update-thread"))))
