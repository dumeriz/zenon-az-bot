(defpackage zaz-bot-bot
  (:use :cl)
  (:nicknames :zaz/bot)
  (:export :start :send-to-chat :send-to-channel))

(in-package :zaz-bot-bot)

;;; CHANNEL SEND
;;;
;;; Unfortunately the lisp-library-bot can't send to channels yet.
;;; For that reason, an external process is called for sends to channel, implemented
;;; by telegram-send - see the *CHANNEL* parameter. That program was installed with
;;; python3-pip, which put the executable in ~/.local/bin. See the github for telegram-send
;;; on instructions regarding configuration for channel sends.

(defparameter *home* (uiop:getenv "HOME"))
(defparameter *channel* (list (str:concat *home* "/.local/bin/telegram-send") "--format" "html")
  "See https://github.com/rahiel/telegram-send")

(defparameter *testing* nil "If set, data is sent to stdout instead of *channel*")

(defun send-to-channel (html)
  "Sends `HTML' to the externally configured telegram channel via `*CHANNEL*'."
  (if *testing*
      (format t "~A~%" html)
      (let ((exec (append *channel* (list html))))
	(uiop:run-program exec))))

;;; BOT SEND
;;;
;;; The remainder of this file defines a telegram bot that could be added to private (or group?) chats.
;;; It is not used currently.

(defvar *the-bot* nil "The telegram bot instance.")
(defvar *the-chat* nil "The chat the bot is working in.")

(cl-telegram-bot:defbot zaz-bot)

(defun get-chat (chat-id)
  (when (or (null *the-chat*) (not (= chat-id (chat-id *the-chat))))
    (setq *the-chat* (cl-telegram-bot/chat:get-chat-by-id *the-bot* chat-id)))
  *the-chat*)

(defun start (token)
  (unless *the-bot*
    (setq *the-bot* (make-zaz-bot token))
    (cl-telegram-bot:start-processing *the-bot*)))

(defun send-to-chat (chat-id html)
  (cl-telegram-bot/message:send-message
   *the-bot* (get-chat chat-id) html :parse-mode :html))
  
