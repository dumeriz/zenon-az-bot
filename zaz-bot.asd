(defsystem "zaz-bot"
  :version "0.1.0"
  :author "Dumeril"
  :license "MIT"
  :depends-on ("alexandria"
	       "uiop"
	       "str"
	       "bordeaux-threads"
               "jsonrpc"
	       "cl-store"
	       "cl-telegram-bot"
	       "cl-who"
	       "cl-zenon")

  :components ((:module "src"
		:serial t
                :components
                ((:file "util")
		 (:file "api")
		 (:file "projects")
		 (:file "projects-db")
		 (:file "classes")
		 (:file "bot")
		 (:file "main"))))
  :description "Bot to update a telegram channel with information about zenon accelerator-z projects")
