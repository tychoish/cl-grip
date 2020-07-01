(defsystem "cl-grip"
  :name "cl-grip"
  :description "Grip is a simple logging interface and framework."
  :author "Sam Kleinman <sam@tychoish.com>"
  :license  "Apache v2"
  :version "0.0.1"
  :serial t
  :depends-on ("local-time" "trivial-types")
  :components ((:module "src"
		:components 
		((:file "level")
		 (:file "message" :depends-on ("level"))
		 (:file "logger" :depends-on ("level" "message"))
		 (:file "grip" :depends-on ("level" "message" "logger"))))))


(defsystem "cl-grip/tests"
  :author "Sam Kleinman <sam@tychoish.com>"
  :license  "Apache v2"
  :description "test system for cl-grip"
  :perform (test-op (op c) (symbol-call :rove :run c))
  :depends-on ("cl-grip"
	       "rove")
  :components ((:module "tests"
		:components 
		((:file "test")))))
