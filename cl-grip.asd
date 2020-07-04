(defsystem "cl-grip"
  :name "grip"
  :description "Grip is a simple logging interface and framework. The
  core package contains basic infrastructure and interfaces."
  :author "Sam Kleinman <sam@tychoish.com>"
  :license  "Apache v2"
  :version "0.0.1"
  :depends-on ("local-time" "trivial-types")
  :components ((:module "src"
		:components
		((:file "level")
		 (:file "message" :depends-on ("level"))
		 (:file "logger" :depends-on ("level" "message"))
		 (:file "grip" :depends-on ("level" "message" "logger")))))
  :in-order-to ((test-op (test-op "cl-grip/tests"))))

(defsystem "cl-grip/ext"
  :name "grip.ext"
  :description "Extensions and tools built on top of core grip interfaces."
  :author "Sam Kleinman <sam@tychoish.com>"
  :license  "Apache v2"
  :depends-on ("local-time" "local-time-duration" "cl-json" "chanl")
  :components ((:module "ext"
		:components
		((:file "buffer")
		 (:file "json")))))

(defsystem "cl-grip/tests"
  :name "grip.tests"
  :author "Sam Kleinman <sam@tychoish.com>"
  :license  "Apache v2"
  :description "test system for cl-grip"
  :perform (test-op (op c) (symbol-call :rove :run c))
  :depends-on ("cl-grip" "cl-grip/ext" "rove")
  :components ((:module "tests"
		:components
		((:file "core")
		 (:file "ext")))))
