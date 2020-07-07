(defpackage test.grip.core
  (:use :cl
	:rove
	:grip.level
	:grip.message
	:grip.logger))
(in-package :test.grip.core)

(deftest level
  (testing "definition"
    (ok (= (grip.level:priority-value +trace+) 20))
    (ok (= (grip.level:priority-value +debug+) 30))
    (ok (= (grip.level:priority-value +info+) 40))
    (ok (= (grip.level:priority-value +notice+) 50))
    (ok (= (grip.level:priority-value +warning+) 60))
    (ok (= (grip.level:priority-value +error+) 70))
    (ok (= (grip.level:priority-value +critical+) 80))
    (ok (= (grip.level:priority-value +alert+) 90))
    (ok (= (grip.level:priority-value +emergency+) 100)))
  (testing "interval string values"
    (ok (string= "trace" (priority-string +trace+)))
    (ok (string= "debug" (priority-string +debug+)))
    (ok (string= "info" (priority-string +info+)))
    (ok (string= "notice" (priority-string +notice+)))
    (ok (string= "warning" (priority-string +warning+)))
    (ok (string= "error" (priority-string +error+)))
    (ok (string= "critical" (priority-string +critical+)))
    (ok (string= "alert" (priority-string +alert+)))
    (ok (string= "emergency" (priority-string +emergency+))))
  (testing "mixed string values"
    (ok (string= "info+2" (priority-string (make-instance 'priority :value 42))))
    (ok (string= "trace+2" (priority-string (make-instance 'priority :value 22))))
    (ok (string= "error+5" (priority-string (make-instance 'priority :value 75))))
    (ok (string= "alert+4" (priority-string (make-instance 'priority :value 94))))
    (ok (string= "emergency+2" (priority-string (make-instance 'priority :value 102)))))
  (testing "string production error"
    (ok (string= "9001" (grip.level::string-for-level 9001))))
  (testing "constructor"
    (ok (eq +info+ (make-priority +info+)))
    (ok (equal 70 (grip.level:priority-value (make-priority 70)))))
  (testing "priority greater than or equal"
    (testing "two priority values"
      (ok (priority>= +alert+ +error+))
      (ok (priority>= +info+ +trace+))
      (ok (priority>= +notice+ +debug+))
      (ok (priority>= +notice+ +notice+))
      (ok (not (priority>= +debug+ +info+)))
      (ok (not (priority>= +info+ +error+)))
      (ok (not (priority>= +notice+ +alert+)))
      (ok (not (priority>= +critical+ +emergency+))))
    (testing "mixed inputs"
      (ok (priority>= +alert+ 10))
      (ok (priority>= +error+ 70))
      (ok (priority>= 90 +alert+))
      (ok (priority>= 85 +error+))
      (ok (not (priority>= 10 +alert+)))
      (ok (not (priority>= 40 +error+)))
      (ok (not (priority>= +alert+ 400)))
      (ok (not (priority>= +error+ 80))))))

(deftest base-message
  (testing "conversion"
    (testing "export"
      (defmethod export-message ((fmt basic-formatter))
	(make-instance 'structured-message :payload '(:formatter "foo" :val fmt)))

      (ok (not (grip.message::export-message-p 1)))
      (ok (grip.message::export-message-p (make-instance 'basic-formatter)))
      (ok (make-message +info+ (make-instance 'basic-formatter))))
    (testing "failures"
      (ok (signals (make-message +info+ 1)))
      (ok (signals (make-message +info+ +trace+))))
    (testing "passthrough"
      (let* ((msg (make-instance 'simple-message :level +info+ :payload "hi"))
	     (converted (make-message +alert+ msg)))
	(ok (eq msg converted))
	(ok (eq +alert+ (message-level msg))))

      (let* ((msg (make-instance 'simple-message :level +info+ :payload "hi"))
	     (converted (new-message msg :level +alert+ :when nil)))
	(ok (eq msg converted))
	(ok (not (message-conditional msg))))))

  (testing "logable checks"
    (testing "simple"
      (ok (loggable-p (make-message +info+ "hi") +info+))
      (ok (not (loggable-p (make-message +debug+ "hi") +info+))))
    (testing "structured"
      (ok (loggable-p (make-message +info+ '(:a 1 :b 2)) +info+))
      (ok (loggable-p (make-message +info+ '(("a" . "b" ) ("c" . "d"))) +info+))

      (let ((ht (make-hash-table)))
	(ok (not (loggable-p (make-message +info+ ht) +info+)))
	(setf (gethash "hi" ht) "foo")
	(ok (loggable-p (make-message +info+ ht) +info+))))
    (testing "conditional logging"
      (ok (not (loggable-p (make-instance 'simple-message :level +alert+ :payload "hi" :when nil) +info+))))
    (testing "zero values are not loggable"
      (ok (not (loggable-p (make-message +info+ nil) +info+)))
      (ok (not (loggable-p (make-message +info+ "") +info+)))))

  (testing "output"
    (testing "simple"
      (ok (string= "hi" (resolve-output nil (make-instance 'simple-message :payload "hi")))))
    (testing "structured"
      (ok (string= "a='one' b='2'" (resolve-output nil (make-instance 'structured-message :payload '(:a "one" :b 2)))))
      (ok (string= "a='one' b='2'" (resolve-output nil (make-instance 'structured-message :payload '(("a" . "one") ("b" . 2))))))
      (let ((ht (make-hash-table)))
	(setf (gethash "a" ht) "one")
	(setf (gethash "b" ht) 2)
      (ok (string= "a='one' b='2'" (resolve-output nil (make-instance 'structured-message :payload ht))))))))

(deftest batch-message
  (testing "loggable"
    (ok (not (loggable-p (make-instance 'batch-message :level +info+) +info+)))
    (let ((batch (make-instance 'batch-message :level +info+)))
      (merge-messages batch (make-message +info+ "hi there"))
    (ok (loggable-p batch +info+))
    (ok (not (loggable-p batch +alert+))))

    (let ((batch (make-instance 'batch-message :level +debug+)))
      (merge-messages batch (make-message +info+ "hi there"))
      (ok (not (loggable-p batch +info+)))
      (ok (loggable-p batch +trace+))
      (setf (message-conditional batch) nil)
      (ok (not (loggable-p batch +trace+)))))

  (testing "set level"
    (testing "override"
      (let ((batch (make-instance 'batch-message :level +info+)))
	(loop repeat 5 do
	  (let ((m (make-instance 'simple-message :payload "hi there")))
	    (ok (not (message-level m)))
	    (merge-messages batch m)))
	(ok (= 5 (length (message-batch batch))))
	(setf (message-level batch) +error+)

	(loop for m across (message-batch batch) do
	  (ok (= 70  (priority-value (message-level m))))))))

    (testing "noop"
      (let ((batch (make-instance 'batch-message :level +info+)))
	(loop repeat 5 do
	  (let ((m (make-instance 'simple-message :level +info+ :payload "hi there")))
	    (ok (message-level m))
	    (merge-messages batch m)))
	(ok (= 5 (length (message-batch batch))))
	(setf (message-level batch) +error+)

	(loop for m across (message-batch batch) do
	  (ok (= 40  (priority-value (message-level m)))))))

  (testing "create batch"
    (testing "merge two base"
      (let* ((one (make-message +info+ "hi"))
	     (two (make-message +info+ "there"))
	     (merged (merge-messages one two)))
	(ok (typep merged 'batch-message))
	(ok (= 2 (length (message-batch merged))))))
    (testing "merge into first"
      (let* ((merged (make-instance 'batch-message))
	     (one (make-message +info+ "hi"))
	     (two (make-message +info+ "there")))
	(ok (= 0 (length (message-batch merged))))
	(merge-messages merged one)
	(ok (= 1 (length (message-batch merged))))
	(merge-messages merged two)
	(ok (= 2 (length (message-batch merged))))))
    (testing "merge into second"
      (let* ((merged (make-instance 'batch-message))
	     (one (make-message +info+ "hi"))
	     (two (make-message +info+ "there")))
	(ok (= 0 (length (message-batch merged))))
	(merge-messages one merged)
	(ok (= 1 (length (message-batch merged))))
	(merge-messages two merged)
	(ok (= 2 (length (message-batch merged))))))
    (testing "merge two batch"
      (let ((one (make-instance 'batch-message))
	     (two (make-instance 'batch-message))
	     (sub-one (make-message +info+ "one"))
	     (sub-two (make-message +info+ "two"))
	     (sub-three (make-message +info+ "three"))
	     (sub-four (make-message +info+ "four")))

	(merge-messages one sub-one)
	(merge-messages one sub-two)
	(merge-messages one sub-three)
	(merge-messages two sub-four)
	(let ((merged (merge-messages two one)))
	  (ok (eq merged one)))
	(ok (= 4 (length (message-batch one))))))))

(defclass in-memory-journal (base-journal)
  ((output-target
    :initform (make-array 10 :adjustable t :fill-pointer 0)
    :reader output-target))
  (:documentation "a basic logger with similar semantics to the basic
  journals but that saves "))

(defmethod send-message ((logger in-memory-journal) (msg grip.message:base-message))
  (when (loggable-p msg (threshold logger))
    (vector-push-extend msg (output-target logger))
    (format-message logger (message-formatter logger) msg)))

(deftest logger
  (testing "format specialization"
    (ok (string= "hello world" (format-message nil (make-instance 'basic-formatter) (make-message +info+ "hello world"))))
    (ok (search "[p=info] hello world" (format-message (make-instance 'base-journal)
						       (make-instance 'basic-formatter)
						       (make-message +info+ "hello world")))))
  (testing "log method"
    (loop for target in (list (make-instance 'base-journal) (make-instance 'stream-journal) (make-instance 'in-memory-journal))
	  do
	     (loop for level in (list +info+ +notice+ +warning+ +error+ +alert+ +critical+ +emergency+)
		   do
		      (loop for msg in (list "hello world" (make-instance 'simple-message :payload "hello world"))
			    do
			       (ok (search (priority-string level) (log> target level msg)))
			       (ok (not (log> target (make-instance 'priority :value (- 10 (grip.level:priority-value level))) msg)))
			       (ok (search "hello world" (log> target level msg)))))))
  (testing "merge loggers"
    (testing "combine simple"
      (let ((merge (make-instance 'merged-journal))
	    (one (make-instance 'in-memory-journal))
	    (two (make-instance 'in-memory-journal)))

	(ok (= 0 (length (output-target one))))
	(ok (= 0 (length (output-target two))))

	(ok (= 0 (length (output-target merge))))

	(merge-journals merge one)
	(ok (= 1 (length (output-target merge))))

	(merge-journals merge two)
	(ok (= 2 (length (output-target merge))))

	(info> merge "hi")

	(ok (= 1 (length (output-target one))))
	(ok (= 1 (length (output-target two)))))))

  (testing "merge peers"
    (let* ((one (make-instance 'in-memory-journal))
	   (two (make-instance 'in-memory-journal))
	   (merged (merge-journals one two)))

      (ok (= 2 (length (output-target merged))))
      (ok (= 0 (length (output-target one))))
      (ok (= 0 (length (output-target two))))

      (info> merged "hi")
      (info> merged "hi")
      (info> merged "hi")
      (ok (= 3 (length (output-target one))))
      (ok (= 3 (length (output-target two))))))

  (testing "reverse merge"
    (let* ((merged (make-instance 'merged-journal))
	   (one (make-instance 'in-memory-journal))
	   (two (make-instance 'in-memory-journal)))

      (merge-journals one merged)
      (merge-journals two merged)

      (ok (= 2 (length (output-target merged))))
      (ok (= 0 (length (output-target one))))
      (ok (= 0 (length (output-target two))))

      (info> merged "hi")
      (info> merged "hi")
      (info> merged "hi")
      (ok (= 3 (length (output-target one))))
      (ok (= 3 (length (output-target two))))))

  (testing "double"
    (let* ((merged-one (make-instance 'merged-journal))
	   (merged-two (make-instance 'merged-journal))
	   (one (make-instance 'in-memory-journal))
	   (two (make-instance 'in-memory-journal))
	   (three (make-instance 'in-memory-journal))
	   (four (make-instance 'in-memory-journal)))

      (merge-journals merged-one one)
      (merge-journals merged-one two)
      (merge-journals merged-two three)
      (merge-journals merged-two four)

      (let ((merged (merge-journals merged-one merged-two)))
	(ok (= 4 (length (output-target merged))))

	(info> merged "hi")
	(info> merged "hi")
	(info> merged "hi")
	(ok (= 3 (length (output-target one))))
	(ok (= 3 (length (output-target two))))
	(ok (= 3 (length (output-target three))))
	(ok (= 3 (length (output-target four)))))))

  (testing "level methods"
    (loop for target in (list (make-instance 'base-journal) (make-instance 'stream-journal) (make-instance 'in-memory-journal))
	  do
	     (ok (not (debug> target "hello")))
	     (ok (not (trace> target "hello")))

	     (ok (search "info" (info> target "hello world")))
	     (ok (search "hello world" (info> target "hello world")))

	     (ok (search "notice" (notice> target "hello world")))
	     (ok (search "hello world" (notice> target "hello world")))

	     (ok (search "warning" (warning> target "hello world")))
	     (ok (search "hello world" (warning> target "hello world")))

	     (ok (search "error" (error> target "hello world")))
	     (ok (search "hello world" (error> target "hello world")))

	     (ok (search "critical" (critical> target "hello world")))
	     (ok (search "hello world" (critical> target "hello world")))

	     (ok (search "alert" (alert> target "hello world")))
	     (ok (search "hello world" (alert> target "hello world")))

	     (ok (search "emergency" (emergency> target "hello world")))
	     (ok (search "hello world" (emergency> target "hello world")))))
  (testing "batches"
    (let* ((logger (make-instance 'in-memory-journal))
	   (one (make-message +info+ "hi"))
	   (two (make-message +info+ "there"))
	   (merged (merge-messages one two)))
      (ok (= 2 (length (message-batch merged))))
      (info> logger merged)
      (ok (= 2 (length (output-target logger))))
      (length (output-target logger)))))

(defun with-tmp-logger (tmp-logger function)
  (let ((std grip:*default-logger*))
    (setf grip:*default-logger* tmp-logger)
    (unwind-protect (funcall function)
      (setf grip:*default-logger* std))))

(defmacro with-logger (logger &body body)
  `(with-tmp-logger ,logger (lambda () ,@body)))

(deftest grip
  (testing "log"
    (let* ((logger (make-instance 'in-memory-journal :threshold (make-instance 'priority :value 0) :name "mem"))
	   (expected (with-logger logger (grip:log> +info+ "hi there")))
	   (output (vector-pop (output-target logger))))
      (ok (search "hi there" expected))
      (ok (search "mem" expected))
      (ok (eq +info+ (message-level output)))
      (ok (search "info" expected))
      (ok (string= "hi there" (message-payload output)))))

  (testing "trace"
    (let* ((logger (make-instance 'in-memory-journal :threshold (make-instance 'priority :value 0) :name "mem"))
	   (expected (with-logger logger (grip:trace> "hi there")))
	   (output (vector-pop (output-target logger))))
      (ok (search "hi there" expected))
      (ok (search "mem" expected))
      (ok (eq +trace+ (message-level output)))
      (ok (search "trace" expected))
      (ok (string= "hi there" (message-payload output)))))

  (testing "debug"
    (let* ((logger (make-instance 'in-memory-journal :threshold (make-instance 'priority :value 0) :name "mem"))
	   (expected (with-logger logger (grip:debug> "hi there")))
	   (output (vector-pop (output-target logger))))
      (ok (search "hi there" expected))
      (ok (search "mem" expected))
      (ok (eq +debug+ (message-level output)))
      (ok (search "debug" expected))
      (ok (string= "hi there" (message-payload output)))))

  (testing "info"
    (let* ((logger (make-instance 'in-memory-journal :threshold (make-instance 'priority :value 0) :name "mem"))
	   (expected (with-logger logger (grip:info> "hi there")))
	   (output (vector-pop (output-target logger))))
      (ok (search "hi there" expected))
      (ok (search "mem" expected))
      (ok (eq +info+ (message-level output)))
      (ok (search "info" expected))
      (ok (string= "hi there" (message-payload output)))))

  (testing "notice"
    (let* ((logger (make-instance 'in-memory-journal :threshold (make-instance 'priority :value 0) :name "mem"))
	   (expected (with-logger logger (grip:notice> "hi there")))
	   (output (vector-pop (output-target logger))))
      (ok (search "hi there" expected))
      (ok (search "mem" expected))
      (ok (eq +notice+ (message-level output)))
      (ok (search "notice" expected))
      (ok (string= "hi there" (message-payload output)))))

  (testing "warning"
    (let* ((logger (make-instance 'in-memory-journal :threshold (make-instance 'priority :value 0) :name "mem"))
	   (expected (with-logger logger (grip:warning> "hi there")))
	   (output (vector-pop (output-target logger))))
      (ok (search "hi there" expected))
      (ok (search "mem" expected))
      (ok (eq +warning+ (message-level output)))
      (ok (search "warning" expected))
      (ok (string= "hi there" (message-payload output)))))

  (testing "error"
    (let* ((logger (make-instance 'in-memory-journal :threshold (make-instance 'priority :value 0) :name "mem"))
	   (expected (with-logger logger (grip:error> "hi there")))
	   (output (vector-pop (output-target logger))))
      (ok (search "hi there" expected))
      (ok (search "mem" expected))
      (ok (eq +error+ (message-level output)))
      (ok (search "error" expected))
      (ok (string= "hi there" (message-payload output)))))

  (testing "critical"
    (let* ((logger (make-instance 'in-memory-journal :threshold (make-instance 'priority :value 0) :name "mem"))
	   (expected (with-logger logger (grip:critical> "hi there")))
	   (output (vector-pop (output-target logger))))
      (ok (search "hi there" expected))
      (ok (search "mem" expected))
      (ok (eq +critical+ (message-level output)))
      (ok (search "critical" expected))
      (ok (string= "hi there" (message-payload output)))))

  (testing "alert"
    (let* ((logger (make-instance 'in-memory-journal :threshold (make-instance 'priority :value 0) :name "mem"))
	   (expected (with-logger logger (grip:alert> "hi there")))
	   (output (vector-pop (output-target logger))))
      (ok (search "hi there" expected))
      (ok (search "mem" expected))
      (ok (eq +alert+ (message-level output)))
      (ok (search "alert" expected))
      (ok (string= "hi there" (message-payload output)))))

  (testing "emergency"
    (let* ((logger (make-instance 'in-memory-journal :threshold (make-instance 'priority :value 0) :name "mem"))
	   (expected (with-logger logger (grip:emergency> "hi there")))
	   (output (vector-pop (output-target logger))))
      (ok (search "hi there" expected))
      (ok (search "mem" expected))
      (ok (eq +emergency+ (message-level output)))
      (ok (search "emergency" expected))
      (ok (string= "hi there" (message-payload output)))))

  (ok (string= (string-upcase "base-journal") (type-of grip:*default-logger*))))
