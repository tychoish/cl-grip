(defpackage test.grip.ext
  (:use :cl :rove
	:grip.message
	:grip.ext.json
	:grip.ext.buffer)
  (:import-from :local-time-duration :duration)
  (:import-from :grip.logger :base-journal :send-message)
  (:import-from :grip.message :make-message)
  (:import-from :grip.level :+info+))
(in-package :test.grip.ext)

(deftest json
  (let ((journal (make-instance 'base-journal)))
    (testing "plain"
      (let ((fmt (make-instance 'json-simple-formatter)))
	(testing "string"
	  (ok (string= "{\"message\":\"hello world!\"}" (format-message journal fmt (make-message +info+ "hello world!")))))
	(testing "plist"
	  (ok (string= "{\"message\":\"hello world!\",\"type\":\"plist\"}" (format-message journal fmt (make-message +info+ '(:message "hello world!" :type "plist"))))))
	(testing "alist"
	  (ok (string= "{\"message\":\"hello world!\",\"type\":\"alist\"}" (format-message journal fmt (make-message +info+ '(("message" . "hello world!") ("type" . "alist")))))))
	(testing "hash"
	  (let ((ht (make-hash-table)))
	    (setf (gethash "message" ht) "hello world!")
	    (setf (gethash "type" ht) "hashmap")
	    (ok (string= "{\"message\":\"hello world!\",\"type\":\"hashmap\"}" (format-message journal fmt (make-message +info+ ht))))))))
  (testing "metadata"
    (let ((fmt (make-instance 'json-metadata-formatter)))
      (testing "string"
	(let* ((message (make-message +info+ "hello world!"))
	       (output (format-message journal fmt message)))
	  (ok (search "metadata" output))
	  (ok (search "level" output))
	  (ok (search "40" output))
	  (ok (search "time" output))
	  (ok (search "message" output))
	  (ok (search "hello world!" output))))
      (testing "plist"
	(let* ((message (make-message +info+ '(:message "hello world!" :type "plist")))
	       (output (format-message journal fmt message)))
	  (ok (search "type" output))
	  (ok (search "plist" output))
	  (ok (search "metadata" output))
	  (ok (search "level" output))
	  (ok (search "40" output))
	  (ok (search "time" output))
	  (ok (search "message" output))
	  (ok (search "hello world!" output))))
    (testing "alist"
	(let* ((message (make-message +info+ '(("message". "hello world!") ("type" . "alist"))))
	       (output (format-message journal fmt message)))
	  (ok (search "type" output))
	  (ok (search "alist" output))
	  (ok (search "metadata" output))
	  (ok (search "level" output))
	  (ok (search "40" output))
	  (ok (search "time" output))
	  (ok (search "message" output))
	  (ok (search "hello world!" output))))
    (testing "hash"
      (let ((ht (make-hash-table)))
	(setf (gethash "message" ht) "hello world!")
	(setf (gethash "type" ht) "hashmap")

	(let* ((message (make-message +info+ ht))
	       (output (format-message journal fmt message)))
	  (ok (search "type" output))
	  (ok (search "hashmap" output))
	  (ok (search "metadata" output))
	  (ok (search "level" output))
	  (ok (search "40" output))
	  (ok (search "time" output))
	  (ok (search "message" output))
	  (ok (search "hello world!" output)))))))))

(defclass in-memory-journal (base-journal)
  ((output-target
    :initform (make-array 10 :adjustable t :fill-pointer 0)
    :reader output-target))
  (:documentation "a basic logger with similar semantics to the basic
  journals but that saves "))

(defmethod send-message ((logger in-memory-journal) (msg base-message))
  (when (loggable-p msg (grip.logger:threshold logger))
    (vector-push-extend msg (output-target logger))
    (grip.logger:format-message logger (grip.logger:message-formatter logger) msg)))


(deftest buffered
  (testing "journal setup")
  (testing "flushing"))

(defun smoke-test-buffered-journal ()
  (let* ((base (make-instance 'in-memory-journal))
	 (journal (make-instance 'buffered-journal :journal base :size 1 :interval (duration :sec 1))))
    (send-message journal (make-message +info+ "hi"))
    (close-journal journal)

    (assert (= 1 (length (output-target base))))
    (length (output-target base)))

  (let* ((base (make-instance 'in-memory-journal))
	 (journal (make-instance 'buffered-journal :journal base :size 1 :interval (duration :sec 1))))
    (grip.logger:log> journal +info+ "hello world!")
    (close-journal journal)
    (assert (= 1 (length (output-target base))))
    (length (output-target base))))

(smoke-test-buffered-journal)
