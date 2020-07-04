(defpackage test.grip.ext
  (:use :cl :rove
	:grip.ext.json)
  (:import-from :grip.logger :base-journal)
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

(deftest buffered
  (testing "journal initialization")
  (testing "journal setup")
  (testing "flushing"))
