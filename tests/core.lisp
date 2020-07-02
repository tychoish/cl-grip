(defpackage test.grip.core
  (:use :cl
	:rove
	:grip.level
	:grip.message))
(in-package :test.grip.core)

(deftest level
  (testing "definition"
    (ok (= (grip.level::value +trace+) 20))
    (ok (= (grip.level::value +debug+) 30))
    (ok (= (grip.level::value +info+) 40))
    (ok (= (grip.level::value +notice+) 50))
    (ok (= (grip.level::value +warning+) 60))
    (ok (= (grip.level::value +error+) 70))
    (ok (= (grip.level::value +critical+) 80))
    (ok (= (grip.level::value +alert+) 90))
    (ok (= (grip.level::value +emergency+) 100)))
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

(deftest message
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
      (let* ((msg (make-instance 'simple-message :level +info+ :description "hi"))
	     (converted (make-message +alert+ msg)))
	(ok (eq msg converted))
	(ok (eq +alert+ (level msg))))))

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
      (ok (not (loggable-p (make-instance 'simple-message :level +alert+ :description "hi" :when nil) +info+))))
    (testing "zero values are not loggable"
      (ok (not (loggable-p (make-message +info+ nil) +info+)))
      (ok (not (loggable-p (make-message +info+ "") +info+)))))

  (testing "output"
    (testing "simple"
      (ok (string= "hi" (resolve-output nil (make-instance 'simple-message :description "hi")))))
    (testing "structured"
      (ok (string= "a='one' b='2'" (resolve-output nil (make-instance 'structured-message :payload '(:a "one" :b 2)))))
      (ok (string= "a='one' b='2'" (resolve-output nil (make-instance 'structured-message :payload '(("a" . "one") ("b" . 2))))))
      (let ((ht (make-hash-table)))
	(setf (gethash "a" ht) "one")
	(setf (gethash "b" ht) 2)
      (ok (string= "a='one' b='2'" (resolve-output nil (make-instance 'structured-message :payload ht))))))))

(deftest logger
  (testing "format specialization"

    ))
