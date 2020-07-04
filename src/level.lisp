(defpackage grip.level
  (:use :cl)
  (:export :priority
	   :priority-string
	   :priority-value
	   :priority>=
	   :make-priority
	   :+debug+
	   :+trace+
	   :+info+
	   :+notice+
	   :+warning+
	   :+error+
	   :+critical+
	   :+alert+
	   :+emergency+))
(in-package :grip.level)

(defclass priority ()
  ((value
    :type integer
    :reader priority-value
    :initarg :value))
  (:documentation "defines a log level or priority to be associated
  with messages"))

(defgeneric priority>= (level threshold)
  (:documentation "implements a greater than or equal to comparison
  for priority objects, as well as comparisons between numbers
  priority objects."))

(defmethod priority>= ((level priority) (threshold priority))
  (>= (priority-value level) (priority-value threshold)))

(defmethod priority>= ((level number) (threshold priority))
  (>= level (priority-value threshold)))

(defmethod priority>= ((level priority) (threshold number))
  (>= (priority-value level) threshold))

(defparameter +trace+ (make-instance 'priority :value 20))
(defparameter +debug+ (make-instance 'priority :value 30))
(defparameter +info+ (make-instance 'priority :value 40))
(defparameter +notice+ (make-instance 'priority :value 50))
(defparameter +warning+ (make-instance 'priority :value 60))
(defparameter +error+ (make-instance 'priority :value 70))
(defparameter +critical+ (make-instance 'priority :value 80))
(defparameter +alert+ (make-instance 'priority :value 90))
(defparameter +emergency+ (make-instance 'priority :value 100))

(defgeneric priority-string (priority)
  (:documentation "returns the name of a priority level"))

(defmethod priority-string ((level priority))
  (let* ((val (priority-value level))
	 (remainder (mod val 10))
	 (resolved (string-for-level (- val remainder))))
    (if (= remainder 0)
	resolved
	(format nil "~A+~D" resolved remainder))))

(defun string-for-level (value)
  (case value
    (20 "trace")
    (30 "debug")
    (40 "info")
    (50 "notice")
    (60 "warning")
    (70 "error")
    (80 "critical")
    (90 "alert")
    (100 "emergency")
    (otherwise (format nil "~D" value))))

(defun make-priority (value)
  (when (typep value 'priority)
    (return-from make-priority value))
  (make-instance 'priority :value value))
