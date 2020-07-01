(defpackage grip.level
  (:use :cl)
  (:export :priority
	   :over-threshold-p
	   :priority-string
	   :+trace+
	   :+debug+
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
    :reader value
    :initarg :value))
  (:documentation "defines a log level or priority to be associated
  with messages"))

(defgeneric over-threshold-p (level threshold))

(defmethod over-threshold-p ((level priority) (threshold priority))
  (>= (value level) (value threshold)))

(defconstant +trace+ (make-instance 'priority :value 20))
(defconstant +debug+ (make-instance 'priority :value 30))
(defconstant +info+ (make-instance 'priority :value 40))
(defconstant +notice+ (make-instance 'priority :value 50))
(defconstant +warning+ (make-instance 'priority :value 60))
(defconstant +error+ (make-instance 'priority :value 70))
(defconstant +critical+ (make-instance 'priority :value 80))
(defconstant +alert+ (make-instance 'priority :value 90))
(defconstant +emergency+ (make-instance 'priority :value 100))

(defgeneric priority-string (priority)
  (:documentation "returns the name of a priority level"))

(defmethod priority-string ((level priority))
  (let* ((val (value level))
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
