(defpackage grip
  (:use :cl
        :grip.level
	:grip.message)
  (:import-from :grip.logger
		:set-logger
		:send-message
		:base-journal)
  (:export :set-logger
	   :log>
	   :trace>
	   :debug>
	   :info>
	   :notice>
	   :warning>
	   :error>
	   :critical>
	   :alert>
	   :emergency>))
(in-package :grip)

(defvar *default-logger* (make-instance 'base-journal)
  "default package logger, should not be exported")

(defmethod set-logger ((log base-journal))
  (setf *default-logger* log))

(defmethod log> ((logger (eql nil)) (level priority) message)
  (log> *default-logger* level msg))

(defun trace> (msg) (log> *default-logger* +trace+ msg))
(defun debug> (msg) (log> *default-logger* +debug+ msg))
(defun info> (msg) (log> *default-logger* +info+ msg))
(defun notice> (msg) (log> *default-logger* +notice+ msg))
(defun warning> (msg) (log> *default-logger* +warning+ msg))
(defun error> (msg) (log> *default-logger* +error+ msg))
(defun critical> (msg) (log> *default-logger* +critical+ msg))
(defun alert> (msg) (log> *default-logger* +alert+ msg))
(defun emergency> (msg) (log> *default-logger* +emergency+ msg))
