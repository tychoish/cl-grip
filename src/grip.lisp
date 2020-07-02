(defpackage grip
  (:use :cl)
  (:import-from :grip.logger
		:log>
		:send-message
		:base-journal)
  (:export :*default-logger*
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

(defmethod log> ((logger (eql nil)) (level grip.level:priority) message)
  (grip.logger:log> *default-logger* level message))

(defun trace> (msg) (log> *default-logger* grip.level:+trace+ msg))
(defun debug> (msg) (log> *default-logger* grip.level:+debug+ msg))
(defun info> (msg) (log> *default-logger* grip.level:+info+ msg))
(defun notice> (msg) (log> *default-logger* grip.level:+notice+ msg))
(defun warning> (msg) (log> *default-logger* grip.level:+warning+ msg))
(defun error> (msg) (log> *default-logger* grip.level:+error+ msg))
(defun critical> (msg) (log> *default-logger* grip.level:+critical+ msg))
(defun alert> (msg) (log> *default-logger* grip.level:+alert+ msg))
(defun emergency> (msg) (log> *default-logger* grip.level:+emergency+ msg))
