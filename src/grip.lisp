(defpackage grip
  (:use :cl)
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

(defvar *default-logger* (make-instance 'grip.logger:base-journal)
  "default package logger, should not be exported")

(defun log> (level message) (grip.logger:log> *default-logger* level message))
(defun trace> (msg) (grip.logger:log> *default-logger* grip.level:+trace+ msg))
(defun debug> (msg) (grip.logger:log> *default-logger* grip.level:+debug+ msg))
(defun info> (msg) (grip.logger:log> *default-logger* grip.level:+info+ msg))
(defun notice> (msg) (grip.logger:log> *default-logger* grip.level:+notice+ msg))
(defun warning> (msg) (grip.logger:log> *default-logger* grip.level:+warning+ msg))
(defun error> (msg) (grip.logger:log> *default-logger* grip.level:+error+ msg))
(defun critical> (msg) (grip.logger:log> *default-logger* grip.level:+critical+ msg))
(defun alert> (msg) (grip.logger:log> *default-logger* grip.level:+alert+ msg))
(defun emergency> (msg) (grip.logger:log> *default-logger* grip.level:+emergency+ msg))
