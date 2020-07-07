(defpackage grip
  (:use :cl)
  (:import-from :grip.message :new-message)
  (:export :*default-logger*
	   :new-message
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
(defun trace> (msg) (grip.logger:trace> *default-logger* msg))
(defun debug> (msg) (grip.logger:debug> *default-logger* msg))
(defun info> (msg) (grip.logger:info> *default-logger* msg))
(defun notice> (msg) (grip.logger:notice> *default-logger* msg))
(defun warning> (msg) (grip.logger:warning> *default-logger* msg))
(defun error> (msg) (grip.logger:error> *default-logger* msg))
(defun critical> (msg) (grip.logger:critical> *default-logger* msg))
(defun alert> (msg) (grip.logger:alert> *default-logger* msg))
(defun emergency> (msg) (grip.logger:emergency> *default-logger* msg))
