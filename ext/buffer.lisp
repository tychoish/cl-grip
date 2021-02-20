(defpackage grip.ext.buffer
  (:use :cl)
  (:import-from :grip.logger
		:base-journal
		:send-message)
  (:import-from :grip.message
		:base-message
		:batch-message
		:message-batch
		:merge-messages)
  (:import-from :local-time :now)
  (:import-from :local-time-duration
		:duration
		:duration-as
		:timestamp-difference)
  (:import-from :chanl
		:bounded-channel
		:send
		:recv
		:thread-alive-p
		:task-thread
		:pcall
		:select
		:thread-name
		:current-thread
		:kill)
  (:export :buffered-journal
	   :send-message
	   :close-journal))
(in-package :grip.ext.buffer)

(defclass buffered-journal (base-journal)
  ((wrapped-journal :reader buffer-journal :initarg :journal :type base-journal :initform nil)
   (size :accessor buffer-size :initarg :size :type integer :initform 100)
   (interval :accessor buffer-interval :initarg :interval :type duration :initform (duration :sec 10))
   (worker-send :accessor buffer-message-worker :type bt:thread :initform nil)
   (worker-timer :accessor buffer-timer-worker :type bt:thread :initform nil)
   (buffer-chan :accessor buffer-chan :type bounded-channel :initform nil)
   (timer-chan :accessor buffer-timer :type bounded-channel :initform (make-instance 'bounded-channel :size 1))
   (signal-chan :accessor buffer-signal :type bounded-channel :initform (make-instance 'bounded-channel :size 1)))
  (:documentation "The buffered journal wraps another journal
  implementation and buffers the underlying implementation to send
  messages on a fixed interval or after a certain volume of
  messages."))

(defmethod send-message ((journal buffered-journal) (message base-message))
  (setup-journal journal)
  (send (buffer-chan journal) message))

(defmethod initialize-instance :after ((journal buffered-journal) &key)
  (unless (buffer-journal journal)
    (signal 'error "must specify journal to buffer"))

  (setf (buffer-chan journal) (make-instance 'bounded-channel :size (buffer-size journal)))
  (setf (buffer-timer-worker journal) (start-timer journal))
  (setf (buffer-message-worker journal) (start-worker journal)))

(defmethod setup-journal ((journal buffered-journal))
  (let ((timer-thread (buffer-timer-worker journal))
	(message-thread (buffer-message-worker journal)))

    (unless (and message-thread (thread-alive-p message-thread))
      (setf (buffer-message-worker journal) (start-worker journal)))

    (unless (and timer-thread (thread-alive-p timer-thread))
      (setf (buffer-timer-worker journal) (start-timer journal)))))

(defmethod start-worker ((journal buffered-journal))
  (task-thread
   (pcall
      (lambda ()
	(let ((buf (make-instance 'batch-message)))
	  (loop
	    (select
	      ((recv (buffer-chan journal) message)
	       (merge-messages buf message))
	      ((recv (buffer-signal journal) res)
	       (send-message (buffer-journal journal) buf)
	       (setf buf (make-instance 'batch-message))
	       (return res))
	      ((recv (buffer-timer journal) res)
	       (send-message (buffer-journal journal) buf)
	       (setf buf (make-instance 'batch-message)) res))

	    (when (>= (length (message-batch buf)) (buffer-size journal))
	      (send-message (buffer-journal journal) buf)
	      (setf buf (make-instance 'batch-message))))))
    :name "grip.ext.buffered.worker")))

(defmethod start-timer ((journal buffered-journal))
  (task-thread
   (pcall
    (lambda ()
      (loop
	(sleep (duration-as (buffer-interval journal) :sec))
	(send (buffer-timer journal) t)))
    :name "grip.ext.buffered.timer")))

(defmethod close-journal ((journal buffered-journal))
  (send (buffer-signal journal) t)

  (with-accessors ((timer-thread buffer-timer-worker)
		   (message-thread buffer-message-worker))
      journal

    (when (and timer-thread (thread-alive-p timer-thread))
      (kill timer-thread)
      (setf timer-thread nil))

    (when (and message-thread (thread-alive-p message-thread))
      (sleep 0.5) ;; this is just a kindness to allow the last flush to go through
      (kill message-thread)
      (setf message-thread nil))))
