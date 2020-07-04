(defpackage grip.ext.buffer
  (:nicknames :grip.buffer)
  (:use :cl)
  (:import-from :grip.logger
		:base-journal)
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
	   :flush-journal
	   :close-journal))
(in-package :grip.ext.buffer)

(defclass buffered-journal (base-journal)
  ((wrapped-journal
    :type base-journal
    :reader buffer-journal
    :initarg :journal)
   (buffer-chan
    :type bounded-channel
    :accessor buffer-chan
    :initform nil)
   (size
    :type integer
    :initarg :size
    :initform 100
    :accessor buffer-size)
   (interval
    :type duration
    :initarg :interval
    :initform (duration :sec 10)
    :accessor buffer-interval)
   (worker-send
    :accessor buffer-worker
    :type bt:thread
    :initform nil)
   (worker-timer
    :accessor buffer-worker
    :type bt:thread
    :initform nil)
   (flush-chan
    :accessor buffer-flush
    :type bounded-channel
    :initform (make-instance 'bounded-channel :size 1))
   (timer-chan
    :accessor buffer-timer
    :type bounded-channel
    :initform (make-instance 'bounded-channel :size 1))
   (signal-chan
    :accessor buffer-signal
    :type bounded-channel
    :initform (make-instance 'bounded-channel :size 1)))
  (:documentation "The buffered journal wraps another journal
  implementation and buffers the underlying implementation to send
  messages on a fixed interval or after a certain volume of
  messages."))

(defmethod send-message ((journal buffered-journal) (message grip.message:base-message))
  (setup-journal journal)
  (send (buffer-chan journal) message))

(defmethod initialize-instance :after ((journal buffered-journal) &key)
  (unless (buffer-journal journal)
    (signal 'error "must specify journal to buffer"))

  (setf (buffer-chan journal) (make-instance 'bounded-channel :size (buffer-size journal)))
  (setf (buffer-timer journal) (start-timer journal))
  (setf (buffer-worker journal) (start-worker journal)))

(defmethod setup-journal ((journal buffered-journal))
  (unless (thread-alive-p (buffer-worker journal))
    (setf (buffer-worker journal) (start-worker journal)))

  (unless (thread-alive-p (buffer-timer journal))
    (setf (buffer-timer journal) (start-timer journal))))

(defmethod start-worker ((journal buffered-journal))
  (task-thread
   (pcall
      (lambda ()
	(let ((buf (make-array (buffer-size journal) :adjustable t :fill-pointer 0)))
	  (loop
	    (select
	      ((recv (buffer-flush journal) res) (flush-buffer journal buf) res)
	      ((recv (buffer-chan journal) message) (vector-push-extend message buf))
	      ((recv (buffer-signal journal) res) (flush-buffer journal buf) (return-from nil res))
	      ((recv (buffer-timer journal) res) (flush-buffer journal buf) res))
	    (when (>= (fill-pointer buf) (buffer-size journal))
	      (flush-buffer journal buf)))))
    :name "grip.ext.buffered.worker")))

(defmethod start-timer ((journal buffered-journal))
  (task-thread
   (pcall
    (lambda ()
      (loop
	(sleep (duration-as (buffer-interval journal) :sec))
	(send (buffer-timer journal) t)))
    :name "grip.ext.buffered.timer")))

(defmethod flush-buffer ((journal buffered-journal) buf)
  (unless (string= "grip.ext.buffered.worker" (thread-name (current-thread)))
    (return-from flush-buffer (send (buffer-flush journal) t)))

  ;; otherwise we're in the worker and we should flush here
  (loop for message across buf do
    (send-message journal message))
  (setf (fill-pointer buf) 0))

(defmethod close-journal ((journal buffered-journal))
  (send (buffer-signal journal) t)
  (kill (buffer-timer journal)))
