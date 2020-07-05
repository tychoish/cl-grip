(defpackage grip.logger
  (:use :cl
	:grip.message)
  (:import-from :local-time
		:format-timestring)
  (:export ;; generic methods for managing logging interfaces and
	   ;; processes
	   :format-message
	   :send-message
	   ;; the default logging implementation and its accesses
	   :stream-journal
	   :base-journal
	   :in-memory-journal
	   :merged-journal
	   ;; logger interface
	   :name
	   :threshold
	   :output-target
	   :message-formatter
	   :merge-journals
	   ;; logging function
	   :log>
	   :trace>
	   :debug>
	   :info>
	   :notice>
	   :warning>
	   :error>
	   :critical>
	   :alert>
	   :emergency>)
  (:documentation "Logger contains basic implementation of logging
  implementations 'journal' with limited external dependencies, and
  related interface."))
(in-package :grip.logger)

(defgeneric format-message (logger formatter message)
  (:documentation "implement format-message to control message output"))

(defgeneric send-message (logger message)
  (:documentation "fundamental mesage sending method."))

(defclass base-journal ()
  ((name
    :initform "grip"
    :type string
    :accessor name
    :initarg :name)
   (threshold
    :initform grip.level:+info+
    :type grip.level:priority
    :accessor threshold
    :initarg :threshold)
   (formatter
    :type format-config
    :accessor message-formatter
    :initform (make-instance 'basic-formatter)
    :initarg :format))
  (:documentation "a basic implementation of a journal logger, that
  can be used as the super-class of most or all journal
  implementations. Generally subclasses only need to implement
  send-message"))

(defclass stream-journal (base-journal)
  ((output-target
    :type stream
    :initform *standard-output*
    :initarg :output-target
    :accessor output-target))
  (:documentation "a simple logger that writes to a specific stream"))

(defmethod format-message (logger (fmt format-config) (msg base-message))
  (format nil (resolve-output fmt msg)))

(defmethod format-message ((logger base-journal) (fmt basic-formatter) (msg base-message))
  (format nil
	  "[~A] ~A [p=~A] ~A"
	   (name logger)
	   (format-timestring nil (message-timestamp msg) :format (format-timestamp fmt))
	   (grip.level:priority-string (message-level msg))
	   (resolve-output fmt msg)))

(defmethod send-message ((logger stream-journal) (msg base-message))
  (when (loggable-p msg (threshold logger))
    (write-line (format-message logger (message-formatter logger) msg) (output-target logger))))

(defmethod send-message ((logger base-journal) (msg base-message))
  (when (loggable-p msg (threshold logger))
    (write-line (format-message logger (message-formatter logger) msg) *standard-output*)))

(defmethod send-message :around (logger (batch batch-message))
  (map nil (lambda (message)
	     (send-message logger message))
       (message-batch batch)))

(defclass merged-journal (base-journal)
  ((output-target
    :initform (make-array 0 :element-type 'base-journal :adjustable t :fill-pointer 0)
    :accessor output-target))
  (:documentation "a journal implementation that dispatches messages
  to more than one output for every message sent"))

(defgeneric merge-journals (journal-one journal-two)
  (:documentation "takes two journals and combines or merges them into
  a single stream, as best as possible"))

(defmethod merge-journals ((base merged-journal) (second merged-journal))
  (loop for journal across (output-target second)
	do (merge-journals base journal))
  base)

(defmethod merge-journals ((second base-journal) (base merged-journal))
  (merge-journals base second))

(defmethod merge-journals ((first base-journal) (second base-journal))
  (let ((out (make-instance 'merged-journal)))
    (vector-push-extend first (output-target out))
    (vector-push-extend second (output-target out))
    out))

(defmethod merge-journals ((base merged-journal) (logger base-journal))
  (vector-push-extend logger (output-target base)))

(defmethod send-message ((logger merged-journal) (msg base-message))
  (loop for journal across (output-target logger)
	do (send-message journal msg)))

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;
;;
;; a high level generic logging interface
;;
;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(defgeneric log> (logger level message)
  (:documentation "provides a basic shim between send-message and
  logging methods and makes it possible to deliver messages to
  different log levels.")
  (:method (logger level message)
    (send-message logger (grip.message:make-message (grip.level:make-priority level) message))))

(defgeneric trace> (logger message)
  (:documentation "Sends the message to a logger at level
  'trace'. This functionality is provided as a generic method to make
  it possible to extend or override for some logging
  implementations. The default implementation wraps 'log'.")
  (:method (logger message)
    (log> logger grip.level:+trace+ message)))

(defgeneric debug> (logger message)
  (:documentation "Sends the message to a logger at level
  'debug'. This functionality is provided as a generic method to make
  it possible to extend or override for some logging
  implementations. The default implementation wraps 'log'.")
  (:method (logger message)
    (log> logger grip.level:+debug+ message)))

(defgeneric info> (logger message)
  (:documentation "Sends the message to a logger at level 'info'. This
  functionality is provided as a generic method to make it possible to
  extend or override for some logging implementations. The default
  implementation wraps 'log'.")
  (:method (logger message)
    (log> logger grip.level:+info+ message)))

(defgeneric notice> (logger message)
  (:documentation "Sends the message to a logger at level
  'notice'. This functionality is provided as a generic method to make
  it possible to extend or override for some logging
  implementations. The default implementation wraps 'log'.")
  (:method (logger message)
    (log> logger grip.level:+notice+ message)))

(defgeneric warning> (logger message)
  (:documentation "Sends the message to a logger at level
  'warning'. This functionality is provided as a generic method to
  make it possible to extend or override for some logging
  implementations. The default implementation wraps 'log'.")
  (:method (logger message)
    (log> logger grip.level:+warning+ message)))

(defgeneric error> (logger message)
  (:documentation "Sends the message to a logger at level
  'debug'. This functionality is provided as a generic method to make
  it possible to extend or override for some logging
  implementations. The default implementation wraps 'log'.")
  (:method (logger message)
    (log> logger grip.level:+error+ message)))

(defgeneric critical> (logger message)
  (:documentation "Sends the message to a logger at level
  'alert'. This functionality is provided as a generic method to make
  it possible to extend or override for some logging
  implementations. The default implementation wraps 'log'.")
  (:method (logger message)
    (log> logger grip.level:+critical+ message)))

(defgeneric alert> (logger message)
  (:documentation "Sends the message to a logger at level
  'alert'. This functionality is provided as a generic method to make
  it possible to extend or override for some logging
  implementations. The default implementation wraps 'log'.")
  (:method (logger message)
    (log> logger grip.level:+alert+ message)))

(defgeneric emergency> (logger message)
  (:documentation "Sends the message to a logger at level
  'emergency'. This functionality is provided as a generic method to
  make it possible to extend or override for some logging
  implementations. The default implementation wraps 'log'.")
  (:method (logger message)
    (log> logger grip.level:+emergency+ message)))
