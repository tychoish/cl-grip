(defpackage grip.logger
  (:use :cl
	:grip.message)
  (:import-from :local-time
		:format-timestring)
  (:export ;; generic methods for managing logging interfaces and
	   ;; processes
	   :format-message
	   ;; the default logging implementation and its accessors
	   :stream-journal
	   :base-journal
	   :name
	   :threshold
	   :output-target
	   :message-formatter
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
	   :emergency>))
(in-package :grip.logger)

(defgeneric format-message (logger formatter message)
  (:documentation "implement format-message to control message output"))

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
	   (format-timestring nil (timestamp msg) :format (timestamp-format fmt))
	   (grip.level:priority-string (level msg))
	   (resolve-output fmt msg)))

(defmethod send-message ((logger stream-journal) (msg base-message))
  (when (loggable-p msg (threshold logger))
    (write-line (format-message logger (message-formatter logger) msg) (output-target logger))))

(defmethod send-message ((logger base-journal) (msg base-message))
  (when (loggable-p msg (threshold logger))
    (write-line (format-message logger (message-formatter logger) msg) *standard-output*)))

(defgeneric log> (logger level message)
  (:documentation "log is the core logging method, sending a message
  object to the logger at the specified level. All other logging
  methods should be implemented in terms of log."))

(defmethod log> ((logger base-journal) (pri grip.level:priority) (message base-message))
  (setf (level message) pri)
  (send-message logger message))

(defmethod log> ((logger base-journal) (level grip.level:priority) message)
  (send-message logger (make-message level message)))

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
