(defpackage grip.message
  (:use :cl :grip.level)
  (:import-from :trivial-types
		:association-list
		:property-list)
  (:import-from :grip.level
		:priority
		:priority>=)
  (:import-from :local-time
		:now)
  (:export :base-message ;; base message class
	   :simple-message
	   :structured-message
	   :new-message
	   ;; accessors for message slots
	   :message-conditional
	   :message-timestamp
	   :message-level
	   :message-payload
	   ;; interface for getting formatted content
	   :format-config
	   :basic-formatter
	   :resolve-output
	   :json-format
	   :format-timestamp
	   ;; interface for message constructors and helper methods
	   :export-message
	   :make-message
	   :loggable-p
	   ;; batch messages
	   :merge-messages
	   :batch-message
	   :message-batch))
(in-package :grip.message)

(defgeneric make-message (level message)
  (:documentation "method to convert an arbitrary type to a message implementation"))

(defgeneric export-message (message)
  (:documentation "Tmplement export-message to support messages which can convert
 themselves into message objects."))

(defgeneric loggable-p (message threshold-level)
  (:documentation "test if a message is logable"))

(defclass base-message ()
  ((conditional
    :initarg :when
    :accessor message-conditional
    :type boolean
    :initform t)
   (timestamp
    :reader message-timestamp
    :type local-time:timestamp
    :initform (now))
   (level
    :type priority
    :initarg :level
    :initform nil
    :accessor message-level))
  (:documentation "basic message is a complete implementation of the
  message protocol, and is the primary form for all messages. To log
  arbitrary types, either convert them to a base-message subclass by
  implementing 'export-message' method, or implement 'export-message'
  or 'make-message' as well as 'send-message'. In most cases,
  'send-message' implementations should generally rely on the
  'format-message', 'resolve-output' and 'loggable-p' methods."))

(defclass structured-message (base-message)
  ((data
    :accessor message-payload
    :initarg :payload
    :initform nil))
  (:documentation "Structured messages are those which wrap alist,
  plist, hash-maps."))

(defclass simple-message (base-message)
  ((description
    :initarg :payload
    :accessor message-payload
    :initform nil
    :type string))
  (:documentation "Simple messages just wrap a single string."))

(defun new-message (input &key (level +debug+) ((:when conditional) t))
  "Constructs a new message from an input type, with optional
parameters:
LEVEL: sets the level of the message that's created, defaulting to debug.

WHEN: sets the conditional flag, making it possible to avoid wrapping
log statements in conditionals, to render a log statement
un-loggable."

  (typecase input
    (string (make-instance 'simple-message :level level :when conditional :payload input))
    (hash-table (make-instance 'structured-message :level level :when conditional :payload input))
    (property-list (make-instance 'structured-message :level level :when conditional :payload input))
    (association-list (make-instance 'structured-message :level level :when conditional :payload input))
    (base-message
     (setf (message-conditional input) conditional)
     input)
    (otherwise
     (if (export-message-p input)
	 (export-message input)
	 (signal 'type-error "message conversion")))))


(defmethod make-message ((pri priority) (msg base-message))
  (setf (message-level msg) pri)
  msg)

(defmethod make-message ((pri priority) input)
  (new-message input :level pri))

(defmethod loggable-p :around ((message base-message) (threshold priority))
  (when
      (and
       (message-conditional message)
       (priority>= (message-level message) threshold))
    (call-next-method)))

(defmethod loggable-p ((message structured-message) (threshold priority))
  (let ((data (message-payload message)))
    (and data
	(typecase data
	  (hash-table (<= 1 (hash-table-count data)))
	  (property-list (<= 1 (length data)))
	  (association-list (<= 1 (length data)))))))

(defmethod loggable-p ((message simple-message) (threshold priority))
  (let ((msg (message-payload message)))
    (and msg (not (string= "" msg)))))

(defun export-message-p (message)
  "Returns true if the message object implements has an applicable message implementation."
  (find-method #'export-message '() (mapcar #'find-class (list (class-name (class-of message)))) nil))

(defclass format-config ()
  ()
  (:documentation "the basic formater class serves as the root class
  for all formater implementations and provides a simple 'plain'
  formating output."))

(defclass basic-formatter (format-config)
  ((timestamp
    :initform '(:year #\/ (:month 2) #\/ (:day 2) #\space (:hour 2) #\: (:min 2) #\: (:sec 2))
    :accessor format-timestamp
    :initarg :timestamp))
  (:documentation "a holder for configuration, composed by the journal
  implementation to allow pluggable message formatting."))

(defgeneric resolve-output (formater message)
  (:documentation "should produce a formatted string with data for
  human consumption in logging output. This should only include the
  message's string form and payload content, other preparation of the
  message, including metadata should be handled by 'format-message'"))

(defmethod resolve-output (formater (message simple-message))
  (message-payload message))

(defmethod resolve-output (formater (message structured-message))
  (string-right-trim
   '(#\Space #\Tab #\Newline)
   (with-output-to-string (out)
     (let ((payload (message-payload message)))
       (typecase payload
	 (hash-table (maphash (lambda (k v) (format out "~A='~A' " k v)) payload))
	 (property-list (loop for (k v) on payload by #'cddr do (format out "~A='~A' " (string-downcase (symbol-name k)) v)))
	 (association-list (loop for pair on payload do (format out "~A='~A' " (caar pair) (cdar pair)))))))))

(defclass batch-message (base-message)
  ((batch
    :initform (make-array 2 :adjustable t :fill-pointer 0)
    :accessor message-batch))
  (:documentation "batch message provides an implementation of
  messages that holds a group of message objects rather than a single
  message."))

(defmethod loggable-p ((message batch-message) (threshold priority))
  (when (= 0 (length (message-batch message)))
    (return-from loggable-p nil))
  (when (and (message-conditional message) (priority>= (message-level message) threshold))
    (return-from loggable-p t)))

(defmethod (setf message-level) (level (batch batch-message))
  (map nil
       (lambda (message)
	 (unless (message-level message)
	   (setf (message-level message) level)))
       (message-batch batch)))

(defgeneric merge-messages (message1 message2)
  (:documentation "combines one or more messages into a batch message
  returning the message. If one or both of the messages is already a
  batch message then the constituent messages are produced."))

(defmethod merge-messages ((batch batch-message) (message base-message))
  (vector-push-extend message (message-batch batch))
  batch)

(defmethod merge-messages ((one base-message) (two base-message))
  (let ((batch (make-instance 'batch-message :level (message-level one))))
    (vector-push-extend one (message-batch batch))
    (vector-push-extend two (message-batch batch))

    batch))

(defmethod merge-messages ((message base-message) (batch batch-message))
  (merge-messages batch message))

(defmethod merge-messages ((one batch-message) (two batch-message))
  (when (> (length (message-batch two)) (length (message-batch one)))
    (return-from merge-messages (merge-messages two one)))

  (let ((messages (message-batch one)))
    (map nil (lambda (item) (vector-push item messages)) (message-batch two)))

  one)
