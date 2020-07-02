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
	   ;; accessors for base-message
	   :conditional
	   :timestamp
	   :level
	   ;; accessor for other message fields
	   :description
	   :payload
	   ;; interface for getting formatted content
	   :format-config
	   :basic-formatter
	   :resolve-output
	   :json-format
	   :timestamp-format
	   ;; interface for message constructors and helper methods
	   :make-message
	   :export-message
	   :loggable-p))
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
    :accessor conditional
    :type boolean
    :initform t)
   (timestamp
    :reader timestamp
    :type local-time:timestamp
    :initform (now))
   (level
    :type priority
    :initarg :level
    :accessor level))
  (:documentation "basic message is a complete implementation of the
  message protocol, and is the primary form for all messages. To log
  arbitrary types, either convert them to a base-message subclass by
  implementing 'export-message' method, or implement 'export-message'
  or 'make-message' as well as 'send-message'. In most cases,
  'send-message' implementations should generally rely on the
  'format-message', 'resolve-output' and 'loggable-p' methods."))

(defclass structured-message (base-message)
  ((data
    :accessor payload
    :initarg :payload
    :initform nil))
  (:documentation "Structured messages are those which wrap alist,
  plist, hash-maps."))

(defclass simple-message (base-message)
  ((description
    :initarg :description
    :accessor description
    :initform nil
    :type string))
  (:documentation "Simple messages just wrap a single string."))

(defmethod make-message ((pri priority) (msg base-message))
  (setf (level msg) pri)
  msg)

(defmethod make-message ((pri priority) input)
  ;; if there's an export message implemented:
  (when (export-message-p input)
    (let ((out (export-message input)))
      (setf (level out) pri)
      (return-from make-message out)))

  (typecase input
    (string (make-instance 'simple-message :level pri :description input))
    (hash-table (make-instance 'structured-message :level pri :payload input))
    (property-list (make-instance 'structured-message :level pri :payload input))
    (association-list (make-instance 'structured-message :level pri :payload input))
    (otherwise (signal 'type-error "message conversion"))))

(defmethod loggable-p :around ((message base-message) (threshold priority))
  (when
      (and
       (conditional message)
       (priority>= (level message) threshold))
    (call-next-method)))

(defmethod loggable-p ((message structured-message) (threshold priority))
  (let ((data (payload message)))
    (and data
	(typecase data
	  (hash-table (<= 1 (hash-table-count data)))
	  (property-list (<= 1 (length data)))
	  (association-list (<= 1 (length data)))))))

(defmethod loggable-p ((message simple-message) (threshold priority))
  (let ((msg (description message)))
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
  ((timestamp-format
    :initform '(:year #\/ (:month 2) #\/ (:day 2) #\space (:hour 2) #\: (:min 2) #\: (:sec 2))
    :accessor timestamp-format
    :initarg :timestamp))
  (:documentation "a holder for configuration, composed by the journal
  implementation to allow pluggable message formatting."))

(defgeneric resolve-output (formater message)
  (:documentation "should produce a formatted string with data for
  human consumption in logging output. This should only include the
  message's string form and payload content, other preparation of the
  message, including metadata should be handled by 'format-message'"))

(defmethod resolve-output (formater (message simple-message))
  (description message))

(defmethod resolve-output (formater (message structured-message))
  (string-right-trim
   '(#\Space #\Tab #\Newline)
   (with-output-to-string (out)
     (let ((payload (payload message)))
       (typecase payload
	 (hash-table (maphash (lambda (k v) (format out "~A='~A' " k v)) payload))
	 (property-list (loop for (k v) on payload by #'cddr do (format out "~A='~A' " (string-downcase (symbol-name k)) v)))
	 (association-list (loop for pair on payload do (format out "~A='~A' " (caar pair) (cdar pair)))))))))
