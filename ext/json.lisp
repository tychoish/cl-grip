(defpackage grip.ext.json
  (:nicknames :grip.json)
  (:use :cl)
  (:import-from :trivial-types
		:association-list
		:property-list)
  (:import-from :grip.logger
		:base-journal
		:name)
  (:export :json-metadata-formatter
	   :json-simple-formatter
	   :format-message
	   :resolve-output))
(in-package :grip.ext.json)

(defclass json-base-formatter (grip.message:format-config)
  ()
  (:documentation "Basic configuration for formatting output in a JSON
  format. This class is just used to establish the correct class
  hierarchy and should not be exported or used externally."))

(defmethod resolve-output ((fmt json-base-formatter) (message grip.message:simple-message))
  (grip.message:message-payload message))

(defmethod resolve-output ((fmt json-base-formatter) (message grip.message:structured-message))
  (grip.message:message-payload message))

(defclass json-metadata-formatter (json-base-formatter)
  ()
  (:documentation "Produces formatting tools for JSON output with
  attaching metadata from the logger including logger name, level info
  and timestamp to every message. Messages will have a
  'metadata' key added with this data."))

(defclass json-simple-formatter (json-base-formatter)
  ()
  (:documentation "Provides a formating output that does to modify the
  data format with metadata, simple (string) messages have structures
  that resemble {'message': <value>}"))


(defmethod format-message ((logger base-journal) (fmt json-metadata-formatter) (message grip.message:structured-message))
  (let ((data (resolve-output fmt message))
	(ts (local-time:format-timestring nil (grip.message:message-timestamp message) :format local-time:+rfc3339-format+))
	(level (grip.level:priority-value (grip.message:message-level message)))
	(logger-name (name logger)))
    (typecase data
      (hash-table
       (let ((mdht (make-hash-table)))
	 (setf (gethash "time" mdht) ts)
	 (setf (gethash "logger" mdht) logger-name)
	 (setf (gethash "level" mdht) level)
	 (setf (gethash "metadata" data) mdht)
	 (cl-json:encode-json-to-string data)))
      (property-list
       (let ((mdal '()))
	 (push (cons "logger"  logger-name) mdal)
	 (push (cons "time"  ts) mdal)
	 (push (cons "level" level) mdal)
	 (setf (getf data :metadata) mdal)
	 (cl-json:encode-json-plist-to-string data)))
      (association-list
       (let ((mdal '()))
	 (push (cons "logger"  logger-name) mdal)
	 (push (cons "time"  ts) mdal)
	 (push (cons "level" level) mdal)
	 (push (cons "metadata" mdal) data)
	 (cl-json:encode-json-alist-to-string data))))))

(defmethod format-message ((logger base-journal) (fmt json-metadata-formatter) (message grip.message:simple-message))
  (let ((ts (local-time:format-timestring nil (grip.message:message-timestamp message) :format local-time:+rfc3339-format+))
	(level (grip.level:priority-value (grip.message:message-level message)))
	(logger-name (name logger))
	(mdal '())
	(data '()))

    (push (cons "logger"  logger-name) mdal)
    (push (cons "time"  ts) mdal)
    (push (cons "level" level) mdal)
    (push (cons "metadata" mdal) data)
    (push (cons "message" (resolve-output fmt message)) data)
    (cl-json:encode-json-alist-to-string data)))

(defmethod format-message ((logger base-journal) (fmt json-simple-formatter) (message grip.message:structured-message))
  (let ((data (resolve-output fmt message)))
    (typecase data
      (hash-table (cl-json:encode-json-to-string data))
      (property-list (cl-json:encode-json-plist-to-string data))
      (association-list (cl-json:encode-json-alist-to-string data)))))

(defmethod format-message ((logger base-journal) (fmt json-simple-formatter) (message grip.message:simple-message))
  (let ((data '()))
    (push (cons "message" (resolve-output fmt message)) data)
  (cl-json:encode-json-alist-to-string data)))
