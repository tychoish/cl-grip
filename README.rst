=============================================
``cl-grip`` -- Logging System for Common Lisp
=============================================

Overview
--------

``cl-grip`` is a high-level logging system for Common Lisp, focusing on a
clear user interface, support for flexible structured logging, and easy
extensibility for different backends to support direct logging to various
inputs.

Concepts
--------

Levels
~~~~~~

Grip represents levels of messages as a number between 1 and 100, with "named
levels" occurring at intervals of 10. The levels are defined as constants in
the ``grip.level`` package. The named levels are: 

- trace
- debug   
- info
- notice  
- warning
- error 
- critical   
- alert
- emergency

Loggers posses a "threshold" level and only messages with levels greater than
or equal to the threshold.

Messages
~~~~~~~~

Messages wrap data sent to the logger and contain additional metadata used in
the production of log messages. Grip's core provides a simple "string" log
message that wraps strings, as well as a "structured" message type that wraps
simple alist/plist and hashmaps.

The following generic functions control message creation and behavior:

- loggable-p
- export-message 
- make-message
- resolve-output

Loggers
~~~~~~~

Loggers store configuration and state related to the delivery of log
messages, in the case of loggers that write to streams, this may a reference
to the stream itself, or may include a message buffer, or credentials to an
external service. The primary implementation is the ``stream-logger`` which
writes all messages to the provided stream, standard output by default, or
perhaps a file. 

The following generic functions control logger behavior: 

- ``send-message`` delivers the message to the configured output, if the
  message is logable. 
- ``format-message`` formats the entire log line.
- ``log`` send a message at a specific log level. There are implementations
  that take ``base-message`` implementations others that use ``make-message``
  to convert to a message type.

By convention send-message should call ``format-message`` (which can in turn
call ``resolve-message``). 

There are also a collection of methods on ``(logger message)`` for each log
level that are implemented generically in terms of ``log``

Formatters
~~~~~~~~~~

Loggers hold a separate formating object which is responsible for configuring
the output format of log lines. These objects hold configuration and are
passed to the following generic functions:

- ``resolve-output`` is responsible for formating the output of the logging
  payload.
- ``format-message`` is responsible for formating the entire log message,
  typically by calling ``resolve-output``, but also has access to the logger
  itself, and may attach some additional metdata (e.g. the log level and
  logger name.)

Use
---

The core "grip" system has very few external dependencies, and a secondary
"ext" system contains implementations that rely on other external libraries,
which you can opt into as needed.

By default you can load the ``grip`` package and begin using the logging
methods without further configuration: there is a default logger configured so
that the following operations will work: ::
  
  (grip:info> "log message")

To send structured data: ::
  
  (grip:info> '(:msg "hello"
               :value 100))

   (let ((msg (make-hash-table :test #'string=)))
     (setf (gethash "message" msg)  "test message")
     (setf (gethash "value" msg)  val)
     (eql "value" "value")

     (grip:error> msg))

In the more advanced case, you can set up your own logger and use the
``set-logger`` function to set the global logger, as in: :: 

   (grip:set-logger (make-instance 'grip.logger:stream-journal :name "example" :output-target *error-output*))

At this point, you can use the default logging methods which will now send
standard error. Alternately, you can define a logging instance and store it
centerally (or pass it around) and pass it directly to logging methods, as in:
::

  (let ((logger (make-instance 'grip.logger:stream-journal :name "example" :output-target *error-output*)))
    (grip.logger:info> logger "hello world"))

Extension
---------

    
Development
-----------

