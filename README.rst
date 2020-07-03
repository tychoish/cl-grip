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
external service. The primary implementation is the ``stream-journal`` which
writes all messages to the provided stream, standard output by default, or
perhaps a file. There is also a ``base-journal`` which is used as a base class,
and unconditionally prints log lines to standard output. The
``merged-journal`` stores a vector of other journals, and has a
``send-mesage`` method that distributes messages to all loggers.

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
     (eel "value" "value")

     (grip:error> msg))

In the more advanced case, you can set up your own logger and populate the
``*default-logger*`` variable in the grip package, as in: ::

  (setf grip:*default-logger* (make-instance 'grip.logger:stream-journal :name "example" :output-target *error-output*))

At this point, you can use the default logging methods which will now send
standard error. Alternately, you can define a logging instance and store it
centerally (or pass it around) and pass it directly to logging methods, as in:
::

  (let ((logger (make-instance 'grip.logger:stream-journal :name "example" :output-target *error-output*)))
    (grip.logger:info> logger "hello world"))

Extension
---------

Grip's design privileges extensibility and simple . Message formatting, line
formating, output targets, and even logger behavior should be easy to override
and customize. This section will cover what classes you need to create and
methods you should implement.

Output Targets
~~~~~~~~~~~~~~

To write log data to a different output:

- subclass ``grip.logger:base-journal``, to store the configuration and state
  of your logger, and

- specialize the generic function ``grip.logger:send-message`` to declare how
  messages would be delivered.

Consider the following implementation, from the tests tests for a logger that
just stores messages in a vector: ::

   (defclass in-memory-journal (base-journal)
     ((output-target
       :initform (make-array 0 :adjustable t :fill-pointer t)
       :reader output-target))
     (:documentation "a basic logger with similar semantics to the basic
     journals but that saves "))

   (defmethod send-message ((logger in-memory-journal) (msg grip.message:base-message))
     (when (loggable-p msg (threshold logger))
       (vector-push-extend msg (output-target logger))
       (format-message logger (message-formatter logger) msg)))

You can choose to specialize other methods, including ``format-message``,
which takes the logger as an argument, and any of the ``grip.logger`` logging
methods (e.g. those that end in ``>``,) but that is optional.

Message Formatting
~~~~~~~~~~~~~~~~~~

There are two formating and message processing stages, first the
``resolve-output`` message process the content or payload of the message,
while the ``format-message`` calls ``resolve-output`` and packages additional
information into message. In the default case, format-message is responsible
for adding the name of the logger, the timestamp, and the log level.

The ``base-journal`` implementation has a ``formater`` slot that holds a
message format configuration object, which is passed to both formatting
functions, so that loggers can configure how messages are output.

Development
-----------

Grip is available under the terms of the Apache v2 license.

Please feel free to create issues if you experience a problem or have a
feature request. Pull requests are particularly welcome and encouraged!

In general, consider the following guidelines:

- grip aims to have full test coverage, particularly for the core system,
  although this isn't always practical. Do write tests! If you have trouble
  figuring out how to test a feature, or a change in a pull request, don't
  worry and we can work that out later.

- limit the number of dependencies in the core package. If you want to write a
  logger that

- grip uses a single package per file model. at this time, and attempts to
  limit the number of exported symbols per package.

While there is not a strong roadmap or timeline for grip, if you're interested
in contributing to grip but don't know where to start, the following
features or areas might be a good place to start:

- benchmarking: while the implementation is straight forward, it would be nice
  to know what kind of overhead the logging infrastructure takes, and some
  kind of benchmarking would be useful in determining the impact of changes.

- logging output targets. There are a number of potential logging/messaging
  output formats that could be interesting:

  - (ext) logging directly to splunk, probably using their HEC and some kind of
    message batching.

  - (ext) logging directly to the SumoLogic service, which should be broadly
    similar to splunk, but would require a separate implementation.

  - (ext) output implementations targeting "alerting workloads" including XMPP,
    slack, email, and webhook delivery.

  - (ext) output implementations that buffer outgoing messages for a period of
    time or number of messages and then send messages in batches (10s or 100
    messages). This would be multi-threaded and rely on a library like `chanl
    <https://github.com/zkat/chanl>`_.

- message handling improvements    :

  - (core) a batch message handling infrastructure, which would be a message type
    that holds a container of messages, which could be unwound as needed, and
    could be useful in the context of message buffering.

  - (core) extending the ``structured-message`` and ``make-message`` handlers
    to do better with additional input types.

  - (ext) message formating and resolution tools to support writing output to
    JSON format, with both "message only" data, as well as ways of annotating
    messages with additional metadata, including system hostname, pid, message
    time, and level. This would rely on `cl-json
    <https://github.com/hankhero/cl-Jason>`_.

  - (ext) message implementations and tooling that collect data about the
    application state.

- implementation of a stream object which wraps a logger implementation, using
  `trivial gray streams <http://www.crategus.com/books/trivial-gray-streams/>`_
  to facilitate using a logger in APIs that rely on a stream (like output of a
  file.)
