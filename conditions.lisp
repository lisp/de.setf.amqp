;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines AMQP conditions respective versions 0-8, 0-9 as part of the 'de.setf.amqp'
 library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

  (long-description "The names were chosen to be analogous to the protocol terms as well as the names
 of RabbitMQ client[1]. The QPID client exceptions are subsumed by JSM classes[2].


 [1]: http://www.rabbitmq.com/releases/rabbitmq-java-client/v1.7.1/rabbitmq-java-client-javadoc-1.7.1/
 [2]: file:///Development/Source/production/Library/org/apache/qpid/client/qpid-0.5/api/index.html"))
 



(define-condition amqp:condition (simple-condition)
  ((message-string :initform "" :initarg :message-string :reader condition-message-string)
   (message-arguments :initform () :initarg :message-arguments :reader condition-message-arguments))
  (:report (lambda (c stream)
             (format stream "~?~%~?"
                     (simple-condition-format-control c) (simple-condition-format-arguments c)
                     (condition-message-string c) (condition-message-arguments c)))))
             

(define-condition amqp:error (simple-error amqp:condition)
  ((code :initarg :code :reader error-code)
   (class-code :initform 0 :initarg :class-code :reader error-class-code)
   (method-code :initform 0 :initarg :method-code :reader error-method-code)))


(defun amqp:error (type &rest arguments &key
                        (code 0) (class-code 0) (method-code 0)
                        (channel nil)
                        (connection (when channel (channel-connection channel)))
                        frame
                        (format-control "~a signaled [~d, ~d, ~d] in context ~s~@[, ~a~].")
                        (format-arguments (list type code class-code method-code
                                                (or channel connection) frame))
                        (message-string "") (message-arguments nil))
  "Construct the message arguments, log the error, and signal it."
  (amqp:log :error (or channel connection) "~?~%~?"
            format-control format-arguments
            message-string message-arguments)
  (apply #'error type :code code :class-code class-code :method-code method-code
         :format-control format-control :format-arguments format-arguments
         :message-string message-string :message-arguments message-arguments
         arguments))


;; the abstract errors for channels and connections bind the
;; respective contexts. the channel definition is included
;; with the others.


(define-condition connection-error (amqp:error)
  ((connection
    :initarg :connection :initform nil
    :reader condition-connection)
   (channel
    :initarg :channel :initform nil
    :reader condition-channel)
   (message
    :initform nil :initarg :message
    :reader condition-message)
   (message-arguments
    :initform nil :initarg :message-arguments
    :reader condition-message-arguments)))



(define-condition invalid-state-error (amqp:error)
  ())

(defun invalid-state-error (&rest args)
  (apply #'error 'invalid-state-error args))



(define-condition channel-condition (amqp:condition)
  ())

(define-condition channel-flow-condition (channel-condition)
  ())

(define-condition channel-flow-start-condition (channel-flow-condition)
  ())

(define-condition channel-flow-stop-condition (channel-flow-condition)
  ())

(define-condition channel-error (channel-condition connection-error)
  ())

(defmacro def-amqp-condition (name code context description &rest slots)
  (unless (search "-error" (string name) :test #'char-equal)
    (setf name (format nil "~a-~a" name :error)))
  (setf name (intern (string name) :amqp))
  `(progn ;; a prog1 here caused sbcl to object.
          ;; attempt to dump reference to obsolete class: #<SB-KERNEL::UNDEFINED-CLASSOID AMQP::CONTENT-TOO-LARGE-ERROR>
     (defun ,name (&rest args) (apply #'amqp:error ',name args))
     (define-condition ,name (,(cons-symbol *package* context  :-error))
       ((code :initform ,code :allocation :class)
        ,@slots)
       (:documentation ,(format nil "[~a] ~a" context description)))))

(macrolet ((def-amqp-conditions (&rest spec-list)
             `(progn ,@(loop for (name code context description . slots) in spec-list
                             collect `(def-amqp-condition ,name ,code ,context ,description ,@slots)))))
  (def-amqp-conditions

    (content-too-large 311 channel
     "The client attempted to transfer content larger than the server could
 accept at the present time. The client may retry at a later time.")

    (no-consumers 313 channel
     "When the exchange cannot deliver to a consumer when the immediate flag is
 set. As a result of pending data on the queue or the absence of any consumers
 of the queue.")

    (connection-forced 320 connection
     "An operator intervened to close the connection for some reason. The client 
 may retry at some later date.")

    (invalid-path 402 connection
     "The client tried to work with an unknown virtual host.")

    (access-refused 403 channel
 "The client attempted to work with a server entity to which it has no access 
 due to security settings.")

    (not-found 404 channel
     "The client attempted to work with a server entity that does not exist. ")

    (resource-locked 405 channel
     "The client attempted to work with a server entity to which it has no access 
 because another client is working with it.")

    (precondition-failed 406 channel
     "The client requested a method that was not allowed because some 
 precondition failed.")

    (frame-error 501 connection
     "The sender sent a malformed frame that the recipient could not decode. 
 This strongly implies a programming error in the sending peer.")

    (syntax-error 502 connection
     "The sender sent a frame that contained illegal values for one or more
 fields.  This strongly implies a programming error in the sending peer.")

    (command-invalid 503 connection
     "The client sent an invalid sequence of frames, attempting to perform an 
 operation that was considered invalid by the server. This usually implies a 
 programming error in the client.")

    (channel-error 504 connection
     "The client attempted to work with a channel that had not been correctly 
 opened. This most likely indicates a fault in the client layer.")

    (unexpected-frame 505 connection
     "The peer sent a frame that was not expected, usually in the context of a 
 content header and body. This strongly indicates a fault in the peer's 
 content processing."
     (frame :initarg :frame :reader condition-frame))

    (resource-error 506 connection
     "The server could not complete the method because it lacked sufficient 
 resources. This may be due to the client creating too many of some type of 
 entity.")

    (not-allowed 530 connection
     "The client tried to work with some entity in a manner that is prohibited
 by  the server, due to security settings or by some other criteria.")

    (not-implemented 540 connection
     "The client tried to use functionality that is not implemented in the
 server.")

    (internal-error 541 connection
     "The server could not complete the method because of an internal error. The 
 server may require intervention by an operator in order to resume normal 
 operations.")))


(define-condition channel-limit-reached (connection-error)
  ())



;;;
;;; handlers


(defgeneric channel-abort (object &rest args)
  (:documentation "Abort a channel.
 Given a channel, log, close and throw to channel-abort.
 Given a condition, first try for a handler - if unprocessed abort.
 Given a designator, construct the condition and continue.")

  (:method ((channel amqp:channel) &rest args)
    (amqp:log :error channel "aborting channel:~{ ~a~}." args)
    (apply #'amqp:send-close channel args)
    (close channel :abort t)
    (throw :channel-abort channel))

  (:method ((error channel-error) &rest args)
    (declare (ignore args))
    (unless (signal error)
      (let ((channel (condition-channel error)))
        (amqp:log :error channel "processing raised channel error: ~a" error)
        (channel-abort channel 
                          :reply-code (error-code error) 
                          :reply-text (format nil "~a" error)
                          :class-id (error-class-code error)
                          :method-id (error-method-code error)))))
  (:method ((error error) &rest args)
    (declare (ignore args))
    (unless (signal error)
      (let ((channel (condition-channel error)))
        (amqp:log :error channel "processing raised channel error: ~a" error)
        (channel-abort channel 
                          :reply-code 541 
                          :reply-text (format nil "~a" error)
                          :class-id 0
                          :method-id 0))))

  (:method ((condition symbol) &rest args)
    (channel-abort (apply #'make-condition condition args)))

  (:method ((condition string) &rest args)
    (channel-abort (apply #'make-condition 'channel-error condition args))))

(defgeneric connection-abort (object &rest args)
  (:documentation "Abort a connection.
 Given a connection, log, close and throw to connection-abort.
 Given a condition, first try for a handler - if unprocessed abort.
 Given a designator, construct the condition and continue.")

  (:method ((connection amqp:connection) &rest args)
    (amqp:log :error connection "aborting connection:~{ ~a~}." args)
    (apply #'amqp:send-close  connection args)
    (close connection :abort t)
    (throw :connection-abort connection))

  (:method ((error connection-error) &rest args)
    (declare (ignore args))
    (unless (signal error)
      (let ((connection (condition-connection error)))
        (amqp:log :error connection "processing raised connection error: ~a" error)
        (connection-abort connection 
                          :reply-code (error-code error) 
                          :reply-text (format nil "~a" error)
                          :class-id (error-class-code error)
                          :method-id (error-method-code error)))))
  (:method ((error error) &rest args)
    (declare (ignore args))
    (unless (signal error)
      (let ((connection (condition-connection error)))
        (amqp:log :error connection "processing raised connection error: ~a" error)
        (connection-abort connection 
                          :reply-code 541 
                          :reply-text (format nil "~a" error)
                          :class-id 0
                          :method-id 0))))

  (:method ((condition symbol) &rest args)
    (connection-abort (apply #'make-condition condition args)))

  (:method ((condition string) &rest args)
    (connection-abort (apply #'make-condition 'connection-error condition args))))

  
(defmacro handling-channel-errors (&rest body)
  `(catch :channel-abort
     (handler-bind
       ((channel-error (function channel-abort)))
       ,@body)))


(defmacro handling-connection-errors (&rest body)
  `(catch :connection-abort
     (handler-bind
       ((error (function connection-abort)))
       ,@body)))

#+mcl
(setf (ccl:assq 'handling-channel-errors ccl::*fred-special-indent-alist*) 1)
#+mcl
(setf (ccl:assq 'handling-connection-errors ccl::*fred-special-indent-alist*) 1)


