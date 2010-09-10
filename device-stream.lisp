;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines a stream interface for AMQP channel and connection instances for the
 'de.setf.amqp' library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (long-description 
  "Stream-based operations are available through the `device-read-content` and `device-write-content` operators
 in the event that the size of the body datum is indeterminate. One specialization accepts a function object as
 the body. This is used to implement `with-open-channel` by passing the form body as the continuation."))


(defun call-with-channel-input-stream (operator channel &key
                                       (direction :input)
                                       (queue (error "queue argument required.")))
  (flet ((content-body-operator (channel content-type)
           (declare (ignore content-type))
           (funcall operator channel)))
    (declare (dynamic-extent #'content-body-operator))
    (assert (eq direction :input) () "Invalid direction for input: ~s." direction)
    (setf queue (etypecase queue
                  (amqp:queue queue)
                  (string (amqp:channel.queue channel :queue queue))
                  (cons (apply #'amqp:channel.queue channel queue))))
    (amqp:declare queue)
    (amqp:request-get channel :queue queue :body #'content-body-operator)))


(defun call-with-channel-output-stream (operator channel &key
                                        (direction :output)
                                        (queue (error "queue argument required."))
                                        (exchange (error "exchange argument required."))
                                        (type "direct")
                                        (routing-key "/"))
  (flet ((content-body-operator (channel content-type)
           (declare (ignore content-type))
           (funcall operator channel)))
    (declare (dynamic-extent #'content-body-operator))
    (assert (eq direction :output) () "Invalid direction for output: ~s." direction)
    (setf queue (etypecase queue
                  (amqp:queue queue)
                  (string (amqp:channel.queue channel :queue queue))
                  (cons (apply #'amqp:channel.queue channel queue))))
    (setf exchange (etypecase exchange
                     (amqp:exchange exchange)
                     (string (amqp:channel.exchange channel :exchange exchange :type type))
                     (cons (apply #'amqp:channel.exchange channel exchange))))
    (amqp:declare exchange)
    (amqp:declare queue)
    (amqp:bind queue :exchange exchange :queue queue :routing-key routing-key)
    (amqp:request-publish (amqp:basic channel) :exchange exchange :body #'content-body-operator
                          :routing-key routing-key)))


(defgeneric call-with-open-channel (operator channel &rest options)
  (:documentation "Given a channel, given a direction declare the necessary exchange/queues and call the
    function with a stream set up to read/write through the channel. if the direction is nil, apply the
    function to the un-configured channel.")

  (:method (operator (channel amqp:channel) &rest options)
    (unwind-protect
      (multiple-value-prog1 (ecase (getf options :direction)
                              (:output (apply #'call-with-channel-output-stream operator channel options))
                              (:input (apply #'call-with-channel-input-stream operator channel options))
                              ((nil)
                               (if (getf options :exchange)
                                 (apply #'call-with-channel-output-stream operator channel options)
                                 (funcall operator channel))))
        (when (open-stream-p channel)
          (close channel))
        (setf channel nil))
      (when channel (close channel :abort t)))))


(defmacro amqp:with-open-channel ((channel-var connection &rest options) &rest body)
  `(flet ((_::with-open-channel-body (,channel-var)
            ,@body))
     (declare (dynamic-extent #'_::with-open-channel-body))
     (call-with-open-channel #'_::with-open-channel-body (amqp:connection.channel ,connection :number t) ,@options)))


(defmacro amqp:with-open-connection ((connection &rest initargs) &rest body)
  (let ((op (gensym "WITH-CONNECTION-")))
    `(flet ((,op (,connection) ,@body))
       (let ((,connection (make-instance 'amqp:connection ,@initargs)))
         (unwind-protect (,op ,connection)
           (close ,connection :abort t))))))
