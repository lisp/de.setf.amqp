;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines the state model for AMQP classes  for the 'de.setf.amqp' library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


;;;
;;; states

(macrolet ((defstate (name supers slots &rest options)
             (setf name (intern (string name) :de.setf.amqp-state))
             (setf supers (or (mapcar #'(lambda (s) (intern (string s) :de.setf.amqp-state)) supers)
                              (unless (eq name 'amqp.s:state) '(de.setf.amqp-state:state))))
             `(prog1 (defclass ,name ,supers ,slots ,@options)
                (eval-when (:compile-toplevel :load-toplevel :execute)
                  (export ',name :de.setf.amqp-state))
                (defparameter ,name (make-instance ',name))))
           (defstates (&rest states)
             `(progn ,@(loop for state in states
                             collect (etypecase state
                                       (symbol
                                        `(defstate ,state () ()))
                                       (cons
                                        (destructuring-bind (name supers &optional slots &rest options) state
                                          `(defstate ,name ,supers ,slots ,@options ))))))))
  (defstates
    state
    connection-state
    channel-state
    open
    (open-connection (open connection-state))
    (open-connection.start (open-connection))
    (open-connection.secure (open-connection))
    (open-connection.tune (open-connection))
    (open-connection.host (open-connection))
    (open-channel (open channel-state))
    use
    (use-connection (use connection-state))
    (use-channel (use channel-state))
    body
    input
    output
    chunked
    (use-channel.body (use-channel body))
    (use-channel.body.input (use-channel.body input))
    (use-channel.body.input.chunked (chunked use-channel.body.input))
    (use-channel.body.output (use-channel.body output))
    (use-channel.body.output.chunked (chunked use-channel.body.output))
    method
    (use-channel.method (use-channel method))
    header
    (use-channel.header (use-channel header))
    heartbeat
    (use-channel.heartbeat (use-channel heartbeat))
    close
    (close-connection (close connection-state))
    (close-channel (close channel-state))))
