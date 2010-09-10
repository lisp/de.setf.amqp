;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)


(:documentation "This file defines version-specific connecttion and channel operators for AMPQ version 0.9r0
 of the `de.setf.amqp` library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


(defmethod shared-initialize ((instance amqp-1-1-0-9-0:connection) (slots t) &key)
  ;; set circumspectly in order to allow update for changed class
  (call-next-method)
  (flet ((conditionally-set (name value)
           (unless (slot-boundp instance name)
             (setf (slot-value instance name) value))))
    (conditionally-set 'input-frame-class '7-byte-header-input-frame)
    (conditionally-set 'output-frame-class '7-byte-header-output-frame)))


(defmethod connection-frame-type-class-name ((connection amqp-1-1-0-9-0:connection) code)
  (ecase code
    (0 nil)
    (1 'amqp:method)
    (2 'amqp:header)
    (3 'amqp:body)
    (4 'amqp:oob-method)
    (5 'amqp:oob-header)
    (6 'amqp:oob-body)
    (7 'amqp:trace)
    (8 'amqp:heartbeat)
    (9 'amqp:request)
    (10 'amqp:response)))

(defmethod connection-class-name-frame-type ((connection amqp-1-1-0-9-0::connection) name)
  (ecase name
    ((nil) 0)
    (amqp:method 1)
    (amqp:header 2)
    (amqp:body 3)
    (amqp:oob-method 4)
    (amqp:oob-header 5)
    (amqp:oob-body 6)
    (amqp:trace 7)
    (amqp:heartbeat 8)
    (amqp:request 9)
    (amqp:response 10)))

