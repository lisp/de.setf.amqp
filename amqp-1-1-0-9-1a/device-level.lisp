;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines version-specific connecttion and channel operators for AMPQ version 0.9r1
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


(defmethod shared-initialize  ((instance amqp-1-1-0-9-1:connection) (slots t) &key)
  ;; after slot-initialization, but before device-open, set frame classes
  ;; set iff unbound to permit reinitialization from change-class
  (call-next-method)
  (flet ((conditionally-set (name value)
           (unless (slot-boundp instance name)
             (setf (slot-value instance name) value))))
    (conditionally-set 'input-frame-class '7-byte-header-input-frame)
    (conditionally-set 'output-frame-class '7-byte-header-output-frame)))


(defmethod connection-frame-type-class-name ((connection amqp-1-1-0-9-1:connection) code)
  (ecase code
    (0 nil)
    (1 'amqp:method)
    (2 'amqp:header)
    (3 'amqp:body)
    (4 'amqp:heartbeat)))

(defmethod connection-class-name-frame-type ((connection amqp-1-1-0-9-1::connection) name)
  (ecase name
    ((nil) 0)
    (amqp:method 1)
    (amqp:header 2)
    (amqp:body 3)
    (amqp:heartbeat 4)))

