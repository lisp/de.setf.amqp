;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines version-specific connecttion and channel operators for AMPQ version 0.8
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


(defmethod shared-initialize ((instance amqp-1-1-0-8-0:connection) (slots t) &key)
  ;; after slot-initialization, but before device-open, set frame classes
  ;; set iff unbound to permit reinitialization from change-class
  (call-next-method)
  (flet ((conditionally-set (name value)
           (unless (slot-boundp instance name)
             (setf (slot-value instance name) value))))
    (conditionally-set 'input-frame-class '7-byte-header-input-frame)
    (conditionally-set 'output-frame-class '7-byte-header-output-frame)))

(defmethod device-open ((device amqp-1-1-0-8-0:channel) #-sbcl (slot-names t) initargs)
  (etypecase (device-state device)
    (amqp.s:open-channel
     (if (or (stream-input-handle device)
             (stream-output-handle device))
       (call-next-method)
       (when (call-next-method)
         (if (zerop (channel-number device))
           device
           (destructuring-bind (&key realm &allow-other-keys) initargs
             (cond (realm
                    (assert (member realm '("/admin" "/data") :test #'string-equal) ()
                            "Invalid channel realm: ~s, ~s" device realm)
                    ;; the realm comprises the realm proper + the additions arguments:
                    ;;  exclusive passive active write read 
                    (apply #'amqp:request-request  (amqp:ensure-object device 'amqp:access
                                                                       :realm realm)
                           realm)
                    (command-case (device)
                      (amqp:request-ok ((access amqp:access) &key ticket)
                         (amqp:log :debug device "device-open: for realm: ~s, ~s"
                                   realm ticket)
                         (setf (amqp.u:channel-ticket device) ticket)
                         device)))
                   (t
                    (amqp:log :debug device "device-open: no realm.")
                    device)))))))
    (amqp.s:use-channel
     (call-next-method))))

#+(or ) ; happens in generated code
(defmethod channel-respond-to-open-ok ((channel amqp-1-1-0-8-0:channel)
                                       (connection amqp-1-1-0-8-0:connection)
                                       &key (known-hosts nil kh-s))
  (when kh-s
    (setf (amqp:connection-known-hosts connection) known-hosts)))

(defmethod connection-frame-type-class-name ((connection amqp-1-1-0-8-0:connection) type-code)
  (ecase type-code
    (0 nil)
    (1 'amqp:method)
    (2 'amqp:header)
    (3 'amqp:body)
    (4 'amqp:oob-method)
    (5 'amqp:oob-header)
    (6 'amqp:oob-body)
    (7 'amqp:trace)
    (8 'amqp:heartbeat)))


(defmethod connection-class-name-frame-type ((connection amqp-1-1-0-8-0:connection) name)
  (ecase name
    ((nil) 0)
    (amqp:method 1)
    (amqp:header 2)
    (amqp:body 3)
    (amqp:oob-method 4)
    (amqp:oob-header 5)
    (amqp:oob-body 6)
    (amqp:trace 7)
    (amqp:heartbeat 8)))
