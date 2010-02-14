;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(document :file
  (description "This file defines abstract protocol classes for AMPQ version 0.8 components of the
 `de.setf.amqp` library.")
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


(defvar amqp-1-1-0-8-0::+protocol-version+
  '(:amqp 1 1 0 8 0)
  "Specifies the protocol header for the highest supported version.")



(defclass amqp-1-1-0-8-0::frame (amqp::frame)
  ((input-buffer :type (simple-array (unsigned-byte 8) 8))))

(defclass amqp-1-1-0-8-0::input-frame (amqp-1-1-0-8-0::frame amqp::input-frame) ())
(defclass amqp-1-1-0-8-0::output-frame (amqp-1-1-0-8-0::frame amqp::output-frame) ())

(defmethod initialize-instance ((instance amqp-1-1-0-8-0::frame)
                                &rest initargs
                                &key
                                connection
                                ;; maximum payload is frame max net both the header
                                ;; _and_ the end byte (cf. http://dev.rabbitmq.com/wiki/Amqp08To091)
                                ;; in this case, 8 byte header + 1 end byte
                                (header (make-array 8 :element-type '(unsigned-byte 8)))
                                (data (make-array (- (connection-frame-size connection) 9)
                                                  :element-type '(unsigned-byte 8))))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method instance
         :header header
         :data data
         initargs))


(defclass amqp-1-1-0-8-0:object (amqp:object)
  ((version
   :initform '(1 1 0 8 0)
   :allocation :class)))

(defclass amqp-1-1-0-8-0:method (amqp:method)
  ((version
   :initform '(1 1 0 8 0)
   :allocation :class)))

(defmethod device-buffer-length ((device amqp-1-1-0-8-0:object))
  (- (connection-frame-size device) 9))


(defmethod device-open ((device amqp-1-1-0-8-0:object) (slot-names t) initargs)
  (etypecase (device-state device)
    (amqps:open-channel
     (if (or (stream-input-handle device)
             (stream-output-handle device))
       (call-next-method)
       (when (call-next-method)
         (destructuring-bind (&key realm ticket) initargs
           (cond ((and realm (not ticket))
                  ;; the realm comprises the realm proper + the additions arguments:
                  ;;  exclusive passive active write read 
                  (apply #'amqp:request-request  (amqp:ensure-object device 'amqp:access
                                                                     :realm realm)
                         realm)
                  (command-case (device)
                    (amqp:request-ok ((access amqp:access) &key ticket)
                     (setf (amqp.utility:channel-ticket device) ticket)
                     (return-from command-case device))))
                 (t
                  device))))))
    (amqps:use-channel
     (call-next-method))))


(defmethod read-frame ((connection amqp-1-1-0-8-0:object) frame &rest args)
  (declare (dynamic-extent args))
  (apply #'read-8-byte-header-frame frame (stream-input-handle connection)
         args))

(defmethod write-frame ((connection amqp-1-1-0-8-0:object) frame &key (start 0) (end nil))
  (let ((stream (stream-output-handle connection)))
    (setf end (or end (frame-size frame)))
    (write-sequence (frame-header frame) stream :start start)
    (write-sequence (frame-data frame) stream :end end)
    (write-sequence #(#xCE) stream)
    (force-output stream)
    (+ end 1 8)))                       ; total count



(defmethod make-input-frame ((connection amqp-1-1-0-8-0:object) &rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance 'amqp-1-1-0-8-0::input-frame
         :connection connection
         args))

(defmethod make-output-frame ((connection amqp-1-1-0-8-0:object) &rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance 'amqp-1-1-0-8-0::output-frame
         :connection connection
         args))



(defmethod frame-type ((frame amqp-1-1-0-8-0::frame))
  (buffer-unsigned-byte-8 (frame-header frame) 0))

(defmethod frame-type-class-name ((frame amqp-1-1-0-8-0::frame))
  (ecase (frame-type frame)
    (1 'amqp:method)
    (2 'amqp:header)
    (3 'amqp:body)
    (4 'amqp:oob-method)
    (5 'amqp:oob-header)
    (6 'amqp:oob-body)
    (7 'amqp:trace)
    (8 'amqp:heartbeat)))

(defmethod setf-frame-type-class-name (name (frame amqp-1-1-0-8-0::frame))
  (setf (buffer-unsigned-byte-8 (frame-header frame) 0)
        (ecase name
          (amqp:method 1)
          (amqp:header 2)
          (amqp:body 3)
          (amqp:oob-method 4)
          (amqp:oob-header 5)
          (amqp:oob-body 6)
          (amqp:trace 7)
          (amqp:heartbeat 8))))

(defmethod frame-cycle ((frame amqp-1-1-0-8-0::frame))
  (buffer-unsigned-byte-8 (frame-header frame) 1))

(defmethod frame-channel ((frame amqp-1-1-0-8-0::frame))
  (buffer-unsigned-byte-16 (frame-header frame) 2))

(defmethod frame-track ((frame amqp-1-1-0-8-0::frame))
  0)

(defmethod frame-size ((frame amqp-1-1-0-8-0::frame))
  (buffer-unsigned-byte-32 (frame-header frame) 4))

(defmethod frame-class-code ((frame amqp-1-1-0-8-0::frame))
  (buffer-unsigned-byte-16 (frame-data frame) 0))

(defmethod frame-method-code ((frame amqp-1-1-0-8-0::frame))
  (buffer-unsigned-byte-16 (frame-data frame) 2))
