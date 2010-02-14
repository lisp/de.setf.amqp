;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(document :file
  (description "This file defines abstract protocol classes for AMPQ version 0.9r1 components of the
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


(defvar amqp-1-1-0-9-1::+protocol-version+
  '(:amqp 1 1 0 9 1)
  "Specifies the protocol header for the highest supported version.")



(defclass amqp-1-1-0-9-1::frame (amqp:frame)
  ((header :type (simple-array (unsigned-byte 8) (7)))))

(defclass amqp-1-1-0-9-1::input-frame (amqp-1-1-0-9-1::frame amqp:input-frame) ())
(defclass amqp-1-1-0-9-1::output-frame (amqp-1-1-0-9-1::frame amqp:output-frame) ())

(defmethod initialize-instance ((instance amqp-1-1-0-9-1::frame)
                                &rest initargs
                                &key
                                connection
                                ;; maximum payload is frame max net both the header
                                ;; _and_ the end byte (cf. http://dev.rabbitmq.com/wiki/Amqp08To091)
                                ;; in this case, 7 byte header + 1 end byte
                                (frame-size (connection-frame-size connection))
                                (header (make-array 7 :element-type '(unsigned-byte 8)))
                                (data (make-array (- frame-size 8)
                                                  :element-type '(unsigned-byte 8))))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method instance
         :header header
         :data data
         initargs))


(defclass amqp-1-1-0-9-1:object (amqp:object)
  ((version
   :initform '(1 1 0 9 1)
   :allocation :class)))

(defclass amqp-1-1-0-9-1:method (amqp:method)
  ((version
   :initform '(1 1 0 9 1)
   :allocation :class)))

(defmethod device-buffer-length ((device amqp-1-1-0-9-1:object))
  (- (connection-frame-size device) 8))


;; nb. these are specialized for the object mixin as the actual connection object
;; definition is generated into classes.lisp

(defmethod read-frame ((connection amqp-1-1-0-9-1:object) frame &rest args)
  (declare (dynamic-extent args))
  (apply #'read-7-byte-header-frame frame (stream-input-handle connection)
         args))

(defmethod write-frame ((connection amqp-1-1-0-9-1:object) frame &key (start 0) (end nil))
  (let ((stream (stream-output-handle connection)))
    (setf end (or end (frame-size frame)))
    (write-sequence (frame-header frame) stream :start start)
    (write-sequence (frame-data frame) stream :end end)
    (write-sequence #(#xCE) stream)
    (force-output stream)
    (+ end 7 1)))                       ; total count



(defmethod make-input-frame ((connection amqp-1-1-0-9-1:object) &rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance 'amqp-1-1-0-9-1::input-frame
         :connection connection
         args))

(defmethod make-output-frame ((connection amqp-1-1-0-9-1:object) &rest args)
  (declare (dynamic-extent args))
  (apply #'make-instance 'amqp-1-1-0-9-1::output-frame
         :connection connection
         args))



(defmethod frame-type ((frame amqp-1-1-0-9-1::frame))
  (buffer-unsigned-byte-8 (frame-header frame) 0))

(defmethod frame-type-class-name ((frame amqp-1-1-0-9-1::frame))
  (case (frame-type frame)
    (1 'amqp:method)
    (2 'amqp:header)
    (3 'amqp:body)
    (4 'amqp:heartbeat)
    (t (frame-type frame))))

(defmethod setf-frame-type-class-name (name (frame amqp-1-1-0-9-1::frame))
  (setf (buffer-unsigned-byte-8 (frame-header frame) 0)
        (ecase name
          (amqp:method 1)
          (amqp:header 2)
          (amqp:body 3)
          (amqp:heartbeat 4)))
  name)

(defmethod frame-cycle ((frame amqp-1-1-0-9-1::frame))
  0)

(defmethod setf-frame-cycle ((cycle t) (frame amqp-1-1-0-9-1::frame))
  cycle)

(defmethod frame-channel-number ((frame amqp-1-1-0-9-1::frame))
  (buffer-unsigned-byte-16 (frame-header frame) 1))

(defmethod setf-frame-channel-number (channel (frame amqp-1-1-0-9-1::frame))
  (setf (buffer-unsigned-byte-16 (frame-header frame) 1) channel))

(defmethod frame-track-number ((frame amqp-1-1-0-9-1::frame))
  0)

(defmethod setf-frame-track-number (value (frame amqp-1-1-0-9-1::frame))
  ;; ignored
  value)

(defmethod frame-size ((frame amqp-1-1-0-9-1::frame))
  (buffer-unsigned-byte-32 (frame-header frame) 3))

(defmethod setf-frame-size (size (frame amqp-1-1-0-9-1::frame))
  ;; (print (cons :set-size size))
  (setf (buffer-unsigned-byte-32 (frame-header frame) 3) size))

(defmethod frame-class-code ((frame amqp-1-1-0-9-1::frame))
  (buffer-unsigned-byte-16 (frame-data frame) 0))

(defmethod setf-frame-class-code (code (frame amqp-1-1-0-9-1::frame))
  (setf (buffer-unsigned-byte-16 (frame-data frame) 0) code))

(defmethod frame-method-code ((frame amqp-1-1-0-9-1::frame))
  (buffer-unsigned-byte-16 (frame-data frame) 2))

(defmethod setf-frame-method-code (code (frame amqp-1-1-0-9-1::frame))
  (setf (buffer-unsigned-byte-16 (frame-data frame) 2) code))

(defmethod method-argument-offset ((method amqp-1-1-0-9-1::method))
  4)
