;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines defines the absolutely minimal `simple-stream` class for the
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

 (long-description "This defines enough of the simple sream framework to implement connection operators
 compatible with simple streams. The classes derive from the sbcl port mostly by deleting things."))


(when (intersection '(:allegro :sbcl) *features*)
  ;; this cannot be permitted as it conflicts with extant definitions 
  (error "DO NOT load this into a runtime which implements simple-streams."))


(deftype simple-stream-buffer () '(SIMPLE-ARRAY (UNSIGNED-BYTE 8) (*)))

(defmacro add-stream-instance-flags (stream &rest flags)
  "In this emulated setting, there's nothing happening here."
  (declare (ignore stream flags))
  (values))

;; define the basic simple-streams device interface

(defgeneric device-open (stream slots initargs)
  (:documentation "Make the connection between the stream structure and the
 actual device being opened, if appropriate, and completes the initialization
 of the stream structure adequately enough to perform operations on the
 stream."))

(defgeneric device-close (stream abort)
  (:documentation "Breaks the connection to the device and resets internal
 state to mark the stream as closed and preclude its use from standard i/o
 operations."))

(defgeneric device-buffer-length (stream)
  (:documentation "Returns the desired length of buffers to be allocated for
 the stream, if any."))

(defgeneric device-file-position (stream))

(defgeneric (setf device-file-position) (value stream))

(defgeneric device-file-length (stream))

(defgeneric device-read (stream buffer start end blocking))

(defgeneric device-clear-input (stream buffer-only))

(defgeneric device-write (stream buffer start end blocking))

(defgeneric device-clear-output (stream))

(defgeneric device-finish-record (stream blocking action))


;; plus, the alternative fu interface

(defgeneric device-allocate-buffer (stream &key length initial-contents)
  )

(defgeneric device-input-element-type (stream)
  )

(defgeneric device-output-element-type (stream)
  )

(defgeneric device-encoded-length (stream buffer &optional start end)
  )

(defgeneric device-write-buffers (stream  &rest args)
  )

(defgeneric device-read-buffers (stream &rest args)
  )


;; add a flag to indicate whether to pad the content body since this is
;; independent of closing the channel/stream
(defgeneric device-flush (stream &optional complete)
  )

;; the essential classes are the simple stream classitself and the
;; socket stream specializations. these are excerpted from the sbcl port

(defclass simple-stream (#-lispworks stream
                         #+lispworks stream:fundamental-input-stream
                         #+lispworks stream:fundamental-output-stream)
  ((plist
    :initform nil :type list
    :accessor stream-plist)
   (external-format
    :initarg :external-format
    :accessor stream-external-format
    :documentation "The external format is used to decide character
 data encodings for content only.")
   (input-handle
    :initform nil :initarg :input-handle :type (or null stream)
    :accessor stream-input-handle
    :documentation "Bound to the socket's stream while open.")
   (output-handle
    :initform nil :initarg :output-handle :type (or null stream)
    :accessor stream-output-handle
    :documentation "Bound to the same value as input-handle.")
   (buffer
    :initform nil :type (or simple-stream-buffer null)
    :reader stream-buffer :writer set-stream-buffer
    :documentation "This is the buffer used most recently for i/o.
 It is initialized and maintained by specialized classes.")
   (buffpos
    :initform 0 :type fixnum
    :documentation "The position resulting from the last operation:
 on input the next byte to get.
 on output the next postiion to store.")
   (buffer-ptr
    :initform 0 :type fixnum
    :documentation "Maximum valid position in buffer, or -1 on eof.
 on input, after the last read byte.
 on output, the length of the byffer.")
   (buf-len 
    :initform 0 :type fixnum
    :documentation "The (usable) length of the buffer.")
   (pending :initform nil :type list))
  
  (:default-initargs :external-format :default)
  
  (:documentation "This simple-stream definition provides the minimal support
 for the connection implemention to act as if there were a complete simple-stream
 implementation."))

(defmethod device-file-position ((stream simple-stream))
  "The base implementation just returns 0. It must be overridden by specializations
 which manage buffers."
  0)

(defmethod (setf device-file-position) ((value integer) (stream simple-stream))
  "The base implementation just returns nil. It must be overridden by specializations
 which manage buffers."
  nil)

(defmethod device-file-length ((stream simple-stream))
  nil)

(defclass dual-channel-simple-stream (simple-stream)
  (;; Output buffer.
   (out-buffer :initform nil :type (or simple-stream-buffer null))
   ;; Current position in output buffer.
   (outpos :initform 0 :type fixnum)
   ;; Buffer length (one greater than maximum output buffer index)
   (max-out-pos :initform 0 :type fixnum)))

(defclass socket-simple-stream (dual-channel-simple-stream)
  (;; keep the socket around; it could be handy e.g. for querying peer
   ;; host/port
   (socket
    :initform nil :initarg :socket
    :type (or usocket:stream-usocket null))))


(defmethod shared-initialize :after ((instance simple-stream) slot-names
                                     &rest initargs &key &allow-other-keys)
  (unless (device-open instance slot-names initargs)
    (device-close instance t)))


;;; as per the gray interface, close is replaced with a generic function.
;;; in an implmentation which supports simple streams, the simple-stream method delegates to device-close
;;; 
(when (typep #'close 'generic-function)
  (defmethod close ((stream simple-stream) &key abort)
    (device-close stream abort)))
