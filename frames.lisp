;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(document :file
  (description "This file defines the wire-level frame model for for the `de.setf.amqp` Connon Lisp library.")
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (long-description "Each protocol version packages the data on the wire in a slightly different form. The
 `frame` is specialized for each version to implement operators which marshall/unmarshall frame data as
 as per the version, and operators which map between abstract objcet and method class names and the
 version-specific numeric identifiers."))


(defclass amqp::frame ()
  ((connection
    :initarg :connection :initform nil ;(error "connection required.")
    :reader frame-connection)
   (header
    :initarg :header :initform nil ;(error "header required")
    :reader frame-header)
   (data
    :initarg :buffer :initarg :data :initform  nil ; (error "buffer required.")
    :reader frame-data :writer setf-frame-data :writer (setf frame-data)
    :type (simple-array (unsigned-byte 8) (*))
    :documentation "A byte buffer serves to capture the i/o for any frames
 which are not streamed. the encoding operators use the buffer if it is present.")
   (end-marker :initform #xce :allocation :class
               :reader frame-end-marker))
  (:documentation "Comprises the header and payload for a single AMQP frame.
 Specializations are defined for input and output and for the respective
 protocol version. The former distinguish methods to treat the data as
 received or sent while the latter distinguish header format."))


(defclass amqp::output-frame (amqp::frame)
  ())


(defclass amqp::input-frame (amqp::frame)
  ())

(defgeneric format-frame (frame stream)
  (:method ((frame amqp::frame) stream)
    (let ((header (when (slot-boundp frame 'header) (frame-header frame)))
          (data (when (slot-boundp frame 'data) (frame-data frame)))
          (connection (when (slot-boundp frame 'connection) (frame-connection frame)))
          (class-name nil))
      (when header
        (format stream "[(~:[-~;+~])~:[?~;~d|~d|c.~d|t.~d~]]"
                connection
                header
                (setf class-name (frame-type-class-name frame))
                (frame-cycle frame)
                (frame-channel-number frame)
                (frame-track-number frame)))
      (write-string "." stream)
      (when (eq class-name 'amqp:method)
        (format stream "{~a.~a}" 
                (frame-class-name frame)
                (frame-method-name frame)))
      (format stream "[~:[?~;~@[~a~]/~d~]:"
              data
              (when header (frame-size frame))
              (length data))
      (let* ((size (if header (frame-size frame) (length data)))
             (beginning (min size 8))
             (end (min 8 (- size beginning))))
        (dotimes (x beginning) (format stream " ~0,'2d" (aref data x)))
        (when (< (+ beginning end) size)
          (write-string " ..." stream))
        (dotimes (x end) (format stream " ~0,'2d" (aref data (+ x (- size end))))))
      (write-string "]" stream))))

(defgeneric format-frame-header (frame stream)
  (:method ((frame amqp::frame) stream)
    (let ((header (when (slot-boundp frame 'header) (frame-header frame)))
          (connection (when (slot-boundp frame 'connection) (frame-connection frame)))
          (class-name nil))
      (when header
        (format stream "[(~:[-~;+~])~:[?~;~d|~d|c.~d|t.~d~]]"
                connection
                header
                (setf class-name (frame-type-class-name frame))
                (frame-cycle frame)
                (frame-channel-number frame)
                (frame-track-number frame)))
      (write-string "." stream)
      (when (eq class-name 'amqp:method)
        (format stream "{~a.~a}" 
                (frame-class-name frame)
                (frame-method-name frame)))
      (write-string "]" stream))))

        
(defmethod print-object ((object amqp::frame) (stream t))
  (print-unreadable-object (object stream :type t :identity t)
    (format-frame object stream)))


(defgeneric frame-type (frame)
  )

(defgeneric frame-type-class-name (frame)
  )

(defgeneric frame-decoder (frame)
  (:method ((frame amqp:frame))
    (c2mop:class-prototype (find-class (frame-type-class-name frame)))))

(defgeneric setf-frame-type-class-name (name frame)
  )
(setf (fdefinition '(setf frame-type-class-name))
  #'setf-frame-type-class-name)

(defgeneric frame-cycle (frame)
  )

(defgeneric setf-frame-cycle (cycle frame)
  )

(defgeneric frame-channel-number (frame)
  )

(defgeneric setf-frame-channel-number (number frame)
  )
(setf (fdefinition '(setf frame-channel-number))
  #'setf-frame-channel-number)

(defgeneric frame-track-number (frame)
  )

(defgeneric setf-frame-track-number (number frame)
  )

(defgeneric frame-size (frame)
  )

(defgeneric setf-frame-size (size frame)
  )

(defgeneric frame-class-code (frame)
  )

(defgeneric setf-frame-class-code (code frame)
  )

(defgeneric frame-method-code (frame)
  )

(defgeneric setf-frame-method-code (code frame)
  )

(defgeneric frame-class-name (frame)
  (:method ((frame amqp::frame))
    (frame-class-code-class-name frame (frame-class-code frame))))

(defgeneric frame-method-name (frame)
  (:method ((frame amqp::frame))
    (frame-method-code-method-name frame (frame-class-code frame) (frame-method-code frame))))

(defgeneric (setf frame-method-name) (name frame)
  (:method ((name symbol) (frame amqp::frame))
    (setf-frame-method-code (frame-method-name-method-code frame (frame-class-code frame) name) frame)))

(defgeneric frame-method-name-method-code (frame class method-name)
  (:method ((frame amqp:frame) (class-code integer) (method-name symbol))
    (frame-method-name-method-code frame (frame-class-code-class-name frame class-code) method-name)))

(defgeneric frame-method-code-method-name (frame class method)
  (:method ((frame amqp::frame) (class-code integer) method)
    (frame-method-code-method-name frame (frame-class-code-class-name frame class-code) method))
  (:method ((frame amqp::frame) (class-code t) method)
    (format  nil "|~d|" method)))


(defgeneric frame-class-code-class-name (frame code)
  (:documentation "Return the abstract protocol class name for the respective
 frame version and class code.")
  (:method ((frame t) (code t))
    (format  nil "|~d|" code)))

(defgeneric frame-method-code-class-name (frame code))

(defgeneric frame-class-name-class-code (connection class-name)
  )




(defgeneric content-header-class-id (frame)
  (:method ((buffer vector))
    (buffer-unsigned-byte-16 buffer 0))
  (:method ((frame amqp:frame))
    (buffer-unsigned-byte-16 (frame-data frame) 0)))

(defgeneric setf-content-header-class-id (id frame)
  (:method (id (buffer vector))
    (setf (buffer-unsigned-byte-16 buffer 0) id))
  (:method (id (frame amqp:frame))
    (setf (buffer-unsigned-byte-16 (frame-data frame) 0) id)))

(defgeneric content-header-class-name (frame)
  (:method ((frame amqp::frame))
    (frame-class-code-class-name frame (content-header-class-id frame))))


(defgeneric content-weight (frame)
  (:method ((buffer vector))
    (buffer-unsigned-byte-16 buffer 2))
  (:method ((frame amqp:frame))
    (buffer-unsigned-byte-16 (frame-data frame) 2)))


(defgeneric setf-content-weight (id frame)
  (:method (id (buffer vector))
    (setf (buffer-unsigned-byte-16 buffer 2) id))
  (:method (id (frame amqp:frame))
    (setf (buffer-unsigned-byte-16 (frame-data frame) 2) id)))


(defgeneric content-body-size (frame)
  (:method ((buffer vector))
    (buffer-unsigned-byte-64 buffer 4))
  (:method ((frame amqp:frame))
    (buffer-unsigned-byte-64 (frame-data frame) 4)))


(defgeneric setf-content-body-size (id frame)
  (:method (id (buffer vector))
    (setf (buffer-unsigned-byte-64 buffer 4) id))
  (:method (id (frame amqp:frame))
    (setf (buffer-unsigned-byte-64 (frame-data frame) 4) id)))



(document (read-7-byte-header-frame read-8-byte-header-frame read-12-byte-header-frame)
  "The abstract frame structure (header . payload), is implemented variously in the respective versions.
 The header size varies. The end marker is eliminated. The field sizes change. To allow for this, each
 connection uses a specialized frame class and that class implements the general frame format.
 This includes the buffer access as well as mediating translation between protocol-specific wire codes
 for operations and classes and the generic names. Each protocol implements its logic in its
 'abstract-classes' and 'classes' files.
 nb. some stream classes which appear as handles may not specialize the respective `stream-read-*`
 operators.")


(defun read-7-byte-header-frame (frame stream &key (start 0))
  "Read a frame with a 7-byte header into a FRAME from a binary STREAM.
 Permit a START offset to allow for the first already-read byte of an initial
 start frame.
 VALUES : the frame
          the complete frame length = header + payload net end marker
 CONDITIONS : signals end-of file if either the header or the payload is incomplete."
  (unless (eql 7 (read-sequence (frame-header frame) stream :start start :end 7))
    (error 'end-of-file :stream stream))
  (let* ((payload-length (buffer-unsigned-byte-32 (frame-header frame) 3))
         (frame-length (+ 7 payload-length))
         (frame-end-marker nil)
         (data (frame-data frame)))
    ;; ignore the end marker for the length calculation
    (assert (<= payload-length (length data)) ()
            "Frame length exceeds buffer size: ~s, ~s"
            payload-length data)
    (unless (eql payload-length (read-sequence data stream :end payload-length))
      (error 'end-of-file :stream stream))
    (assert (eql (setf frame-end-marker (read-byte stream)) #xCE) ()
            "Invalid frame end: ~s" frame-end-marker)
    
    (values frame frame-length)))


(defun read-8-byte-header-frame (frame stream &key (start 0))
  "Read a frame with a 8-byte header into a FRAME from a binary STREAM.
 Permit a START offset to allow for the first already-read byte of an iitial
 start frame.
 VALUES : the frame
          the complete frame length = header + payload net end marker
 CONDITIONS : signals end-of file if either the header or the payload is incomplete."
  (unless (eql 8 (read-sequence (frame-header frame) stream :start start :end 8))
    (error 'end-of-file :stream stream))
  (let* ((payload-length (buffer-unsigned-byte-32 (frame-header frame) 4))
         (frame-length (+ 8 payload-length))
         (frame-end-marker nil)
         (data (frame-data frame)))
    ;; ignore the end marker for the length calculation
    (assert (<= payload-length (length (frame-data frame))) ()
            "Frame length exceeds buffer size: ~s, ~s"
            payload-length data)
    (unless (eql payload-length (read-sequence data stream :end payload-length))
      (error 'end-of-file :stream stream))
    (assert (eql (setf frame-end-marker (read-byte stream)) #xCE) ()
            "Invalid frame end: ~s" frame-end-marker)
    (values frame frame-length)))

(defun read-12-byte-header-frame (frame stream &key (start 0))
  "Read a frame with a 12-byte header into a BUFFER from a binary STREAM.
 Permit a START offset to allow for the first already-read byte of an iitial
 start frame.
 VALUES : the frame
          the complete frame length = header + payload net end marker
 CONDITIONS : signals end-of file if either the header or the payload is incomplete."
  (unless (eql 12 (read-sequence (frame-header frame) stream :start start :end 12))
    (error 'end-of-file :stream stream))
  (let* ((frame-length (buffer-unsigned-byte-16 (frame-header frame) 2))
         (payload-length (- frame-length 12))
         (data (frame-data frame)))
    (assert (<= payload-length (length data)) ()
            "Frame length exceeds buffer size: ~s, ~s"
            payload-length (length data))
    (unless (eql payload-length (read-sequence data stream :end payload-length))
      (error 'end-of-file :stream stream))
    (values frame frame-length)))
        

