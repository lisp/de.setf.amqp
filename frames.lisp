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


(defclass amqp:frame ()
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


(defclass output-frame (amqp:frame)
  ())

(defclass input-frame (amqp:frame)
  ())

(defclass 7-byte-header-frame (amqp:frame)
  ((input-buffer :type (simple-array (unsigned-byte 8) 7))))

(defclass amqp.u:7-byte-header-input-frame (7-byte-header-frame input-frame) ())
(defclass amqp.u:7-byte-header-output-frame (7-byte-header-frame output-frame) ())


(defclass 8-byte-header-frame (amqp:frame)
  ((input-buffer :type (simple-array (unsigned-byte 8) 8))))

(defclass amqp.u:8-byte-header-input-frame (8-byte-header-frame input-frame) ())
(defclass amqp.u:8-byte-header-output-frame (8-byte-header-frame output-frame) ())


(defclass 12-byte-header-frame (amqp:frame)
  ((input-buffer :type (simple-array (unsigned-byte 8) 12))))

(defclass amqp.u:12-byte-header-input-frame (12-byte-header-frame input-frame) ())
(defclass amqp.u:12-byte-header-output-frame (12-byte-header-frame output-frame) ())



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
      (let* ((size (if header (min (frame-size frame) (length data)) (length data)))
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



;;;
;;; frame accessors

(defgeneric frame-overhead (frame)
  (:method ((frame 7-byte-header-frame))
    8)

  (:method ((frame 8-byte-header-frame))
    9)

  (:method ((frame 12-byte-header-frame))
    12))
  

(defgeneric frame-decoder (frame)
  (:method ((frame amqp:frame))
    (c2mop:class-prototype (find-class (frame-type-class-name frame)))))

(defgeneric frame-type (frame)
  (:method ((frame 7-byte-header-frame))
    (buffer-unsigned-byte-8 (frame-header frame) 0))

  (:method ((frame 8-byte-header-frame))
    (buffer-unsigned-byte-8 (frame-header frame) 0))

  (:method ((frame 12-byte-header-frame))
    (buffer-unsigned-byte-8 (frame-header frame) 1)))

(defgeneric (setf frame-type) (type frame)
  (:method (type (frame 7-byte-header-frame))
    (setf (buffer-unsigned-byte-8 (frame-header frame) 0) type))

  (:method (type (frame 8-byte-header-frame))
    (setf (buffer-unsigned-byte-8 (frame-header frame) 0) type))

  (:method (type (frame 12-byte-header-frame))
    (setf (buffer-unsigned-byte-8 (frame-header frame) 1) type)))


(defgeneric frame-type-class-name (frame)
  (:method ((frame amqp:frame))
    (connection-frame-type-class-name (frame-connection frame) (frame-type frame))))

(defgeneric connection-frame-type-class-name (connection type-code))


(defgeneric setf-frame-type-class-name (name frame)
  (:method ((name t) (frame amqp:frame))
    (setf (frame-type frame) (connection-class-name-frame-type (frame-connection frame) name))
    name))

(defgeneric connection-class-name-frame-type (connection type-code))

(setf (fdefinition '(setf frame-type-class-name))
  #'setf-frame-type-class-name)


(defgeneric frame-cycle (frame)
  (:method ((frame 7-byte-header-frame))
    0)

  (:method ((frame 8-byte-header-frame))
    (buffer-unsigned-byte-8 (frame-header frame) 1))

  (:method ((frame 12-byte-header-frame))
    0))


(defgeneric setf-frame-cycle (cycle frame)
  (:method (cycle (frame 7-byte-header-frame))
    cycle)

  (:method (cycle (frame 8-byte-header-frame))
    (setf (buffer-unsigned-byte-8 (frame-header frame) 1) cycle))

  (:method (cycle (frame 12-byte-header-frame))
    cycle))


(defgeneric frame-channel-number (frame)
  (:method ((frame 7-byte-header-frame))
    (buffer-unsigned-byte-16 (frame-header frame) 1))

  (:method ((frame 8-byte-header-frame))
    (buffer-unsigned-byte-16 (frame-header frame) 2))

  (:method ((frame 12-byte-header-frame))
    (buffer-unsigned-byte-16 (frame-header frame) 6)))

(defgeneric setf-frame-channel-number (number frame)
  (:method (number (frame 7-byte-header-frame))
    (setf (buffer-unsigned-byte-16 (frame-header frame) 1) number))

  (:method (number (frame 8-byte-header-frame))
    (setf (buffer-unsigned-byte-16 (frame-header frame) 2) number))

  (:method (number (frame 12-byte-header-frame))
    number
    (error "NYI")))

(setf (fdefinition '(setf frame-channel-number))
  #'setf-frame-channel-number)


(defgeneric frame-track-number (frame)
  (:method ((frame 7-byte-header-frame))
    0)

  (:method ((frame 8-byte-header-frame))
    0)

  (:method ((frame 12-byte-header-frame))
    (logand (buffer-unsigned-byte-8 (frame-header frame) 5) #x0f)))

(defgeneric setf-frame-track-number (number frame)
  (:method (number (frame 7-byte-header-frame))
    number)

  (:method (number (frame 8-byte-header-frame))
    number)

  (:method (number (frame 12-byte-header-frame))
    (setf (buffer-unsigned-byte-8 (frame-header frame) 5)
          (logior (logand number #x0f)
                  (logand (buffer-unsigned-byte-8 (frame-header frame) 5) #xf0)))))


(defgeneric frame-size (frame)
  (:method ((frame 7-byte-header-frame))
    (buffer-unsigned-byte-32 (frame-header frame) 3))

  (:method ((frame 8-byte-header-frame))
    (buffer-unsigned-byte-32 (frame-header frame) 4))

  (:method ((frame 12-byte-header-frame))
    (buffer-unsigned-byte-16 (frame-header frame) 2)))

(defgeneric setf-frame-size (size frame)
  (:method (size (frame 7-byte-header-frame))
    (setf (buffer-unsigned-byte-32 (frame-header frame) 3) size))

  (:method (size (frame 8-byte-header-frame))
    (setf (buffer-unsigned-byte-32 (frame-header frame) 4) size))

  (:method (size (frame 12-byte-header-frame))
    (setf (buffer-unsigned-byte-16 (frame-header frame) 2) size)))


(defgeneric frame-class-code (frame)
  (:method ((frame 7-byte-header-frame))
    (buffer-unsigned-byte-16 (frame-data frame) 0))

  (:method ((frame 8-byte-header-frame))
    (buffer-unsigned-byte-16 (frame-data frame) 0))

  (:method ((frame 12-byte-header-frame))
    (buffer-unsigned-byte-8 (frame-data frame) 0)))

(defgeneric setf-frame-class-code (code frame)
  (:method (code (frame 7-byte-header-frame))
    (setf (buffer-unsigned-byte-16 (frame-data frame) 0) code))

  (:method (code (frame 8-byte-header-frame))
    (setf (buffer-unsigned-byte-16 (frame-data frame) 0) code))

  (:method (code (frame 12-byte-header-frame))
    (setf (buffer-unsigned-byte-8 (frame-data frame) 0) code)))


(defgeneric frame-class-name (frame)
  (:method ((frame amqp::frame))
    (connection-class-code-class-name (frame-connection frame) (frame-class-code frame))))

(defgeneric (setf frame-class-name) (name frame)
  (:method (name (frame amqp::frame))
    (setf-frame-class-code (connection-class-name-class-code (frame-connection frame) name)
                           frame)))


(defgeneric frame-method-code (frame)
  (:method ((frame 7-byte-header-frame))
    (buffer-unsigned-byte-16 (frame-data frame) 2))

  (:method ((frame 8-byte-header-frame))
    (buffer-unsigned-byte-16 (frame-data frame) 2))

  (:method ((frame 12-byte-header-frame))
    (buffer-unsigned-byte-8 (frame-data frame) 1)))

(defgeneric setf-frame-method-code (code frame)
  (:method (code (frame 7-byte-header-frame))
    (setf (buffer-unsigned-byte-16 (frame-data frame) 2) code))

  (:method (code (frame 8-byte-header-frame))
    (setf (buffer-unsigned-byte-16 (frame-data frame) 2) code))

  (:method (code (frame 12-byte-header-frame))
    (setf (buffer-unsigned-byte-8 (frame-data frame) 1) code)))

(defgeneric frame-method-name (frame)
  (:method ((frame amqp::frame))
    (connection-method-code-method-name (frame-connection frame) (frame-class-code frame) (frame-method-code frame))))

(defgeneric (setf frame-method-name) (name frame)
  (:method ((name symbol) (frame amqp::frame))
    (setf-frame-method-code (connection-method-name-method-code (frame-connection frame)
                                                                (frame-class-code frame)
                                                                name)
                            frame)))


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
    (connection-class-code-class-name (frame-connection frame) (content-header-class-id frame))))


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


        

;;;
;;; frame initialization -
;;; leave them as individually specialized until know if there is some reliable system...

(defmethod initialize-instance ((instance 8-byte-header-frame)
                                &rest initargs
                                &key
                                connection
                                ;; maximum payload is frame max net both the header
                                ;; _and_ the end byte (cf. http://dev.rabbitmq.com/wiki/Amqp08To091)
                                ;; in this case, 8 byte header + 1 end byte
                                (frame-size (connection-frame-size connection))
                                (header (make-array 8 :element-type '(unsigned-byte 8)))
                                (data (make-array (- frame-size 9) :element-type '(unsigned-byte 8))))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method instance
         :header header
         :data data
         initargs))

(defmethod initialize-instance ((instance 7-byte-header-frame)
                                &rest initargs
                                &key
                                connection
                                ;; maximum payload is frame max net both the header
                                ;; _and_ the end byte (cf. http://dev.rabbitmq.com/wiki/Amqp08To091)
                                ;; in this case, 7 byte header + 1 end byte
                                (frame-size (connection-frame-size connection))
                                (header (make-array 7 :element-type '(unsigned-byte 8)))
                                (data (make-array (- frame-size 8) :element-type '(unsigned-byte 8))))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method instance
         :header header
         :data data
         initargs))

(defmethod initialize-instance ((instance 12-byte-header-frame)
                                &rest initargs
                                &key
                                connection
                                (frame-size (connection-frame-size connection))
                                (header (make-array 12 :element-type '(unsigned-byte 8)))
                                (data (make-array (- frame-size 12) :element-type '(unsigned-byte 8))))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method instance
         :header header
         :data data
         initargs))

;;;
;;; frame input operators


(defmethod read-frame ((connection amqp:connection) (frame amqp.u:7-byte-header-input-frame) &rest args)
  (declare (dynamic-extent args))
  (apply #'read-7-byte-header-frame frame (stream-input-handle connection)
         args))

(defmethod write-frame ((connection amqp:connection) (frame amqp.u:7-byte-header-output-frame) &key (start 0) (end nil))
  (let ((stream (stream-output-handle connection)))
    (setf end (or end (frame-size frame)))
    (write-sequence (frame-header frame) stream :start start)
    (write-sequence (frame-data frame) stream :end end)
    (write-sequence #(#xCE) stream)
    (force-output stream)
    (+ end 7 1)))                       ; total count


(defmethod read-frame ((connection amqp:connection) (frame amqp.u:8-byte-header-input-frame) &rest args)
  (declare (dynamic-extent args))
  (apply #'read-8-byte-header-frame frame (stream-input-handle connection)
         args))

(defmethod write-frame ((connection amqp:connection) (frame amqp.u:8-byte-header-output-frame) &key (start 0) (end nil))
  (let ((stream (stream-output-handle connection)))
    (setf end (or end (frame-size frame)))
    (write-sequence (frame-header frame) stream :start start)
    (write-sequence (frame-data frame) stream :end end)
    (write-sequence #(#xCE) stream)
    (force-output stream)
    (+ end 1 8)))                       ; total count


(defmethod read-frame ((connection amqp:connection) (frame amqp.u:12-byte-header-input-frame) &rest args)
  (declare (dynamic-extent args))
  (apply #'read-12-byte-header-frame frame (stream-input-handle connection)
         args))

(defmethod write-frame ((connection amqp:connection) (frame amqp.u:12-byte-header-output-frame) &key (start 0) (end nil))
  (let ((stream (stream-output-handle connection)))
    (setf end (or end (frame-size frame)))
    (write-sequence (frame-header frame) stream :start start)
    (write-sequence (frame-data frame) stream :end end)
    (+ end 12)))


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
