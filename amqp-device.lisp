;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)


(:documentation "This file defines the `amqp-device` class to extend `simple-stream` devices as part of the
 'de.setf.amqp' library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)  All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))




(defclass amqp-device (dual-channel-simple-stream)
  ((uri
    :initform (error "initialization argument required: uri")
    :initarg :uri
    :reader device-uri
    :writer setf-device-uri)
   (body-position
    :initform 0
    :accessor device-body-position)
   (body-length
    :initform 0
    :accessor device-body-length)
   (frame-position
    :initform 0
    :accessor device-frame-position
    :documentation "Counts the byte offset of the beginning of the current frame in the
     content stream. It is reset to zero by device-read/write-content and incremented for
     each successively completed frame. It combines as the base offset with buffpos
     to yield the device-file-position and therby also the stream-position.")
   (free-input-frames
    :accessor device-free-input-frames
    :type locked-stack
    :documentation "This stack resources frames to be used to
     read data from the device. It is shared directly by a connection and
     all of its channels.")
   (free-output-frames
    :accessor device-free-output-frames
    :type locked-stack
    :documentation "This stack resources frames to be used to
     write data to the device. It is shared directly by a connection and
     all of its channels.")
   (read-frames
    :accessor device-read-frames
    :type locked-queue
    :documentation "This queue buffers frames which have been read from a
     device, but not yet processed by the respective channel or (for channel 0)
     by the connection.")
   (encoded-frames
    :accessor device-encoded-frames
    :type locked-queue
    :documentation "This stack resources frames to be used to
     read data from the device. It is shared directly by a connection and
     all of its channels.")
   (content-type
    :initarg :content-type
    :reader device-content-type :writer setf-device-content-type
    :type mime:*/*
    :documentation "Specifies the encoding type for character-oriented stream
     operations. The publish, deliver, and get operations consult it to determine
     which encoding filter to install. (See (setf channel-content-type).)")
   (element-type
    :initarg :element-type
    :accessor device-element-type
    :documentation "A type specifier which indicates what objects may be read
 from or written to the stream. The type determine how device-read-content-body
 processes delivered content. In addition to character and integer, which
 are treated as equivalent to string and vector with respect to a complete body,
 the types stream and list return the stream and read an s-expression. any other
 type is to be encoded as an instantiation form.")
   (encoder
    :type function
    :reader  device-encoder :writer setf-device-encoder
    :documentation "Optionally binds a function which is then used to encode
 character values for output to the device.")
   (decoder
    :type function
    :reader  device-decoder :writer setf-device-decoder
    :documentation "Optionally binds a function which is then used to decode
 character values from input from the device.")
   (state
    :initform nil
    :accessor device-state)
   (line-buffer
    :initform (make-array 32 :element-type 'character :fill-pointer 0 :adjustable t)
    :reader stream-line-buffer)
   (eof-marker
    :initform :eof :initarg :eof-marker
    :accessor stream-eof-marker)
   (eol-marker
    :initform #\newline :initarg :eol-marker
    :accessor stream-eol-marker))
  (:default-initargs
    :element-type 'character
    :content-type mime:text/plain ))



(defclass amqp-connection-device (amqp-device)
  ())

(defclass amqp-socket-device (amqp-device)
  ((socket
    :initform nil
    :reader device-socket)))



(defmethod shared-initialize ((instance amqp-device) (slots t) &rest initargs
                              &key (content-type nil ct-s))
  (declare (dynamic-extent initargs))
  
  ;; coerce and set the type, and pass it to any other initializers
  (if ct-s
    (apply #'call-next-method instance slots
           :content-type (setf (device-content-type instance) content-type)
           initargs)
    (call-next-method)))


(defgeneric (setf device-content-type) (type device)
  (:method ((type mime:*/*) (device amqp-device))
    (setf-device-content-type type device)
    (slot-makunbound device 'decoder)
    (slot-makunbound device 'encoder)
    (update-device-codecs device type)
    type)
  (:method ((type t) (device amqp-device))
    (setf (device-content-type device) (mime:mime-type type))))

 
(defmethod update-device-codecs ((device amqp-device) (type mime:*/*))
  (multiple-value-bind (decoder encoder)
                       (compute-charset-codecs (device-content-type device))
    (unless (slot-boundp device 'decoder)
      (setf-device-decoder decoder device))
    (unless (slot-boundp device 'encoder)
      (setf-device-encoder encoder device)))
  type)


(defmethod update-device-codecs ((device amqp-device) (type mime:application/octet-stream))
  "given a binary content type, set the codecs to byte-identity - but note they will fail
 for any callers which expect characters."
  (flet ((get-unsigned-byte (get-byte destination)
           (funcall get-byte destination))
         (put-unsigned-byte (byte put-byte destination)
           (funcall put-byte destination byte)))
    (setf-device-decoder #'get-unsigned-byte device)
    (setf-device-encoder #'put-unsigned-byte device)
    type))


(defmethod mime-type-charset ((device amqp-device))
  "Given a device, delegate to the content-type."
  (mime-type-charset (device-content-type device)))


(defmethod mime:mime-type ((device amqp-device) &rest args)
  (declare (dynamic-extent args) (ignore args))
  (device-content-type device))


(defmethod device-initialize-input-buffer ((instance amqp-device))
  (with-slots (buffer buffpos buffer-ptr buf-len) instance
    ;; claim a frame to get the size right and confiscate it's buffer
    (let ((frame (claim-input-frame instance)))
      (setf buffer (frame-data frame)     ; don't give it back
            buffpos 0
            buffer-ptr 0
            buf-len (length buffer)))))

(defmethod device-initialize-output-buffer ((instance amqp-device))
  (with-slots (out-buffer max-out-pos outpos) instance
    ;; claim a frame to get the size right and confiscate it's buffer
    (let ((frame (claim-output-frame instance)))
      (setf out-buffer (frame-data frame)     ; don't give it back
            outpos 0
            max-out-pos (length out-buffer)))))

(defmethod device-initialize-buffers ((instance amqp-device))
  (device-initialize-output-buffer instance)
  (device-initialize-input-buffer instance))


(defmethod device-reset-buffers ((device amqp-device))
  (with-slots (buffer buffpos buffer-ptr buf-len
               out-buffer max-out-pos outpos
               position length pending) device
    (setf buffer nil
          buffpos  0
          buffer-ptr  0
          buf-len 0
          pending nil
          max-out-pos 0
          out-buffer nil
          outpos 0)))



(defmethod device-open ((device amqp-device) #-sbcl (slots t) (options t))
  
  (device-reset-buffers device)
  #+sbcl ;; set up the cached i/o functions
  (with-stream-class (amqp-device device)
    (add-stream-instance-flags device :dual :simple :input :output)
    (install-amqp-device-strategy device))
  t)


(defmethod device-close ((device amqp-device) (abort t))
  (unless (eq (stream-external-format device) :void)
    (ignore-errors (if abort
                     (clear-output device)
                     (finish-output device)))
    (when (next-method-p) (call-next-method))
    (setf (stream-external-format device) :void)))


(defmethod device-file-position ((device amqp-device))
  "Add together the current frame offset and frame buffer position to return the
 effective position since the last call to device-read-content-body"
  (with-slots (outpos buffpos) device
    (+ (device-frame-position device)
       (typecase (device-state device)
         (amqp.s:output outpos)
         (amqp.s:input buffpos)
         (t 0)))))

(defmethod (setf device-file-position) (position (device amqp-device))
  (declare (ignore position))
  nil)

(defmethod device-close ((device amqp-connection-device) (abort t))
  (call-next-method)
  (setf (stream-input-handle device) nil)
  (setf (stream-output-handle device) nil))


(defgeneric device-listen (device)
  (:method ((device amqp-connection-device))
    (when (stream-input-handle device)
      (device-listen (stream-input-handle device))))

  (:method ((device amqp-socket-device))
    (when (stream-input-handle device)
      (listen (stream-input-handle device)))))

(defmethod device-open ((device amqp-socket-device) #-sbcl (slots t) options)
  (if (or (stream-input-handle device)
          (stream-output-handle device))
    (call-next-method)
    (flet ((argument-error ()
             (error "device-open on ~S requires :remote-host and :remote-port arguments"
                    (type-of device))))
      (destructuring-bind (&key (remote-host (argument-error))
                                (remote-port (argument-error))
                                (direction :io)
                                &allow-other-keys)
                          options
        (assert (member direction '(:probe :io)) ()
                "Invalid diection: ~s." direction)
        (when (call-next-method)
          (let ((opened-socket (usocket:socket-connect remote-host
                                                       remote-port
                                                       :element-type 'unsigned-byte)))
              (ecase direction
                (:io
                 (setf (device-socket device) opened-socket)
                 #+mcl
                 (ccl:terminate-when-unreachable device))
                (:probe
                 (usocket:socket-close opened-socket))))
            device)))))


(defmethod device-close ((device amqp-socket-device) (abort t))
  (call-next-method)
  (when (device-socket device)
    (usocket:socket-close (device-socket device))
    (setf (device-socket device) nil))
  #+mcl
    (ccl:cancel-terminate-when-unreachable device))


(defmethod (setf device-socket) ((socket null) (device amqp-socket-device))
  (setf (slot-value device 'socket) nil)
  (setf (stream-input-handle device) nil)
  (setf (stream-output-handle device) nil))

(defmethod (setf device-socket) ((socket usocket:stream-usocket) (device amqp-socket-device))
  (setf (slot-value device 'socket) socket)
  (setf (stream-input-handle device) (usocket:socket-stream socket))
  (setf (stream-output-handle device) (usocket:socket-stream socket)))


#+mcl
(defmethod terminate ((object simple-stream))
  ;; double-check for incompletely closed channels etc.
  (typecase (device-state object)
    (amqp.s:close )
    (t
     (device-close object t))))

(when (fboundp 'stream-close)
  (defmethod stream-close ((stream amqp-device))
    (when (next-method-p) (call-next-method))
    (device-close stream nil)))


;;;
;;; simple stream i/o strategies to be cached in the device instance

#+sbcl
(progn
  (defun amqp-j-read-char (device eof-error-p eof-value blocking)
    (when (or blocking (stream-listen device))
      (with-slots (decoder last-char-read-size buffpos) device
        (flet ((buffer-extract-byte (stream)
                 (with-slots (buffer buffpos buffer-ptr body-position body-length) stream
                   (when (or (< buffpos buffer-ptr)
                             (and (not (minusp buffer-ptr))
                                  ; resets buff-pos / renews buffer content unless eof
                                  (plusp (device-read stream nil 0 nil t))))
                     (incf body-position)
                     (prog1 (aref buffer buffpos)
                       (incf buffpos))))))
          (let ((start-buffpos buffpos)
                (char (funcall decoder #'buffer-extract-byte device)))
            (cond (char
                   (setf last-char-read-size (- buffpos start-buffpos))
                   char)
                  (eof-error-p
                   (error 'end-of-file :stream device))
                  (t
                   eof-value)))))))
  
  (defun amqp-j-read-chars (device sequence search start end blocking)
    "like stream-read-sequence, but terminate on match, exclusive of match"
    (when (or blocking (stream-listen device))
      (setf end (or end (length sequence)))
      (with-slots (decoder last-char-read-size buffpos) device
        (flet ((buffer-extract-byte (stream)
                 (with-slots (buffer buffpos buffer-ptr body-position) stream
                   (when (or (< buffpos buffer-ptr)
                             (and (not (minusp buffer-ptr))
                                  ; resets buff-pos / buffer content unless eof
                                  (plusp (device-read stream nil 0 nil t))))
                     (prog1 (aref buffer buffpos)
                       (incf body-position)
                       (incf buffpos))))))
          (if (> end start)
            (let ((i start)
                  (start-buffpos buffpos)
                  (char #\null))
              (loop (when (>= i end)
                      (return (values (- i start) nil)))
                    (unless (setf char (funcall decoder #'buffer-extract-byte device))
                      (return (values (- i start) :eof)))
                    (setf last-char-read-size (- buffpos start-buffpos))
                    (setf start-buffpos buffpos)
                    (when (eql char search)
                      (return (values (- i start) t)))
                    (setf (char sequence i) char)
                    (incf i)))
            0)))))
  
  (defun amqp-j-unread-char (stream char)
    (with-slots (buffpos body-position buffer) stream
      (let ((old-decoder (device-decoder stream)))
        (flet ((unread-decoder (byte-decoder stream)
                 (declare (ignore byte-decoder))
                 (with-slots (buffpos body-position) stream
                   (setf-device-decoder old-decoder stream)
                   (incf body-position)
                   (incf buffpos)
                   char)))  
          (decf body-position)
          (decf buffpos)
          (setf char (code-char (aref buffer buffpos)))
          (setf-device-decoder #'unread-decoder stream)
          nil))))
    
  (defun amqp-j-write-char (character stream)
    (amqp-stream-write-char stream character))
  
  (defun amqp-j-write-chars (string stream start end)
    (amqp-stream-write-string stream string start end))
  
  (defun amqp-j-listen (stream)
    (stream-listen (stream-input-handle stream)))
  
  (defun install-amqp-device-strategy (device)
    ;; no attention to external format, as that is handled based on
    ;; messag header and/or individual publish request
    (sb-simple-streams::with-stream-class (amqp-device device)
      (setf (sm sb-simple-streams::j-read-char device) #'amqp-j-read-char
            (sm sb-simple-streams::j-read-chars device) #'amqp-j-read-chars
            ;; bad stack overflow !
            ;; (sm sb-simple-streams::j-unread-char device) #'sb-simple-streams::%unread-char
            (sm sb-simple-streams::j-unread-char device) #'amqp-j-unread-char
            (sm sb-simple-streams::j-write-char device) #'amqp-j-write-char
            (sm sb-simple-streams::j-write-chars device) #'amqp-j-write-chars
            (sm sb-simple-streams::j-listen device) #'amqp-j-listen)))
  )