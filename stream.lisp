;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines a stream interface for AMQP channel and connection instances for the
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

 (long-description "The goal here is to hook the channel/connection stream operations into standard
 stream operations. this file implements

 - gray stream compatibility 
 - stream-ty* support

 the original "gray streams" proposal source[1] has not been forthcoming, but there is a copy on the clozure
 ftp site.[2] kmp has notes[3], corman lisp publishes an implementation[4], and clim discusses in interim gray
 streams implementation[5], to be used in the interim, until a real implementation is available.


 the stream uses a channel's the dual-channel-simple-stream device implementation to manage body length,
 position and buffering together with the its operators for character coding. the interface
 supports (vector (unsigned byte 8)) and string element types and (vector (unsigned byte 8))
 i/o buffers. codecs are always present - even for ascii, and are used to implement unread-char.

 the protocol is
 on input:

   buffer        is the buffer
   buf-len       is the full buffer size
   buffer-ptr    is the length of read data, which is given by the frame payload size
   buffpos       is the position from which to take the next byte 
   body-position is that position wrt the entire body
   body-length   is the length of the entire body

 the decoders maintain buffpos and body-position, and use the relation between buffpos
 and buffer-ptr to refresh the buffer iff needed. if buff-pr is negative, that indicates that eof
 has already been reached. the relation between body-position and body-size can indicate
 the end of the  current content, depends on whether the channel expects chunked content.
 (see device-read)

 on output:

   out-buffer    is the buffer
   max-out-pos   is the full buffer size
   outpos        is the position at which to put the next byte 
   body-position is that position wrt the entire body
   body-length   is the length of the entire body

 the encoders maintian outpos and body-position and use the relation between outpos and
 max-out-pos to flush the buffer iff needed. the relation between body-position and
 body-size indicates the end of intended content and is used to effect chunking. (see device-write)

 body-position and body-length can be multiplexed between input and output so long as the channels
 remain half-duplex. the application must adhere to the single-channel per process constraint and
 limit stream i/o to device-read/write-content or the respetive request-publish/get. should interleaved
 input and output be required, the channel will need distinct state for each direction.

 ---
 [1]: ftp://parcftp.xerox.com/pub/cl/cleanup/mail/stream-definition-by-user.mail
 [2]: ftp://ftp.clozure.com/pub/stream-definition-by-user.mail
 [3]: http://www.nhplace.com/kent/CL/Issues/stream-definition-by-user-notes.html
 [4]: http://www.grumblesmurf.org/corman-patches/modules/gray-streams.lisp
 [5]: http://www.cs.cmu.edu/afs/cs/project/clisp/hackers/phg/clim/src/utils/cl-streams.lisp"))


;;;
;;; input

(defun amqp-stream-read-char (stream)
  "Read a character from an open channel on an amqp-device according to the channel's current encoding.
  At EOF return the stream-eof-marker."
  (with-slots (decoder) stream
    (flet ((buffer-extract-byte (stream)
             (with-slots (buffer buffpos buffer-ptr body-position body-length) stream
               (when (or (< buffpos buffer-ptr)
                         (and (not (minusp buffer-ptr))
                              ; resets buff-pos / renews buffer content unless eof
                              (plusp (device-read stream nil 0 nil t))))
                 (incf body-position)
                 (prog1 (aref buffer buffpos)
                   (incf buffpos))))))
      (or (funcall decoder #'buffer-extract-byte stream) (stream-eof-marker stream)))))

(defmethod stream-read-char ((stream amqp-device))
  (amqp-stream-read-char stream))


;;;!!! for multi-byte encodings the content-encoding needs to supply this
;;;!!! in order that the position is properly maintained.
;;;!!! it's also not clear what happens when this backs up over a frame boundary
(defmethod stream-unread-char ((stream amqp-device) char)
  (with-slots (buffpos body-position) stream
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
        (setf-device-decoder #'unread-decoder stream)
        nil))))


(defmethod stream-read-char-no-hang ((stream amqp-device))
  "If input is already available from an open channel on an amqp-device read the next character according
 to the channel's current encoding. If none is available, return NIL. At EOF return the stream-eof-marker."
  (with-slots (body-position body-length) stream
    (if (< body-position body-length)
      (when (stream-listen (stream-input-handle stream))
        (stream-read-char stream))
      (stream-eof-marker stream))))


(defmethod stream-peek-char ((stream amqp-device))
  (with-slots (decoder) stream
    ;; cannot do this manipulating buffer position, as a multi-byte encoding
    ;; might cross a frame boundary
    (flet ((buffer-extract-byte (stream)
             (with-slots (buffer buffpos buffer-ptr body-position body-length) stream
               (when (or (< buffpos buffer-ptr)
                         (and (not (minusp buffer-ptr))
                              ; resets buff-pos / renews buffer content unless eof
                              (plusp (device-read stream nil 0 nil t))))
                 (incf body-position)
                 (prog1 (aref buffer buffpos)
                   (incf buffpos))))))
      (let ((char (funcall decoder #'buffer-extract-byte stream)))
        (cond (char
               (stream-unread-char stream char)
               char)
              (t
               (stream-eof-marker stream)))))))


(defmethod stream-listen ((stream amqp-device))
  ;; sbcl has no stream-listen for the input stream
  (device-listen stream))


(defmethod stream-read-line ((stream amqp-device))
   "Read a line of characters from an open channel on an amqp-device according to the channel's current encoding.
 Returns those up to the next stream-eol-marker as a new string. Iff the line is terminated by EOF, returns
 a second value, the stream-eof-marker."
  (with-slots (decoder) stream
    (let ((eol-char (stream-eol-marker stream))
          (line (stream-line-buffer stream)))
      (setf (fill-pointer line) 0)
      (flet ((buffer-extract-byte (stream)
             (with-slots (buffer buffpos buffer-ptr body-position body-length) stream
               (when (or (< buffpos buffer-ptr)
                         (and (not (minusp buffer-ptr))
                              ; resets buff-pos / renews buffer content unless eof
                              (plusp (device-read stream nil 0 nil t))))
                 (incf body-position)
                 (prog1 (aref buffer buffpos)
                   (incf buffpos))))))
        (loop (let ((char (funcall decoder #'buffer-extract-byte stream)))
                (cond ((eql char eol-char)
                       (return (copy-seq line)))
                      (char
                       (vector-push-extend char line))
                      (t
                       (return (values (copy-seq line) (stream-eof-marker stream)))))))))))


(defmethod stream-clear-input ((stream amqp-device))
  "Clear any pending buffered input from the stream's device."
  (device-clear-input stream t))


(defmethod stream-line-column ((stream amqp-device))
  "Constantly nil."
  nil)


(defmethod stream-start-line-p ((stream amqp-device))
  "Constantly nil."
  nil)


(defun amqp-stream-write-char (stream character)
  (with-slots (encoder) stream
    (flet ((buffer-insert-byte (stream byte)
             (with-slots (out-buffer outpos max-out-pos body-position body-length encoder) stream
               (setf (aref out-buffer outpos) byte)
               (incf body-position)
               (incf outpos)
               (when (>= outpos max-out-pos)
                 ;; resets outpos / flushes out-buffer content
                 (device-write stream nil 0 nil t)))))
      (funcall encoder character #'buffer-insert-byte stream))
    character))

(defmethod stream-write-char ((stream amqp-device) character)
  "Write a character to an open channel on an amqp-device according to the channel's current encoding."
  (amqp-stream-write-char stream character))


(defun amqp-stream-write-string (stream string start end)
  (with-slots (encoder) stream
    (flet ((buffer-insert-byte (stream byte)
             (with-slots (out-buffer outpos max-out-pos body-position body-length) stream
               (setf (aref out-buffer outpos) byte)
               (incf body-position)
               (incf outpos)
               (when (>= outpos max-out-pos)
                 (device-write stream nil 0 nil t)))))
      (unless start (setf start 0))
      (unless end (setf end (length string)))
      (do ((i start (1+ i)))
          ((>= i end))
        (funcall encoder (char string i) #'buffer-insert-byte stream)))
    string))

(defmethod stream-write-string ((stream amqp-device) string #-mcl &optional start end)
  "Write a string to an open channel on an amqp-device according to the channel's current encoding."
  (amqp-stream-write-string stream string start end))


(defmethod stream-terpri ((stream amqp-device))
  (stream-write-char stream (stream-eol-marker stream)))


;; stream-fresh-line :
;; default suffices


(defmethod stream-finish-output ((stream amqp-device))
  "Force output and delegate the finish to the output stream."
  (stream-force-output stream)
  (stream-finish-output (stream-output-handle stream)))


(defmethod stream-force-output ((stream amqp-device))
  "Flush the current buffer, w/o checking for content. (see device-flush)"
  (device-flush stream))


(defmethod stream-clear-output ((stream amqp-device))
  "Discard anything in the present output buffer."
  (with-slots (outpos body-position) stream
    (when (plusp outpos)
      ;; back up in the global stream
      (decf body-position outpos)
      ;; reset the current frame
      (setf outpos 0))))


(defmethod stream-advance-to-column ((stream amqp-device) (column t))
  nil)


;;; close :
;;; see extremely-simple-streams.lisp


#-mcl
(defmethod open-stream-p ((stream amqp-device))
  "Return true iff the stream's channel is open."
  ;; This replicates the mcl definition, so that s single stream-direction suffices.

  (not (eql (stream-direction stream) :closed)))

(defmethod stream-direction ((stream amqp-device))
  "A device's direction depends on the state of its handles."
  (if (stream-input-handle stream)
    (if (stream-output-handle stream)
      :io
      :input)
    (if (stream-output-handle stream)
      :output
      :closed)))


#+ccl
(defmethod stream-eofp ((stream amqp-device))
  (with-slots (body-length body-position) stream
    (>= body-position body-length)))

;; input-stream-p :
;; mcl version is not generic, it is based on direction, which suffices

;; output-stream-p :
;; mcl version is not generic, it is based on direction, which suffices

(defmethod stream-element-type ((stream amqp-device))
  (device-element-type stream))

(defmethod stream-position ((stream amqp-device) &optional new)
  (when (null new) 
    (device-file-position stream)))

#-lispworks
(defmethod stream-file-position ((stream amqp-device) &optional new)
  (when (null new) 
    (device-file-position stream)))
#+lispworks
(defmethod stream-file-position ((stream amqp-device))
  (device-file-position stream))

;; pathname : NYI


;; truename : NYI


(defun amqp-stream-read-byte (stream)
  "Read a byte from an open channel on an amqp-device. Manage buffer positions and refresh
 buffers from read frames as required. AAt EOF return the stream-eof-marker."
  (with-slots (buffer buffpos buffer-ptr body-position) stream
    (if (or (< buffpos buffer-ptr)
            (and (not (minusp buffer-ptr))
                 ; resets buff-pos / buffer content unless eof
                 (plusp (device-read stream nil 0 nil t))))
      (prog1 (aref buffer buffpos)
        (incf buffpos)
        (incf body-position))
      (stream-eof-marker stream))))

(defmethod stream-read-byte ((stream amqp-device))
  (amqp-stream-read-byte stream))


(defun amqp-stream-write-byte (stream byte)
  "Write a byte to an open channel on an amqp-device. Add the byte at the current buffer position.
 If either the buffer is full, or the stream length is reached, write the buffer."
  (with-slots (out-buffer outpos max-out-pos body-position body-length) stream
    (setf (aref out-buffer outpos) byte)
    (incf body-position)
    (incf outpos)
    (when (>= outpos max-out-pos)
      (device-write stream nil 0 nil t))
    byte))

(defmethod stream-write-byte ((stream amqp-device) byte)
  (amqp-stream-write-byte stream byte))



(defmethod stream-read-sequence
           #+ccl ((stream amqp-device) (sequence vector) &key (start 0) end)
           #+lispworks ((stream amqp-device) (sequence vector) start end)
           #-(or ccl lispworks) ((stream amqp-device) (sequence vector) #-ccl &optional  (start 0) end)
  "Read a byte sequence from an open channel on an amqp-device. Invokes device-read to read and transfer data 
 directly via the device frame buffers."
  (setf end (or end (length sequence)))
  (let ((count (device-read stream sequence start end nil)))
    (when (plusp count) (+ start count))))



(defmethod stream-read-sequence
           #+ccl ((stream amqp-device) (sequence string) &key (start 0) end)
           #+lispworks ((stream amqp-device) (sequence string) start end)
           #-(or ccl lispworks) ((stream amqp-device) (sequence string) #-ccl &optional  (start 0) end)
  "Read a character sequence from an open channel on an amqp-device. Arrange to read bytes from the device buffer,
 construct characters, and return the the next position. Iff the first byte read shows eof, return nil."
  (setf end (or end (length sequence)))
  (with-slots (decoder) stream
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
        (let ((char (funcall decoder #'buffer-extract-byte stream)))
          (when char
            (setf (char sequence start) char)
            (do ((i (1+ start) (1+ i)))
                ((>= i end) end)
            (if (setf char (funcall decoder #'buffer-extract-byte stream))
              (setf (char sequence i) char)
              (return i)))))
        end))))

  

(defmethod stream-write-sequence
           #+ccl ((stream amqp-device) (sequence vector) &key (start 0) end)
           #+lispworks ((stream amqp-device) (sequence vector) start end)
           #-(or ccl lispworks) ((stream amqp-device) (sequence vector) #-ccl &optional  (start 0) end)
  "Write a byte sequence to an open channel on an amqp-device. Invokes device-read to transfer data write
 directly via the device frame buffers."
  (setf end (or end (length sequence)))
  (device-write stream sequence start end 0))


(defmethod stream-write-sequence
           #+ccl ((stream amqp-device) (sequence string) &key (start 0) end)
           #+lispworks ((stream amqp-device) (sequence string) start end)
           #-(or ccl lispworks) ((stream amqp-device) (sequence string) #-ccl &optional  (start 0) end)
  "Write a character sequence from an open channel on an amqp-device. Arrange to read bytes from the device buffer,
 construct characters, and return the the next position."
  (setf end (or end (length sequence)))
  (stream-write-string stream sequence start end))



#+clozure
(progn
  (defmethod ccl:stream-read-vector ((stream amqp-device) (sequence vector) start end)
    (let ((count (device-read stream sequence (or start 0) (or end (length sequence)) nil)))
      (when (plusp count) (+ start count))))

  (defmethod ccl:stream-read-vector ((stream amqp-device) (sequence string) start end)
    (stream-read-sequence stream sequence :start (or start 0) :end (or end (length sequence))))

  (defmethod ccl:stream-write-vector ((stream amqp-device) (sequence vector) start end)
    (device-write stream sequence (or start 0) (or end (length sequence)) t))

  (defmethod ccl:stream-write-vector ((stream amqp-device) (sequence string) start end)
    (stream-write-sequence stream sequence :start (or start 0) :end (or end (length sequence)))))


(defmethod stream-tyo ((stream amqp-device) character)
  ;; (stream-tyo *trace-output* character)
  (stream-write-char stream character))

(defmethod stream-tyi ((stream amqp-device))
  (let ((char (stream-read-char stream)))
    (typecase char
      (character char)
      (t nil))))

(defmethod stream-untyi ((stream amqp-device) character)
  (stream-unread-char stream character))



;;;
;;; fu interface

(defmethod device-allocate-buffer ((stream amqp-device) &key
                                   (length (device-buffer-length stream))
                                   (initial-contents nil))
  ;; the description does not make this obvious, but it
  ;; makes sense for thi initial contents to be in the application domain
  ;; - that is, decoded
  (let ((new-buffer (make-frame-buffer length)))
    (when initial-contents
      (assert (= (length initial-contents) length) ()
              "Inconsistent lengths: ~d, ~d" length (length initial-contents))
      (with-slots (out-buffer outpos max-out-pos body-length body-position) stream
        (let ((.out-buffer out-buffer)
              (.outpos outpos)
              (.max-out-pos max-out-pos)
              (.body-length body-length)
              (.body-position body-position))
          (unwind-protect
            (progn (setf out-buffer new-buffer
                         outpos 0
                         max-out-pos (1+ length)        ; prevent writing
                         body-length (1+ length)
                         body-position 0)
                   (stream-write-sequence stream initial-contents))
            (setf  out-buffer .out-buffer
                   outpos .outpos
                   max-out-pos .max-out-pos
                   body-length .body-length
                   body-position .body-position)))))
    new-buffer))
    

(defmethod device-input-element-type ((stream amqp-device))
  ;; one only
  (device-element-type stream))


(defmethod device-output-element-type ((stream amqp-device))
  ;; one only
  (device-element-type stream))

(defmethod device-encoded-length ((stream amqp-device) buffer &optional start end)
  (declare (ignore buffer start end))
  nil)

;;;
;;; the description does not make this obvious, but it
;;; [http://paste.lisp.org/display/65229], and the names do imply that
;;; the buffers are in the device domain - that is the content is encoded
;;; thus device-write rather than stream-write-sequence

(defmethod device-write-buffers ((stream amqp-device) &rest buffer-specs)
  (loop for (buffer start end) in buffer-specs by #'cddr
        do (device-write stream buffer start end t)))

(defmethod device-read-buffers ((stream amqp-device) &rest buffer-specs)
  (loop for (buffer start end) in buffer-specs by #'cddr
        do (device-read stream buffer start end t)))



;;;
;;; stream-reader/writer

(defmethod stream-reader ((stream amqp-device))
  (typecase (amqp.u:channel-content-type stream)
    (mime:binary
     (values #'amqp-stream-read-byte stream))
    (t
     (values #'amqp-stream-read-char stream))))

(defmethod stream-writer ((stream amqp-device))
  (typecase (amqp.u:channel-content-type stream)
    (mime:binary
     (values #'amqp-stream-write-byte stream))
    (t
     (values #'amqp-stream-write-char stream))))
