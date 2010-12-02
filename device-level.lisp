;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file implements device-level support for streams based on AMQP connections as part of the
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

  (long-description "Device-level operations are managed by channel instances, which delegete in turn to
connection-wrapped tcp sockets. The AMQP standards permit this combination to behave in either of (at
least) two ways. On one hand, one can use all allowed protocoal variability and treat the socket
connection as a multiplexed multi-channel stream. In this mode each thread would instantiate its own
channel on the shared connection. On the other hand, is also possible to constrain the connection's use
to dedicate it to a single channel - and thereby to a single thread.

 The multiplexed mechanism sets a much lower demand for system resources, but increases the processing
for a single channel and constrains the i/o operations to block mode. A dedicated mechanims requires
more sockets, but can dedicate the buffering to a single thread, which permits more efficient i/o
operations.

 When the socket is multiplexed, then it is possible that content frames for one channel will be
interleaved with frames for another channel, which means it is not possible to guarantee that a given
socket read operation will contain an expected content frame. The multiplexing requires per-channel
queuing and processing upon delivery, whereby the actual buffer is not to be predicted. When the socket
is dedicated, read operatation can target a given buffer, since the intra-channel ordering constraints
require that content be delivered in-order and uninterrupted. The protocol meta-data can be read and
processed in-line ro parameterize the read and recognize exceptions, while data i/o targets given
buffers. The 1.0pr documentintroduces additional session and context for frame processing, but those are
not considered here. (see amqp0-8.pdf,p.56 amqp0-9.pdf,p.35)

 In the implementation below, the two amqp classes - connection and channel, are each specialized from
simple-stream and implement the standard interface. The required device operators are implemented for
both amqp:channel and amqp:connection, despite that stream operations are limited to channels while
connections are to be used only to consolidate frame-based operations with the network stream. The
principle operators are

    device-open (stream #-sbcl slots initargs)
    device-close (stream abort)
    device-read (stream buffer start end blocking)
    device-clear-input (stream buffer-only)
    device-write (stream buffer start end blocking)
    device-clear-output (stream)
    device-flush (device)
    device-read-content (device &rest content-arguments)
    device-write-content (device body &rest content-arguments)

 The device-open operator returns an instance which can be used directly. In the case of a channel, it
connects to the respective connection and prepares any exchange/queue specified in uri initialization
argument.  In that state it can be supplied to publish, consume, or get operations to initiate stream
i/o, or to transfer isolated objects. In the case of a connection, a socket stream is opened to the
designated server and the protocol handshake and open-connection phases are completed. (see
open-connection and negotiate-client-connection) In that state, one can construct and use channels. In the
case of a connection, it should be used only to make channels. Note that the connection class is adjusted to
match that of the protocol version which it negotiates with the remote broker. That is, a

    (make-instance 'amqp:connection  :uri \"amqp://guest:guest@localhost/\")

will return a connection sub-class. This places constraints on the effective methods for `device-open` and the
various constituents of the standard instantiation protocol for `simple-stream`.
The respective effective methods for the base implementation look something like

    initialize-instance : (CONNECTION) (STANDARD-OBJECT)
    shared-initialize (in mcl) : (CONNECTION T) (AMQP-DEVICE T) :AFTER (SIMPLE-STREAM T) (STANDARD-OBJECT T)
    reinitialize-instance : (STANDARD-OBJECT)
    device-open (in sbcl) : (CONNECTION T) (AMQP-SOCKET-DEVICE T) (AMQP-DEVICE T) (SIMPLE-STREAM T) (SLOT-OBJECT T)

The exact form depends on the run-time (cf. `standard-object` v/s `sloto-object`), but all share the topology,
that no next method call follows the class change. Should protocol-specific specialization be necessary, any
specialized operation subsequent to the change would need to be independent of these effective methods.

 The device-read and device-write operators are defined at the respective levels to delegate a channel's
operations to the respective connection and to perform connection i/o through the socket stream. When
channel operations  with an explicit buffer, are intended to implement data stream-based data transfer
for a channel which has already initiated a message exchange and observe the body size constraints
specified fro the message. Where the size is known at the outset, the body-size and body-position govern
eof behaviour. Where the size was unkown, the channel implemented chunked transfer in terms of maxmial
sized messages, and relies on device-flush and device-clear-input to indicate and manage the effective
eof.

 The state operators for position and length return meaningful information for fixed content sizes only
and have no effect to modify channel state. 

 For more information on simple streams, see Franz's documentation[3] and the sbcl implementation[4] of same,
as well as the discussions of the the alternative fu interface.[5]

 ---
 [1]: https://jira.amqp.org/confluence/download/attachments/720900/amqp0-8.pdf?version=1
 [2]: https://jira.amqp.org/confluence/download/attachments/720900/amqp0-9-1.pdf?version=1
 [3]: http://www.franz.com/support/documentation/current/doc/streams.htm
 [4]: http://sbcl.cvs.sourceforge.net/viewvc/sbcl/sbcl/contrib/sb-simple-streams/
 [5]: http://paste.lisp.org/display/65229"))




(defmacro assert-device-state (device state &optional op)
  (let ((state (or (find-symbol (string state) :amqp.s)
                   (error "invalid state indicator: ~s." state)))
        (device-state-var (gensym)))
    `(let ((,device-state-var (device-state ,device)))
       (assert (typep ,device-state-var ',state) ()
               "~@[~a: ~]Required device state ~a is not satisfied by ~a."
               ',op ',state ,device-state-var))))


;;;
;;; channel operations


(defmethod device-open ((device amqp:channel) #-sbcl (slots t) initargs)
  "Prepare a channel by binding it locally to a connection and declaring its
 exchange and queue on the connection's server. The connection may be provided
 as an instance to be re-used, or as a server designator, in which case a
 news connection is opened."

  (etypecase (device-state device)
    (amqp.s:open-channel
     (if (or (stream-input-handle device)
             (stream-output-handle device))
       (call-next-method)
       (destructuring-bind (&key (connection (or (channel-connection device)
                                                 (error "connection required.")))
                                 (input-handle connection)
                                 (output-handle connection)
                                 &allow-other-keys)
                           initargs
         (call-next-method)
         (setf (stream-input-handle device) input-handle)
         (setf (stream-output-handle device) output-handle)

         ;; once that has been done, adjust the channel class
         (let ((connection-channel-class (class-find-object-class connection 'amqp:channel)))
           (unless (typep device connection-channel-class)
             (change-class device connection-channel-class)))

         ;; bind the channel to the connection to obtain the queues and initialize buffers
         (connect-channel connection device)

         ;; unless it's the connection's own channel,
         ;; open the channel w/ the broker 
         
         (unless (zerop (channel-number device))
           ;; resolve the channel's identifer relative to the connection - with
           ;; non-strict handling to allow a scheme.
           (let* (;; (uri (merge-uris (channel-uri device) (connection-uri connection) nil nil))
                  ;; don't merge again here. iinitialize-instance already does it to the initialization argument
                  ;; and the channel should never move from one connection to another.
                  (uri (channel-uri device))
                  (host (uri-host uri)))
             (setf-device-uri uri device)

             ;; if the connection is 'for real', that is, if it specifies a host, open the channel
             (unless (zerop (length host))
               (amqp:open device)
               #+(or ) ;; don't do this here:
                       ;; the 0.8r0 channel specializes device-open to get a ticket, which would need to
                       ;; happen before this, as the ticket is an argument.
                       ;; could go in initialize instance
               (let ((exchange (uri-exchange uri))
                     (queue (uri-queue uri))
                     (object-path (uri-path uri))
                     (cond (exchange
                            (let ((queue (amqp:channel.queue device :queue queue))
                                  (exchange (amqp:channel.exchange device :exchange exchange :type "direct")))
                              (amqp:declare queue)
                              (amqp:declare exchange)
                              (amqp:bind queue :exchange exchange :queue queue
                                         :routing-key object-path)))
                        (queue
                         ;; if there is no exchange, allow input only
                         (assert (eq (stream-direction device) :input) ()
                                 "An exchange must be provided for channel output.")
                         (amqp:declare (amqp:channel.queue device :queue queue)))))))))
          (setf (device-state device) amqp.s:use-channel)
          device)))
    (amqp.s:use-channel
     (call-next-method))))


(defmethod device-close ((device amqp:channel) (abort t))
  "remove the channel from the connection."
  (amqp:log :debug device "Close in state: ~s" (channel-state device))
  (if (zerop (channel-number device))
    (amqp:log :warn device "Attempt to close channel zero.")
    (when (open-stream-p device)
      (cond (abort
             (setf (channel-state device) amqp.s:close-channel)
             (call-next-method))
            (t 
             (let ((initial-state (shiftf (channel-state device) amqp.s:close-channel)))
               (typecase initial-state
                 ;; if in use, send the close request, the flush it
                 (amqp.s:use-channel
                  (amqp:request-close device)
                  ;; complete and flush the content.
                  (device-flush device t)))
               (call-next-method))))
      ;; in any case disconnect
      (disconnect-channel (channel-connection device) device))))


(defmethod device-read ((device amqp:channel) buffer-arg start end blocking)
  "Channels read a frame at a time through a connection.
 the connection manages the actual stream and makes frames available as
 they appear. the specified 'blocking' mode determines whether to
 wait if there is nothing present."
  (assert-device-state device use-channel.body.input device-read)
  (with-slots (buffer buffpos buffer-ptr buf-len body-position body-length) device
    (cond ((< buffer-ptr 0)
           -1)
          ((eql start end)              ; interpret blocking
           (if blocking
             ;; nothing is read anyway
             0
             ;; iff not blocking , see if anything is present or in the read queue
             (if (or (< buffpos buffer-ptr) (not (collection-empty-p (device-read-frames device))))
               -3
               0)))
          ((>= body-position body-length)
           (typecase (device-state device)
             (amqp.s:use-channel.body.input.chunked
              ;; chunked => start the next message
              ;; - if the last was a deliver, wait for the next
              ;; - if it was a get-ok, ask for the next
              (command-case (device)
                ((or amqp:get-ok amqp:deliver) ((basic amqp:basic) &rest args)
                 (amqp:log :debug device "chunk continuation: (~s ~s) . ~s"
                           (type-of amqp:method) (type-of basic) args)
                 t)
                (t ((class t) &rest args)
                   (amqp:log :error device "Unexpected chunk continuation: (~s ~s) . ~s"
                             (type-of amqp:method) (type-of class) args)
                   t))
              (let ((result (device-read-content-header device)))
                (amqp:log :debug device "read chunk header: ~s" result))
              ;; if that succeeds, read the 
              (if (< body-position body-length)
                ;; just try again
                (device-read device buffer-arg start end blocking)
                -1))
             (t
              ; not chunked => mark eof
              (setf buffer-ptr -1))))
          (buffer-arg
           ;; if a buffer is provided, use it+bounds together with the devices buffer+bounds
           ;; to iteratively fill the argument buffer. recurse for more input
           ;; maintain body-position since the buffer is accepting the content.
           (let ((total-count 0)
                 (last-device-read 0))
             (unless end (setf end (length buffer-arg)))
             (unless start (setf start 0))
             (loop (unless (> start end) (return))
                   (when (>= buffpos buffer-ptr)
                     (unless (and (< body-position body-length)
                                  (plusp (setf last-device-read (device-read device nil 0 nil blocking))))
                       (return)))
                   (let* ((count (min (- end start) (- buffer-ptr buffpos)))
                          (end1 (+ start count))
                          (end2 (+ buffpos count)))
                     (replace buffer-arg buffer :start1 start :end1 end1 :start2 buffpos :end2 end2)
                     (setf start end1
                           buffpos end2)
                     (incf total-count count)
                     (incf body-position count)))
             (if (zerop total-count)
               last-device-read
               total-count)))
          (t
           ;; otherwise read a frame buffer
           (assert (and (zerop start) (or (null end) (= end (length buffer)))) ()
                   "Frame buffer i/o permitted for entire buffers only.")
           (let* ((frame nil))
             (loop (setf frame (get-read-frame device :wait blocking))
                   (amqp:log :debug device "device-read: next read frame: state: ~a, size: ~d, body: ~s/~s, buffer: ~s/~s/~s."
                             (type-of (device-state device)) (frame-size frame) body-position body-length buffpos buffer-ptr buf-len)
                   ;; if non-blocking, maybe no frame
                   (if frame 
                     (cond ((plusp (frame-size frame))
                            (let* ((data (frame-data frame))
                                   (length (frame-size frame)))
                              (rotatef buffer data)
                              (setf-frame-data data frame)
                              (release-frame frame)
                              (setf buffpos 0)
                              (setf buffer-ptr length)
                              (setf buf-len (length buffer))   ; could change iff possible to re-tune
                              (assert (<= buffer-ptr buf-len) ()
                                      "Invalid buffer sizes: ptr ~d, len ~d." buffer-ptr buf-len)
                              (amqp:log :debug device "device-read: adjusted pointers: state: ~a, body: ~s/~s, buffer: ~s/~s/~s."
                                        (type-of (device-state device)) body-position body-length buffpos buffer-ptr buf-len)
                              (return length)))
                           (t
                            ;; if the frame is a zero-length frame, always skip it 
                            (release-frame frame)
                            ;; iff chunking, also end chunking, indicate end of the body and
                            ;; skip next padding frame as well.
                            (typecase (device-state device) 
                              (amqp.s:use-channel.body.input.chunked
                               (amqp:log :debug device "device-read: skip zero chunk: state: ~a, body: ~s/~s, buffer: ~s/~s/~s."
                                         (type-of (device-state device)) body-position body-length buffpos buffer-ptr buf-len)
                               (setf (device-state device) amqp.s:use-channel.body.input)
                               (cond ((setf frame (get-read-frame device :wait t))      ; resolve it now
                                      (assert (and (eq (frame-type-class-name frame) 'amqp:body)
                                                   (= (frame-size frame) (- body-length body-position))) ()
                                              "Invalid pad frame: ~s." frame)
                                      (setf body-length body-position)          ; 'truncate' the body
                                      (release-frame frame)
                                      (return (setf buffer-ptr -1)))
                                     (t
                                      (return (setf buffer-ptr -1))))))))
                     (return (when blocking (setf buffer-ptr -1))))))))))


(defmethod device-write ((device amqp:channel) buffer-arg start end (blocking t))
  "Channels read a frame at a time through a connection.
 The connection manages the actual stream and writes frames as required. The
 specified 'blocking' mode has no affect on output."
  (assert-device-state device use-channel.body device-write)
  (with-slots (out-buffer outpos max-out-pos) device
    (cond (buffer-arg
           ;; if a buffer is provided, use it+bounds together with the devices buffer+bounds
           ;; to iteratively empty the argument buffer. recurse for progressive output
           (let ((total-count 0))
             (loop (when (>= start end)
                     (return))
                   (let* ((count (min (- end start) (- max-out-pos outpos)))
                          (source-start start) (source-end (+ source-start count))
                          (buffer-start outpos) (buffer-end (+ outpos count)))
                     (setf start source-end
                           outpos buffer-end)
                     (when (> (+ (device-body-position device) count)
                              (device-body-length device))
                       (error "DEVICE-WRITE: output exceeds content length: ~s, ~s."
                              (+ (device-body-position device) count)
                              (device-body-length device)))
                     (replace out-buffer buffer-arg :start1 buffer-start :end1 buffer-end :start2 source-start :end2 source-end)
                     (incf total-count count)
                     (if (>= outpos max-out-pos)
                       ;; flush the buffer
                       (let ((result (device-write device nil 0 nil blocking)))
                         (when (minusp result)
                           (return-from device-write result)))
                       (incf (device-body-position device) count))))
             total-count))
          (t
           (assert (and (zerop start) (or (null end) (= end (length out-buffer)))) ()
                   "Frame buffer i/o permitted for entire buffers only.")
           (device-flush device)))))



(defmethod device-flush ((device amqp:channel) &optional (complete nil))
  "Push data to the channel'c connection.
 DEVICE : amqp:channel : an open channel
 COMPLETE : boolean : iff true, an in-progress chunked body stream is closed. 

 Given a buffer, its content is passed through the channel's
 frame buffers. Lacking a buffer, the exisitng frame buffer is sent. The effect is to support
 single-frame commands, sequence-based stream io, and buffer based stream io. The device state is checked to
 confirm an operation makes sense.

 On framed content, streams, and chunking:
 there are three aspects:
 * is there buffered content: (zerop outpos) ?
 * is the content complete?
 * was the content length known ahead of time: .output or .output.chunked ?

 if there is content, wrap up the body frame and send it.
 given the now empty buffer, if the content body is now complete, then it matters whether the length was
 predetermined. if it was, that is, in state .output, since this is not an abort, sufficient
 frames must be sent to achieve the content length. if the length was not predetermined, then the
 state is .output.chunked and the end of the sequence of commands is indicated by sending a
 zero-length body frame and then a frame padded to fill the content length. should this have been an
 empty buffer, then the zero-length frame will be followed by a full length pad."

  (let ((result-length 0))
    (typecase (device-state device)
      (amqp.s:use-channel.body.output
       (with-slots (out-buffer outpos max-out-pos body-position body-length) device
         (amqp:log :debug device "device-flush (~a) ~d, ~d, ~d"
              (device-state device) body-length body-position max-out-pos)
         (flet ((flush-frame ()
                  (let* ((frame (claim-output-frame device))
                         (length outpos))
                    (rotatef out-buffer (frame-data frame))
                    (setf-frame-type-class-name 'amqp:body frame)
                    (setf-frame-cycle 0 frame)
                    (setf-frame-channel-number (channel-number device) frame)
                    (setf-frame-track-number (channel-track device) frame)
                    (setf-frame-size outpos frame)
                    (put-encoded-frame device frame)
                    (setf outpos 0)
                    (setf max-out-pos (length out-buffer))
                    length)))
           ;; check whether there is anything to flush
           (when (or (plusp outpos) (zerop body-length))       ; always at least one frame
             ;; if there is content, send the frame out.
             (setf result-length (flush-frame)))
           ;; check completion
           (cond (complete
                  (typecase (device-state device)
                    (amqp.s:use-channel.body.output.chunked
                     ;; if the content was chunked, send a zero-length frame and revert to .output
                     (amqp:log :debug device "Ending chunking. padding ~d v/s ~d bytes..."
                               (- body-length body-position) max-out-pos)
                     (flush-frame)
                     (setf (device-state device) amqp.s:use-channel.body.output)))
                  ;; now send frames to fill the difference between content-position and
                  ;; content-length - as many as needed
                  (do ((count (min (- body-length body-position) max-out-pos)
                              (min (- body-length body-position) max-out-pos)))
                      ((<= count 0))
                    (fill out-buffer 0 :start 0 :end count)
                    (setf outpos count)
                    (flush-frame)
                    (incf body-position count))
                  (amqp:log :debug device "Ended padding."))
                 (t
                  (typecase (device-state device)
                    (amqp.s:use-channel.body.output.chunked
                     ;; if so, need to send a new command:
                     ;; send a new publish, reset the buffer positions, and continue streaming
                     (let ((basic (channel.basic device)))    
                       (amqp:log :debug basic "Starting next chunk: publish...")
                       (amqp:send-publish basic :exchange (amqp:basic-exchange basic))
                       (setf outpos 0
                             max-out-pos (length out-buffer)
                             ;; start a new body
                             body-length (class-body-size basic)
                             body-position 0)
                       (amqp:log :debug basic "Starting next chunk: header...")
                       (send-header basic)
                       (amqp:log :debug basic "Starting next chunk: done."))))))
           result-length))))))


(defmethod device-clear-input ((device amqp:channel) buffer-only)
  ;;; call the decoder to clear a possible pushed character and correct position
  ;;; then "empty" the buffer.
  ;;; then, unless buffer-only, also flush any not yet read frames until the end of the body is reached
  (with-slots (decoder buffer buffpos buffer-ptr buf-len body-position body-length) device
    (when decoder                       ; maybe not present for binary streams
      (funcall decoder #'(lambda (s) (declare (ignore s)) 0) device))
    ;; skip over anything already in the buffer
    (amqp:log :debug device "device-clear-input: skip current-frame: state: ~a, body: ~s/~s, buffer: ~s/~s/~s."
                  (type-of (device-state device)) body-position body-length buffpos buffer-ptr buf-len)
    (setf body-position (+ body-position (- buffer-ptr buffpos)))
    (setf buffpos buffer-ptr)
    ;; optionally drain pending input
    (unless buffer-only
      ;; flush input
      (loop
        (amqp:log :debug device "device-clear-input: drain expected frames: state: ~a, at ~s of ~s"
                  (device-state device) body-position body-length)
        (when (>= body-position body-length)
          (return))
        (unless (plusp (device-read device nil 0 nil t))
          (return))
        (incf body-position buffer-ptr)))
    nil))


(defmethod device-buffer-length ((device amqp:channel))
  (let ((connection (channel-connection device)))
    (if connection
      (device-buffer-length connection)
      0)))


(defmethod (setf device-file-position) (position (device amqp:channel))
  (declare (ignore position))
  (device-body-length device))


(defmethod device-file-length ((device amqp:channel))
  (device-body-length device))


(defmethod (setf device-file-length) (length (device amqp:channel))
  (declare (ignore length))
  nil)



;;;
;;; connection operations


(defmethod device-open ((device amqp:connection) #-sbcl (slots t) initargs)
  "Prepare a connection by opening a socket to broker, negotiating the
 protocol parameter, and opening the virutal host."

  (typecase (device-state device)
    (amqp.s:open-connection
     (if (or (stream-input-handle device)
             (stream-output-handle device))
       (call-next-method)
       (destructuring-bind (&key (uri (connection-uri device))
                                 (version (class-protocol-version device))
                                 (direction :io)
                                 &allow-other-keys)
                           initargs
         ;; merge the host and port information from/to the uri
         (let ((remote-host (uri-host uri))
               (remote-port (or (uri-port uri) *standard-port*)))
           
           (ecase direction
             (:probe (call-next-method device #-sbcl slots
                                       (list* :remote-host remote-host
                                              :remote-port remote-port
                                              :direction :probe
                                              initargs)))
             (:io (when (call-next-method device #-sbcl slots
                                          (list* :remote-host remote-host
                                                 :remote-port remote-port
                                                 :direction :io
                                                 initargs))
                    (when (open-connection device :version version)
                      ;; once the concrete class is fixed, initialize the buffer
                      (device-initialize-buffers device)
                      (negotiate-client-connection device)
                      t))))))))
    (amqp.s:use-connection
     (call-next-method))
    (t
     ;; have observed it called from shared-initialize to update an obsolete instance
     ;; when terminating it in for gc !!
     nil)))


(defmethod device-close ((device amqp:connection) (abort t))
  (map nil #'(lambda (c)
               (when (and c (plusp (channel-number c)))
                 (device-close c abort)))
       (get-connection-channels device))
  (if abort
    (call-next-method)
    (typecase (shiftf (device-state device) amqp.s:close-connection)
      ;; never succeeded to open
      (amqp.s:open-connection
       (call-next-method))
      ;; if it's in use, perform a protocol close, then flush that data
      ;; close the stream, and reset to the initial state
      (amqp.s:use-connection
       (amqp:request-close device)
       (device-flush device t)
       (multiple-value-prog1 (call-next-method)
         (setf (connection-state device) amqp.s:open-connection)))
      ;; otherwise, the protocol close is already in progress;
      ;; just flush and close the stream
      (t
       (device-flush device t)
       (call-next-method)))))
      



(defmethod device-read ((device amqp:connection) buffer-arg start end (blocking t))
  "Connections permit stream io only if there is just one channel read a frame at a time through a connection.
 the connection manages the actual stream and makes frames available as
 they appear. the specified 'blocking' mode determines whether to
 wait of there is noting present."
  
  (assert-device-state device use-connection amqp.s:use-channel.body.input)
  (with-slots (buffer buffpos buffer-ptr buf-len) device
    (if (or (< buffer-ptr 0)
            (>= (device-body-position device) (device-body-length device)))
      -1
      (cond (buffer-arg
             ;; if a buffer is provided, use it+bounds together with the devices buffer+bounds
             ;; to iteratively fill the argument buffer. recurse for more input
             (let ((total-count 0))
               (loop (let* ((count (min (- end start) (- buffer-ptr buffpos)))
                            (start1 start) (end1 (+ start1 count))
                            (start2 buffpos) (end2 (+ buffpos count)))
                       (setf start end1
                             buffpos end2)
                       (replace buffer-arg buffer :start1 start1 :end1 end1 :start2 start2 :end2 end2)
                       (incf total-count count)
                       (when (>= start end)
                         (incf (device-body-position device) count)
                         (return))
                       ;; read more
                       (let ((result (device-read device nil 0 nil blocking)))
                         (when (minusp result)
                           (return-from device-read result)))))
               total-count))
            (t
             ;; otherwise read a frame buffer
             (assert (and (zerop start) (null end)) ()
                     "Frame buffer i/o permitted for entire buffers only.")
             (loop (let ((frame (get-read-frame device :wait blocking)))
                     (if frame
                       (when (plusp (length frame))
                         (let* ((data (frame-data frame))
                                (length (frame-size frame)))
                           (rotatef buffer data)
                           (setf-frame-data data frame)
                           (release-frame frame)
                           (setf buffpos 0)
                           (setf buffer-ptr length)
                           (setf buf-len (length buffer))
                           (assert (<= buffer-ptr buf-len) ()
                                   "Invalid buffer sizes: ptr ~d, len ~d." buffer-ptr buf-len)
                           (return length)))
                       (return (setf buffer-ptr -1))))))))))


;;; this duplicates most of the channel version, but for the buffer source
(defmethod device-write ((device amqp:connection) buffer-arg start end (blocking t))
  (assert-device-state device :use-connection device-write)
  (with-slots (out-buffer outpos max-out-pos) device
    (cond (buffer-arg
           ;; if a buffer is provided, use it+bounds together with the devices buffer+bounds
           ;; to iteratively empty the argument buffer. recurse for progressive output
           (let ((total-count 0))
             (loop (when (>= start end)
                     (return))
                   (let* ((count (min (- end start) (- max-out-pos outpos)))
                          (source-start start) (source-end (+ source-start count))
                          (buffer-start outpos) (buffer-end (+ outpos count)))
                     (setf start source-end
                           outpos buffer-end)
                     (when (> (+ (device-body-position device) count)
                              (device-body-length device))
                       (error "DEVICE-WRITE: output exceeds content length: ~s, ~s."
                              (+ (device-body-position device) count)
                              (device-body-length device)))
                     (replace out-buffer buffer-arg :start1 buffer-start :end1 buffer-end :start2 source-start :end2 source-end)
                     (incf total-count count)
                     (if (>= outpos max-out-pos)
                       ;; flush the buffer
                       (let ((result (device-write device nil 0 nil blocking)))
                         (when (minusp result)
                           (return-from device-write result)))
                       (incf (device-body-position device) count))))
             total-count))
        (t
         (assert (and (zerop start) (null end)) ()
                 "Frame buffer i/o permitted for entire buffers only.")
         (device-flush device)))))

(defmethod device-flush ((device amqp:connection) &optional complete-p)
  (declare (ignore complete-p))
  (typecase (device-state device)
    (amqp.s:use-connection
     (with-slots (out-buffer outpos max-out-pos) device
       (when (plusp outpos)
         (let* ((frame (claim-output-frame device))
                (length outpos))
           (rotatef out-buffer (frame-data frame))
           (setf-frame-type-class-name 'amqp:body frame)
           (setf-frame-cycle 0 frame)
           (setf-frame-channel-number (channel-number device) frame)
           (setf-frame-track-number (channel-track device) frame)
           (setf-frame-size outpos frame)
           (put-encoded-frame device frame)
           (setf outpos 0)
           (setf max-out-pos (length out-buffer))
           length))))))

(defmethod device-buffer-length ((device amqp:connection))
  "Until the connection has been specialized for the protocol, propose the full frame
 as the buffer size."
  (let ((frame-class (connection-input-frame-class device)))
    (- (connection-frame-size device)
       (if frame-class
         (frame-overhead (allocate-instance (find-class frame-class)))
         0))))

(defmethod device-file-position ((device amqp:connection))
  (device-body-position device))

(defmethod (setf device-file-position) (position (device amqp:connection))
  (declare (ignore position))
  nil)

(defmethod device-file-length ((device amqp:connection))
  (device-body-length device))

(defmethod (setf device-file-length) (length (device amqp:connection))
  (declare (ignore length))
  nil)



(defmethod open-connection ((connection amqp:connection)
                            &key (version (class-protocol-version connection))
                            (attempt 1))
  "Given a CONNECTION and an optional VERSION, open a socket to the respective
 host/port, negotiate the version and process any initial frame(s).

 CONNECTION : amqp:connection : an initialized connection w/o a socket. May be
  an abstract connection
 :VERSION : amqp:version : Intended version
 VALUES : (or stream null) : the open stream if succeeded
          amqp:version : the negotiated version

 The socket is opened and an attempt is made to read a permitted version. The
 criteria is, whether a connection class is defined which supports the version.
 If so, the given connection instance's class is changed to agree.
 In addition, a possible initial frame is read and queued."

  (let ((buffer-out (make-array 8 :element-type 'unsigned-byte))
        (buffer-in (make-array 8 :element-type 'unsigned-byte))
        (version-received nil)
        (byte-zero 0))
    (labels ((negotiation-failed (&optional reason)
               (error "Connection negotiation failed~@[ (~a)~]: requested ~s, class ~s, received ~s"
                      reason
                      version (class-protocol-version connection) version-received))
             (update-connection-class (version-received)
               (let ((new-class (amqp:find-protocol-class connection version-received)))
                 (unless new-class
                   (negotiation-failed "Unsupported protocol version"))
                 (cond ((eq new-class (class-of connection)))
                       ((eql attempt 1)
                        (change-class connection new-class)
                        (amqp:log :debug connection "open-connection: updated class to ~s."
                                  (type-of connection)))
                       (t
                        (negotiation-failed "Re-negotiation failed to match")))))
             (cycle-socket ()
               (let* ((uri (device-uri connection))
                      (remote-host (uri-host uri))
                      (remote-port (or (uri-port uri) *standard-port*)))
                 (setf (device-socket connection)
                       (usocket:socket-connect remote-host remote-port 
                                               :element-type 'unsigned-byte)))))
      
      (setf (buffer-protocol-header-version buffer-out) version)
      (amqp:log :debug connection "open-connection: requesting version: ~s/~s."
                buffer-out version)
      (write-sequence buffer-out (stream-output-handle connection))
      (force-output (stream-output-handle connection))
      (case (setf byte-zero (read-byte (stream-input-handle connection)))
        ;; the later protocols reply with a version to confirm, but
        ;; the early ones just send the start frame immediately
        (#.(char-code #\A)
         ;; if a protocol header is returned, if the version matches, ok
         ;; otherwise close the connection and return the version
         (setf (aref buffer-in 0) #.(char-code #\A))
         (cond ((= 8 (read-sequence buffer-in (stream-input-handle connection) :start 1))
                (setf version-received (buffer-protocol-header-version buffer-in nil))
                (amqp:log :debug connection "open-connection: parsed version: ~s / ~s."
                          buffer-in version-received)
                ;; negotiate the protocol
                (cond (version-received
                       ;; use received versions to specialize the given instance
                       (update-connection-class version-received)
                       ;; cycle the socket and retry to connect
                       (cycle-socket)
                       (open-connection connection :attempt (1+ attempt) :version version-received))
                      (t
                       (negotiation-failed (format nil "Unsupported protocol header: ~a." buffer-in)))))
               (t
                (negotiation-failed "Incomplete protocol header"))))
        (t
         ;; version accepted w/ immediate connection-open
         ;; still, update to change from abstract to concrete class
         (amqp:log :debug connection "open-connection: byte-zero: ~s, connection class ~s."
                   byte-zero (type-of connection))
         (setf version-received version)
         (update-connection-class version)

         ;; this had to wait until the connection had been transformed in to a  concrete class
         (let ((frame (claim-input-frame connection)))
           (setf (aref (frame-header frame) 0) byte-zero)
           ;; (print :no-header)
           (read-frame connection frame :start 1)
           ;; make channel-zero and prime it with the first frame
           (amqp:connection.channel connection :number 0)
           (put-read-frame connection frame)
           (values version frame)))))))


(:documentation (negotiate-client-connection open-connection)
  "AMQP connection negotiation occurs in two steps. First, the peers agree on a protocol version. Second
they exchange  authentication and control information to set up the connection. The first step is
implemented by open-connection. It negotiates with the broker to agree on a supported protocol version
and changes the connection instance's class to that of the implementation class for that version. For
some protocol versions, it is also confronted with the initial frame, which it buffers for the
configuration step.

 The second step, authentication and configuration, is implemented in negotiate-client-connection. It
exchanges connection commands with the broker for authentication and tuning. The configured connection is
returned.")



(defmethod negotiate-client-connection ((device amqp:connection) &key (retry-limit 2))
  "Negotiate security and open a virtual host:
 - construct a channel-zero instance.
 - go through the handshake w/ start, secure, tune comamnds.
 - open channel-zero for the connection's host. "

  (let ((channel (amqp:connection.channel device :number 0)))
    (command-case (channel)
      (amqp:start ((class amqp:connection) &rest args)
       (setf (connection-state device) amqp.s:open-connection.start)
       (apply #'channel-respond-to-start channel device args)
       t)
      (t ((class t) &rest args)
         (error "Invalid negotiation command: ~s~{ ~s~}." class args)))
    
    (dotimes (x retry-limit (error "Security negotiation failed."))
      (command-case (channel)
        (amqp:secure ((class amqp:connection) &rest args)
                     (setf (connection-state device) amqp.s:open-connection.secure)
                     (apply #'channel-respond-to-secure channel class args)
                     t)
        (amqp:tune ((class amqp:connection) &rest args)
                   (setf (connection-state device) amqp.s:open-connection.tune)
                   (apply #'channel-respond-to-tune channel class args)
                   (return))))

    (setf (connection-state device) amqp.s:open-connection.host)
    (channel-request-open channel device :virtual-host (connection-virtual-host device))
    (setf (connection-state device) amqp.s:use-connection)
    
    device))


(:documentation (device-read-content device-write-content)
  "The content processing interface comprises the two operators
  * device-read-message (channel &rest)
  * device-write-message (channel body &rest)

 Each accepts the keywords which apply to the respctive protocol operation, that is, any method arguments and the class'
 header properties. for reading this means the arguments for get and deliver, while for writing those for publish.
 The interface supports two use patterns : body instances and continuation based. The decision is made by the writer
 according to whether the body size is known at the outset. For fixed length vectors this is true. For aeverything else, it
 is not. Where it is known, a single content-bearing command is sent. Where is it not known, A sequence of commands is
 sent until the writer terminates the stream by indicating completion in a call to device-flush.

 Streams are broken into and reconstituted from the three frame constituents for a command : method, header, and body.
 * a method frame : is emitted by a request operator and parsed and processed by the respond-to- operator to cache the
   arguments in the channel. The protocol interface operator then invokes device-read/write-content.
 * a content header : ion inpt, s parsed in device-read-content/content-header to extract the properties and cache them
   in the channel's basic class instance. On output, it is generated by device-write-content based on the channel's current
   basic instance
 * the content body : is parsed based on the channel's internal type combined with the content type. It is generated
   based on the bassed body instance in combination with the content type /encoding.

  [ channel-request-publish channel-respond-to-get-ok channel-respond-to-deliver ]
  --> [ device-read-content  device-write-content]
      --> [ device-read-content-header device-write-content-header ]
      --> [ device-read-content-body device-write-content-body ]
 ")

(defgeneric device-read-content (channel &key delivery-tag redelivered exchange routing-key
                                         ;; from get-ok only
                                         message-count
                                         ;; from deliver only
                                         consumer-tag
                                         )
  (:documentation "Given a channel which has received a Basic.Deliver or Basic.Get/Get-ok,
 first, prepare the channel based on the content header properties, and read the content
 according to the combined channel data type and content type. Combine the header's possibly
 incomplete content type with the channel's to specify the effective decoding.")

  (:method ((channel amqp:channel) &key body delivery-tag redelivered exchange routing-key message-count consumer-tag)
    (declare (ignore delivery-tag redelivered exchange routing-key message-count consumer-tag))
    (setf (channel-state channel) amqp.s:use-channel.body.input)
    (assert-argument-type device-read-content body (or null function))
    (let* ((basic (device-read-content-header channel))
           (headers (amqp:basic-headers basic))
           (element-type (getf headers :element-type))
           (package (getf headers :package))
           (content-type (mime:mime-type (amqp:basic-content-type basic))))
      ;; element-type in the basic header combines the read values with the channel's content-type
      (when element-type
        (setf element-type (or (find-symbol element-type package)
                               (error "Invalid type x package combination: ~s, ~s."
                                      element-type package))))
      (amqp:log :debug channel "device-read-content: in (~s ~s) in state ~s x~s"
                element-type content-type (channel-state channel) (device-body-length channel))
      (with-slots (buffpos buffer-ptr) channel
        ;; clear possible past EOF; permits immediate device-clear to skip w/o first reading any input
        (setf buffpos 0)
        (setf buffer-ptr 0))
      (device-read-content-body channel (or body element-type) content-type))))


(defgeneric device-read-content-header (channel )
  (:method ((channel amqp:channel))
    (command-loop (channel)
      (amqp:header ((basic amqp:basic) &key frame &allow-other-keys)
        (declare (ignore frame))
        ;; merge the header's content type with the stream's
        (let* ((body-size (class-body-size basic))
               (headers (amqp:basic-headers basic))
               (content-type (mime:mime-type (amqp:basic-content-type basic))))
          (assert-argument-type device-read-content-body body-size integer)

          (with-slots (buffer buffer-ptr body-length body-position) channel
            (unless buffer
              (device-initialize-input-buffer channel))
            (setf body-length body-size
                  buffer-ptr 0
                  body-position 0))
          ;; cause (update-device-codecs channel mime-type)
          (unless (eq (channel-content-type channel) content-type)
            (setf (channel-content-type channel) content-type))
          (setf (channel-state channel)
                (if (string-equal (getf headers :transfer-encoding) "chunked")
                  amqp.s:use-channel.body.input.chunked
                  amqp.s:use-channel.body.input)) 
          (return-from command-loop basic))))))

(defgeneric device-read-content-body (device type content-type)

  (:method ((channel amqp:channel) (type (eql 'string)) (content-type mime:text/plain))
    (let* ((body-length (device-body-length channel))
           ;; construct a string with the message content
           (body (make-string body-length)))
      (read-sequence body channel :start 0 :end body-length)
      body))

  (:method ((channel amqp:channel) (type null) (content-type mime:text/plain))
    (device-read-content-body channel 'string content-type))

  (:method ((channel amqp:channel) (body-op function) (content-type mime:*/*))
    "Given a the null type, just return the channel as a stream to be read."
    (prog1 (funcall body-op channel content-type)
      ;; once the operator has read, clear to the end of the message
      (device-clear-input channel nil)))

  (:method ((channel amqp:channel) (type (eql 'vector)) (content-type mime:application/octet-stream))
    "Given a the type 'vector, create one and copy the stream content into it."
     (let* ((body-length (device-body-length channel))
            (body (make-frame-buffer body-length)))
        (device-read channel body 0 body-length nil)
        body))

  (:method ((channel amqp:channel) (type null) (content-type mime:application/octet-stream))
    (device-read-content-body channel 'vector content-type))

  (:method ((channel amqp:channel) (channel-type (eql 'list)) (content-type mime::application/sexp))
    "Given an sexp mime type, then read the form."
    (let* ((basic (amqp:channel.basic channel))
           (headers (amqp:basic-headers basic))
           (package (getf headers :package)))
      (setf package (or (find-package package)
                        (amqp:syntax-error :channel channel
                                           :class-code (amqp:class-id basic)
                                           :message-string "Invalid package designator: ~s"
                                           :message-arguments (list package)))) ;;;!!! need a text
      (with-standard-io-syntax
        (setq *read-eval* nil)
        (let ((*package* package))
          (prog1 (read channel)
            (device-clear-input channel nil))))))

  (:method ((channel amqp:channel) (channel-type (eql 'standard-object)) (content-type mime::application/sexp))
    (let* ((basic (amqp:channel.basic channel))
           (headers (amqp:basic-headers basic))
           (package (getf headers :package)))
      (setf package (or (find-package package)
                        (amqp:syntax-error :channel channel
                                           :class-code (amqp:class-id basic)
                                           :message-string "Invalid package designator: ~s"
                                           :message-arguments (list package))))
      (with-standard-io-syntax
        (let ((*package* package))
          (setq *read-eval* nil)
          (let ((form (read channel)))
            (prog1 (reconstitute form)
              (device-clear-input channel nil))))))))

(defun reconstitute (channel-form)
  "Interpret a received object representation as the two values generated by make-load-form.
 This version expects a let expression, because that expresses the necessary circularity."
  (let ((object nil))
    (destructuring-bind (let ((marker creation)) initialization) channel-form
      (declare (ignore let))
      (labels ((simple-eval (form)
                 (typecase form
                   (cons (ecase (first form)
                           (quote (second form))
                           (progn (map nil #'simple-eval (rest form)))
                           ((make-instance initialize-instance allocate-instance reinitialize-instance
                                           shared-initialize find-class)
                            (apply (first form) (mapcar #'simple-eval (rest form))))
                           (setf (loop for ((slot-value x name) value) on (rest form) by #'cddr
                                       do (progn
                                            (assert (eq slot-value 'slot-value) () "invalid form: ~s" form)
                                            (setf (slot-value (simple-eval x) (simple-eval  name))
                                                  (simple-eval value)))))))
                   (t form))))
        (setf object (simple-eval creation))
        (simple-eval (subst object marker initialization))
        object))))

;(reconstitute '(let ((:m (allocate-instance (find-class 'tc)))) (setf (slot-value ':m 'a) '(as d f))))



(defgeneric device-write-content (channel body &rest args
                                          &key class-id weight body-size
                                          exchange routing-key mandatory immediate
                                          content-type content-encoding headers delivery-mode
                                          priority correlation-id reply-to expiration message-id timestamp
                                          type user-id)
  (:documentation "Given a channel which has sent a Basic.Publish,
 firat, write a content header based on the properties, then write the content
 according to the combined channel data type and content type. Combine the header's possibly
 incomplete content type with the channel's to specify the effective encoding.")
  (declare (dynamic-extent args))

  (:method ((channel amqp:channel) body &rest args)
    ;; configure the respective basic for the (content x element-type x content-type)
    ;; combination. this resolve the body size, the transfer encoding, and the 
    ;; transfer element type
    (let* ((basic (apply #'device-write-content-header channel body args)))

      (prog1 (apply #'device-write-content-body channel body (mime-type basic) args)
        (device-flush channel t)))))


(defgeneric device-write-content-header (channel body
                                         &key class-id weight body-size
                                         exchange routing-key mandatory immediate
                                         content-type content-encoding headers delivery-mode
                                         priority correlation-id reply-to expiration message-id timestamp
                                          type user-id)

  (:method ((channel amqp:channel) (body t) &rest args)
    (let* ((basic (apply #'amqp:channel.basic channel :body body args))
           (body-size (class-body-size basic))
           (headers (amqp:basic-headers basic))
           (mime-type (mime-type basic)))
      (with-slots (out-buffer max-out-pos outpos body-length body-position) channel
        (unless out-buffer
          (device-initialize-output-buffer channel))
        (setf body-length body-size
              body-position 0))
      (update-device-codecs channel mime-type)
      (setf (channel-state channel)
            (if (string-equal (getf headers :transfer-encoding) "chunked")
              amqp.s:use-channel.body.output.chunked
              amqp.s:use-channel.body.output))
      (send-header basic)
      basic)))


(defgeneric device-write-content-body (device body content-type
                                       &key
                                       body-size class-id consumer-tag content-type content-encoding correlation-id
                                       delivery-mode delivery-tag exchange expiration headers immediate
                                       mandatory message-count
                                       priority routing-key reply-to message-id
                                       redelivered timestamp type user-id weight)

  (:method ((channel amqp:channel) (body null) (content-type mime:*/*) &rest args)
    "Given a null body , configure the channel to write the message body
 and return the stream."
    (declare (dynamic-extent args) (ignore args))
    nil)

  (:method ((channel amqp:channel) (body string) (content-type mime:text/plain) &rest args)
    (declare (dynamic-extent args) (ignore args))
    
    ;; write the content,
    (stream-write-string channel body 0 nil))

  (:method ((channel amqp:channel) (body-op function) (content-type mime:*/*) &rest args)
    (declare (dynamic-extent args) (ignore args))
    
    ;; call the function
    (funcall body-op channel content-type))

  (:method ((channel amqp:channel) (body vector) (content-type mime:application/octet-stream)  &rest args)
    "Given a the type 'vector, create one and copy the stream content into it."
    (declare (dynamic-extent args) (ignore args))

    (device-write channel body 0 (length body) t))

  (:method ((channel amqp:channel) (body cons) (content-type mime::application/sexp) &rest args)
    "Given an sexp mime type, set up the stream, then read the form."
    (declare (dynamic-extent args) (ignore args))
    (with-standard-io-syntax
      (write body :stream channel :circle t)))

  (:method ((channel amqp:channel) (body standard-object) (content-type mime::application/sexp) &rest args)
    "Given an sexp mime type, set up the stream, then read the form."
    (declare (dynamic-extent args) (ignore args))

    (multiple-value-bind (creation initialization)
                         (make-load-form body)
      (with-standard-io-syntax
        (let ((marker (list :object)))
          (let ((form `(let ((,marker ,creation))
                         ,(subst marker body initialization))))
            (write form :stream channel :circle t)))))))
