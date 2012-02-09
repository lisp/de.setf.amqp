;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines the AMQP input processing pipeline for the 'de.setf.amqp' library."

 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (long-description "The reponse to connected input is computed through a sequence of function calls
 to decode, dispatch, compute, encode and finally send the response. The default path devolves to the static
 operators defined in `def-amqp-command` forms. (see `commands.lisp`.) The combination of stages supports
 sufficient specialization, to permit an application to bind additional mechanisms into the process with both
 static and dynamic extent, based on both lexical and dynamic definitions, and to achieve both synchronous and
 asynchronous processing.

 The principal distinction is between read- and event-mode processing. The
 operators

 - `amqp:command-case`
 - `amqp:command-loop`
 - `amqp:with-commands`

 initiate explicit processing of read frames for a given channel. Each frame
 is tested against clauses and processed by the first which matches. If none
 matches, the first form, by default, initiates standard processing for the
 connection - which has the effect of an event dispatch. Optionally, the
 form can suppress processing, delegate to some other operator, or signal an
 error. The -ecase version signals an error.

 The alternative path initiates the standard processing for a connection
 based on read frames either as part of a thread's explicit input processing
 loop, or when interrupted by a connection's event process in a
 multi-threaded configuration.

 -`process-frame (connection frame)`
    decodes the frame into frame class. delegates to process-typed-frame
    with process-decoded-frame as the continuation.
 - `process-typed-frame (connection channel type frame)`
    if the frame is a method, applies process-command via call-with-decoded-arguments.
    otherwise calls it directly with the class respective the frame type and
    the frame itself
 -`respond-to-command (connection channel class method-or-frame &rest args)`
    computes the class.method function, and applies it in a dynamic context
    augmented by the channel's response functions. If the command target is
    a class, that is passed, otherwise - for headers and body, the frame
    itself is passed.


 In general application processing and output frame generation is coordinated with the stream of
input frames. input and output frame streams are both queue mediated. each channel is associated with a
single process, but multiple channels share a single connection. There are three separate issues
involved in application-specific request/response processing:

 - control : asynchronous/synchronous binding of control flow to the command;
 - protocol: which operations and which logic apply to which read/to-be-written commands;
 - visibility : the scope and extent of the definition;

 The _control_ mechanism must allow for several variations:

               process        channel          connection      control
    1-1-1-s     single         single            single         sync
    1-1-1-a     single         single            single         async
    1-*-1-s     single        multiple           single         sync
    1-*-1-a     single        multiple           single         async
    *-*-1-s    multiple       multiple           single         sync
    *-*-1-a    multiple       multiple           single         async
    1-*-*-s     single        multiple           multiple       sync
    1-*-*-a     single        multiple           multiple       async
    *-*-*-s    multiple       multiple           multiple       sync
    *-*-*-a    multiple       multiple           multiple       async

 This includes just a subset of all possible combinations, as multi-process/single-channel processing is not
supported, and single-channel/multi-connection is not plausible. The async control mechanism does not
necessarily imply an additional "event" process, just that input processong can occur other than as a response
to an explicit read/poll. this would happen when input frames are processed
as a side-effect of output, or frames are processed in addition to those which the
application is intending to read.

 The _protocol_ issues are managed through conventions for class and method naming and maps between
line codes and abstract and protocl-version specific names.

 Each possible response to _visibility_ issues supports a different the application architecture

 - specialize the standard request-/respond-to operators with an additional protocol class for
   synchronous message processing. this can follow a clim/presentation `with-` pattern, or
   it can be in terms of an intrinsic element, such as the connection or channel. in the latter
   case, where the aim is to avoid the additional parameter, pervasiv specialization is accomplished by
   - specializing the ensure-object constructor for the connection or the channel classes 
   - implementing a dynamic, context-specific type->class map for connections and/or channels
   this approach is supported by the static `request-` / `respond-to-` operators, for which the base generic
   operators incorporate an initial channel argument.
 - establish command filters as part of the applications dynamic state / call-stack to implement arbitrary
   state machines. this is accomplished with the `command-case` and
   `command-loop` forms which conditionalize processing clauses by command (object x method) types.
   static state machine.
 - bind handlers to the communication stream - in the form of the connection or channel
   to implement custom behavior with arbitrary  universal or selective operations on frames.

In combination, these approaches yield the generic interface structure

    ;; read frame
    handle-frame (frame) ->
     ;; parse class, method, arguments
     handle-class-and-method (class . method) ->
       ;; delegate to a dynamic handler stack
       statically-declared/dynamically-extent handlers (case class method)
       ;; alternatively, to channel/instance handlers
       dynamically-declare/indefinite-extent handlers (funcall (channel-handler class method))
       ;; ultimately, to static functions
       (apply-static-method method class)


 For other takes on processing patterns see alternative implementations:
 RabbitMQ's java library interposes the AMPCommand [1] class on the channel, which
 acts as a state machine to filter the incoming frames. It releases composed
 commands which combine the operator/arguments, an envelope with content header
 properties, and any content body in on entity or passes it through an event-invocation
 interface. The filtreing means that the library can impose state constraints - eg,
 that all message content is the correct length, w/o interleaving that with application
processing. On the other hand, it impedes streaming.

---
 [1]: http://www.rabbitmq.com/releases/rabbitmq-java-client/v1.7.0/rabbitmq-java-client-javadoc-1.7.0/com/rabbitmq/client/impl/AMQCommand.html"))

;;; the two process-connection-loop version are identical but for the means used to ascertain available input.
;;; #+poll-for-input loops over draining the output queue, listening on and processing input from the connection, and yielding
;;; #-poll-for-input selects on the connection's socket instead of yielding.

#+poll-for-input
(defgeneric process-connection-loop (connection)
  (:documentation "As run in the connection thread loop:
 - write any pending output frames.
 - read any pending input frames and dispatch then through process frame
 - at the outset and before i/o, check that the connection is still open.
   if not return nil. if io fails - an interrupt or network failure closed
   the connection, also return nil.")

  (:method ((connection amqp:connection))
    (let ((in 0) (out 0) (deadline nil))
      (loop
        (flet ((ensure-open (&optional (open-p (open-stream-p connection)))
                 (unless open-p
                   (return-from process-connection-loop (values in out))))
               (heartbeat-needed ()
                 (and deadline
                      (>= (get-universal-time) deadline)))
               (update-heartbeat ()
                 (let ((heartbeat (connection-heartbeat connection)))
                   (setf deadline
                         (when (plusp heartbeat)
                           (+ (get-universal-time) heartbeat))))))
          (loop (ensure-open)
                (let ((frame (get-encoded-frame connection)))
                  (unless frame (return))
                  (unless (write-frame connection frame)
                    (ensure-open nil))
                  (incf out)
                  (release-frame frame)
                  (update-heartbeat)))
          (when (heartbeat-needed)
            (send-heartbeat connection))
          (loop (ensure-open)
                (unless (stream-listen connection) (return))
                (let ((frame (claim-input-frame connection)))
                  (unless (read-frame connection frame)
                    (ensure-open nil))
                  (process-frame connection frame)
                  (incf in)))
          ;; once all io is finished, step back...
          (bt:thread-yield))))))

#-poll-for-input
(defgeneric process-connection-loop (connection)
  (:documentation "Called to processes input and output frames to a single connection.
 - write any pending output frames.
 - read any pending input frames and dispatch then through process frame
 - at the outset and before i/o, check that the connection is still open.
   if not return nil. if io fails - an interrupt or network failure closed
   the connection, also return nil.
 - once all input is processed, wait on the connection's socket.
 If run in a dedicated connection thread, the wait happens w/o holding nay resources, which allows other threads
 to write any output through to the connection. If run single-threaded all processing is event driven through
 the process-frame call.")

  (:method ((connection amqp:connection))
    (let ((in 0) (out 0) (deadline nil)
          (waiters (usocket:make-wait-list (list (device-socket connection)))))
      (loop
        (flet ((ensure-open (&optional (open-p (open-stream-p connection)))
                 (unless open-p
                   (return-from process-connection-loop (values in out))))
               (heartbeat-needed ()
                 (and deadline
                      (>= (get-universal-time) deadline)))
               (update-heartbeat ()
                 (let ((heartbeat (connection-heartbeat connection)))
                   (setf deadline
                         (when (plusp heartbeat)
                           (+ (get-universal-time) heartbeat))))))
          (loop (ensure-open)
                (let ((frame (get-encoded-frame connection)))
                  (unless frame (return))
                  (unless (write-frame connection frame)
                    (ensure-open nil))
                  (incf out)
                  (release-frame frame)
                  (update-heartbeat)))
          (when (heartbeat-needed)
            (send-heartbeat connection))
          (loop (ensure-open)
                (unless (stream-listen connection) (return))
                (let ((frame (claim-input-frame connection)))
                  (unless (read-frame connection frame)
                    (ensure-open nil))
                  (process-frame connection frame)
                  (incf in)))
          ;; run idle handlers
          (dolist (idle-handler (amqp.u::connection-idle-handlers connection))
            (unless (funcall idle-handler connection)
              (return-from process-connection-loop (values in out))))
          ;; once all io is finished, step back...
          ;; !!! presuming single thread
          (usocket:wait-for-input waiters :timeout 1))))))


(defun connection-toplevel-loop (&optional (*connection* *connection*))
  (loop
    (handling-connection-errors
      (handling-channel-errors
        (process-connection-loop *connection*)))))


(defgeneric make-connection-thread (connection)
  (:method ((connection amqp:connection))
    (bt:make-thread 'connection-toplevel-loop
                    :name "Socket i/o"
                    :initial-bindings `((*connection . ,connection)))))      


(defgeneric process-channel-command (channel &key wait)
  (:method ((channel amqp:channel) &key (wait t))
    "Read successive frames from the channel, interpret and respond to them."
    (unless (open-stream-p channel)
      (amqp:channel-error :channel channel :message-string "process-channel-command on a closed stream."))
    (let ((frame (get-read-frame channel :wait wait)))
      (when frame
        (process-frame channel frame))))
  (:method ((connection amqp:connection) &key (wait t))
    (process-channel-command (connection.channel connection :number 0) :wait wait))
  (:method ((class amqp-connected-object) &key (wait t))
    (process-channel-command (object-channel class) :wait wait)))

(defgeneric process-channel-loop (channel &key wait)
  (:method ((class amqp:object) &key (wait t))
    "Read successive frames from the context, interpret and respond to them."
    (loop (unless (process-channel-command class :wait wait)
            (return)))))


(defun channel-toplevel-loop (&optional (*channel* *channel*))
  (loop
    (handling-channel-errors
      (process-channel-loop *channel* :wait t))))


(defgeneric process-frame (context frame)
  (:documentation "Given a frame as read from the connection socket, extract the channel and type
 information an process accordingly. This step happens either in the socket i/o process for asynchronous
 configurations, or in synchronous configurations, in a thread which reads a frame for its own channel.
 In the asynchronous case, the aim is to classify the frame, do any immediate and connection/channel-zero
 processing, and queue the remainder for the respective channel's process. In the synchronous case, where
 a process has read-through a get-read-frame call, immediate/c-0 processing is also unconditional, and
 frames for the respective process' channel are returned to the caller. In addition, any frames for channels
 which the process owns, are processed in the standard pipeline, while those for for other processes, are
 queued for later processing, for the respective channel's thread to process on its own.")
  
  (:method ((connection amqp:connection) (frame input-frame))
    "Determine the channel - which must exist for read framed, and
 the frame class, the dispatch for those as well as the connection."
    
    ;; number and channel are extracted from the frame header.
    ;; iff the frame is a method, the type will be an instance of the specialized method class,
    ;; as decoded from the frame data. otherwise - for content header and body and for heartbeat,
    ;; the type determined by the frame itself.
    (let* ((number (frame-channel-number frame))
           (channel (connection.channel connection :number number))
           (decoder (frame-decoder frame)))
      (process-typed-frame channel decoder frame)))
  
  (:method ((channel amqp:channel) (frame input-frame))
    ;; use an autonomous decoder to specialize initial processing
    (let* ((decoder (frame-decoder frame)))
      (process-typed-frame channel decoder frame))))


(defgeneric process-typed-frame (channel decoder frame)
  (:documentation "Process frames specific to their type,
 CHANNEL : amqp:channel
 TYPE : amqp:frame-type
 FRAME : amqp:frame

 The supported frame types (as of 0.9r1) are
  METHOD : is decoded and processed as a command
  HEADER : is expected only when a channel is open and receiving a body;
   pass it through unparsed to the current response function
  BODY : is expected when a channel is open and receiving;
   pass it through unparsed to the current response function
  HEARTBEAT : generate a response hearbeat in non-threaded connections
 Others are logged and ignored. In general, the frames' basic attributes are
 decoded for a type specific action. Where the current process is the channel
 owner, this happens directly. Otherwise the frame is queued. Whereby, if there
 is no input processing at the moment, the owner process is interrupted
 asynchronously to handle the frame.
 Iff the frame is processed, release it.")

  (:method :around ((*channel* amqp:channel) (type deferrable-frame-decoder) (frame amqp:frame))
    "The :around method compares the channel's thread with the active one, to decide
 whether to process immediately, or to delegate the procesing to the other thread."
    (let ((channel-thread (channel-thread *channel*)))
      (cond ((eq (bt:current-thread) channel-thread)
             (call-next-method))
            ((channel-thread *channel*)
             ;; interlock with the respective thread process and interrupt it if necessary
             (enqueue frame (device-read-frames *channel*)
                      :if-empty #'(lambda () (bt:interrupt-thread channel-thread #'process-typed-frame
                                                                  *channel* type frame))))
            (t
             (amqp:log :warn *channel* "process-typed-frame: disowned channel.")
             (release-frame frame)))))

  (:method ((channel amqp:channel) (method amqp:method) (frame amqp:frame))
    "Determine the particular (class x method) combination - first the class, then the
 respective method. This combination will be without any keys to designate a specific cache entry,
 so no history will be apperent in them. They control the method argument decoding, and determine the
 response function and/or intermediate filtering. Processing code must refer to its own instances in order
 to access past properties."
    (let* ((class (amqp:ensure-object channel frame))
           (method (amqp:ensure-method class frame)))
      (setf (channel-content-object channel) nil)        ; not in content
      (flet ((call-process-command (class method &rest args)
               (declare (dynamic-extent args))
               (when args
                 (apply #'reinitialize-instance method args))
               (apply #'process-command channel class method args)))
        (declare (dynamic-extent #'call-process-command))
        (unwind-protect (call-with-decoded-arguments #'call-process-command class method frame)
          (release-frame frame)))))

  (:method ((channel amqp:channel) (header amqp:header) (frame amqp:frame))
    "An header frame is received to initiate a message. The channel state should be open in input mode.
 Interpose the implicit basic target class and treat the header type as the method."
    (typecase (channel-state channel)
      (amqp.s:use-channel)
      (t
       (amqp:log :warn channel "~a frame, for inconsistent channel state: ~a, ~a"
                 header (channel-state channel) frame)))
    ;; the content components are singletons, so this will be the last one used
    (let ((object (amqp:ensure-object channel (content-header-class-name frame))))
      (setf (channel-content-object channel) object)
      (flet ((call-process-command (class &rest args)
               (declare (dynamic-extent args))
               (when args
                 (apply #'reinitialize-instance class args))
               (apply #'process-command channel class header :frame frame args)))
        (declare (dynamic-extent #'call-process-command))
        (unwind-protect (call-with-decoded-properties #'call-process-command object frame)
          (release-frame frame)))))

  (:method ((channel amqp:channel) (body amqp:body) (frame amqp:frame))
    "A body frame is received as part of a message. The channel state should be open in body input mode.
 Interpose the implicit basic target object and treat the body type as the method.
 In this case, there are no properties to decode, just manage the frame cache in the object and take
 the next process step."
    (typecase (channel-state channel)
      (amqp.s:use-channel.body.input)
      (t
       (amqp:log :warn channel "~a frame, for inconsistent channel state: ~a, ~a"
                 body (channel-state channel) frame)))
    (let ((object (channel-content-object channel)))
      (unless object
        (amqp:unexpected-frame-error :channel channel
                                     :frame frame
                                     :message-string "Body frame w/o header: ~s."
                                     :message-arguments (list frame)))
      (setf (object-frame object) frame)
      (unwind-protect (process-command channel object body :frame frame)
        (setf (object-frame object) nil)
        (release-frame frame))))

  (:method ((channel amqp:channel) (type amqp:heartbeat) (frame amqp:frame))
    "Received heartbeats cause read-frame updates the last frame  read
 timestamp. If there is a connection thread, nothing more needs
 to be done. If there is none, then just echo the heartbeat.
 (See [http://qpid.apache.org/configure-broker-and-client-heartbeating.html].)"
    (let ((connection (channel-connection channel)))
      (unless (connection-thread connection)
        (send-heartbeat connection))
      (amqp:log :debug channel "~a frame: ~a" type frame)))

  (:method ((channel amqp:channel) (type unsupported-frame-decoder) (frame amqp:frame))
    "Unsupported frames are logged and ignored."
    (amqp:log :warn channel "Unsupported ~a frame: ~a" type frame))

  (:method ((channel amqp:channel) (type t) (frame amqp:frame))
    "Unknown frames are logged and ignored."
    (amqp:log :error channel "Unknown ~a frame: ~a" type frame)))




;;;
;;; decoding utilities

(defgeneric call-with-decoded-arguments (operator class method buffer &rest args)
  (:method ((operator function) class method (frame amqp:frame) &rest args)
    "Given a frame, extract the drame's data buffer and continue."
    (declare (dynamic-extent args))
    (setf (object-frame class) frame)
    (unwind-protect
      (apply #'call-with-decoded-arguments operator class method (frame-data frame)
             args)
      (setf (object-frame class) nil)))
  (:method ((operator function) class (method symbol) (frame t) &rest args)
    (declare (dynamic-extent args))
      (apply #'call-with-decoded-arguments operator class (amqp:ensure-method class method) frame
             args)))

(defgeneric call-with-encoded-arguments (operator class method &rest args)
  (:method (op (class amqp:object) (method symbol) &rest args)
    (declare (dynamic-extent args))
    (apply #'call-with-encoded-arguments op class (amqp:ensure-method class method)
           args))
  (:method (op (class amqp:object) (method fixnum) &rest args)
    (declare (dynamic-extent args))
    (apply #'call-with-encoded-arguments op class (amqp:ensure-method class method)
           args)))


(defgeneric call-with-decoded-properties (operator class frame &rest args)
  (:method ((operator function) class (frame amqp:frame) &rest args)
    "Given a frame, decode the properties from the data buffer and continue."
    (declare (dynamic-extent args))
    (setf (object-frame class) frame)
    (unwind-protect
      (apply #'call-with-decoded-properties operator class (frame-data frame)
             args)
      (setf (object-frame class) nil))))


(defgeneric call-with-encoded-properties (operator class  &rest args)
  )


#+(or ) ;; this is ambiguous: is it the response or request side?
(defgeneric apply-method (method class &rest argument-list)
  (:documentation "Apply the function specific to the combination of
 (class x method) to the class and the given argument list.

 CONNECTION : amqp:connection : the distinction client v/s server connection
  determines the concrete behaviour
 METHOD : amqp:object : the version-specific method instance
 CLASS : amqp:object : the version-specific class instance
 ARGUMENT-LIST : list : the argument list.")
  (declare (dynamic-extent argument-list))

  (:method ((method t) (class t) &rest args)
    (declare (dynamic-extent args))
    (apply (compute-class-method class method) class args))

  (:method ((method amqp:method) (class amqp:object) &rest args)
    (declare (dynamic-extent args))
    (let ((handler (channel-command-handler class)))
      (or (and handler (apply handler class method args))
          (apply (method-request-function method) (object-channel class) class
                 (apply #'list* args))))))



(defmethod send-method ((method t) (class amqp:object) &rest args)
  (declare (dynamic-extent args))
  (flet ((write-encoded-method (frame class method)
           (amqp:log :debug class "send-method: ~a  ~a" method frame)
           ;; nb. this places the constraint on connections, that they have their channel-0 bound
           ;; before they send any command to the broker.
           (put-encoded-frame (object-channel class) frame)))
    (declare (dynamic-extent #'write-encoded-method))
    (amqp:log :debug class "send-method: ~a . ~s" method args)
    (apply #'call-with-encoded-arguments
           #'write-encoded-method class method
           args)))

(defmethod encode-method ((method t) (class amqp:object) &rest args)
  (declare (dynamic-extent args))
  (flet ((return-encoded-method (frame class method)
           (declare (ignore class method))
           (return-from encode-method frame)))
    (declare (dynamic-extent #'return-encoded-method))
    (apply #'call-with-encoded-arguments
           #'return-encoded-method class method
           args)))

(defmethod decode-class-properties ((class amqp:object))
  (reduce #'nconc
          (mapcar #'(lambda (name)
                      (list (cons-symbol :keyword name)
                            (slot-value class name)))
                  (class-property-slot-names class))))

(defmethod decode-method-arguments ((method amqp:method))
  (reduce #'nconc
          (mapcar #'(lambda (name)
                      (list (cons-symbol :keyword name)
                            (slot-value method name)))
                  (method-argument-slot-names method))))

(defun amqp.u:class-properties (class)
  (decode-class-properties class))

(defun amqp.u:method-arguments (class)
  (decode-method-arguments class))


(defun send-method* (method class &rest args)
  (declare (dynamic-extent args))
  (apply #'send-method method class (apply #'list* args)))

(defgeneric send-heartbeat (connection)
  (:method ((connection amqp:connection))
    "Encode and enqueue an heartbeat frame. Enqueue it just in case, something
 else happened between the deadline and now. Most likely passes through and is written directly.
 managing deadlines is left to the caller(s)."
    (let ((frame (claim-output-frame connection)))
      (setf-frame-type-class-name 'amqp:heartbeat frame)
      (setf-frame-channel-number 0 frame)
      (setf-frame-track-number 0 frame)
      (setf-frame-size 0 frame)
      (write-frame connection frame)
      (put-encoded-frame connection frame))))

(defmethod send-header ((class amqp:object) &rest args)
  (declare (dynamic-extent args))
  (flet ((write-encoded-properties (frame class)
           (setf-frame-type-class-name 'amqp:header frame)
           (put-encoded-frame (object-channel class) frame)))
    (declare (dynamic-extent #'write-encoded-properties))
    (apply #'call-with-encoded-properties
           #'write-encoded-properties class
           args)))



(:documentation (process-command dynamic-process-command)
  "Interface Operators :


 static / dynamic / lexical / instantial command handler binding

 Each read frame, once typed and decoded, must be "handled". This
 can occur in a dynamic context where a frame appears immediately
 subsequent, and in response to a command on the same channel, or it can be
 a frame which has appeared asynchronously on a channel for which
 the most recent request was to subscribe to a queue, or it could be
 autononomously sent from the broker - eg, to close a queue or
 return a message.

 The processing mechanism must permit both forms of processing. In
 particular, because a given channel may recieve a frame in either
 category at a given time. Rather than configure a channel for this,
 the mechanism should interpret respective context and operate accordingly.
 To achieve this, it integrates the following definition forms:

   with-commands (clauses . body)
     defines a command processor in the current lexical context and
     binds it dynamically, such that it takes first precedence.

   (setf channel-command) (channel method &optional class) (function)
     binds a handler function for that (method x class) for frames to be processed
     in that channel. any extant binding is returned.
 
   dynamic-process-command (handler channel class type . args)
     is implemented to apply the hander iff it is a function, otherwise to
     apply a channel handler iff it is a function, and otherwise to
     apply the method's respective response function.

   The standard definitions arrange that dynamic-process-command traces
   back through dynamically bound handlers until either one handles the frame -
   in that it returns true, or the binding is null - which is the global default.
   At that point it tries the channel's handle. If there is none, or it declines,
   then the method's own response function is used.")

(defparameter *channel-command-handler* nil
  "When this is bound, it interposes operators before the channel's bound
 command handlers.")

(defgeneric process-command (channel class operation &rest args)
  (:documentation "Process a decoded, read frame as a 'command'. This applies to
 both method and content frames. A method is dispatched to the respective
 respond-to operator.")

  (:method ((channel amqp:channel) (class amqp:object) (operation frame-decoder) &rest args)
    ;; at this point, the frame object has been reified, and any operation arguments or properties
    ;; have been decoded. the frame-decoder either indicated the method, the content element.
    ;; try the available response functions and apply the first found to the
    ;; connection, target class, and the argument list
    (declare (dynamic-extent args))
    (amqp:log :debug channel "process-command: ~a ~a . ~s" class operation args)
    (apply #'dynamic-process-command *channel-command-handler*
           channel class operation args)))

(defgeneric dynamic-process-command (dynamic-handler channel class type &rest args)

  (:method ((operator t) (channel t)  (class t) (method amqp:method) &rest args)
    "Given an unhandled method, apply its own operator, for which the initial definition 
 is the static response operator."
    (declare (dynamic-extent args))
    (apply (method-response-function method) channel class args))

  (:method ((operator null) (channel amqp:channel) (class t) (type t) &rest args)
    "Given a channel, if it has a handler try it. If none is present, or it declines
 continue with the next method, to apply the method's own operator."
    (declare (dynamic-extent args))
    (let ((channel-operator (channel-command-handler channel)))
      (etypecase channel-operator
        (null (call-next-method))
        (function (or (apply channel-operator channel class type args)
                      (call-next-method))))))

  (:method ((operator function) (channel t) (class t) (type t) &rest args)
    (declare (dynamic-extent args))
    (apply operator channel class type args))

  (:method ((operator t) (channel t)  (class t) (frame frame-decoder) &rest args)
    ;; this shouldn't happen - it means that a content frame arrived outside
    ;; of a dynamic context which should be integrating it in an earlier operation
    (declare (dynamic-extent args) (ignore args))
    (amqp:unexpected-frame-error :connection (when (typep channel 'amqp:channel) (channel-connection channel))
                                 :channel channel
                                 :frame frame
                                 :message-string "[process-command] frame not processed: ~s, ~s."
                                 :message-arguments (list class frame))))




;;; It would be possible to implement these as methods, install and remove
;;; for the dynamic extent, but that would mean that the effective
;;; function is continuously recomputed.
;;; The implemented mechanism binds a current handler to a dynamic variable
;;; and retains a previous version in a lexical binding, which it calls
;;; should the new handler not 'accept' the combined (class x method). Should
;;; there be no previous version, it calls the respective channel's command
;;; handler. from which, by default, the static function is invoked as the
;;; default method.

(defun compute-command-clauses (class-var0 method-var0 args-var clauses)
  "Transform the command clauses, each of the form
     (method-name ((class type) . args) . body)
 into cond clauses for inclusion in a command processing operator.
 This does _not_ add any defaults, it just sorts the clauses by subtype.
 The with-commands operator intends to pass anything which falls through to the
 next handler on the dynamic stack."
  (flet ((rewrite-clause (clause)
             (destructuring-bind (method-type (class &rest arglist) &rest body)
                                 clause
               (let ((class-var (if (consp class) (first class) class))
                     (class-type (if (consp class) (second class) t))
                     (method-var (gensym (string :method-))))
                         
               `((and (typep ,class-var0 ',class-type) (typep ,method-var0 ',method-type))
                  (let ((,class-var ,class-var0) (,method-var ,method-var0))
                    (declare (ignorable ,class-var ,method-var))
                    (destructuring-bind ,arglist ,args-var
                      ,@body)))))))

      ;; sort by method, then class, then channel
    (setf clauses (sort (copy-list clauses)
                         #'(lambda (c1 c2)
                             (let* ((m1 (first c1))
                                    (m2 (first c2))
                                    (c1 (first (second c1)))
                                    (c2 (first (second c2))))
                               (setf c1 (if (consp c1) (second c1) t))
                               (setf c2 (if (consp c2) (second c2) t))
                               (if (eql c1 c2)
                                 (subtypep m1 m2)
                                 (subtypep c1 c2))))))
      (mapcar #'rewrite-clause clauses)))


(defmacro amqp:with-commands (command-clauses &rest body)
  "Defines a lexically scoped command handler, which is to take precedence over any extand
 dynamic handlers. Each clause has the form
   (method-type (class . arguments) . body)
 these are collected into an operator which is shadows the existing global
 definition during the dynamic extend of the forms body. If no form matches a to-be-processed
 frame, the next handler in the dynamic chain is invoked. (See dynamic-process-command)"

  (let ((handlers-op (gensym (string :command-handler-)))
        (body-op (gensym (string :command-body-)))
        (channel-var (gensym (string :channel-)))
        (class-var (gensym (string :class-)))
        (type-var 'amqp:method)
        (args-var (gensym (string :arglist-))))
    `(flet ((,handlers-op (,channel-var ,class-var ,type-var &rest ,args-var)
              (declare (dynamic-extent ,args-var)
                       ;; this is present to keep the interface uniform. in the
                       ;; dynamic context, the channel instance is known, so there's
                       ;; no reason for it to appear in the clauses
                       (ignore ,channel-var))
              (cond ,@(compute-command-clauses class-var type-var args-var command-clauses)))
            (,body-op () ,@body))
       (declare (dynamic-extent #',handlers-op #',body-op))
       (call-with-command-handlers #',body-op #',handlers-op))))

(defun call-with-command-handlers (op commands-op)
  (declare (dynamic-extent op commands-op))
  (let ((previous-handler *channel-command-handler*))
    (flet ((apply-command-handler (channel class type &rest args)
             (declare (dynamic-extent args))
             (or (apply commands-op channel class type args)
                 (apply #'dynamic-process-command previous-handler
                        channel class type args))))
      (declare (dynamic-extent #'apply-command-handler))
      (let ((*channel-command-handler* #'apply-command-handler))
        (funcall op)))))



(defmacro command-loop ((channel &key (wait nil wait-s)) &rest command-clauses)
  `(block command-loop
     (amqp:with-commands ,command-clauses (process-channel-loop ,channel ,@(when wait-s `(:wait ,wait))))))

(defmacro command-case ((channel &key (wait nil wait-s)) &rest command-clauses)
  `(block command-case
     (amqp:with-commands ,command-clauses (process-channel-command ,channel ,@(when wait-s `(:wait ,wait))))))

#+mcl
(setf (ccl:assq 'command-loop ccl::*fred-special-indent-alist*) 1)
#+mcl
(setf (ccl:assq 'command-case ccl::*fred-special-indent-alist*) 1)


(:documentation (compute-channel-command-handler channel-command-handler channel-command)
  "Instance-scoped Commands :

 Instance-scoped commands are integrated into a channel's command-handler function.
 Each is a implemented in a method, specialized on the method's class
 the operator is defined on demand and the handlers are added/removed
 with the (setf channel-command-handler) operator.")

(defun compute-channel-command-handler (channel)
  (let* ((name (channel-name channel))
         (function (ensure-generic-function name
                                           :lambda-list '(channel class method &rest args)
                                           :declare '((dynamic-extent args)))))
    (c2mop:ensure-method function
                   '(lambda (channel class method &rest args)
                      (declare (ignore channel class method args)
                               (dynamic-extent args))
                      ;; the default method returns nil
                      nil)
                   :specializers (mapcar #'find-class '(amqp:channel t t)))
    function))

(defmethod channel-command-handler ((channel amqp:channel))
  (with-slots (command-handler) channel
    (or command-handler
        (setf command-handler (compute-channel-command-handler channel)))))

(defmethod (setf channel-command-handler) (function (channel amqp:channel))
  (with-slots (command-handler) channel
    (when (typep command-handler 'generic-function)
      (dolist (method (c2mop:generic-function-methods command-handler))
        (remove-method method command-handler)))
    (setf command-handler function)))

(defgeneric (setf channel-command) (method-function channel method-name)
  (:documentation "Binds the respective command in the channel's instance
 context to the given function. If the function is null, then removes the
 command handler. Returns the existing handler.
 nb. uses ensure-method, which requires a lambda expression in order to
 wrap it is a method function. which means function literals are wrapped.")

  (:method :before ((function t) (channel t) (method-name symbol))
    (assert (subtypep method-name 'amqp:method) ()
            "Invalid method name: ~s." method-name))

  (:method ((function null) (channel amqp:channel) (method-name symbol))
    (let* ((operator (channel-command-handler channel))
           (method (when operator (find-method operator nil (list 'amqp:channel 'amqp:object method-name)
                                               nil))))
      (when method
        (remove-method operator method)
        method)))

  (:method ((lambda cons) (channel amqp:channel) (method-name symbol))
    (let* ((operator (channel-command-handler channel))
           (method (when operator (find-method operator nil (list 'amqp:channel 'amqp:object method-name)
                                               nil))))
      (when method (remove-method operator method))
      (c2mop:ensure-method operator lambda
                           :qualifiers nil
                           :specializers (mapcar #'find-class `(amqp:channel amqp:object ,method-name))
                           :lambda-list '(channel class method &rest args))
      method))

  (:method ((function function) (channel amqp:channel) (method-name symbol))
    (setf (channel-command channel method-name)
          `(lambda (channel class method &rest args)
             (declare (dynamic-extent args))
             (apply ,function channel class method args)))))


