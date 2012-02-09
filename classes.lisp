;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines the CLOS model for AMQP `object` and `method` entities for the
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

  (long-description
   "Each AMQP protocol version presents a particular model for message processing. At
 the same time, despite the variations, all message-level models distinguish
 between 'class' and 'operator' entities, and the transport-level models all
 concern exchanges of 'framed' data through 'connections'. At the message
 level the classes exhibit various relations - either connection/channel/class
 or connection/session/channel/links/class, while operators are termed variously
 'methods', 'commands', and 'controls'. at the same time, their specialized roles do not affect
 their basic representation. All are 'classes' on an abstract level. The distinction
 is of a containment hierarchy.

 At the transport level, transported data is broken into various constituents -
 either 'content' and 'frames' or 'assemblies', 'segments' and 'frames', but
 this does not affect the respective representation either.

 This implementation reflects the following general order.

 1. The primary interface classes are amqp:object and amqp:method. These are specialized in terms
 of protocol version, and  according to the respetive protocol's functional model. In terms of the
 protocols' taxonometric class hierarchy - eg, for 0.8r0, 0.9r1, this manifests in the class definitions
 and their relations:
    connection (1)--
      -- (*) channel (1) ---
               --(*) access basic cluster dtx exchange file link queue stream tx
   with each class associated with the respective methods, eg.
      CHANNEL : OPEN OPEN-OK FLOW FLOW-OK CLOSE CLOSE-OK
      EXCHANGE : DECLARE DECLARE-OK DELETE DELETE-OK
   (See the version's respective classes.lisp)

 2. At the transport level, each protocol's model is reflected in the
 data containers - eg channel, content, assembly, segment, link, and frame.

 3. At the wire level each protocol version specifies encoding rules and
 how to map its defined data types to lisp data.

 This file defines the abstract classes, the 'commands' file implements the
 protocol logic in terms of them, and the 'data-wire-encoding' file defines the
 general data codecs. Each protocol is implemented in three files in the
 eponymous directory:
  'data-wire-encoding' : defines codecs in terms of the protocol's type domain names
    which are used in the protocol operators. Compound codecs - eg, map,
    array, and list, require version-specific type codes.
  'abstract-classes'   : defines the protocol-specific abstract classes.
  'classes'            : defines codecs for the concrete classes and methods."))



(defun amqp-version-p (x)
  "Return true iff the argument is present in a known version. This is determined by its
 presence as a key in *version-headers*. That a-list is augmented by each loaded
 protocol version to bind a version key to a concrete protocol header."
  (assoc x amqp.u:*version-headers*))

(deftype amqp:version () '(satisfies amqp-version-p))


;;;
;;; frame decoders

(defclass frame-decoder ()
  ((context
    :initform nil :initarg :context
    :accessor frame-context
    :type (or null amqp:object)
    :documentation "Records the frames context. If the frame is an autonomous prototype, no context is
 present, as the same instance is reused to specialize initial processing. Once an object is associated
 with the frame, a context-specific decocder (most likely the respective method) is interposed."))
  (:documentation "The abstract root class for all parsed representations for frames. This includes the
 command methods as well as content haeaders and bodies. Each frame encodes a command. In order to perform if
 the frame is decomposed into two constituents: and object and an operation. The decomposition step first
 associates a decoder with the frame and then delegates to parsing functions to extract the constituent
 details. The initial association is with abstract singletons which belong to htbased on frame type
 singletons based on the frame class and method codes. Once parsing proceeds, methods
 are decimaed into keywords and the class is replaced with a channel-specific instance
 which can cache the of the frame content. In the case of content bodies, however, no replacement
 is necessary, as the body passes through the processing tree opaquely."))

(defclass supported-frame-decoder (frame-decoder) ())
(defclass unsupported-frame-decoder (frame-decoder) ())
(defclass deferrable-frame-decoder (supported-frame-decoder) ())
(defclass immediate-frame-decoder (supported-frame-decoder) ())
(defclass content-frame-decoder (deferrable-frame-decoder) ())

(defclass amqp:body (content-frame-decoder) ())

(defclass amqp:header (content-frame-decoder) ())

(defclass amqp:heartbeat (immediate-frame-decoder) ())

(defclass amqp:oob-method (unsupported-frame-decoder) ())

(defclass amqp:oob-header (unsupported-frame-decoder) ())

(defclass amqp:oob-body (unsupported-frame-decoder) ())

(defclass amqp:trace (unsupported-frame-decoder) ())

#+(or ) ;; this inteferes with the access.request method class
(defclass amqp:request (unsupported-frame-decoder) ())

(defclass amqp:response (unsupported-frame-decoder) ())

(defmethod shared-initialize ((instance frame-decoder) (slots t) &rest initargs
                              &key context (channel nil channel-s) (connection nil connection-s))
  (when (and (not channel-s) (amqp:channel-p context))
    (setf channel context))
  (when (and (not connection-s) (amqp:connection-p context))
    (setf channel context))
  (apply #'call-next-method instance slots
         :channel channel
         :connection connection
         initargs))

;;;
;;; the abstract class of amqp protocol objects

(defclass amqp:object ()
  ((id :reader amqp:class-id)
   (protocol-version
    :initform *default-version* :allocation :class
    :reader class-protocol-version)
   (state
    :initform nil
    :accessor class-state)
   (frame
    :initform nil
    :accessor object-frame
    :documentation "Caches the current frame while the class is processed.")
   (property-slot-names
    :reader class-property-slot-names
    :documentation "A list of slot names for those slots which correspond to
 protocol class properties.")
   (argument-slot-names
    :reader class-argument-slot-names
    :documentation "A list of slot names for those slots which correspond to
 protocol method arguments.")
   (method-names
    :initform nil
    :reader class-method-names
    :allocation :class)
   (context
    :initform nil :initarg :context
    :accessor object-context
    :type (or null amqp:object)))
  (:documentation "The abstract root class for all AMQP protocol classes."))


(defclass amqp-connected-object (amqp:object)
  ((context
    :initform (error "connection required")
    :initarg :connection
    :reader object-connection))
  (:documentation "Specified for all classes other than the connection itself in
 order to delegate to it."))

(defclass amqp-channeled-object (amqp-connected-object)
  ((context
    :initarg :channel
    :reader object-channel))
  (:documentation "Specified for all classes other than the channel itself in
 order to delegate to it."))

(defclass amqp-content-object (amqp:object)
  ((weight
    :initform 0 :initarg :weight
    :accessor class-weight)
   (body-size
    :initform 0 :initarg :body-size
    :accessor class-body-size))
  (:documentation "Mixed into classes which are associated with content, to provide
 slots for the data which is carried directly in the header, as the protocol objects does not include those
 fields."))   



;;;
;;; the abstract method class is not differentiated client/server as operations
;;; are specialized by connection


(defclass amqp:method (deferrable-frame-decoder)
  ((id :reader amqp:method-id)
   (context :reader method-object)
   (name
    :initform (error "name required.")
    :reader amqp:method-name
    :documentation "The protocol's version-independent name for this method.
 Defined per abstract method class.")
   (request-function
    :reader method-request-function)
   (response-function
    :reader method-response-function)
   (argument-slot-names
    :reader method-argument-slot-names
    :documentation "A list of slot names for those slots which correspond to
 protocol arguments.")))

(defclass amqp:message ()
  ((channel :initform nil :initarg :channel)
   (class :initform nil :initarg :class)
   (weight :initform nil :initarg :weight)
   (content :initform nil :initarg :content)
   (flags :initform nil :initarg :flags)
   (properties :initform nil :initarg :properties)))



(defgeneric object-connection (class)
  (:documentation "Returns the protocol object's connection."))

(defgeneric object-channel (class)
  (:documentation "Returns the protocol object's channel.
 For a channel, this is the channel itself. For a connection, this is the control channel."))



;;;
;;; abstract class classes


(def-amqp-abstract-class amqp:access (amqp-channeled-object) 
  ()
  (:documentation "The abstract access class is specialized for each protocol version."))


(def-amqp-abstract-class amqp:basic (amqp-channeled-object amqp-content-object)
  ((context
    :reader basic-channel)
   #+(or) ;; use the standard one
   (exchange-instance
    :initform nil
    :accessor basic-exchange
    :type (or string null)
    :documentation "Caches the exchange from the most recent publish for re-use in chunked content.")
   #+(or) ;; use basic.content-type
   (mime-type
    :initform nil :initarg :mime-type
    :accessor class-mime-type))
  (:documentation "The abstract basic class is specialized for each protocol version."))


(def-amqp-abstract-class amqp:channel (amqp-connected-object amqp-connection-device)
  ((context
    :reader channel-connection)
   (uri
    :reader channel-uri)
   (name
    :initform (gensym "channel")
    :reader channel-name)
   (number
    :initform (error "number required") :initarg :number
    :reader channel-number :writer setf-channel-number
    :type fixnum)
   (track
    :initform 0 :initarg :track
    :reader channel-track)
   (state
    :initform amqp.s:open-channel
    :type amqp.s:channel-state)
   (thread
    :initform (bt:current-thread)
    :reader channel-thread
    :documentation "The thread which processes this channel.
 Likely, the thread which created it.")
   (conditions
    :initform nil
    :accessor channel-conditions
    :documentation "Caches conditions raised on the channel for re-use.")
   (command-handler
    :initform nil
    :documentation "Binds a handler function, with the signature (class method &rest arguments),
 which are applied when handle-channel-methods.")
   (realm
    :initform nil :initarg :realm
    :reader amqp.u:channel-realm
    :documentation "Should the protocol support/reauire realms, the channel negotiates access
 as a late step in the device-open procedure and bind both the realm and the tick for future use.
 The channel's realm comprises the realm proper +value exclusive passive active write read")
   (ticket
    :initform nil :initarg :ticket
    :accessor amqp.u:channel-ticket
    :documentation "Should the protocol support/reauire realms, the channel negotiates access
 as a late step in the device-open procedure and bind both the realm and the tick for future use.")   
   (content-object
    :initform nil
    :accessor channel-content-object
    :documentation "The most recent class which contained (the current) content.
 Updated by respond to typed-frame, but not cleared.")
   (acknowledge-messages
    :initform nil
    :accessor channel-acknowledge-messages
    :documentation "Indicates whether no-ack was selected when a a message was
 requested via Consume or Get. If selected, then each receipt is automatically
 acknowledged as the last step of the respond-to operation.")
   ;; caches for protocol instances
   (amqp-basic
    :initform nil
    :reader get-channel-basic :writer setf-channel-basic
    :type (or null amqp:basic)
    :documentation "Caches the channel basic instance.")
   (amqp-body
    :initform nil
    :reader get-channel-body :writer setf-channel-body
    :type (or null amqp:body)
    :documentation "Caches the channel body instance.")
   (amqp-exchanges
    :initform nil
    :reader get-channel-exchanges :writer setf-channel-exchanges
    :type list
    :documentation "Caches the channel exchange instances by exchange name.")
   (amqp-file
    :initform nil
    :reader get-channel-file :writer setf-channel-file
    :type (or null amqp:file)
    :documentation "Caches the channel file instance.")
   (amqp-header
    :initform nil
    :reader get-channel-header :writer setf-channel-header
    :type (or null amqp:header)
    :documentation "Caches the channel header instance.")
   (amqp-queues
    :initform nil
    :reader get-channel-queues :writer setf-channel-queues
    :type list
    :documentation "Caches channel queues according to queue name.")
   (amqp-stream
    :initform nil
    :reader get-channel-stream :writer setf-channel-stream
    :type (or null amqp:stream)
    :documentation "Caches the channel stream instance.")
   (amqp-tx
    :initform nil
    :reader get-channel-tx :writer setf-channel-tx
    :type (or null amqp:tx)
    :documentation "Caches the channel tx instance."))
  (:documentation "The abstract channel class is specialized for each protocol version.
 Each channel is associated with a connection and identified by channel-number.
 Once a channel is opened, it serves as the context for message- and stream-based operations."))

(def-amqp-abstract-class amqp:cluster (amqp-channeled-object)
  ())

(def-amqp-abstract-class amqp:connection (amqp:object amqp-socket-device)
  ((uri
    :reader connection-uri)
   (frame-size :type number
               :initform *frame-size* 
               :initarg :frame-size
    :reader connection-frame-max :reader connection-frame-size
    :writer set-connection-frame-max)
   (amqp:heartbeat
    :initform 0 :initarg :heartbeat
    :accessor connection-heartbeat
    :type (unsigned-byte 16))
   (mode
    :initform :simplex :initarg :mode
    :reader connection-mode
    :type (member :multiplex :simplex))
   (amqp:mechanism
    :initform *default-mechanism* :initarg :mechanism
    :reader connection-mechanism)
   (amqp:locale
    :initform *default-locale* :initarg :locale
    :reader connection-locale
    :documentation "Specifies the connection's locale. If set as an initializati on argument, this
 constrains the connection negotiation. If not set, the first of the server's suggestions is
 adopted.")
   (lock
    :reader connection-lock)
   (state
    :initform amqp.s:open-connection
    :type amqp.s:connection-state)
   (protocol-version
    :reader connection-protocol-version
    :documentation "Provide a connection- accessor and also a default version for the
 abstract class, for use to start the negotiation process.")
   (input-frame-class
    :initarg :input-frame-class
    :reader connection-input-frame-class
    :type symbol
    :documentation "Specifies the class to use to decode wire-level frames.
     The default value is specific per protocol version. each is wrapped around
     an input data buffer and header to control the decoding process.")
   (output-frame-class
    :initarg :output-frame-class
    :reader connection-output-frame-class
    :type symbol
    :documentation "Specifies the class to use to encode wire-level frames.
     The default value is specific per protocol version. These are wrapped around
     and output data buffer and header to control the encoding process.")
   (amqp::server-properties
    :initform nil
    :accessor amqp:connection-server-properties)
   (amqp::client-properties
    :initform nil :initarg :client-properties
    :accessor amqp:connection-client-properties
    :documentation "The properties sent to the server in as start-ok response.")
   (protocol-instances
    :reader connection-protocol-instances
    :documentation "Caches classes for uses as protocol elements.
 Ideintical with the method cache, but they concern different namespaces")
   (thread
    :initform nil
    :reader connection-thread
    :documentation "If null, then no asynchronous processing occurs.
 Otherwise, it is the thread which reads/writes the connection's
 socket, dispatches read frames to responds finctions, and generates
 heartbeats. The initial value is nil, a true :threaded initialization
 argument cause creation and activation of a new thread.")
   (read-frame-timestamp
    :initform 0
    :accessor connection-read-frame-timestamp)
   (write-frame-timestamp
    :initform 0
    :accessor connection-write-frame-timestamp)
   ;; protocol instance cache slots
   (amqp-channels
    :initform nil
    :reader get-connection-channels :writer setf-connection-channels
    :type (or null vector)
    :documentation "Caches the connection channel instances by number.")
   (amqp-heartbeat
    :initform nil
    :reader get-connection-heartbeat :writer setf-connection-heartbeat
    :type (or null amqp:heartbeat)
    :documentation "Caches the connection heartbeat instance.")
   (idle-handlers
    :initform nil
    :accessor amqp.u::connection-idle-handlers
    :type list
    :documentation "A list of functions, of one argument, each of which is in applied to
     the connection in process-connection-loop after input/output is completed and before
     selecting on the conenction's socket."))
  (:documentation "The abstract connection class is specialized for each protocol version.
    Each connection binds the properties negotiated with the peer broker, and a sequence of
    open channels, each identified by number. Of these channel-zero is used for control operations."))


(def-amqp-abstract-class amqp:dtx (amqp-channeled-object)
  ((context
    :reader dtx-channel))
  (:documentation "The abstract connection class is specialized for each protocol version."))

(def-amqp-abstract-class amqp:exchange (amqp-channeled-object)
  ((context
    :reader exchange-channel)
   (amqp::type
    :initform "direct"
    :initarg :type
    :type string
    :reader amqp::exchange-type))
  (:documentation "The abstract exchange class is specialized for each protocol version."))

(def-amqp-abstract-class amqp:file (amqp-channeled-object)
  ((context
    :reader file-channel))
  (:documentation "The abstract connection file is specialized for each protocol version."))

(def-amqp-abstract-class amqp:link (amqp-channeled-object)
  ((context
    :reader link-channel))
  (:documentation "The abstract link class is specialized for each protocol version."))

(def-amqp-abstract-class amqp:queue (amqp-channeled-object)
  ((context
    :reader queue-channel))
  (:documentation "The abstract queue class is specialized for each protocol version."))   

(def-amqp-abstract-class amqp:stream (amqp-channeled-object)
  ((context
    :reader stream-channel))
  (:documentation "The abstract stream class is specialized for each protocol version."))

(def-amqp-abstract-class amqp:session (amqp-channeled-object)
  ((context
    :reader session-channel))
  (:documentation "The abstract session class is specialized for each protocol version."))

(def-amqp-abstract-class amqp:tx (amqp-channeled-object)
  ((context
    :reader tx-channel))
  (:documentation "The abstract tx class is specialized for each protocol version."))

(def-amqp-abstract-class amqp:test (amqp-channeled-object)
  ((context
    :reader test-connection))
  (:documentation "The abstract test class is specialized for each protocol version."))

(def-amqp-abstract-class amqp:tunnel (amqp-channeled-object)
  ((context
    :reader tunnel-connection))
  (:documentation "The abstract tunnel class is specialized for each protocol version."))


(:documentation "class and connection relative id-to-abstract-type maps"
  "to version-specific classes. the primary operators (ensure-method ensure-object)
  combine a context and a designator - either a code when parsing, or an abstract
  name  in processing functions, and produce an instance of the concrete
  versioned class. in the case of methods, the instance never changes state, while
  in the case of classes, each is reinitialized if supplied initargs.
  the primary operators rely on versiour resolution operators which map between
  class/method names and codes for the given version.")


(defgeneric connection-class-code-class-name (connection class-code)
  (:documentation "Map a version-specific class id code to the abstract class name.
 This is specialized for each concrete connection class.")

  (:method ((connection amqp:connection) (code (eql 0)))
    nil))


(defgeneric connection-class-name-class-code (connection class-name)
  (:documentation "Map an abstract class name to connection-specific class id code.
 This is specialized for each concrete connection class."))


(defgeneric class-method-code-method-name (class method-code)
  (:documentation "Map a class-specific, version-specific method code to an abstract method name.
 This is specialized for each concrete protocol object class."))


(defgeneric class-method-name-method-code (class method-name)
  (:documentation "Map an abstract method name to a  class-specific, version-specific method code.
 This is specialized for each concrete protocol object class."))


(defgeneric connection-method-code-method-name (connection class method-code)
  (:method ((connection amqp:connection) (class null) (code (eql 0)))
    nil)
  (:method ((connection amqp:connection) (class-code integer) (method-name t))
    (connection-method-code-method-name connection
                                        (connection-class-code-class-name connection class-code)
                                        method-name)))

(defgeneric connection-method-name-method-code (connection class method-name)
  (:method ((connection amqp:connection) (class-code integer) method)
    (connection-method-name-method-code connection
                                        (connection-class-code-class-name connection class-code)
                                        method)))


(defgeneric class-find-object-class (class class-class-designator)
  (:method ((connection amqp:channel) (designator (eql 'amqp:header)))
    (find-class 'amqp:header))
  (:method ((connection amqp:channel) (designator (eql 'amqp:body)))
    (find-class 'amqp:body))
  (:method ((connection amqp:connection) (designator (eql 'amqp:heartbeat)))
    (find-class 'amqp:heartbeat)))

(defgeneric class-find-method-class (class method-class-designator)
  )


(:documentation (amqp:ensure-method amqp:ensure-object)
  "Each class combines with its operators to perform commands. In addition each channel is associated with
 class.command instances which apply to it and a connection is assocaiated with it channels. In order that
 subsequent operations reflect previous settings, each context caches constituents. In the case of the
 (connection x channel) relation the channel number is the designator. For (channel x (exchange + queue))
 relations it is the respective name. For anonymous entities, the type suffices.")


(defgeneric amqp:ensure-method (class designator &rest initargs)
  (:documentation "Retrieve or create a version-specific method instance given the a class instance and a
  method designator. As designator, accept either a wire code or an abstract method name. Concrete method
 names should not be specified. The name is used as a cache key to treat the methon as a singleton with
 respect to the class.  If none exists, a new instance is cached and returned."))


(defgeneric amqp:ensure-object (context class-designator &rest initargs)
  (:documentation "Construct a new class instance respective the given context.
 CONTEXT : (or connection channel) : the context for the class. channels are relative
  to a connection, all others relative to a channel.
 CLASS-NAME : symbol : the abstract protocol class name
 . INITARGS : list : initialization arguments supplied to create a new instance or reinitialize a cache done.

 A connection allows channels only. A channel treats the channel and connection
 types as designating the respective instances and everything else as a
 channel-relative singleton. All other conntected contexts delegate to their channel."))





(:documentation "class methods"
  "on input, methods act as markers to permit filtering rather than calling a static function
  (even dynamically rebound). the arguments are passed on the stack, but also cached for future reference
  ?in the method, the class or the channel?
  channel, no - since things like queue, exchange, realm input is specific to that class
  class, no -a queue.bind can specify more than one exchange and a channel.publish takes exchange, and
  routing. 
  method-scoped binding is required.
  exceptions can be implemented for specific things, like basic's content type, as additional methods.

  operators
   (method-name class . args)        :  perform the command request. this is a useful shorthand, but
     !! is not sufficient for method re-use. publish, for example, allows as arguments exchange and
     !! routing-key, which are multiple-per-channel. one needs to cache them in the publish instance
     !! and apply them to the channel & explicit args to generate the effective request. to accomplish this,
     !! the elementary method-name operator delegates to the request- operator, which takes explicit
     !! arguments or defaults them from the class. if applied to a method instance, the defaults come
     !! first from the method, which then delegates to the class.
     -> (REQUEST-method-name class . args)       [explicitly coded]
        -> (send-method-name class . args)
   (class-name.class-name class . args)         :  make a class-scoped class [explicitly coded according to dependency]
   (class-name.method-name class . args) : make a class-scoped method  [in def-amqp-method]
   (SEND-METHOD method class . args) :  encode and send request w/ default arguments from the method
   (SEND-method-name class . args)   :
     -> (SEND-METHOD (class-name.method-name class . designator-arg) class . args)

  eg.
  
  (defmethod amqp:send-publish ((class amqp:basic) &rest arguments)
    (declare (dynamic-extent arguments))
    (apply #'send-method (amqp:basic.publish class :exchange (getf arguments :exchange)) class arguments))")

;;;
;;; amqp:access

#+(or )
(progn  ;; this conflicts with the class for request methods.
  ;; ?? change method classes to *-method?
  (def-ensure-method (amqp:access amqp:request) )
  (def-ensure-method (amqp:access amqp:request-ok)))


;;; basic

(def-ensure-method (amqp:basic amqp:qos) )
(def-ensure-method (amqp:basic amqp:qos-ok) )
(def-ensure-method (amqp:basic amqp:consume) )
(def-ensure-method (amqp:basic amqp:consume-ok) )
(def-ensure-method (amqp:basic amqp:cancel) )
(def-ensure-method (amqp:basic amqp:cancel-ok) )
(def-ensure-method (amqp:basic amqp:publish) )
(def-ensure-method (amqp:basic amqp:return) )
(def-ensure-method (amqp:basic amqp:deliver) )
(def-ensure-method (amqp:basic amqp:get) )
(def-ensure-method (amqp:basic amqp:get-ok) )
(def-ensure-method (amqp:basic amqp:get-empty) )
(def-ensure-method (amqp:basic amqp:ack) )
(def-ensure-method (amqp:basic amqp:reject) )
(def-ensure-method (amqp:basic amqp:recover-async) )
(def-ensure-method (amqp:basic amqp:recover) )
(def-ensure-method (amqp:basic amqp:recover-ok) )

(defmethod shared-initialize ((instance amqp:basic) (slots t) &rest args
                              &key 
                              (context (bound-slot-value instance 'context))
                              (channel context)
                              (content-type (device-content-type channel))
                              content-encoding
                              (body nil body-s) (body-size nil) headers
                              (package *package*))
  "Initialize a basic class by augmenting the content type/encoding from the
 respective channel's values and coercing them to their respective
 specifications. the effect is that - even after defaulting from the channel they must both
 be strings"

  (declare (dynamic-extent args))
  (assert-argument-types shared-initialize
    (channel amqp:channel)
    (content-type (or string mime:*/*))
    (content-encoding (or null string symbol content-encoding)))
  ;; coerce content type and encoding to instances to initialize
  ;; but later pass strings as slot values
  (setf content-type (mime:mime-type content-type))
  (setf content-encoding
        (etypecase content-encoding
          (content-encoding content-encoding)
          (null (content-encoding (mime-type-charset content-type)))
          ((or string symbol) (content-encoding content-encoding))))
  (unless (eq (content-encoding-name content-encoding) (mime-type-charset content-type))
    (setf content-type (clone-instance content-type
                                       :charset (content-encoding-name content-encoding))))

  ;; if given a body, but no body size, try to figure it out.
  ;; if that's not possible, indicate continued in the header
  (when body-s
    (assert (typep content-type 'mime:*/*) ()      ; ought to always be true (see above)
            "Supplied body requires a content type.")
    (unless body-size
      ;; try to determine the size
      (setf body-size (channel-compute-body-size channel body content-type))
      (etypecase body-size
        (null
         (setf (getf headers :transfer-encoding) "chunked")
         (setf body-size (device-buffer-length channel)))
        (integer
         (remf headers :transfer-encoding))))
    (unless (getf headers :package)
      (setf (getf headers :package) (package-name *package*)))
    (unless (getf headers :element-type)
      (multiple-value-bind (concrete effective-type match-p)
                           (canonical-element-type channel body (device-element-type channel))
        (declare (ignore concrete))
        (assert match-p ()
                "Supplied body type is invalid for channel: ~s, ~s"
                (type-of body) (device-element-type channel))
        (assert (eq (find-symbol (string effective-type) package)
                    effective-type) ()
                "Invalid type x package combination: ~s, ~s."
                effective-type package)
        (setf (getf headers :element-type) (string effective-type)))))

  ;; rabbitmq/qpid are broken wrt 0.9.1. they encode headers with a non-standard wire encoding
  ;; this would avoid the problem, but suppresses headers. 
  #+(or)
  (let ((connection (when channel (channel-connection channel))))
    (when (and connection
               (equal (getf (amqp:connection-server-properties connection) :|product|)
                      "RabbitMQ")
               (equal (getf (amqp:connection-server-properties connection) :|version|)
                             "2.1.0"))
      (when headers
        (amqp:log :warn instance "headers suppressed: ~s" headers)
        (setf headers nil))))

  (apply #'call-next-method instance slots
         :content-type (string (type-of content-type))
         :content-encoding (if content-encoding
                             (string (content-encoding-name content-encoding))
                             "")
         :body-size body-size
         :headers headers
         args) 
  instance )


(defgeneric channel-compute-body-size (channel object encoding)
  (:method ((channel amqp:channel) (object null) (encoding mime:*/*))
    0)
  (:method ((channel amqp:channel) (object function) (encoding mime:*/*))
    nil)
  (:method ((channel amqp:channel) (object list) (encoding mime:*/*))
    nil)

  (:method ((channel amqp:channel) (object string) (encoding mime:text/*))
    (multiple-value-bind (decoder encoder)
                         (compute-charset-codecs encoding)
      (declare (ignore decoder))
      (let ((bytes 0))
        ;; use the prospective encoder to count bytes
        (flet ((count-byte (stream byte)
                 (declare (ignore stream byte))
                 (incf bytes)))
          (declare (dynamic-extent #'count-byte))
          (do ((i 0 (1+ i)))
              ((>= i (length object)))
            (funcall encoder (char object i) #'count-byte nil)))
        bytes)))

  (:method ((channel amqp:channel) (object vector) (encoding mime:application/*))
    ;;!!! todo: binary codecs:
    ;; this supports multi-byte binary vectors even though the i/o operator do not
    (let* ((type (array-element-type object))
           (element-size (typecase type
                           (symbol )
                           (cons (case (first type)
                                   ((signed-byte unsigned-byte)
                                    (ceiling (second type) 8)))))))
      (when element-size (* (length object) element-size))))

  (:method ((channel amqp:channel) (object standard-object) (encoding mime:application/*))
    nil))


(defmethod mime:mime-type ((basic amqp:basic) &rest args)
  (declare (ignore args))
  (amqp:basic-content-type  basic))


(defgeneric canonical-element-type (channel concrete-type abstract-typ)

  (:method ((channel t) (object symbol) (element-type symbol))
    (cond ((subtypep object element-type)
           ;; ok, if there is a subtype relation
           (values object element-type t))
          ((and (eq object element-type) (fboundp element-type))
           (values element-type (fdefinition element-type) t))))
  
  (:method ((channel t) (object symbol) (element-type (eql 'character)))
    (canonical-element-type channel object 'string))

  (:method ((channel t) (object (eql 'cons)) (element-type (eql 'list)))
    (values 'list 'list t))

  (:method ((channel t) (object (eql 'vector)) (element-type (eql 'integer)))
    (canonical-element-type channel object 'vector))

  (:method ((channel t) (object cons) (element-type t))
    (if (symbolp (first object))
      (case (first object)
        ((array simple-array)
         (case (second object)
           (character (canonical-element-type channel 'string element-type))
           (t (canonical-element-type channel 'vector element-type))))
        (t
         (canonical-element-type channel (first object) element-type)))
      (canonical-element-type channel 'list element-type)))

  (:method ((channel t) (object t) (element-type t))
    (canonical-element-type channel (type-of object) element-type))

  (:method ((channel t) (operator function) (element-type t))
    (values operator element-type t))

  (:method ((channel t) (object symbol) (element-type t))
    nil))

;;; (canonical-element-type nil 'ctring 'character)
;;; (canonical-element-type nil "string" 'character)


(defgeneric amqp:basic-headers (basic)
  (:documentation "Returns the basic instance's headers."))

(defun amqp.u:basic-header (context keyword)
  (loop for (key value) on (amqp:basic-headers context) by #'cddr
        when (string-equal key keyword)
        do (return value)))

(defun (setf amqp.u:basic-header) (value context keyword)
  (setf (getf (amqp:basic-headers context) keyword) value))

(defun amqp.u:basic-header-list (context keyword)
  (loop for (key value) on (amqp:basic-headers context) by #'cddr
        when (string-equal key keyword)
        collect value))


(macrolet ((def-basic-accessors (&rest names)
             `(progn ,@(mapcar #'(lambda (name)
                                   (etypecase name
                                     (symbol `(defgeneric ,(intern (string name) :amqp) (basic)))
                                     (cons `(defgeneric ,(intern (string (first name)) :amqp) (basic) ,@(rest name)))))
                               names))))
  (def-basic-accessors
    :basic-consumer-tag
    :basic-content-encoding
    (:basic-content-type (:documentation "Returns the basic instance's content type as a string."))
    :basic-correlation-id
    :basic-delivery-mode
    :basic-delivery-tag
    :basic-exchange
    :basic-expiration
    :basic-headers
    :basic-immediate
    :basic-mandatory
    :basic-message-id
    (:basic-no-ack (:documentation "Returns the basic instance's acknowledgement setting."))
    :basic-no-local
    :basic-no-wait
    :basic-queue
    :basic-redelivered
    :basic-reply-to
    :basic-routing-key
    :basic-timestamp
    :basic-user-id))


#+(or ) ;; mcl's clos implements method dispatch by hand
(progn
  (defclass subtyping-generic-function (standard-generic-function)
    (:metaclass c2mop:funcallable-standard-class))


  (defgeneric canonical-element-type (channel concrete-type abstract-type)
    (:documentation "Recognizes combinations based on subtype behavior and
 return the maximal permited type to encode in the message. If if null is
 returned, the type is not supported.")
    (:generic-function-class subtyping-generic-function)
    
    (:method ((channel t) (concrete character) (abstract string))
      'string)
    (:method ((channel t) (concrete string) (abstract string))
      'string)
    
    (:method ((channel t) (concrete integer) (abstract vector))
      'vector)
    (:method ((channel t) (concrete string) (abstract string))
      'vector)
    
    (:method ((channel t) (concrete standard-object) (abstract standard-object))
      concrete)
    (:argument-precedence-order abstract-type concrete-type channel))

  (defmethod compute-applicable-methods ((function subtyping-generic-function) arguments)
    (let ((methods (c2mop:generic-function-methods function)))
      (print arguments)
      (print methods)
      (flet ((matches (method)
               (let ((specializers (c2mop:method-specializers method)))
                 (and (typep (first arguments) (first specializers))
                      (every #'(lambda (atype mtype) (subtypep atype mtype))
                             (rest arguments)
                             (rest specializers)))))
             (preceeds (s1 s2)
               (subtypep s1 s2))) 
        (let ((length (length arguments)))
          (setf methods (remove-if-not #'matches methods)) (print methods)
          (dotimes (i length)
            (setf methods (sort methods #'preceeds
                                :key #'(lambda (method)
                                         (elt (c2mop:method-specializers method)
                                              (- length i)))))))
        methods))))

;;; (compute-applicable-methods #'canonical-element-type (list nil 'character 'string))
;;; in mcl, this works, but it's never called

;;;
;;; channel

;;; interface classes
(def-ensure-object (amqp:channel amqp:basic) ())
(def-ensure-object (amqp:channel amqp:exchange) (exchange))
(def-ensure-object (amqp:channel amqp:file) ())
(def-ensure-object (amqp:channel amqp:queue) (queue))
(def-ensure-object (amqp:channel amqp:stream) ())
(def-ensure-object (amqp:channel amqp:tx) ())
;;; internal classes
(def-ensure-object (amqp:channel amqp:body) ())
(def-ensure-object (amqp:channel amqp:header) ())
;;; it's own connection (for channel-zero)
(defmethod amqp:ensure-object ((channel amqp:channel) (type (eql 'amqp:connection)) &key)
  (channel-connection channel))
(defmethod amqp:channel.connection ((channel amqp:channel) )
  (channel-connection channel))
;;; itself
(defmethod amqp:ensure-object ((channel amqp:channel) (type (eql 'amqp:channel)) &key)
  channel)

(def-ensure-method (amqp:channel amqp:open))
(def-ensure-method (amqp:channel amqp:open-ok))
(def-ensure-method (amqp:channel amqp:flow))
(def-ensure-method (amqp:channel amqp:flow-ok))
(def-ensure-method (amqp:channel amqp:alert))
(def-ensure-method (amqp:channel amqp:close))
(def-ensure-method (amqp:channel amqp:close-ok))

(defmethod initialize-instance ((instance amqp:channel) &rest initargs
                                &key
                                context (connection context) uri
                                exchange queue path)
  (declare (dynamic-extent initargs))
  (assert-argument-type initilialize-instance connection amqp:connection)
                        
  (apply #'call-next-method instance
         :channel instance
         :context connection
         :connection connection
         :uri (if uri
                (merge-uris uri (connection-uri connection))
                (merge-uris (uri (list :plist (list :exchange exchange :queue queue :path path)))
                            (connection-uri connection)))
         initargs))

(defmethod print-object ((instance amqp:channel) stream  &aux (*print-pretty* nil))
  ;; this signals an error if sbcl is tracing a function which is run during initialization
  ;; with an attempt to reference the %flags slot. 
  (handler-case (print-unreadable-object (instance stream :identity t :type t)
                  (format stream " [~s].~d"
                          (bound-slot-value instance 'puri:uri)
                          (bound-slot-value instance 'amqp.i::number)))
    (error (c) (format stream "can't print: ~s" c))))


(defmethod object-channel ((channel amqp:channel))
  channel)

(defgeneric channel-state (channel)
  (:documentation "The channel-specific state accessors delegate
 to the class methods. These esist as an alternative prrotocol hook.")

  (:method ((channel amqp:channel))
    (class-state channel)))

(defgeneric (setf channel-state) (state channel)
  (:documentation "The channel-specific state accessors delegate
 to the class methods. These esist as an alternative prrotocol hook.")

  (:method (state (channel amqp:channel))
    (setf (class-state channel) state)))

(defmethod channel-content-type ((channel amqp:channel))
  (device-content-type channel))

(defmethod (setf channel-content-type) (type (channel amqp:channel))
  (setf (device-content-type channel) type))

(defgeneric channel-user-id (channel)
  (:documentation "Return the user id specified in the channel's connection.")

  (:method ((channel amqp:channel))
    (connection-user-id (channel-connection channel))))


;;; nb. the channel number is one-based! so channel[0] is always the connection's.

(defmethod connection-frame-max ((channel amqp:channel))
  (with-slots (connection) channel
    (if connection
      (connection-frame-max connection)
      0)))


(defmethod channel-condition (channel type)
  (or (assoc type (channel-conditions channel))
      (first (push (make-condition type :channel channel)
                   (channel-conditions channel)))))


(defgeneric connected-channel-p (channel)
  (:documentation "Indicate whether the channel is still bound to a
 connection.")

  (:method ((channel amqp:channel))
    (let ((connection (channel-connection channel)))
      (and connection
           (integerp (channel-number channel))
           (eq (connection.channel connection :number (channel-number channel))
               channel)))))

(defgeneric channel-track (channel)
  (:documentation "Defines a default method for protocols which have none.")

  (:method ((class t))
    0))

(defgeneric channel-number (channel)
  (:documentation "Definee a method to delegate from channeled classes to their channel.")

  (:method ((class amqp-channeled-object))
    (let ((channel (object-channel class)))
      (if channel
        (channel-number channel)
        (error "Class has no channel: ~s." class)))))

(defmethod amqp:basic-headers ((channel amqp:channel))
  (amqp:basic-headers (channel.basic channel)))


;;;
;;; amqp:cluster  - nothing yet

;;;
;;; amqp:connection

(def-ensure-object (amqp:connection amqp:channel) ((number fixnum))
  (:class.object ((connection amqp:connection) &rest initargs &key number &allow-other-keys)
   "Ensure a (connection channel), cached by number."
   (declare (dynamic-extent initargs))
   (etypecase number 
     ((member t nil)
      (bt:with-lock-held ((connection-lock connection))
        (apply #'amqp::connection.channel connection
               :number (or (position nil (get-connection-channels connection))
                           (error "Connection full."))
               initargs)))
     (fixnum
      (assert-condition (< number (length (get-connection-channels connection)))
                        connection.channel)
      (let* ((instance (connection.channel connection :number number)))
        (if instance
          (if (cddr initargs)
            (apply #'reinitialize-instance instance initargs)
            instance)
          (setf (connection.channel connection :number number)
                (apply #'make-instance (class-find-object-class connection 'amqp:channel)
                       :context connection
                       initargs))))))))


(def-ensure-object (amqp:connection amqp:heartbeat) ())

(def-ensure-method (amqp:connection amqp:start))
(def-ensure-method (amqp:connection amqp:start-ok))
(def-ensure-method (amqp:connection amqp:secure))
(def-ensure-method (amqp:connection amqp:secure-ok))
(def-ensure-method (amqp:connection amqp:tune))
(def-ensure-method (amqp:connection amqp:tune-ok))
(def-ensure-method (amqp:connection amqp:open))
(def-ensure-method (amqp:connection amqp:open-ok))
(def-ensure-method (amqp:connection amqp:redirect))
(def-ensure-method (amqp:connection amqp:close))
(def-ensure-method (amqp:connection amqp:close-ok))


(defmethod shared-initialize ((instance amqp:connection) (slots t) &key
                              (threaded nil))
  "provide or renew the connection state. This includes a lock, a vector to hold channels, and
 an optional connection thread. Iff the trheaded argument is true, create a thread for the
 connection and start a processing loop.
 NB. this is likely to be called repeatedly - in particular by change class subsequent to protocol
 version negotiation."

  (with-slots (lock thread amqp-channels) instance
    (unless (slot-boundp instance 'lock)
      (setf lock (bt:make-lock (make-instance-tag instance))))
    (unless (and (slot-boundp instance 'amqp-channels) amqp-channels)
      (setf-connection-channels (make-array (1+ *max-channels*) :initial-element nil) instance))
    (unless (slot-boundp instance 'thread)      ; happens if reinitializing
      (setf thread
            (ecase threaded
              ((nil) )
              ((t)
               ;; find the current socket io handler create a new thread
               (setf thread (make-connection-thread instance))))))

    ;; nb. may not yet be able to create channel-0, since the class may be abstract
    (call-next-method)))


(defmethod initialize-instance ((instance amqp:connection) &rest initargs &key
                                uri
                                (remote-host (unless uri (error "uri or remote-host required")))
                                (remote-port *standard-port*))
  "Provide the initial connection state. This includes the frame queues. Integrate any host/port
 arguments in to an uri for use to open the connection."
 
  (declare (dynamic-extent initargs))
  (flet ((make-name (tag)
           (with-output-to-string (ss)
             (print-unreadable-object (instance ss :identity t :type t) (princ tag ss))))
         (generate-output-frame ()   (make-output-frame instance))
         (generate-input-frame ()    (make-input-frame instance)))
    (with-slots (free-input-frames free-output-frames read-frames encoded-frames)
                instance
      (setf free-input-frames
            (make-instance 'locked-stack :name (make-name :free-input-frames)
                           :if-empty #'generate-input-frame))
      (setf read-frames
            (make-instance 'locked-queue :name (make-name :read-frames)))
      (setf free-output-frames
            (make-instance 'locked-stack :name (make-name :free-output-frames)
                           :if-empty #'generate-output-frame))
      (setf encoded-frames
            (make-instance 'locked-queue :name (make-name :encoded-frames)))

      (apply #'call-next-method instance
             :uri (if uri
                    (uri uri)
                    (uri 'amqp :scheme :amqp :host remote-host :port remote-port))
             initargs))))

(defmethod print-object ((instance amqp:connection) stream)
  ;; override simple-streams method
  (print-unreadable-object (instance stream :identity t :type t)))


(defmethod object-connection ((connection amqp:connection))
  connection)


(defmethod object-channel ((connection amqp:connection))
  "Any connection commands use channel 0"
  (connection.channel connection :number 0))


(defgeneric connection-state (connection)
  (:documentation "The connection-specific state accessors delegate
 to the class methods. These esist as an alternative prrotocol hook.")

  (:method ((connection amqp:connection))
    (class-state connection)))

(defgeneric (setf connection-state) (state connection)
  (:documentation "The connection-specific state accessors delegate
 to the class methods. These exist as an alternative protocol hook.")

  (:method (state (connection amqp:connection))
    (setf (class-state connection) state)))

(defgeneric connection-user-id (connection)
  (:documentation "Return the user id specified in the connection's url or NIL of none is present.")

  (:method ((connection amqp:connection))
    (uri-user (connection-uri connection))))

(defmethod channel-number ((class amqp:connection))
  0)

(defmethod connection-host ((connection amqp:connection))
  "Return the host portion of the connection's uri."
  (uri-host (connection-uri connection)))


(defmethod connection-port ((connection amqp:connection))
  "Return the port portion of the connection's uri."
  (uri-port (connection-uri connection)))

(defmethod connection-virtual-host ((connection amqp:connection))
  "Return the virtual host specified when the connection was instantiated."
  (uri-virtual-host (connection-uri connection)))


(defgeneric connect-channel (connection channel)
  (:documentation "Bind a channel to a connection: share the connection's queues for the
 channel to use to set up encoded frame to send and t pick out read froms for decoding and processing.")

  (:method ((connection t) (channel t))
    channel)

  (:method ((connection amqp:connection) (channel amqp:channel))
    (with-slots (free-input-frames free-output-frames read-frames encoded-frames)
                channel

      (call-next-method)

      ;; share queues
      (setf free-input-frames (device-free-input-frames connection)
            free-output-frames (device-free-output-frames connection)
            read-frames (device-read-frames connection)
            encoded-frames (device-encoded-frames connection))
      ;; flush anything from an earlier incarnation
      (loop (when (null (get-read-frame channel :wait nil)) (return)))
      ;; and initialize buffers
      (device-initialize-buffers channel)
      channel)))


(defgeneric disconnect-channel (connection channel)
  (:method ((connection null) (channel amqp:channel))
    nil)
  (:method ((connection amqp:connection) (channel amqp:channel))
    (slot-makunbound channel 'free-input-frames)
    (slot-makunbound channel 'free-output-frames)
    (slot-makunbound channel 'read-frames)
    (slot-makunbound channel 'encoded-frames)
    (when (connected-channel-p channel)
      (setf (connection.channel connection :number (channel-number channel)) nil))
    (setf-channel-number 0 channel)
    (setf (object-context channel) nil)))



;;;
;;; amqp:dtx

;;; class: dtx [id method-names]
;;;   dtx.select
;;;   dtx.select-ok
;;;   dtx.start [dtx-identifier]
;;;   dtx.start-ok


;;;
;;; amqp:exchange

(def-ensure-method (amqp:exchange amqp:declare))
(def-ensure-method (amqp:exchange amqp:declare-ok))
(def-ensure-method (amqp:exchange amqp:delete))
(def-ensure-method (amqp:exchange amqp:delete-ok))


(defmethod print-object ((instance amqp:exchange) stream &aux (*print-pretty* nil))
  (print-unreadable-object (instance stream :identity t :type t)
    (format stream "[~a]" (or (ignore-errors (amqp:exchange-exchange instance)) "?"))))

(defgeneric amqp:exchange-exchange (object)
  (:documentation "The exchange name accessor is extended with a string method to allow to
 coerce arguments to a string value in request/response operators.")
  
  (:method ((exchange-name string))
    exchange-name))


;;;
;;; amqp:file

(def-ensure-method (amqp:file amqp:qos))
(def-ensure-method (amqp:file amqp:qos-ok))
(def-ensure-method (amqp:file amqp:consume))
(def-ensure-method (amqp:file amqp:consume-ok))
(def-ensure-method (amqp:file amqp:cancel))
(def-ensure-method (amqp:file amqp:cancel-ok))
(def-ensure-method (amqp:file amqp:open))
(def-ensure-method (amqp:file amqp:open-ok))
(def-ensure-method (amqp:file amqp:stage))
(def-ensure-method (amqp:file amqp:publish))
(def-ensure-method (amqp:file amqp:return))
(def-ensure-method (amqp:file amqp:deliver))
(def-ensure-method (amqp:file amqp:ack))
(def-ensure-method (amqp:file amqp:reject))


;;;
;;; amqp:link - nyi


;;;
;;; amqp:object

(defmethod reinitialize-instance :before ((instance amqp-connected-object) &key class)
  "When reinitialized from a frame, check that the class is correct."
  (when (and class  (not (typep instance class)))
    (amqp:frame-error :channel (object-channel instance)
                      :message-string "Attempt to reinitialize from a different frame class: ~s, ~s."
                      :message-arguments (list instance class))))

(defmethod connection-frame-size ((object amqp-connected-object))
  (connection-frame-size (object-connection object)))



;;;
;;; amqp:queue

(def-ensure-method (amqp:queue amqp:declare))
(def-ensure-method (amqp:queue amqp:declare-ok))
(def-ensure-method (amqp:queue amqp:bind))
(def-ensure-method (amqp:queue amqp:bind-ok))
(def-ensure-method (amqp:queue amqp:unbind))
(def-ensure-method (amqp:queue amqp:unbind-ok))
(def-ensure-method (amqp:queue amqp:purge))
(def-ensure-method (amqp:queue amqp:purge-ok))
(def-ensure-method (amqp:queue amqp:delete))
(def-ensure-method (amqp:queue amqp:delete-ok))


(defmethod print-object ((instance amqp:queue) stream &aux (*print-pretty* nil))
  (print-unreadable-object (instance stream :identity t :type t)
    (format stream "[~a]" (or (ignore-errors (amqp:queue-queue instance)) "?"))))


(defgeneric amqp:queue-queue (object)
  (:documentation "The queue name accessor is extended with a string method to allow to
 coerce arguments to a string value in request/response operators.")
  (:method ((queue-name string))
    queue-name))


;;;
;;; amqp:session - nyi

;;;
;;; amqp:stream

(def-ensure-method (amqp:stream amqp:qos))
(def-ensure-method (amqp:stream amqp:qos-ok))
(def-ensure-method (amqp:stream amqp:consume))
(def-ensure-method (amqp:stream amqp:consume-ok))
(def-ensure-method (amqp:stream amqp:cancel))
(def-ensure-method (amqp:stream amqp:cancel-ok))
(def-ensure-method (amqp:stream amqp:publish))
(def-ensure-method (amqp:stream amqp:return))
(def-ensure-method (amqp:stream amqp:deliver))


;;;
;;; amqp:tx

(def-ensure-method (amqp:tx amqp:select))
(def-ensure-method (amqp:tx amqp:select-ok))
(def-ensure-method (amqp:tx amqp:commit))
(def-ensure-method (amqp:tx amqp:commit-ok))
(def-ensure-method (amqp:tx amqp:rollback))
(def-ensure-method (amqp:tx amqp:rollback-ok))


;;;

;;; class: test [id method-names]
;;;   test.integer [integer-1 integer-2 integer-3 integer-4 operation]
;;;   test.integer-ok [result]
;;;   test.string [string-1 string-2 operation]
;;;   test.string-ok [result]
;;;   test.table [table integer-op string-op]
;;;   test.table-ok [integer-result string-result]
;;;   test.content
;;;   test.content-ok [content-checksum]

;;;
;;; amqp:tunnel

;;;   tunnel.request [meta-data]



;;;
;;; decoding definition operators


(defgeneric method-argument-offset (method)
  (:documentation "Contributes to codecs for version-specific methods,
 as the method body layout varies."))

(defgeneric class-property-offset (class)
  (:documentation "Contributes to codecs for version-specific classes,
 if the class content header layout varies.")

  (:method ((class amqp:object))
    12))






;;; frame operations:
;;; allocate, resource-manage, read, and write immediately through
;;; a connection or indirectly through a channel.

(defgeneric frame-channel-number (frame) )

(defgeneric (setf frame-channel-number) (number frame) )

(defgeneric claim-input-frame (device)
  (:documentation "Returns a free input frame or creates a new one.")

  (:method ((channel amqp:channel))
    (let ((frame (dequeue (device-free-input-frames channel))))
      (setf (frame-channel-number frame) (channel-number channel))
      frame))

  (:method ((connection amqp:connection))
    (let ((frame (dequeue (device-free-input-frames connection))))
      (setf (frame-channel-number frame) 0)
      frame)))



(defgeneric claim-output-frame (connection)
  (:documentation "Returns a free output frame or creates a new one.")

  (:method ((channel amqp:channel))
    (flet ((make-channel-frame ()
             (make-output-frame channel)))
      (declare (dynamic-extent #'make-channel-frame))
      (let ((frame (dequeue (device-free-output-frames channel)
                            :if-empty #'make-channel-frame)))
        (setf (frame-channel-number frame) (channel-number channel))
        frame)))

  (:method ((class amqp-channeled-object))
    (claim-output-frame (object-channel class)))

  (:method ((connection amqp:connection))
    (flet ((make-connection-frame ()
             (make-output-frame connection)))
      (declare (dynamic-extent #'make-connection-frame))
      (let ((frame (dequeue (device-free-output-frames connection)
                            :if-empty #'make-connection-frame)))
        (setf (frame-channel-number frame) 0)
        frame))))



;;;
;;; resolving an abstract class to the version specific one

(defgeneric amqp:find-protocol-class (abstract-class version &key if-does-not-exist)
  (:documentation "GIven an abstract protocol class and a version,
 examine the desigated package for a class with the same name.")

  (:method ((instance amqp:object) version &rest args)
    (apply #'amqp:find-protocol-class (type-of instance) version args))

  (:method ((designator t) (version symbol) &rest args)
    (apply #'amqp:find-protocol-class designator
           (or (find-package version)
               (error "no protocol implementation for version: ~s" version))
           args))

  (:method ((class-name symbol) (package package)  &key (if-does-not-exist :error))
    (let ((found
           (or (if (eq package (symbol-package class-name))
                 (find-class class-name if-does-not-exist)
                 (let ((symbol (find-symbol (symbol-name class-name) package)))
                   (and symbol (find-class symbol nil))))
               (ecase if-does-not-exist
                 ((nil) nil)
                 (:error (error "no protocol implementation for class in version: ~s, ~s"
                                class-name (package-name package)))))))
      (when found
        (c2mop:finalize-inheritance found)
        (assert (find 'amqp:object (closer-mop:class-precedence-list found) :key #'class-name) ()
                "Class is not a protocol class: ~s." class-name)
        found))))


#+(or )  ;; a version which worked by searching class specializations...
(defgeneric amqp:find-protocol-class (abstract-class version &key if-does-not-exist)
  (:documentation "GIven an abstract protocol class and a version,
 return the most specialized class with the highest matching version.")

  (:method ((abstract-class symbol) version &rest args)
    (apply #'amqp:find-protocol-class (find-class abstract-class) version args))

  (:method ((instance amqp:object) version &rest args)
    (apply #'amqp:find-protocol-class (class-of instance) version args))

  (:method ((abstract-class class) version &key (if-does-not-exist :error))
    (let ((found nil))
      (labels ((walk-subclasses (class)
                 (when (and (typep class 'amqp:class-class)
                            (null (closer-mop:class-direct-subclasses class)))
                   (unless (version-lessp version (class-protocol-version class))
                     (cond ((null found)
                            (setf found class))
                           ((equalp (class-protocol-version found)
                                    (class-protocol-version class))
                            ;; replace the more abstract with the more specific
                            (unless (subtypep (class-name class) (class-name found))
                              (warn "duplicate protocol implementations for version: ~s"
                                    (class-protocol-version found)))
                            (setf found class))
                           ((version-lessp (class-protocol-version found)
                                           (class-protocol-version class))
                            (setf found class)))))
                 (map nil #'walk-subclasses (closer-mop:class-direct-subclasses class))))
        (walk-subclasses abstract-class)
        (or found
            (ecase if-does-not-exist
              ((nil) nil)
              (:error (error "no protocol implementation for version: ~s" version))))))))

