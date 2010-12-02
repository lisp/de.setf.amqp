;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines the protocol operators for AMQP `class` and `METHOD` entities for the
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

 (long-description "Each (object . method) combination corresponds to several operators, which act in concert
 to implement the protocol:

 - `respond-to-_method_`   peforms the command on a client object in response to a broker message.
 This includes changes to instance state, open/close side effects for `connection` and `channel` methods,
 instantiating and binding, or releasing any related, as well as any requisite broker message responses
 as confirmation or as further processing.

 - `request-_method_`      issues the request to the broker, together with any client object operations
 required by the protocol.

 - `send-_method_`         encodes frames and performs transport-level operations to send the command to
 the broker. This delegates to protocol-specific methods, which encode the respective arguments, and to
 the network device operations for the network stream functions. 

 - no explicit receive operators are defined, as messages are self-describing and decoded accordingly. 
 Application code is writtein in terms of `command-case` or `command-loop` statements which dispatch based
 on received commands type.

  The `def-amqp-command` forms below define the protocol class and the generic method operators.
 The `:response` and `:request` clauses include methods as appropriate to whether both the broker and
 the client implement the operation. A `:request` clause automatically defined a `send-` operators.
 An additional (possibly blank) `:send` clause can be included if sending must be supported in addition to
 a complete command request.

  The respective respond-to and and request operators are implemented in two layers.
 The interface operator, which uses the elementary name, is implemented in terms of a second
 operator: `channel-respond-to-`, or `channel-request-`, which requires an additional initial argument,
 the `channel`. The delegation call interposes the respective `objects-channel` value as this initial
 argument. The specialized methods are defined with `amqp:channel` as the initial specialization.

 The interface architecture makes it possible for applications to alter the api behavior by specializing
 just the channel, just the protocol class, or both."))




(defun response-function (name)
  "For use as the initform for method response functions, if the target is defined, ok. Otherwise use instead
 the default response function, which signals an error."
  (if (fboundp name)
     name
     #'(lambda (class &rest args) (apply #'default-channel-respond-to class name args))))


(defgeneric default-channel-respond-to
  (channel class &rest args)
  (:documentation "the base protocol response operator for alert.")
  (:method :before ((channel t) (class t) &rest args) "a before method logs the response-to-be and updates the class instance."
           (declare (dynamic-extent args))
           (amqp:log* default-channel-respond-to class args))
  (:method ((channel amqp:channel) (class t) &rest args)
    (amqp:not-implemented-error :message-string "Unimplemented method: ~s . ~s"
                                :message-arguments (list class args))))


(def-amqp-command amqp:ack (class &key delivery-tag multiple)
  (:documentation "C-->S : acknowledge one of more messages")
  
  (:request
   (:method ((class amqp::basic) &rest args &key delivery-tag multiple)
     (declare (ignore delivery-tag multiple))
     (apply #'amqp:send-ack class args)
     class)
   
   (:method ((class amqp::file) &rest args &key delivery-tag multiple)
     (declare (ignore delivery-tag multiple))
     (apply #'amqp:send-ack class args)
     class)))


(def-amqp-command amqp:alert (class &key reply-code reply-text details)
  (:documentation "C<--S : send a non-fatal warning message : Async , carries content ")

  (:response
   (:method ((class amqp::channel) &rest args)
     (declare (ignore args))
     "Do nothing more than log the message."
     class)))


(def-amqp-command amqp:bind (class &key ticket queue exchange routing-key no-wait arguments)
  (:documentation "C-->S: Bind queue to an exchange")
  
  (:request
   (:method ((queue-class amqp::queue) &rest args &key ticket queue exchange routing-key no-wait arguments)
     (declare (dynamic-extent args))

     (assert-argument-types amqp:bind
       (ticket integer nil)
       (queue (or string amqp:queue))
       (exchange (or string amqp:exchange))
       (routing-key string nil)
       (no-wait amqp:bit nil)
       (arguments list nil))

     (setf exchange (amqp:exchange-exchange exchange))
     (setf queue (amqp:queue-queue queue))

     (apply #'amqp::send-bind queue-class :exchange exchange :queue queue
            args)
     (command-loop (queue-class)
       (amqp:bind-ok (queue) 
        (amqp:log :debug queue "bound.")
        (return-from command-loop))
       (t
        (class &rest args)
        (amqp:log :warn class "Unexpected response: ~s . ~s" class args)
        (return-from command-loop)))
     queue-class)))


(def-amqp-command amqp:bind-ok (class &key)
  (:documentation "C<--S : Confirm bind successful.
 This command appears as eventual response to a Bind, and should be processed
 synchronously by a request-bind. If one appears independently, log it.
 and continue.")
  
  (:response
   (:method ((queue amqp:queue) &key)
     "Simply log and continue."
     queue)))


(def-amqp-command amqp:cancel (class &rest args &key consumer-tag no-wait)
  (:documentation "C-->S :
 This method cancels a consumer. This does not affect already delivered messages, but it does mean the 
server will not send any more messages for that consumer. The client may receive an arbitrary number of 
messages in between sending the cancel method and receiving the cancel-ok reply.")
  
  (:request
   (:method ((basic amqp::basic) &rest args &key consumer-tag no-wait)
     (declare (dynamic-extent args))
     (assert-argument-types amqp:cancel
       (consumer-tag (amqp:string 8) nil)
       (no-wait amqp:bit nil))

     (apply #'amqp:send-cancel basic args)

     (command-loop (basic)
       ;; skip everything except the -ok
       (amqp:cancel-ok (class) (return-from command-loop))
       (amqp:header (frame) t)
       (amqp:body (frame) t))
     basic)))


(def-amqp-command amqp:cancel-ok (class &key consumer-tag)
  (:documentation "C<--S : confirm a canceled consumer.
 This command appears as eventual response to Cancel and should be processed
 synchronously by a request-cancel. If one appears independently, log it.
 and continue.")

  (:response
   (:method ((basic amqp::basic) &key consumer-tag)
     (declare (ignore consumer-tag))
     "Simply log and continue."
     basic)))


(def-amqp-command amqp:close (class &key reply-code reply-text class-id method-id)
  (:documentation "C<->S : request a connection or a channel close")

  (:request
   (:method ((channel amqp:channel) &key (reply-code 0) (reply-text "") (class-id 0) (method-id 0))
     "Perform a local channel close and forward the reauest to the broker.
 Invoked ambivalently with close->device-close. The channel
 state indicates the progress: if it's :close-channel, then the stream close is
 in progress. stream close after the protocol close. The broker request
 entails a synchronous close-ok response."
     
     (let ((initial-state (shiftf (channel-state channel) amqp.s:close-channel)))
       (etypecase initial-state
         ((or amqp.s:use-channel amqp.s:close-channel)
          (when (connected-channel-p channel)
            (amqp::send-close channel
                             :reply-code reply-code
                             :reply-text reply-text
                             :class-id class-id
                             :method-id method-id)
            (command-loop (channel)
              (amqp:header (basic &rest args)
                (declare (dynamic-extent args))
                (amqp:log :debug basic "Draining closed channel: ~s . ~s" basic args)
                nil)
              (amqp:body (basic &rest args)
                (declare (dynamic-extent args))
                (amqp:log :debug basic "Draining closed channel: ~s . ~s" basic args)
                nil)
              (amqp:close-ok (channel &key &allow-other-keys) (return-from command-loop)))

            ;; once the channel is flushed, close the stream if that's not already in progress
            (unless (typep initial-state 'amqp.s:close-channel)
              (device-close channel nil)))))
       channel))

   (:method ((connection amqp:connection) &key (reply-code 0) (reply-text "")
             (class-id (amqp:class-id connection))
             (method-id 0))
     "Perform a local connection close and forward the request to the broker.
 Then close the local stream."
     
     (let ((initial-state (shiftf (connection-state connection) amqp.s:close-connection)))
       (etypecase initial-state
         ;; if in use, or closing due to stream close, then send the close, and
         ;; check whether to close the stream.
         ((or amqp.s:use-connection amqp.s:close-connection)
          (amqp::send-close connection
                           :reply-code reply-code
                           :reply-text reply-text
                           :class-id class-id
                           :method-id method-id)

          (command-loop ((connection.channel connection :number 0))
            (amqp:close-ok (connection) (return-from command-loop)))
          
          ;; once the connection is flushed, if the initial state was in use, close the stream
          (typecase initial-state
            (amqp.s:use-connection
             (close connection)
             ;; once it has been closed, reset to the initial state
             (setf (connection-state connection) amqp.s:open-connection))))
         ;; if, eg. already closing, do nothing
         (amqp.s:connection-state ))
       connection)))

  (:response
   (:method ((channel amqp:channel) &key &allow-other-keys)
     "Perform a remotely requested on the channel by sending the ok to the server and
 disconnecting and closing the local stream."
     
     (when (connected-channel-p channel)
       (amqp::send-close-ok channel)
       
       ;; disconnect it and close the stream
       (disconnect-channel (channel-connection channel) channel)
       (close channel)
       channel))
   
   (:method ((connection amqp:connection) &key reply-code reply-text class-id method-id)
     "Perform a remotely requested connection close by sending the ok to the server and
 closing the local stream."
     (declare (ignore reply-code reply-text class-id method-id))
     
     (when (open-stream-p connection)
       (amqp::send-close-ok connection)
       
       ;; once the response is sent, close the stream
       (close connection))
     connection)))


(def-amqp-command amqp:close-ok (class &key)
  (:documentation "C<->S : confirm a channel or connection close close : Sync response to Close.
 This command appears as the eventual response to Cancel and should be processes
 synchronously together with that. I one appears independently, ignore it.")

  (:request
   (:method ((class amqp:connection) &key)
     (amqp::send-close-ok class)
     class)
   
   (:method ((class amqp:channel) &key)
     (amqp::send-close-ok class)
     class))

  (:response
   (:method ((class amqp:connection) &key)
     class)
   
   (:method ((class amqp:channel) &key)
     class)))


(def-amqp-command amqp:commit (class &key)
  (:documentation "C-->S : Commit the current transaction.")

  (:request
   (:method ((tx amqp:tx) &key)
     "Send the command and wait for the response."
     
     (amqp::send-commit tx)
     (command-loop (tx)
       (amqp:commit-ok (class) (return-from command-loop))))))
  

(def-amqp-command amqp:commit-ok (class &key)
  (:documentation "C<--S : Confirm a transaction as a syncronous response to Commit
 This command appears as eventual response to Commit and should be processed
 synchronously together with that. I one appears independently, ignore it.")

  (:response
   (:method ((tx amqp:tx) &key)
     tx)))


(def-amqp-command amqp:consume (class &key queue consumer-tag no-local no-ack exclusive no-wait arguments)
  (:documentation "C-->S : Create a consumer for a given queue.

 CLASS : amqp:basic : a basic class instance bound to a channel.

 The passed basic instance mediates a consume request on the channel and is
 returned as a handle to mediate responses. In a synchronous application,
 the channel owner can proceed directly to process deliver replies. In an
 event-driven application, the owner can register a handler for future
 deliver commands and process them either as polled or asynchronous events.")
  
  (:request
   (:method ((queue amqp:queue) &rest args)
     (declare (dynamic-extent args))
     (let ((channel (queue-channel queue)))
       (apply #'channel-request-consume channel (amqp:channel.basic channel)
              :queue queue
              args)))
     
   (:method ((basic amqp:basic) &rest args &key queue consumer-tag no-local no-ack exclusive no-wait arguments)
     (declare (dynamic-extent args))

     (assert-argument-types amqp:consume
       (queue (or (amqp:string 8) amqp:queue))
       (consumer-tag (amqp:string 8) nil)
       (no-local amqp:bit nil)
       (no-ack amqp:bit nil)
       (exclusive amqp:bit nil)
       (no-wait amqp:bit nil)
       (arguments list nil))
     
     (setf queue (amqp:queue-queue queue))
     (apply #'amqp::send-consume basic :queue queue args)

     (command-loop (basic)
       (amqp:consume-ok ((class amqp:basic) &key consumer-tag)
         (amqp:log :debug class "consume ok: ~s"  consumer-tag)
         (return-from command-loop)))
     
     ;; once the request is acknowledged, return the issuing class
     basic)))


(def-amqp-command amqp:consume-ok (class &key consumer-tag)
  (:documentation "C<--S : Confirm a consume.  Sync response to Commit
 This command appears as eventual response to Consume and should be processed
 synchronously together with that. If one appears independently, ignore it.")

  (:response
   (:method ((basic amqp:basic) &key consumer-tag)
     (declare (ignore consumer-tag))
     basic)))


(def-amqp-command amqp:declare (class &key ticket queue exchange passive durable exclusive auto-delete no-wait arguments
                                             type)
  (:documentation "C-->S : Request the broker to declare an exchange or a queue,
 and create it if needed.")

  (:request
   (:method ((exchange amqp:exchange) &rest args)
     (declare (dynamic-extent args))
     (apply #'amqp::send-declare exchange args)
     (command-loop (exchange)
       (amqp:declare-ok ((class amqp:exchange) &key ) (return-from command-loop)))
     exchange)

   (:method ((queue amqp:queue) &rest args)
     (apply #'amqp::send-declare queue args)
     (command-loop (queue)
       (amqp:declare-ok ((class amqp:queue) &key  queue message-count consumer-count)
        (amqp:log :debug queue "queue declared: ~a ~a ~a"  queue message-count consumer-count)
        (return-from command-loop)))
     queue)))
    

(def-amqp-command amqp:declare-ok (class &key queue message-count consumer-count)
  (:documentation "C<--S : Confirm a declare.  Sync response to Declare.
 This command appears as eventual response to Declare and should be processed
 synchronously together with that. I one appears independently, ignore it.")

  (:response
   (:method ((class amqp:object) &rest args)
     (declare (dynamic-extent args) (ignore args))
     class)))


(def-amqp-command amqp:Delete (class &key queue if-unused if-empty)
  (:documentation "C-->S : ")

  (:request
   (:method ((exchange amqp:exchange) &rest args)
     (declare (dynamic-extent args))
     (apply #'amqp::send-delete exchange args)
     (command-loop (exchange)
       (amqp:delete-ok (class) (return-from command-loop)))
     exchange)

   (:method ((queue amqp:queue) &rest args)
     (declare (dynamic-extent args))
     (apply #'amqp::send-declare queue args)
     (command-loop (queue)
       (amqp:declare-ok (class) (return-from command-loop)))
     queue)))


(def-amqp-command amqp:delete-ok (class &key queue message-count)
  (:documentation "C<--S : ")

  (:response
   (:method ((class amqp:object) &rest args)
     (declare (dynamic-extent args) (ignore args))
     class)))


(def-amqp-command amqp:deliver (class &key body consumer-tag delivery-tag redelivered exchange routing-key)
  (:documentation "C<--S : notify a client of an incoming consumer message.
 CLASS : The client class to which the message is being delivered.
  A read frame generates an immediate basic instance, which then delegates
  further processing based on the connection's mode:
   :queue   causes the entire message to be read and enqueued as a raw sequence
   :stream  causes the connection/channel to be placed in content mode to, with
    adjustments to stream parameters for future reading.")
  
  (:response
   (:method ((basic amqp:basic) &rest args)
     (declare (dynamic-extent args))
     (let ((channel (object-channel basic)))
       (prog1 (apply #'device-read-content channel args)
         (when (channel-acknowledge-messages channel)
           (amqp::send-ack basic)))))))


(def-amqp-command amqp:Flow (class &key active)
  (:documentation "C<->S : enable/disable flow from peer : Sync request ")

  (:response
   (:method ((channel amqp:channel) &key active)

     (amqp::send-flow-ok channel :active active)
     (ecase active
       (0 (signal (channel-condition channel 'channel-flow-stop-condition)))
       (1 (signal (channel-condition channel 'channel-flow-start-condition))))))

  (:request
   (:method ((channel amqp:channel) &key active)
     (amqp::send-flow channel :active active)
     ;; what happens now? the flow-ok appears in the content stream?
     channel)))


(def-amqp-command amqp:Flow-Ok (class &key active)
  (:documentation "C<->S : confirm a flow method : Async response to Flow
 This command appears as eventual response to Flow and should be processed
 synchronously together with that. I one appears independently, ignore it.")

  (:response
   (:method ((class amqp:channel) &key active)
     (declare (ignore active))
     class))

  (:send ))                             ; needed for the send rsponse


(def-amqp-command amqp:get (object &key queue no-ack body)
  (:documentation "C-->S : C:GET ( S:GET-OK content / S:GET-EMPTY )
    Request the 'next' message for the given queue.
 OBJECT : (or amqp:channel amqp:basic amqp:queue) : designates the queue

 Resolves the given object to the queue and encodes a Basic.Get with the appropriate arguments.
 Processes the responses get-ok and get-empty. If the reply is -ok invoke `device-read-content`
 and return the result. If -empty, return nil.")

  (:request
   (:method ((channel amqp:channel) &rest args)
     (declare (dynamic-extent args))
     (apply #'channel-request-get channel (amqp:channel.basic channel) args))

   (:method ((channel amqp:queue) &rest args &key queue no-ack body)
     (declare (dynamic-extent args) (ignore no-ack body))
     ;;;??? should better use the queues own channel?
     (apply #'channel-request-get amqp:channel (amqp:channel.basic amqp:channel)
            :queue queue
            args))

   (:method ((basic amqp:basic) &rest args &key queue no-ack (body nil body-s))
     (declare (dynamic-extent args))
     (assert-argument-type amqp:get queue (or string amqp:queue))
     (setf queue (amqp:queue-queue queue))
     (setf (channel-acknowledge-messages (object-channel basic)) (not no-ack))
     (when body-s
       (setf args (copy-list args))
       (remf args :body))
     (apply #'amqp::send-get basic :queue queue args)

     (command-case (basic)
       (amqp:get-empty ((basic amqp:basic) &rest get-empty-args)
                       (declare (dynamic-extent get-empty-args))
                       (amqp:log :debug basic "respond-to-get, get-empty: ~s"  get-empty-args)
                       (return-from command-case nil))
       (amqp:get-ok ((basic amqp:basic) &rest get-ok-args
                     &key delivery-tag redelivered exchange routing-key message-count)
                    (declare (dynamic-extent get-ok-args)
                             (ignore redelivered exchange routing-key message-count))
         (amqp:log :debug basic "respond-to-get, get-ok: ~s"  get-ok-args)
         (let ((channel (object-channel basic)))
           (return-from command-case
             (prog1 (apply #'device-read-content channel :body body get-ok-args)
               (unless (amqp:basic-no-ack basic)
                 (amqp::send-ack basic :delivery-tag delivery-tag))))))))))


(def-amqp-command amqp:get-ok (class &key delivery-tag redelivered exchange routing-key message-count)
  (:documentation "C<--S : provide client with a message")

  (:response
   (:method ((basic amqp:basic) &rest args)
     (declare (dynamic-extent args))
     (let ((channel (object-channel basic)))
       ;;; nb. do not ack a get-ok
       (prog1 (apply #'device-read-content channel args))))))


(def-amqp-command amqp:Get-Empty (class &key)
  (:documentation "C<--S : indicate no message available")

  (:response
   (:method ((basic amqp:basic) &key)
     nil)))


(def-amqp-command amqp:open (class &key virtual-host)
  (:documentation "C-->S : open a connection or channel for use : Sync request , carries content.
 If on a connection, it specifies the virtual host name. On a channel, the id is in the header.")
  
  (:request
   (:method ((class amqp:connection) &rest args
             &key virtual-host &allow-other-keys)
     "Set-Up the connection for a given virutal host"
     (declare (dynamic-extent args))
     (assert (stringp virtual-host) ()
             "The required virtual-host must be a string: ~s" virtual-host)
     (apply #'amqp::send-open class args)
     (command-loop (class)
       (amqp:open-ok (class &rest args)
         (declare (dynamic-extent args))
         (apply #'amqp::respond-to-open-ok class args)
         (return-from command-loop)))
     class)

   (:method ((class amqp:channel) &rest args)
     (apply #'amqp::send-open class args)
     (command-loop (class)
       ; qpid answers with a channel command
       (amqp:open-ok (class &rest args)
         (amqp:log :debug class "Opened: ~s" args)
         (return-from command-loop))))))


(def-amqp-command amqp:Open-Ok (class &key)
  (:documentation "C<--S : signal that connection is ready")

  (:response
   (:method ((class amqp::connection) &key &allow-other-keys)
     class)
   (:method ((class amqp::channel) &key &allow-other-keys)
     class)))


(def-amqp-command amqp:Publish (class &key  body exchange routing-key mandatory immediate
                                      content-type content-encoding headers delivery-mode
                                      priority correlation-id reply-to expiration message-id timestamp
                                      type user-id)
  (:documentation "C-->S : publish a message :
This method publishes a message to a specific exchange. The message will be routed to queues as 
defined by the exchange configuration and distributed to any active consumers when the transaction, if 
any, is committed.")
  
  (:request
   (:method ((exchange amqp:exchange) &rest args)
     "Given an exchange, delegate to its channel's basic instance."
     (declare (dynamic-extent args))
     (apply #'amqp::request-publish (amqp:channel.basic (amqp.u:exchange-channel exchange)) args))
   
   (:method ((channel amqp:channel) &rest args)
     "The class' channel is state is set to use-channel.body.output, the stream is cleared,
 and the encoding is asserted. If a body is supplied, then, it is written. Otherwise the
 channel is left available as a stream."
     (declare (dynamic-extent args))
     ;; delegate to the channel's basic class
     (apply #'amqp::request-publish (amqp:channel.basic channel) args))
   
   (:method ((basic amqp:basic) &rest args &key (body nil body-s)
             (ticket nil t-s)
             (exchange (amqp:basic-exchange basic))
             (routing-key (amqp:basic-routing-key basic))
             (mandatory (amqp:basic-mandatory basic))
             (immediate (amqp:basic-immediate basic))
             content-type content-encoding headers delivery-mode
             priority correlation-id reply-to expiration message-id timestamp
             type user-id)
     (declare (ignore content-type content-encoding headers delivery-mode
                      priority correlation-id reply-to expiration message-id timestamp
                      type user-id))
     (setf exchange (amqp:exchange-exchange exchange))          ; coerce to a string
     (setf (amqp:basic-exchange basic) exchange)     ; cache for possible use in chunk headers
     (when body-s
       (setf args (copy-list args))
       (remf args :body))
     (if t-s                            ; version variation
       (amqp::send-publish basic :ticket ticket :exchange exchange :routing-key routing-key
                           :mandatory mandatory :immediate immediate)
       (amqp::send-publish basic :exchange exchange :routing-key routing-key
                           :mandatory mandatory :immediate immediate))
     
     (let ((channel (object-channel basic)))
       (apply #'device-write-content channel body :exchange exchange args)))))


(def-amqp-command amqp:purge (class &key queue no-wait)
  (:documentation "C<->S : "))


(def-amqp-command amqp:purge-ok (class &key message-count)
  (:documentation "C<->S : "))


(def-amqp-command amqp:qos (class &key prefetch-size prefetch-count global)
  (:documentation "C-->S : ")

  (:request
   (:method ((basic amqp:basic) &rest args)
     (apply #'amqp::send-qos basic args)
     (command-loop (basic)
       (amqp:qos-ok (basic) (return-from command-loop)))
     basic)))

(def-amqp-command amqp:qos-ok (class &key)
  (:documentation "C<-S : ")

  (:response
   (:method ((class amqp:basic) &key)
     class)))

(def-amqp-command amqp:recover (class &key requeue)
  (:documentation "C-->S : ")

  (:request
   (:method ((basic amqp:basic) &rest args)
     (apply #'amqp::send-recover basic args)
     (command-loop (basic)
       (amqp:recover-ok (basic) (return-from command-loop)))
     basic)))

(def-amqp-command amqp:recover-async (class &key requeue)
  (:documentation "C-->S : ")

  (:request
   (:method ((basic amqp:basic) &rest args)
     (apply #'amqp::send-recover-async basic args)
     basic)))

(def-amqp-command amqp:recover-ok (class &key )
  (:documentation "C<-S : ")

  (:response
   (:method ((class amqp:basic) &key)
     class)))


(def-amqp-command amqp:Redirect (class &key)
  (:documentation ""))


(def-amqp-command amqp:Reject (class &key delivery-tag multiple)
  (:documentation "C-->S : reject an incoming message"))


(def-amqp-command amqp:request (class &key realm exclusive passive active write read)
  (:documentation "C-->S : ")

  (:request
   (:method ((access amqp:access) &rest args)
     (apply #'amqp::send-request access args)
     (command-loop (access)
       (amqp:request-ok (access) (return-from command-loop)))
     access)))

(def-amqp-command amqp:request-ok (class &key)
  (:documentation "C<-S : ")

  (:response
   (:method ((access amqp:access) &key ticket)
     (declare (ignore ticket))
     access)))


(def-amqp-command amqp:Return (class &key reply-code reply-text exchange routing-key)
  (:documentation "C<--S : return a failed message"))


(def-amqp-command amqp:rollback (class &key)
  (:documentation "C-->S : ")

  (:request
   (:method ((tx amqp:tx) &key)
     "Send the command and wait for the response."
     
     (amqp::send-rollback tx)
     (command-loop (tx)
       (amqp:rollback-ok ((tx amqp:tx)) (return-from command-loop)))
     tx)))


(def-amqp-command amqp:rollback-ok (class &key queue message-count)
  (:documentation "C<--S : ")
  (:response
   (:method ((class amqp::connection) &key &allow-other-keys)
     class)
   (:method ((class amqp::channel) &key &allow-other-keys)
     class)))


(def-amqp-command amqp:Secure (class &key challenge)
  (:documentation "C<--S : security mechanism challenge ")
  
  (:response
   (:method ((connection amqp:connection) &key &allow-other-keys)
     (amqp::send-secure-ok connection :response (uri-userinfo (connection-uri connection))))))


(def-amqp-command amqp:Secure-Ok (class &key response)
  (:documentation "C->S : security mechanism response")
  
  (:request
   (:method ((connection amqp:connection) &key response)
     (declare (ignore response))
    connection)))


(def-amqp-command amqp:select (class &key)
  (:documentation "C-->S : Select transaction mode.")

  (:request
   (:method ((tx amqp:tx) &key)
     "Send the command and wait for the response."
     
     (amqp::send-select tx)
     (command-loop (tx)
       (amqp:select-ok ((tx amqp:tx)) (return-from command-loop)))
     tx)))
  

(def-amqp-command amqp:select-ok (class &key)
  (:documentation "C<--S : Confirm a transaction as a syncronous response to select
 This command appears as eventual response to select and should be processed
 synchronously together with that. I one appears independently, ignore it.")

  (:response
   (:method ((tx amqp:tx) &key)
     tx)))


;; SASL rfc4422
;; . anonymous rfc4606
;; . plain rfc4616
;; QPID configuration : http://qpid.apache.org/qpid-design-authentication.html
(def-amqp-command amqp:start (class &key version-major version-minor server-properties mechanisms locales)
  (:documentation "C<--S : start connection negotiation")
  
  (:response
   (:method ((connection amqp:connection)
             &key version-major version-minor server-properties mechanisms locales)
     (declare (ignore version-major version-minor))
     (with-slots (amqp:locale amqp:mechanism) connection
       (setf (amqp:connection-server-properties connection) server-properties)
       (cond (amqp:locale
              (unless (search amqp:locale locales)
                (error "Specified locale not supported by server: ~s: ~s"
                       amqp:locale locales)))
             ((stringp (setf amqp:locale (first (split-string " " locales)))))
             (t
              (error "No locale available.")))
       (cond (amqp:mechanism
              (unless (search amqp:mechanism mechanisms)
                (error "Specified mechanism not supported by server: ~s: ~s"
                       amqp:mechanism mechanisms)))
             ((stringp (setf amqp:mechanism (first (split-string " " mechanisms)))))
             (t
              (error "No mechanism available.")))
       
       (amqp::send-start-ok connection
                           :client-properties nil
                           :mechanism amqp:mechanism
                           :response (format nil "~c~a~c~a"
                                             #\null (or (uri-user (connection-uri connection)) "")
                                             #\null (or (uri-password (connection-uri connection)) ""))
                           :locale amqp:locale)
       connection))))


(def-amqp-command amqp:start-ok (class &key client-properties mechanism response locale)
  (:documentation "C->S : select security mechanism and locale")

  (:request
   (:method ((connection amqp:connection) &rest args)
     (declare (dynamic-extent args))
     (apply #'amqp::send-start-ok connection args))))


(def-amqp-command amqp:tune (class &key channel-max frame-max heartbeat)
  (:documentation "C<--S : propose connection tuning parameters")
  
  (:response
   (:method ((connection amqp:connection) &key channel-max frame-max heartbeat)
     (when (> channel-max 0) 
       (setf channel-max (min channel-max *max-channels*))
       (unless (> channel-max (position-if #'identity (get-connection-channels connection) :from-end t))
         (amqp:not-allowed-error :connection connection
                                 "Attempt to tune an active connection: ~s." connection)
         (setf-connection-channels (adjust-array (get-connection-channels connection)
                                                 (1+ channel-max) :initial-element nil)
                                   connection)))
     (when (> frame-max 0)
       (assert (>= frame-max (connection-frame-max connection)) ()
               "Connection frame size too small: ~s, ~s" connection frame-max))
     (setf (connection-heartbeat connection) heartbeat)
     (setf frame-max (connection-frame-max connection))
     (amqp::send-tune-ok connection :channel-max channel-max :frame-max frame-max :heartbeat heartbeat))))


(def-amqp-command amqp:tune-ok (class &key channel-max frame-max heartbeat)
  (:documentation "C->S : negotiate connection tuning parameters")
  
  (:request
   (:method ((connection amqp:connection) &rest args)
     (apply 'amqp::send-tune-ok connection args))))


(def-amqp-command amqp:unbind (class &key queue exchange routing-key arguments)
  (:documentation "C<->S : ")

  (:request
   (:method ((class amqp:queue) &rest args)
     (apply #'amqp::send-unbind class args)
     (command-loop (class)
       (amqp:unbind-ok ((class amqp:queue))
                       (return-from command-loop)))
     class))
   
  (:response
   (:method ((queue amqp::queue) &rest args)
     (declare (ignore args))
     queue)))

(def-amqp-command amqp:unbind-ok (class &key)
  (:documentation "C<->S : ")

  (:request
   (:method ((queue amqp::queue) &rest args)
     (apply 'amqp::send-unbind-ok queue args)))

  (:response
   (:method ((queue amqp::queue) &rest args)
     (declare (ignore args))
     queue)))


;;;
;;; convenience operators

(defgeneric call-with-consumer (operator channel &key queue consumer-tag no-local no-ack exclusive no-wait arguments)

  (:method (operator (channel amqp:channel) &rest args)
    (declare (dynamic-extent args))

    (apply #'amqp:request-consume channel args)
    (command-loop (channel)
      ;; up to the caller to rtansfer out
      (amqp:deliver ((basic amqp:basic) &rest args)
                    (apply operator basic args)))))

