;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines codec tests for 0.9r1 components of the 'de.setf.amqp' library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License along with 'de.setf.amqp'.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

;;;
;;; test elementary class/method instantiation, classes w/ initargs,
;;; direct encode/decode, and request/response loop-back

(defclass amqp-1-1-0-9-1-loopback-connection (amqp-1-1-0-9-1:connection loopback-connection)
  ())


;;;
;;;

(test:with-test-situation (:define)

  (test:test amqp/ensure-classes+methods/9-1
    (let ((amqp:*class.connection* 'amqp-1-1-0-9-1:connection))
      (flet ((test-methods (object)
               (every #'(lambda (method-name)
                          (typep (amqp:ensure-method object method-name) method-name))
                      (class-method-names object))))
        (with-test-connection (connection)
          (and (let ((channel (amqp:ensure-object connection 'amqp:channel)))
                 (and (typep channel 'amqp:channel)
                      (test-methods channel)
                      (every #'(lambda (class-name)
                                 (let ((object (if (consp class-name)
                                                 (apply #'amqp:ensure-object channel
                                                        (shiftf class-name (first class-name)))
                                                 (amqp:ensure-object channel class-name))))
                                   (and (typep object class-name)
                                        (or (not (typep object 'amqp:object))
                                            (test-methods object)))))
                             '(amqp:basic
                               amqp:body
                               (amqp:exchange :type "direct")
                               amqp:header
                               (amqp:queue :queue "q1")
                               amqp:tx))))
               (amqp:ensure-object connection 'amqp:heartbeat))))))


  (test:test amqp/codec/basic/9-1
    "basic method codec tests:
   [id content-type content-encoding headers delivery-mode priority correlation-id reply-to expiration message-id timestamp type user-id app-id reserved]
   qos [prefetch-size prefetch-count global]
   qos-ok
   consume [reserved-1 queue consumer-tag no-local no-ack exclusive no-wait arguments]
   consume-ok [consumer-tag]
   cancel [consumer-tag no-wait]
   cancel-ok [consumer-tag]
   publish [reserved-1 exchange routing-key mandatory immediate]
   return [reply-code reply-text exchange routing-key]
   deliver [consumer-tag delivery-tag redelivered exchange routing-key]
   get [reserved-1 queue no-ack]
   get-ok [delivery-tag redelivered exchange routing-key message-count]
   get-empty [reserved-1]
   ack [delivery-tag multiple]
   reject [delivery-tag requeue]
   recover-async [requeue]
   recover [requeue]
   recover-ok"
    (let ((amqp:*class.connection* 'amqp-1-1-0-9-1-loopback-connection))
      (test-class-method-codecs 'amqp:basic
                                `((amqp:qos
                                   :prefetch-size 1024 :prefetch-count 2 :global t)
                                  (amqp:qos-ok)
                                  (amqp:consume
                                   :queue "queue"
                                   :consumer-tag "consumed"
                                   :no-local t
                                   :no-ack t
                                   :exclusive t
                                   :no-wait t
                                   :arguments (:arg1 1 :arg2 "two"))
                                  (amqp:consume-ok
                                   :consumer-tag "tag")
                                  (amqp:cancel
                                   :consumer-tag "comsumed" :no-wait t)
                                  (amqp:cancel-ok
                                   :consumer-tag "consumed")
                                  (amqp:publish
                                   :exchange "an exchange"
                                   :routing-key "/routing/key"
                                   :mandatory t
                                   :immediate t)
                                  (amqp:return
                                   :reply-code 1
                                   :reply-text "a reply text"
                                   :exchange "an exchange"
                                   :routing-key "/routing/key")
                                  (amqp:deliver
                                   :consumer-tag "consumed"
                                   :delivery-tag ,(+ 1 (expt 2 40))
                                   :redelivered t
                                   :exchange "an exchange"
                                   :routing-key "routing/key")
                                  (amqp:get
                                   :queue "a queue"
                                   :no-ack t)
                                  (amqp:get-ok
                                   :delivery-tag ,(+ 1 (expt 2 40))
                                   :redelivered t
                                   :exchange "an exchange"
                                   :routing-key "/../.."
                                   :message-count 2)
                                  (amqp:get-empty)
                                  (amqp:ack
                                   :delivery-tag ,(+ 1 (expt 2 40))
                                   :multiple t)
                                  (amqp:reject
                                   :delivery-tag ,(+ 1 (expt 2 40))
                                   :requeue t)
                                  (amqp:recover-async
                                   :requeue t)
                                  (amqp:recover
                                   :requeue t)
                                  (amqp:recover-ok)))))

  (test:test amqp/codec/channel/9-1
    "channel method codec tests
   [id]
   open [reserved-1]
   open-ok [reserved-1]
   flow [active]
   flow-ok [active]
   close [reply-code reply-text class-id method-id]
   close-ok"
    (let ((amqp:*class.connection* 'amqp-1-1-0-9-1-loopback-connection))
      (test-class-method-codecs 'amqp:channel
                                '((amqp:open)
                                  (amqp:open-ok)
                                  (amqp:flow
                                   :active t)
                                  (amqp:flow-ok
                                   :active t)
                                  (amqp:close
                                   :reply-code 123
                                   :reply-text "a text"
                                   :class-id 0
                                   :method-id 0)
                                  (amqp:close-ok)))))
  
  (test:test amqp/codec/connection/9-1
    "connection method codec tests:
   [id]
   start [version-major version-minor server-properties mechanisms locales]
   start-ok [client-properties mechanism response locale]
   secure [challenge]
   secure-ok [response]
   tune [channel-max frame-max heartbeat]
   tune-ok [channel-max frame-max heartbeat]
   open [virtual-host reserved-1 reserved-2]
   open-ok [reserved-1]
   close [reply-code reply-text class-id method-id]
   close-ok"
    (test-class-method-codecs 'amqp-1-1-0-9-1-loopback-connection
                                `((amqp:start
                                   :version-major 0 :version-minor 9
                                   :server-properties (:p1 0 :p2 1)
                                   :mechanisms "one two"
                                   :locales "en_US enGB")
                                  (amqp:start-ok
                                   :client-properties (:p1 0 :p2 1)
                                   :mechanism "one"
                                   :response "security response"
                                   :locale "en_US")
                                  (amqp:secure
                                   :challenge "secure challenge")
                                  (amqp:secure-ok
                                   :response "secure response")
                                  (amqp:tune
                                   :channel-max 2
                                   :frame-max ,*frame-size*
                                   :heartbeat 60)
                                  (amqp:tune-ok
                                   :channel-max 2
                                   :frame-max ,*frame-size*
                                   :heartbeat 60)
                                  (amqp:open
                                   :virtual-host "vhost 1")
                                  (amqp:open-ok)
                                  (amqp:close
                                   :reply-code 505
                                   :reply-text "test failure"
                                   :class-id 0
                                   :method-id 0)
                                  (amqp:close-ok))))
  
  (test:test amqp/codec/exchange/9-1
    "exchange method codec tests:
   [id]
   declare [reserved-1 exchange type passive durable reserved-2 reserved-3 no-wait arguments]
   declare-ok
   delete [reserved-1 exchange if-unused no-wait]
   delete-ok"
    (let ((amqp:*class.connection* 'amqp-1-1-0-9-1-loopback-connection))
      (test-class-method-codecs '(amqp:exchange :type "direct")
                                '((amqp:declare
                                   :exchange "an exchange"
                                   :type "a type"
                                   :passive t
                                   :durable t
                                   :no-wait t
                                   :arguments (:arg1 "value1" :arg2 2))
                                  (amqp:declare-ok)
                                  (amqp:delete
                                   :exchange "an exchange"
                                   :if-unused t
                                   :no-wait t)
                                  (amqp:delete-ok)))))
  
  (test:test amqp/codec/queue/9-1
    "queue method codec tests:
   [id]
   declare [reserved-1 queue passive durable exclusive auto-delete no-wait arguments]
   declare-ok [queue message-count consumer-count]
   bind [reserved-1 queue exchange routing-key no-wait arguments]
   bind-ok
   unbind [reserved-1 queue exchange routing-key arguments]
   unbind-ok
   purge [reserved-1 queue no-wait]
   purge-ok [message-count]
   delete [reserved-1 queue if-unused if-empty no-wait]
   delete-ok [message-count]"
    (let ((amqp:*class.connection* 'amqp-1-1-0-9-1-loopback-connection))
      (test-class-method-codecs 'amqp:queue
                                '((amqp:declare
                                   :queue "a queue"
                                   :passive t
                                   :durable t
                                   :exclusive t
                                   :auto-delete t
                                   :no-wait t
                                   :arguments (:arg1 "value1" :arg2 2))
                                  (amqp:declare-ok
                                   :queue "a queue"
                                   :message-count 1
                                   :consumer-count 1)
                                  (amqp:bind
                                   :queue "a queue"
                                   :exchange "an exchange"
                                   :routing-key "/routing/key"
                                   :no-wait t
                                   :arguments (:arg1 "value1" :arg2 2))
                                  (amqp:bind-ok)
                                  (amqp:unbind
                                   :queue "a queue"
                                   :exchange "an exchange"
                                   :routing-key "/routing/key"
                                   :arguments (:arg1 "value1" :arg2 2))
                                  (amqp:unbind-ok)
                                  (amqp:purge
                                   :queue "a queue"
                                   :no-wait t)
                                  (amqp:purge-ok
                                   :message-count 2)
                                  (amqp:delete
                                   :queue "a queue"
                                   :if-unused t
                                   :if-empty t
                                   :no-wait t)
                                  (amqp:delete-ok
                                   :message-count 0)))))
     
  (test:test amqp/codec/tx/9-1
    "tx method codec tests:
   [id]
   select
   select-ok
   commit
   commit-ok
   rollback
   rollback-ok"
    (let ((amqp:*class.connection* 'amqp-1-1-0-9-1-loopback-connection))
      (test-class-method-codecs 'amqp:tx
                                `((amqp:select)
                                  (amqp:select-ok)
                                  (amqp:commit)
                                  (amqp:commit-ok)
                                  (amqp:rollback)
                                  (amqp:rollback-ok)))))

  )


;;;
;;; loopback tests

(test:with-test-situation (:define)

  ;; simple loopbacks
  (test:test 0-9-1-time-loopback
    (run-loopback :passes 1000 :operations '() :log-level :warn)
    )

  ;; simple declarations
  (test:test 0-9-1-basic/loopback/exchange.declare
    (test-class-method-loopback '((amqp:send-declare-ok amqp:exchange) ; bounce that first
                                  (amqp:request-declare amqp:exchange :exchange "testx")) ; then request
                                '((amqp:declare amqp:exchange :exchange "testx"))
                                :connection-class 'amqp-1-1-0-9-1-loopback-connection))

  (test:test 0-9-1-basic/loopback/queue.declare
    (test-class-method-loopback '((amqp:send-declare-ok amqp:queue :queue "testq" :message-count 0 :consumer-count 0) ; bounce that first
                                  (amqp:request-declare amqp:queue :queue "testq")) ; then request
                                '((amqp:declare amqp:queue :queue "testq"))
                                :connection-class 'amqp-1-1-0-9-1-loopback-connection))

  (test:test 0-9-1-basic/loopback/queue.declare
    (test-class-method-loopback '((amqp:send-declare-ok amqp:queue :queue "testq" :message-count 0 :consumer-count 0) ; bounce that first
                                  (amqp:request-declare amqp:queue :queue "testq")) ; then request
                                '((amqp:declare amqp:queue :queue "testq"))
                                :connection-class 'amqp-1-1-0-9-1-loopback-connection))
  )

(test:with-test-situation (:define)

  (test:test 0-9-1-basic/loopback/general
    "Loopback one send receive for each (class x method) combination"
    (loopback-class-methods '((basic qos ; [prefetch-size prefetch-count global]
                                     qos-ok
                                     consume ; [reserved-1 queue consumer-tag no-local no-ack exclusive no-wait arguments]
                                     consume-ok ; [consumer-tag]
                                     cancel ; [consumer-tag no-wait]
                                     cancel-ok ; [consumer-tag]
                                     publish ; [reserved-1 exchange routing-key mandatory immediate]
                                     return ; [reply-code reply-text exchange routing-key]
                                     deliver ; [consumer-tag delivery-tag redelivered exchange routing-key]
                                     get ; [reserved-1 queue no-ack]
                                     get-ok ; [delivery-tag redelivered exchange routing-key message-count]
                                     get-empty ; [reserved-1]
                                     ack ; [delivery-tag multiple]
                                     reject ; [delivery-tag requeue]
                                     recover-async ; [requeue]
                                     recover ; [requeue]
                                     recover-ok)
                              (exchange declare ; [reserved-1 exchange type passive durable reserved-2 reserved-3 no-wait arguments]
                                     declare-ok
                                     delete ; [reserved-1 exchange if-unused no-wait]
                                     delete-ok)
                              (queue declare ; [reserved-1 queue passive durable exclusive auto-delete no-wait arguments]
                                     declare-ok ; [queue message-count consumer-count]
                                     bind ; [reserved-1 queue exchange routing-key no-wait arguments]
                                     bind-ok
                                     unbind ; [reserved-1 queue exchange routing-key arguments]
                                     unbind-ok
                                     purge ; [reserved-1 queue no-wait]
                                     purge-ok ; [message-count]
                                     delete ; [reserved-1 queue if-unused if-empty no-wait]
                                     delete-ok ; [message-count]
                                     )
                              (tx select
                                  select-ok
                                  commit
                                  commit-ok
                                  rollback
                                  rollback-ok)))
    :predicate integerp)
          

  (test:test 0-9-1-basic/consume.string
    "Create a loopback connection with two channels, loop one to the other. Pass strings."
    (loopback-objects '("test1" "test2")
                      :log-level :error
                      :element-type 'character
                      :content-type mime:text/plain)
    '("test1" "test2"))

  (test:test 0-9-1-basic/consume.object
    "Create a loopback connection with two channels, loop one to the other. Pass objects."
    (every #'(lambda (x) (typep x 'test-object))
           (loopback-objects (list (make-instance 'test-object :number 0 :character #\a)
                                   (make-instance 'test-object :string "a" :vector #(a s d f)))
                             :log-level :error)))

  (test:test 0-9-1-basic/consume.list
    "Create a loopback connection with two channels, loop one to the other.
 Restrict the frame size to just fit header data and stream long strings."
    (let ((*frame-size* 256)
          (lengths '(32 64 128 256)))
      (every #'(lambda (result length)
                 (and (consp result) (= (length result) length) (every #'stringp result)))
             (loopback-objects (mapcar #'(lambda (length &aux (i -1))
                                           (map-into (make-list length)
                                                     #'(lambda () (format nil "~02,'0x" (incf i)))))
                                       lengths)
                               :log-level :error
                               :element-type 'list)
             lengths)))
  
  )