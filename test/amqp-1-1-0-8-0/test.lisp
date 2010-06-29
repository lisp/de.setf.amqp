;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines codec tests for 0.8r0 components of the 'de.setf.amqp' library."
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

(defclass amqp-1-1-0-8-0-loopback-connection (amqp-1-1-0-8-0:connection loopback-connection)
  ()
  (:documentation "A loopback connection specialized for 0.8r0 conneciton parameters."))


(test:with-test-situation (:define)

  (test:test amqp/codec/basic/8-0
    "class: basic [id content-type content-encoding headers delivery-mode priority correlation-id reply-to expiration message-id timestamp type user-id app-id cluster-id]
      qos [prefetch-size prefetch-count global]
      qos-ok
      consume [ticket queue consumer-tag no-local no-ack exclusive no-wait]
      consume-ok [consumer-tag]
      cancel [consumer-tag no-wait]
      cancel-ok [consumer-tag]
      publish [ticket exchange routing-key mandatory immediate]
      return [reply-code reply-text exchange routing-key]
      deliver [consumer-tag delivery-tag redelivered exchange routing-key]
      get [ticket queue no-ack]
      get-ok [delivery-tag redelivered exchange routing-key message-count]
      get-empty [cluster-id]
      ack [delivery-tag multiple]
      reject [delivery-tag requeue]
      recover [requeue]k"
    (let ((amqp:*class.connection* 'amqp-1-1-0-8-0-loopback-connection))
      (test-class-method-codecs 'amqp:basic
                                `((amqp:qos
                                   :prefetch-size 1024 :prefetch-count 2 :global t)
                                  (amqp:qos-ok)
                                  (amqp:consume
                                   :ticket 0
                                   :queue "queue"
                                   :consumer-tag "consumed"
                                   :no-local t
                                   :no-ack t
                                   :exclusive t
                                   :no-wait t)
                                  (amqp:consume-ok
                                   :consumer-tag "tag")
                                  (amqp:cancel
                                   :consumer-tag "comsumed" :no-wait t)
                                  (amqp:cancel-ok
                                   :consumer-tag "consumed")
                                  (amqp:publish
                                   :ticket 17
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
                                   :ticket 17
                                   :queue "a queue"
                                   :no-ack t)
                                  (amqp:get-ok
                                   :delivery-tag ,(+ 1 (expt 2 40))
                                   :redelivered t
                                   :exchange "an exchange"
                                   :routing-key "/../.."
                                   :message-count 2)
                                  (amqp:get-empty
                                   :cluster-id "a cluster")
                                  (amqp:ack
                                   :delivery-tag ,(+ 1 (expt 2 40))
                                   :multiple t)
                                  (amqp:reject
                                   :delivery-tag ,(+ 1 (expt 2 40))
                                   :requeue t)
                                  (amqp:recover
                                   :requeue t)))))
  
  (test:test amqp/codec/channel/8-0
    "class: channel [id]
      open [out-of-band]
      open-ok
      flow [active]
      flow-ok [active]
      alert [reply-code reply-text details]
      close [reply-code reply-text class-id method-id]
      close-ok"
    (let ((amqp:*class.connection* 'amqp-1-1-0-8-0-loopback-connection))
      (test-class-method-codecs 'amqp:channel
                                `((amqp:open
                                   :out-of-band "unknown string")
                                  (amqp:open-ok)
                                  (amqp:flow
                                   :active t)
                                  (amqp:flow-ok
                                   :active t)
                                  (amqp:alert
                                   :reply-code 123
                                   :reply-text "a text"
                                   :details (:item "some details"))
                                  (amqp:close
                                   :reply-code 123
                                   :reply-text "a text"
                                   :class-id 0
                                   :method-id 0)
                                  (amqp:close-ok)))))
  
  (test:test amqp/codec/connection/8-0
    "class: connection [id]
      start [version-major version-minor server-properties mechanisms locales]
      start-ok [client-properties mechanism response locale]
      secure [challenge]
      secure-ok [response]
      tune [channel-max frame-max heartbeat]
      tune-ok [channel-max frame-max heartbeat]
      open [virtual-host capabilities insist]
      open-ok [known-hosts]
      redirect [host known-hosts]
      close [reply-code reply-text class-id method-id]
      close-ok"
    (test-class-method-codecs 'amqp-1-1-0-8-0-loopback-connection
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
                                 :virtual-host "vhost 1"
                                 :capabilities "unknown"
                                 :insist t)
                                (amqp:open-ok
                                 :known-hosts "a list of hosts")
                                (amqp:redirect
                                 :host "localhost"
                                 :known-hosts "host1 host2")
                                (amqp:close
                                 :reply-code 505
                                 :reply-text "test failure"
                                 :class-id 0
                                 :method-id 0)
                                (amqp:close-ok))))
  
  (test:test amqp/codec/exchange/8-0
    "[id]
    class: exchange [id]
      declare [ticket exchange type passive durable auto-delete internal no-wait arguments]
      declare-ok
      delete [ticket exchange if-unused no-wait]
      delete-ok"
    (let ((amqp:*class.connection* 'amqp-1-1-0-8-0-loopback-connection))
      (test-class-method-codecs 'amqp:exchange
                                `((amqp:declare
                                   :ticket 32
                                   :exchange "an exchange"
                                   :type "a type"
                                   :passive t
                                   :durable t
                                   :auto-delete t
                                   :internal t
                                   :no-wait t
                                   :arguments (:arg1 "value1" :arg2 2))
                                  (amqp:declare-ok)
                                  (amqp:delete
                                   :ticket 33
                                   :exchange "an exchange"
                                   :if-unused t
                                   :no-wait t)
                                  (amqp:delete-ok)))))
  
  (test:test amqp/codec/queue/8-0
    "class: queue [id]
      declare [ticket queue passive durable exclusive auto-delete no-wait arguments]
      declare-ok [queue message-count consumer-count]
      bind [ticket queue exchange routing-key no-wait arguments]
      bind-ok
      purge [ticket queue no-wait]
      purge-ok [message-count]
      delete [ticket queue if-unused if-empty no-wait]
      delete-ok [message-count]"
    (let ((amqp:*class.connection* 'amqp-1-1-0-8-0-loopback-connection))
      (test-class-method-codecs 'amqp:queue
                                `((amqp:declare
                                   :ticket 17
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
                                   :ticket 17
                                   :queue "a queue"
                                   :exchange "an exchange"
                                   :routing-key "/routing/key"
                                   :no-wait t
                                   :arguments (:arg1 "value1" :arg2 2))
                                  (amqp:bind-ok)
                                  (amqp:purge
                                   :ticket 17
                                   :queue "a queue"
                                   :no-wait t)
                                  (amqp:purge-ok
                                   :message-count 2)
                                  (amqp:delete
                                   :ticket 17
                                   :queue "a queue"
                                   :if-unused t
                                   :if-empty t
                                   :no-wait t)
                                  (amqp:delete-ok
                                   :message-count 0)))))

     
  (test:test amqp/codec/tx/8-0
    "class: tx [id]
      select
      select-ok
      commit
      commit-ok
      rollback
      rollback-ok"
    (let ((amqp:*class.connection* 'amqp-1-1-0-8-0-loopback-connection))
      (test-class-method-codecs 'amqp:tx
                                `((amqp:select)
                                  (amqp:select-ok)
                                  (amqp:commit)
                                  (amqp:commit-ok)
                                  (amqp:rollback)
                                  (amqp:rollback-ok)))))
  )