;;; -*- Package: de.setf.amqp.implementation; -*-

;;; test amqp-1-1-0-8-0 transport operators

(in-package :de.setf.amqp.implementation)




#+de.setf.utility.test
(test:with-test-situation (:define :mode :terse)
  (test:test 0-8-0-connection/codec
    "class: connection [id]
;;;   start [version-major version-minor server-properties mechanisms locales]
;;;   start-ok [client-properties mechanism response locale]
;;;   secure [challenge]
;;;   secure-ok [response]
;;;   tune [channel-max frame-max heartbeat]
;;;   tune-ok [channel-max frame-max heartbeat]
;;;   open [virtual-host capabilities insist]
;;;   open-ok [known-hosts]
;;;   redirect [host known-hosts]
;;;   close [reply-code reply-text class-id method-id]
;;;   close-ok
"
    (and
     (test-class-method-codec 'amqp-1-1-0-8-0:connection 'amqp-1-1-0-8-0:connection.start
                              :version-major 0 :version-minor 9
                              :server-properties '(:p1 0 :p2 1)
                              :mechanisms "one two"
                              :locales "en_US enGB")
     (test-class-method-codec 'amqp-1-1-0-8-0:connection 'amqp-1-1-0-8-0:connection.start-ok
                              :client-properties '(:p1 0 :p2 1)
                              :mechanism "one"
                              :response "security response"
                              :locale "en_US")
     (test-class-method-codec 'amqp-1-1-0-8-0:connection 'amqp-1-1-0-8-0:connection.secure
                              :challenge "secure challenge")
     (test-class-method-codec 'amqp-1-1-0-8-0:connection 'amqp-1-1-0-8-0:connection.secure-ok
                              :response "secure response")
     (test-class-method-codec 'amqp-1-1-0-8-0:connection 'amqp-1-1-0-8-0:connection.tune
                              :channel-max 2
                              :frame-max *frame-size*
                              :heartbeat 60)
     (test-class-method-codec 'amqp-1-1-0-8-0:connection 'amqp-1-1-0-8-0:connection.tune-ok
                              :channel-max 2
                              :frame-max *frame-size*
                              :heartbeat 60)
     (test-class-method-codec 'amqp-1-1-0-8-0:connection 'amqp-1-1-0-8-0:connection.open
                              :virtual-host "vhost 1"
                              :capabilities "unknown"
                              :insist t)
     (test-class-method-codec 'amqp-1-1-0-8-0:connection 'amqp-1-1-0-8-0:connection.open-ok
                              :known-hosts "a list of hosts")
     (test-class-method-codec 'amqp-1-1-0-8-0:connection 'amqp-1-1-0-8-0:connection.close
                              :reply-code 505
                              :reply-text "test failure"
                              :class-id 0
                              :method-id 0)
     (test-class-method-codec 'amqp-1-1-0-8-0:connection 'amqp-1-1-0-8-0:connection.close-ok)))
  
  (test:test 0-8-0-channel/codec
    "class: channel [id]
;;;   open [out-of-band]
;;;   open-ok
;;;   flow [active]
;;;   flow-ok [active]
;;;   alert [reply-code reply-text details]
;;;   close [reply-code reply-text class-id method-id]
;;;   close-ok"
    (and
     (test-class-method-codec 'amqp-1-1-0-8-0:channel 'amqp-1-1-0-8-0:channel.open
                              :out-of-band "unknown string")
     (test-class-method-codec 'amqp-1-1-0-8-0:channel 'amqp-1-1-0-8-0:channel.open-ok)
     (test-class-method-codec 'amqp-1-1-0-8-0:channel 'amqp-1-1-0-8-0:channel.flow
                              :active t)
     (test-class-method-codec 'amqp-1-1-0-8-0:channel 'amqp-1-1-0-8-0:channel.flow-ok
                              :active t)
     (test-class-method-codec 'amqp-1-1-0-8-0:channel 'amqp-1-1-0-8-0:channel.close
                              :reply-code 123
                              :reply-text "a text"
                              :class-id 0
                              :method-id 0)
     (test-class-method-codec 'amqp-1-1-0-8-0:channel 'amqp-1-1-0-8-0:channel.close-ok)))
  
  (test:test 0-8-0-exchange/codec
    "[id]
;;; class: exchange [id]
;;;   declare [ticket exchange type passive durable auto-delete internal no-wait arguments]
;;;   declare-ok
;;;   delete [ticket exchange if-unused no-wait]
;;;   delete-ok"
    (and
     (test-class-method-codec 'amqp-1-1-0-8-0:exchange 'amqp-1-1-0-8-0:exchange.declare
                              :ticket 32
                              :exchange "an exchange"
                              :type "a type"
                              :passive t
                              :durable t
                              :auto-delete t
                              :internal t
                              :no-wait t
                              :arguments '(:arg1 "value1" :arg2 2))
     (test-class-method-codec 'amqp-1-1-0-8-0:exchange 'amqp-1-1-0-8-0:exchange.declare-ok)
     (test-class-method-codec 'amqp-1-1-0-8-0:exchange 'amqp-1-1-0-8-0:exchange.delete
                              :ticket 33
                              :exchange "an exchange"
                              :if-unused t
                              :no-wait t)
     (test-class-method-codec 'amqp-1-1-0-8-0:exchange 'amqp-1-1-0-8-0:exchange.delete-ok)))
  
  (test:test 0-8-0-queue/codec
    "class: queue [id]
;;;   declare [ticket queue passive durable exclusive auto-delete no-wait arguments]
;;;   declare-ok [queue message-count consumer-count]
;;;   bind [ticket queue exchange routing-key no-wait arguments]
;;;   bind-ok
;;;   purge [ticket queue no-wait]
;;;   purge-ok [message-count]
;;;   delete [ticket queue if-unused if-empty no-wait]
;;;   delete-ok [message-count]"
    (and
     (test-class-method-codec 'amqp-1-1-0-8-0:queue 'amqp-1-1-0-8-0:queue.declare
                              :ticket 17
                              :queue "a queue"
                              :passive t
                              :durable t
                              :exclusive t
                              :auto-delete t
                              :no-wait t
                              :arguments '(:arg1 "value1" :arg2 2))
     (test-class-method-codec 'amqp-1-1-0-8-0:queue 'amqp-1-1-0-8-0:queue.declare-ok
                              :queue "a queue"
                              :message-count 1
                              :consumer-count 1)
     (test-class-method-codec 'amqp-1-1-0-8-0:queue 'amqp-1-1-0-8-0:queue.bind
                              :ticket 17
                              :queue "a queue"
                              :exchange "an exchange"
                              :routing-key "/routing/key"
                              :no-wait t
                              :arguments '(:arg1 "value1" :arg2 2))
     (test-class-method-codec 'amqp-1-1-0-8-0:queue 'amqp-1-1-0-8-0:queue.bind-ok)
     (test-class-method-codec 'amqp-1-1-0-8-0:queue 'amqp-1-1-0-8-0:queue.purge
                              :ticket 17
                              :queue "a queue"
                              :no-wait t)
     (test-class-method-codec 'amqp-1-1-0-8-0:queue 'amqp-1-1-0-8-0:queue.purge-ok
                              :message-count 2)
     (test-class-method-codec 'amqp-1-1-0-8-0:queue 'amqp-1-1-0-8-0:queue.delete
                              :ticket 17
                              :queue "a queue"
                              :if-unused t
                              :if-empty t
                              :no-wait t)
     (test-class-method-codec 'amqp-1-1-0-8-0:queue 'amqp-1-1-0-8-0:queue.delete-ok
                              :message-count 0)))
  
  (test:test 0-8-0-basic/codec
    "class: basic [id content-type content-encoding headers delivery-mode priority correlation-id reply-to expiration message-id timestamp type user-id app-id cluster-id]
;;;   qos [prefetch-size prefetch-count global]
;;;   qos-ok
;;;   consume [ticket queue consumer-tag no-local no-ack exclusive no-wait]
;;;   consume-ok [consumer-tag]
;;;   cancel [consumer-tag no-wait]
;;;   cancel-ok [consumer-tag]
;;;   publish [ticket exchange routing-key mandatory immediate]
;;;   return [reply-code reply-text exchange routing-key]
;;;   deliver [consumer-tag delivery-tag redelivered exchange routing-key]
;;;   get [ticket queue no-ack]
;;;   get-ok [delivery-tag redelivered exchange routing-key message-count]
;;;   get-empty [cluster-id]
;;;   ack [delivery-tag multiple]
;;;   reject [delivery-tag requeue]
;;;   recover [requeue]k"
     (and
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.qos
                               :prefetch-size 1024 :prefetch-count 2 :global t)
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.qos-ok)
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.consume
                               :ticket 0
                               :queue "queue"
                               :consumer-tag "consumed"
                               :no-local t
                               :no-ack t
                               :exclusive t
                               :no-wait t)
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.consume-ok
                               :consumer-tag "tag")
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.cancel
                               :consumer-tag "comsumed" :no-wait t)
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.cancel-ok
                               :consumer-tag "consumed")
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.publish
                               :ticket 17
                               :exchange "an exchange"
                               :routing-key "/routing/key"
                               :mandatory t
                               :immediate t)
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.return
                               :reply-code 1
                               :reply-text "a reply text"
                               :exchange "an exchange"
                               :routing-key "/routing/key")
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.deliver
                               :consumer-tag "consumed"
                               :delivery-tag (+ 1 (expt 2 40))
                               :redelivered t
                               :exchange "an exchange"
                               :routing-key "routing/key")
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.get
                               :ticket 17
                               :queue "a queue"
                               :no-ack t)
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.get-ok
                               :delivery-tag (+ 1 (expt 2 40))
                               :redelivered t
                               :exchange "an exchange"
                               :routing-key "/../.."
                               :message-count 2)
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.get-empty
                               :cluster-id "a cluster")
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.ack
                               :delivery-tag (+ 1 (expt 2 40))
                               :multiple t)
      (test-class-method-codec 'amqp-1-1-0-8-0:basic 'amqp-1-1-0-8-0:basic.reject
                               :delivery-tag (+ 1 (expt 2 40))
                               :requeue t)))

     
  (test:test 0-8-0-tx/codec
    "class: tx [id]
;;;   select
;;;   select-ok
;;;   commit
;;;   commit-ok
;;;   rollback
;;;   rollback-ok"
    (and
     (test-class-method-codec 'amqp-1-1-0-8-0:tx 'amqp-1-1-0-8-0:tx.select)
     (test-class-method-codec 'amqp-1-1-0-8-0:tx 'amqp-1-1-0-8-0:tx.select-ok)
     (test-class-method-codec 'amqp-1-1-0-8-0:tx 'amqp-1-1-0-8-0:tx.commit)
     (test-class-method-codec 'amqp-1-1-0-8-0:tx 'amqp-1-1-0-8-0:tx.commit-ok)
     (test-class-method-codec 'amqp-1-1-0-8-0:tx 'amqp-1-1-0-8-0:tx.rollback)
     (test-class-method-codec 'amqp-1-1-0-8-0:tx 'amqp-1-1-0-8-0:tx.rollback-ok)))
  )