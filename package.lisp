;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :cl-user)

(de.setf.utility:document :file
  (description "This file defines the packages for the 'de.setf.amqp' library.")
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (long-description "Several packages are used

 - `_` isolates macro symbols
 - `de.setf.amqp` (`amqp`) : exports the names of protocol classes and operators.
 - `de.setf.amqp-state` (`amqps`) : exports the channel and connection state names
 - `de.setf.amqp.utility` (`amqp.u`) : exports operator names which are not part of the protocol
 specifications, but should be available to an application.
 - `de.setf.amqp.implementation` (`amqp.i`) : the package for implementation code."))


(unless (find-package :_)
  (defpackage :_
    (:use)
    (:documentation "An isolated package for symbols in macros.")))

(modpackage :amqp
  (:use)
  (:nicknames :de.setf.amqp)
  (:documentation "")
  (:export
   :*connection-timeout*
   :*default-locale*
   :*default-mechanism*
   :*default-version*
   :*frame-size-maximum*
   :*log-level*
   :*standard-port*
   :*timestamp-epoch*
   :+frame-end+
   :access
   :ack
   :alert
   :amqp-exception 
   :array
   :array-p
   :basic
   :basic-headers
   :basic-no-ack
   :binary
   :binary-1024
   :binary-128
   :binary-16
   :binary-256
   :binary-32
   :binary-40
   :binary-48
   :binary-512
   :binary-64
   :binary-72
   :binary-8
   :bind
   :bind-ok
   :bit
   :body
   :cancel
   :cancel-ok
   :channel
   :channel-error
   :channel-handler
   :channel-handlers
   :channel-limit-reached
   :channel-p
   :channel.connection
   :class-class
   :class-exchange
   :class-headers
   :class-id
   :class-methods
   :class-queue
   :close
   :close-ok
   :cluster
   :cluster
   :command
   :command-case
   :command-handler
   :command-invalid-error
   :command-loop
   :commit
   :commit-ok
   :condition
   :connection
   :connection-client-properties
   :connection-error
   :connection-forced-error
   :connection-p
   :connection-server-properties
   :connection.exchange
   :connection.queue
   :consume
   :consume-ok
   :control
   :decimal
   :decimal-p
   :declare
   :declare-ok
   :def-handler
   :delete
   :delete-ok
   :deliver
   :dtx
   :ensure-method
   :ensure-object
   :error
   :exchange
   :exchange-exchange
   :field
   :file
   :find-protocol-class
   :flow
   :flow-ok
   :frame
   :frame-arguments
   :frame-body-size
   :frame-buffer
   :frame-channel
   :frame-class
   :frame-class-id
   :frame-class-id
   :frame-cycle
   :frame-error
   :frame-instance
   :frame-major-version
   :frame-method-id
   :frame-minor-version
   :frame-payload
   :frame-property-flags
   :frame-property-list
   :frame-protocol
   :frame-size
   :frame-type
   :frame-weight
   :get
   :get-empty
   :get-ok
   :handler-bind
   :handler-case
   :handler-ecase
   :header
   :heartbeat
   :initialize
   :input-frame
   :internal-error
   :invalid-path-error 
   :iso-8859-character
   :iso-8859-character-p
   :link
   :list
   :list-p
   :locale
   :log
   :log*
   :long
   :long-long
   :longstr
   :make-channel
   :make-connection
   :mechanism
   :message
   :method
   :method-id
   :method-name
   :not-allowed-error 
   :not-implemented-error 
   :object
   :oob-body
   :oob-header
   :oob-method
   :open
   :open-channel
   :open-connection
   :open-ok
   :output-frame
   :protocol-header
   :publish
   :purge
   :purge-ok
   :qos
   :qos-ok
   :queue
   :queue-queue
   :recover
   :recover-async
   :recover-ok
   :redirect
   :register-handlers
   :reject
   :request
   :request
   :request-ok
   :resource-error 
   :response
   :return
   :rollback
   :rollback-ok
   :secure
   :secure-ok
   :select
   :select-ok
   :send-ack
   :send-cancel
   :send-close
   :send-open
   :session
   :short
   :shortstr
   :stage
   :start
   :start
   :start-ok
   :start-ok
   :stream
   :string
   :string-16-p
   :string-32-p
   :string-8-p
   :struct
   :struct-p
   :syntax-error
   :table
   :table-p
   :test
   :trace
   :tune
   :tune-ok
   :tunnel
   :tx
   :unbind
   :unbind-ok
   :undefined-method-error
   :unexpected-frame-error 
   :use-channel
   :use-connection
   :utf32-character
   :utf32-character-p
   :vbinary
   :version
   :wire-length
   :wire-level-type
   :with-commands
   :CONTENT-TOO-LARGE-ERROR
   :NO-CONSUMERS-ERROR
   :CONNECTION-FORCED-ERROR
   :INVALID-PATH-ERROR
   :ACCESS-REFUSED-ERROR
   :NOT-FOUND-ERROR
   :RESOURCE-LOCKED-ERROR
   :PRECONDITION-FAILED-ERROR
   :FRAME-ERROR
   :SYNTAX-ERROR
   :COMMAND-INVALID-ERROR
   :UNEXPECTED-FRAME-ERROR
   :RESOURCE-ERROR
   :NOT-ALLOWED-ERROR
   :NOT-IMPLEMENTED-ERROR
   :INTERNAL-ERROR
   ))


(modpackage :amqp.s
  (:documentation "The home package for state names")
  (:nicknames :de.setf.amqp-state)
  (:use )
  (:export
   :state
   :channel-state
   :connection-state
   :open
   :open-connection
   :open-channel
   :use
   :use-connection
   :use-channel
   :body
   :use-channel.body
   :method
   :use-channel.method
   :header
   :use-channel.header
   :heartbeat
   :use-channel.heartbeat
   :close
   :close-connection
   :close-channel))


(defpackage :de.setf.amqp.utility
  (:use )
  (:nicknames :amqp.u)
  (:import-from :de.setf.amqp :*log-level*)
  (:export
   *log-level*
   :channel-condition
   :channel-error
   :channel-flow-condition
   :channel-flow-start-condition
   :channel-flow-stop-condition
   :connection-error
   :queue
   :dequeue
   :enqueue
   :collection-empty-p
   :collection-size
   :class-mime-type
   :class-properties
   :channel-ticket
   :channel-realm
   :invalid-state-error
   :method-arguments
))

(defpackage :de.setf.amqp.implementation
  (:nicknames :amqp.i)
  (:use :common-lisp
        :de.setf.utility
        :de.setf.amqp.utility)
  ;; don't depending on load order, this introduces conflict.
  ;; eg, tools->xml->cl-http->quickdraw#line
  ;; #+ccl (:use :ccl)
  #+clozure
  (:import-from :ccl
                :double-float-positive-infinity
                :double-float-negative-infinity
                #+ccl-1.4 :double-float-nan)
  #+sbcl
  (:import-from :sb-ext
                :double-float-positive-infinity
                :double-float-negative-infinity
                :single-float-positive-infinity
                :single-float-negative-infinity)
  #+sbcl
  (:use :sb-simple-streams)
  #+sbcl
  (:import-from :sb-simple-streams      ; need the slots
                :buf-len
                :buffer
                :buffer-ptr
                :buffpos
                :last-char-read-size
                :max-out-pos
                :out-buffer
                :outpos
                :pending)
  (:import-from :amqp
                :*standard-port*
                :*default-version*
                :*frame-size-maximum*
                :*connection-timeout*
                :*default-locale*
                :*default-mechanism*
                :*log-level*
                :frame-buffer
                :command-loop
                :command-case)
  #+(or mcl ccl)
  (:import-from :ccl
                #:open-stream-p
                #:stream-clear-input
                #:stream-clear-output
                #:stream-direction
                #:stream-element-type
                #:stream-finish-output
                #:stream-force-output
                #:stream-fresh-line
                #:stream-listen
                #:stream-read-byte
                #:stream-write-byte
                #:stream-write-string
                )
  #+clozure
  (:import-from :ccl
                #:stream-write-char
                #:stream-unread-char
                #:stream-read-char
                #:stream-read-char-no-hang
                #:stream-terpri
                )
  #+mcl
  (:import-from :ccl
                #:stream-close
                #:stream-read-sequence
                #:stream-untyi
                #:stream-tyi
                #:stream-tyo
                #:stream-write-sequence
                )
  #+sbcl
  (:import-from :sb-gray
                #:open-stream-p
                #:stream-clear-input
                #:stream-clear-output
                #:stream-element-type
                #:stream-finish-output
                #:stream-force-output
                #:stream-fresh-line
                #:stream-listen
                #:stream-read-byte
                #:stream-read-char
                #:stream-read-char-no-hang
                #:stream-read-sequence
                #:stream-terpri
                ;; #:stream-tyi
                ;; #:stream-tyo
                #:stream-unread-char
                ;; #:stream-untyi
                #:stream-write-byte
                #:stream-write-char
                #:stream-write-sequence
                #:stream-write-string
                ))


(defpackage :de.setf.amqp.user
  (:nicknames :amqp-user)
  (:use :common-lisp :puri :de.setf.amqp.utility)
  #+mcl (:use :ccl)
  #+mcl (:shadowing-import-from :de.setf.amqp.utility
                                :connection-error)
  #+clozure (:use :ccl)
  #+sbcl (:use :sb-alien :sb-debug :sb-ext :sb-gray :sb-profile))