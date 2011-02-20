;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :cl-user)

(:documentation "This file defines the packages for the 'de.setf.amqp' library."
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
  (:documentation "The :de.setf.amqp package is the 'AMQP standard' package. It comprises symbols which name
    classes, conditions, methods, and data types which are declared in the AMQP specifications.
    It is intended as an interface package only.
    It uses no packages, and should not be used, as the specification contains numerous names which
    would conflict with those from :common-lisp. as well as abbreviated terms which are likely to conflict
    with an application's terms.")
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
   :basic-consumer-tag
   :basic-content-encoding
   :basic-content-type
   :basic-correlation-id
   :basic-delivery-mode
   :basic-delivery-tag
   :basic-exchange
   :basic-expiration
   :basic-headers
   :basic-immediate
   :basic-mandatory
   :basic-message-id
   :basic-no-ack
   :basic-no-local
   :basic-no-wait
   :basic-queue
   :basic-redelivered
   :basic-reply-to
   :basic-routing-key
   :basic-timestamp
   :basic-user-id
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
   :binary-1024-p
   :binary-128-p
   :binary-16-p
   :binary-256-p
   :binary-32-p
   :binary-40-p
   :binary-48-p
   :binary-512-p
   :binary-64-p
   :binary-72-p
   :binary-8-p
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
   :channel.exchange
   :channel.queue
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
   :connection.channel
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
   :request-get
   :request-ok
   :request-publish
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
   :with-open-channel
   :with-open-connection
   :content-too-large-error
   :no-consumers-error
   :connection-forced-error
   :invalid-path-error
   :access-refused-error
   :not-found-error
   :resource-locked-error
   :precondition-failed-error
   :frame-error
   :syntax-error
   :command-invalid-error
   :unexpected-frame-error
   :resource-error
   :not-allowed-error
   :not-implemented-error
   :internal-error
   ))


(modpackage :amqp.s
  (:documentation "The home package for state names. These device-level operators use these to constrain
    operations on devices / connections / channels.")
  (:nicknames :de.setf.amqp-state)
  (:use )
  (:export
   :state
   :channel-state
   :chunked
   :connection-state
   :input
   :open
   :open-connection
   :open-channel
   :output
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
  (:use :common-lisp  :puri )
  (:documentation "The :de.setf.amqp.utility package exports names to make library components available to
    applications. It uses no package, but is intended to be used - as is the case with the library implementation
    itself. It cross-exports several terms from the AMQP standard package for this purpose.")
  (:nicknames :amqp.u)
  (:import-from :de.setf.amqp
                :*connection-timeout*
                :*default-locale*
                :*default-mechanism*
                :*default-version*
                :*frame-size-maximum*
                :*log-level*
                :*standard-port*
                :*timestamp-epoch*
                :+frame-end+
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
                :binary-1024-p
                :binary-128-p
                :binary-16-p
                :binary-256-p
                :binary-32-p
                :binary-40-p
                :binary-48-p
                :binary-512-p
                :binary-64-p
                :binary-72-p
                :binary-8-p
                :frame-buffer
                :string-16-p
                :string-32-p
                :string-8-p
                )
  (:import-from :de.setf.utility
                :stream-reader
                :stream-writer
                )
  #+ccl
  (:import-from :ccl
                #:open-stream-p
                #:stream-clear-input
                #:stream-clear-output
                #:stream-direction
                #:stream-element-type
                #:stream-eofp
                #:stream-finish-output
                #:stream-force-output
                #:stream-fresh-line
                #:stream-listen
                #:stream-position
                #:stream-read-byte
                #:stream-write-byte
                #:stream-write-string
                )
  #+clozure
  (:import-from :ccl
                :double-float-positive-infinity
                :double-float-negative-infinity
                #+ccl-1.4 :double-float-nan
                #:stream-advance-to-column
                #:stream-line-column
                #:stream-peek-char
                #:stream-read-char-no-hang
                #:stream-read-char
                #:stream-read-line
                #:stream-start-line-p
                #:stream-terpri
                #:stream-unread-char
                #:stream-write-char
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
                #:stream-advance-to-column
                #:stream-clear-input
                #:stream-clear-output
                #:stream-element-type
                #:stream-file-position
                #:stream-finish-output
                #:stream-force-output
                #:stream-fresh-line
                #:stream-line-column
                #:stream-listen
                #:stream-peek-char
                #:stream-read-byte
                #:stream-read-char
                #:stream-read-char-no-hang
                #:stream-read-line
                #:stream-read-sequence
                #:stream-start-line-p
                #:stream-terpri
                #:stream-unread-char
                #:stream-write-byte
                #:stream-write-char
                #:stream-write-sequence
                #:stream-write-string
                )
  #+sbcl
  (:import-from :sb-ext
                :double-float-positive-infinity
                :double-float-negative-infinity
                :single-float-positive-infinity
                :single-float-negative-infinity)
  #+sbcl
  (:import-from :sb-simple-streams
                :device-close
                :device-file-position
                :device-open
                :device-read
                :device-write
                :simple-stream
                :stream-plist
                )
  (:export
   :*channel*                           ; variable
   :*channel-command-handler*           ; variable
   :*connection*                        ; variable
   :*connection-timeout*                ; variable
   :*decimal-scale*                     ; variable
   :*decimal-scale-factor*              ; variable
   :*default-locale*                    ; variable
   :*default-mechanism*                 ; variable
   :*default-version*                   ; variable
   :*frame-size*                        ; variable
   :*frame-size-maximum*                ; variable
   :*log-level*                         ; variable
   :*log-levels*                        ; variable
   :*log-stream*                        ; variable
   :*max-channels*                      ; variable
   :*standard-port*
   :*timestamp-epoch*
   :+frame-end+
   :*version-headers*
   :*version*
   :12-byte-header-input-frame
   :12-byte-header-output-frame
   :7-byte-header-input-frame
   :7-byte-header-output-frame
   :8-byte-header-input-frame
   :8-byte-header-output-frame
   :amqp-connection-device              ; class
   :amqp-content-object                 ; class
   :amqp-device                         ; class
   :amqp-socket-device                  ; class
   :basic-channel                       ; function
   ; :basic-exchange                      ; function
   ; :basic-headers                       ; function
   ; :basic-no-ack                        ; function
   :basic-header                        ; function
   :basic.ack                           ; function
   :basic.cancel                        ; function
   :basic.cancel-ok                     ; function
   :basic.consume                       ; function
   :basic.consume-ok                    ; function
   :basic.deliver                       ; function
   :basic.get                           ; function
   :basic.get-empty                     ; function
   :basic.get-ok                        ; function
   :basic.publish                       ; function
   :basic.qos                           ; function
   :basic.qos-ok                        ; function
   :basic.recover                       ; function
   :basic.recover-async                 ; function
   :basic.recover-ok                    ; function
   :basic.reject                        ; function
   :basic.return                        ; function
   :binary-1024-p                       ; function
   :binary-128-p                        ; function
   :binary-16-p                         ; function
   :binary-256-p                        ; function
   :binary-32-p                         ; function
   :binary-40-p                         ; function
   :binary-48-p                         ; function
   :binary-512-p                        ; function
   :binary-64-p                         ; function
   :binary-8-p                          ; function
   :buffer-binary-1024                  ; function
   :buffer-binary-128                   ; function
   :buffer-binary-16                    ; function
   :buffer-binary-256                   ; function
   :buffer-binary-32                    ; function
   :buffer-binary-40                    ; function
   :buffer-binary-48                    ; function
   :buffer-binary-512                   ; function
   :buffer-binary-64                    ; function
   :buffer-binary-72                    ; function
   :buffer-binary-8                     ; function
   :buffer-bit                          ; function
   :buffer-boolean                      ; function
   :buffer-character                    ; function
   :buffer-decimal                      ; function
   :buffer-double-float                 ; function
   :buffer-integer                      ; function
   :buffer-iso-8859-character           ; function
   :buffer-offset                       ; function
   :buffer-property-flags-16            ; function
   :buffer-protocol-header              ; function
   :buffer-protocol-header-version      ; function
   :buffer-short-float                  ; function
   :buffer-signed-byte-16               ; function
   :buffer-signed-byte-32               ; function
   :buffer-signed-byte-64               ; function
   :buffer-signed-byte-8                ; function
   :buffer-string-16                    ; function
   :buffer-string-16-utf8               ; function
   :buffer-string-32                    ; function
   :buffer-string-32-utf8               ; function
   :buffer-string-8                     ; function
   :buffer-string-8-utf8                ; function
   :buffer-timestamp                    ; function
   :buffer-unsigned-byte-16             ; function
   :buffer-unsigned-byte-32             ; function
   :buffer-unsigned-byte-64             ; function
   :buffer-unsigned-byte-8              ; function
   :buffer-utf32-character              ; function
   :buffer-vbinary-16                   ; function
   :buffer-vbinary-32                   ; function
   :buffer-vbinary-8                    ; function
   :call-with-channel-input-stream      ; function
   :call-with-channel-output-stream     ; function
   :call-with-command-handlers          ; function
   :call-with-consumer                  ; function
   :call-with-decoded-arguments         ; function
   :call-with-decoded-properties        ; function
   :call-with-encoded-arguments         ; function
   :call-with-encoded-properties        ; function
   :call-with-open-channel-stream       ; function
   :canonical-element-type              ; function
   :channel-abort                       ; function
   :channel-acknowledge-messages        ; function
   :channel-command-handler             ; function
   :channel-compute-body-size           ; function
   :channel-condition
   :channel-conditions                  ; function
   :channel-connection                  ; function
   :channel-content-object              ; function
   :channel-content-type                ; function
   :channel-error
   :channel-flow-condition
   :channel-flow-start-condition
   :channel-flow-stop-condition
   :channel-limit-reached               ; class
   :channel-name                        ; function
   :channel-number                      ; function
   :channel-realm
   :channel-request-ack                 ; function
   :channel-request-bind                ; function
   :channel-request-cancel              ; function
   :channel-request-close               ; function
   :channel-request-close-ok            ; function
   :channel-request-commit              ; function
   :channel-request-consume             ; function
   :channel-request-declare             ; function
   :channel-request-delete              ; function
   :channel-request-flow                ; function
   :channel-request-get                 ; function
   :channel-request-open                ; function
   :channel-request-publish             ; function
   :channel-request-qos                 ; function
   :channel-request-recover             ; function
   :channel-request-recover-async       ; function
   :channel-request-request             ; function
   :channel-request-rollback            ; function
   :channel-request-secure-ok           ; function
   :channel-request-select              ; function
   :channel-request-start-ok            ; function
   :channel-request-tune-ok             ; function
   :channel-request-unbind              ; function
   :channel-request-unbind-ok           ; function
   :channel-respond-to-alert            ; function
   :channel-respond-to-bind-ok          ; function
   :channel-respond-to-cancel-ok        ; function
   :channel-respond-to-close            ; function
   :channel-respond-to-close-ok         ; function
   :channel-respond-to-commit-ok        ; function
   :channel-respond-to-consume-ok       ; function
   :channel-respond-to-declare-ok       ; function
   :channel-respond-to-delete-ok        ; function
   :channel-respond-to-deliver          ; function
   :channel-respond-to-flow             ; function
   :channel-respond-to-flow-ok          ; function
   :channel-respond-to-get-empty        ; function
   :channel-respond-to-get-ok           ; function
   :channel-respond-to-open-ok          ; function
   :channel-respond-to-qos-ok           ; function
   :channel-respond-to-recover-ok       ; function
   :channel-respond-to-request-ok       ; function
   :channel-respond-to-rollback-ok      ; function
   :channel-respond-to-secure           ; function
   :channel-respond-to-select-ok        ; function
   :channel-respond-to-start            ; function
   :channel-respond-to-tune             ; function
   :channel-respond-to-unbind           ; function
   :channel-respond-to-unbind-ok        ; function
   :channel-state                       ; function
   :channel-thread                      ; function
   :channel-ticket
   :channel-toplevel-loop               ; function
   :channel-track                       ; function
   :channel-uri                         ; function
   :channel.alert                       ; function
   :channel.basic                       ; function
   :channel.body                        ; function
   :channel.close                       ; function
   :channel.close-ok                    ; function
   :channel.exchange                    ; function
   :channel.file                        ; function
   :channel.flow                        ; function
   :channel.flow-ok                     ; function
   :channel.header                      ; function
   :channel.open                        ; function
   :channel.open-ok                     ; function
   :channel.queue                       ; function
   :channel.stream                      ; function
   :channel.tx                          ; function
   ; :class-mime-type
   :class-properties
   :collection                          ; class
   :collection-content                  ; function
   :collection-if-empty                 ; function
   :collection-name                     ; function
   :command-case                        ; function macro
   :command-loop                        ; function macro
   :condition-channel                   ; function
   :condition-connection                ; function
   :condition-frame                     ; function
   :condition-message                   ; function
   :condition-message-arguments         ; function
   :condition-message-string            ; function
   :connected-channel-p                 ; function
   :connection-error
   :connection-frame-max                ; function
   :connection-frame-size               ; function
   :connection-heartbeat                ; function
   :connection-host                     ; function
   :connection-locale                   ; function
   :connection-mechanism                ; function
   :connection-port                     ; function
   :connection-protocol-version         ; function
   :connection-toplevel-loop            ; function
   :connection-uri                      ; function
   :connection-virtual-host             ; function
   :connection.channel                  ; function
   :connection.close                    ; function
   :connection.close-ok                 ; function
   :connection.heartbeat                ; function
   :connection.open                     ; function
   :connection.open-ok                  ; function
   :connection.redirect                 ; function
   :connection.secure                   ; function
   :connection.secure-ok                ; function
   :connection.start                    ; function
   :connection.start-ok                 ; function
   :connection.tune                     ; function
   :connection.tune-ok                  ; function
   :content-body-size                   ; function
   :decode-class-properties             ; function
   :decode-method-arguments             ; function
   :dequeue
   :device-close                        ; function
   :device-file-position                ; function
   :device-flush                        ; function
   :device-listen                       ; function
   :device-open                         ; function
   :device-read                         ; function
   :device-socket                       ; function
   :device-state                        ; function
   :device-uri                          ; function
   :device-write                        ; function
   :double-float-nan                    ; variable
   :double-float-negative-infinity      ; variable
   :double-float-positive-infinity      ; variable
   :dtx-channel                         ; function
   :encode-method                       ; function
   :enqueue
   :error-class-code                    ; function
   :error-method-code                   ; function
   :exchange-channel                    ; function
   :exchange.declare                    ; function
   :exchange.declare-ok                 ; function
   :exchange.delete                     ; function
   :exchange.delete-ok                  ; function
   :file-channel                        ; function
   :file.ack                            ; function
   :file.cancel                         ; function
   :file.cancel-ok                      ; function
   :file.consume                        ; function
   :file.consume-ok                     ; function
   :file.deliver                        ; function
   :file.open                           ; function
   :file.open-ok                        ; function
   :file.publish                        ; function
   :file.qos                            ; function
   :file.qos-ok                         ; function
   :file.reject                         ; function
   :file.return                         ; function
   :file.stage                          ; function
   :frame-buffer                        ; function
   :frame-channel-number                ; function
   :frame-class-code                    ; function
   :frame-class-name                    ; function
   :frame-connection                    ; function
   :frame-cycle                         ; function
   :frame-data                          ; function
   :frame-header                        ; function
   :frame-method-code                   ; function
   :frame-method-name                   ; function
   :frame-size                          ; function
   :frame-track-number                  ; function
   :frame-type                          ; function
   :frame-type-class-name               ; function
   :handling-channel-errors             ; function macro
   :handling-connection-errors          ; function macro
   :invalid-state-error
   :method-arguments
   :process-channel-command             ; function
   :process-channel-loop                ; function
   :process-command                     ; function
   :process-connection-loop             ; function
   :process-frame                       ; function
   :queue
   :queue-channel                       ; function
   :queue.bind                          ; function
   :queue.bind-ok                       ; function
   :queue.declare                       ; function
   :queue.declare-ok                    ; function
   :queue.delete                        ; function
   :queue.delete-ok                     ; function
   :queue.purge                         ; function
   :queue.purge-ok                      ; function
   :queue.unbind                        ; function
   :queue.unbind-ok                     ; function
   :send-header                         ; function
   :send-heartbeat                      ; function
   :send-method                         ; function
   :session-channel                     ; function
   :signed-byte-16                      ; function
   :signed-byte-32                      ; function
   :signed-byte-64                      ; function
   :signed-byte-8                       ; function
   :simple-stream                       ; class
   :single-float-nan                    ; variable
   :single-float-negative-infinity      ; variable
   :single-float-positive-infinity      ; variable
   :stream-advance-to-column            ; function
   :stream-buffer                       ; function
   :stream-channel                      ; function
   :stream-clear-input                  ; function
   :stream-clear-output                 ; function
   :stream-close                        ; function
   :stream-direction                    ; function
   :stream-element-type                 ; function
   :stream-eof-marker                   ; function
   :stream-eofp                         ; function
   :stream-eol-marker                   ; function
   :stream-finish-output                ; function
   :stream-file-position                ; function
   :stream-force-output                 ; function
   :stream-fresh-line                   ; function
   :stream-line-buffer                  ; function
   :stream-line-column                  ; function
   :stream-listen                       ; function
   :stream-peek-char                    ; function
   :stream-plist                        ; function
   :stream-position                     ; function
   :stream-read-byte                    ; function
   :stream-read-char                    ; function
   :stream-read-char-no-hang            ; function
   :stream-read-line                    ; function
   :stream-read-sequence                ; function
   :stream-reader                       ; function
   :stream-start-line-p                 ; function
   :stream-terpri                       ; function
   :stream-tyi                          ; function
   :stream-tyo                          ; function
   :stream-unread-char                  ; function
   :stream-untyi                        ; function
   :stream-write-byte                   ; function
   :stream-write-char                   ; function
   :stream-write-sequence               ; function
   :stream-write-string                 ; function
   :stream-writer                       ; function
   :stream.cancel                       ; function
   :stream.cancel-ok                    ; function
   :stream.consume                      ; function
   :stream.consume-ok                   ; function
   :stream.deliver                      ; function
   :stream.publish                      ; function
   :stream.qos                          ; function
   :stream.qos-ok                       ; function
   :stream.return                       ; function
   :string-16-p                         ; function
   :string-32-p                         ; function
   :string-8-p                          ; function
   :test-connection                     ; function
   :tunnel-connection                   ; function
   :tx-channel                          ; function
   :tx.commit                           ; function
   :tx.commit-ok                        ; function
   :tx.rollback                         ; function
   :tx.rollback-ok                      ; function
   :tx.select                           ; function
   :tx.select-ok                        ; function
   :uri                                 ; function class
   :uri-exchange                        ; function
   :uri-fragment                        ; function
   :uri-host                            ; function
   :uri-password                        ; function
   :uri-path                            ; function
   :uri-plist                           ; function
   :uri-port                            ; function
   :uri-query                           ; function
   :uri-query-parameter                 ; function
   :uri-query-plist                     ; function
   :uri-queue                           ; function
   :uri-scheme                          ; function
   :uri-user                            ; function
   :uri-userinfo                        ; function
   :uri-virtual-host                    ; function
   ))


(defpackage :de.setf.amqp.implementation
  (:nicknames :amqp.i)
  (:documentation "The :de.setf.amqp.implementation package is the de.setf.amqp source code package.
    It uses general packages - :common-lisp and :de.setf.utility, as well as :de.setf.amqp.utility.")
  (:use :common-lisp
        :de.setf.utility
        :de.setf.amqp.utility)
  ;; don't depending on load order, this introduces conflict.
  ;; eg, tools->xml->cl-http->quickdraw#line
  ;; #+ccl (:use :ccl)
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
                :pending
                :stream-output-handle     
                :output-handle
                :stream-input-handle
                :input-handle
                ))


(defpackage :de.setf.amqp.user
  (:nicknames :amqp-user)
  (:documentation "The :amqp-user package is the basic AMQP application development package.
    It uses the necessary AMQP packages as well as thos customary for the respective lisp implementation.")
  (:use :common-lisp :de.setf.amqp.utility)
  #+mcl (:use :ccl)
  #+mcl (:shadowing-import-from :de.setf.amqp.utility
                                :connection-error)
  #+clozure (:use :ccl)
  #+sbcl (:use :sb-alien :sb-debug :sb-ext :sb-gray :sb-profile))


#|
(let ((stream (make-instance 'ccl:fred-window)))
  (with-package-iterator (next :de.setf.amqp.implementation :internal)
    (let ((symbols ())
          (*print-case* :downcase))
      (loop (multiple-value-bind (next-p symbol) (next)
              (unless next-p (return))
              (let ((status `(,@(when (fboundp symbol) '(function))
                              ,@(when (macro-function symbol) '(macro))
                              ,@(when (boundp symbol) '(variable))
                              ,@(when (find-class symbol nil) '(class)))))
                (when status (pushnew (cons symbol status) symbols :key #'first)))))
      (loop for (symbol . bindings) in (sort symbols #'string-lessp :key #'first)
            do (format stream "~&~3T:~a~40T;~{ ~a~}" symbol bindings)))
    (finish-output stream)))
|#