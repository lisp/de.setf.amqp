;;; -*- Package: common-lisp-user; -*-
;; $Id: //info.ravenbrook.com/user/ndl/lisp/cl-rabbit/pkg.lisp#2 $

(in-package :cl-user)

;;                              PKG.LISP
;;           Nick Levine, Ravenbrook Limited, 2007-09-04
;;           James Anderson, setf.de, 2010-02-04
;; 
;; 1.  INTRODUCTION
;;
;; The purpose of this document is to define the RABBITMQ package.
;;
;; See Appendix C below for copyright and license.


(pushnew :rabbitmq *features*)

(eval-when (:compile-toplevel :load-toplevel :execute)
  (unless (find-package :jfli)
    (defpackage :jfli
      (:use :common-lisp)
      (:intern
       :initialize-rabbitmq))))


(defpackage :rabbitmq
  #+lw   (:add-use-defaults t)
  #-lw   (:use :common-lisp)
  (:import-from :jfli
                :initialize-rabbitmq)
  (:export
   ;; Connection
   :new-connection                  ; Open new connection with plain login as guest/guest, e.g. (new-connection "localhost" "/")
   :destroy-connection              ; Clean shut-down preferred. This will destroy any outstanding sessions
   ;; Connection properties
   :connection-alive                ; (boolean) - nil when connection has had an error
   :connection-server-product       ; (string) - product name reported by server
   :connection-server-version       ; (string) - product version reported by server
   :connection-server-platform      ; (string) - operating system platform reported by server
   :connection-server-copyright     ; (string) - copyright notice reported by server
   :connection-server-info          ; (string) - other information reported by server
   ;;
   ;; Channel (aka session)
   :new-channel                     ; Creates a new channel on this connection. Create as many as you like
   :destroy-channel                 ; Clean shut-down for this channel
   :next-message                    ; Returns next incoming message, or nil if there weren't any
   :channel-arrived-count           ; How many incoming messages are there?
   :channel-wait                    ; Wait for server content, timeout given in ms (non-zero). Return value is either nil or number of messages which have arrived
   :channel-wait-forever            ; Wait for server content as above, no timeout
   ;;
   ;; Channel properties
   :channel-alive                   ; (boolean) - nil when channel has had an error
   ;;
   ;; Channel properties set by server, in response to various methods
   :channel-consumer-count          ; (integer) number of consumers
   :channel-consumer-tag            ; (integer as string) consumer tag
   ;;
   ;; Exchange
   :declare-exchange                ; Creates an exchange with the given name, if none such already exists, and with the given type (:fanout / :direct / :topic)
   :delete-exchange                 ; Deletes exchange with given name
   :test-exchange                   ; Tests for existence of named exchange
   ;;
   ;; Queue
   :declare-queue                   ; Creates a queue with the given name, if none such already exists. Note that the auto-delete flag will be set.
   :delete-queue                    ; Deletes queue with given name
   :bind-queue                      ; Binds named queue to exchange and routing key
   :test-queue                      ; Tests for existence of named queue
   ;;
   ;; Message processing
   :consume-queue                   ; Start a queue consumer
   :cancel-queue                    ; End a queue consumer
   :destroy-message                 ; Destroy a message (no-op for RabbitMQ)
   :publish                         ; Publish a message
   ;;
   ;; Message
   :new-message                     ; Creates a new message. It'll need a body-string and an ID before you publish it
   :message-body                    ; [setfable] Payload of message, as vector (see below), string (ditto) or null. Reads / sets message-content-type. 
   :message-body-data               ; [setfable] Payload of message as SIGNED bytes. Reader takes :element-type argument, default is t (i.e. simple-vector)
   :message-body-string             ; [setfable] Payload of message, as a simple-base-string. ** NO NULLS! JNI WILL TRUNCATE!! **
   :message-body-size               ; (integer) Size of content
   :message-first-byte              ; (byte) Peek, without having to build full sequence
   :message-id                      ; [setfable] (string) Message identifier. In the examnples these are short unique strings. Is uniqueness required? I don't _think_ so.
   :message-exchange                ; (string) Exchange to which content was published (incoming messages only)
   :message-routing-key             ; (string) Original routing key specified by publisher (incoming messages only)
   :message-application-id          ; [optionally setfable] (string) ID of creating application
   :message-content-encoding        ; [optionally setfable] (string) MIME content encoding
   :message-content-type            ; [optionally setfable] MIME content type; both :string and :octets are currently supported
   :message-correlation-id          ; [optionally setfable] (string) Application correlation identifier
   :message-expiration              ; [optionally setfable] (string) Expiration specification
   :message-reply-to                ; [optionally setfable] (string) Destination to reply to
   :message-type                    ; [optionally setfable] (string) Message type name
   :message-user-id                 ; [optionally setfable] (string) ID of creating user
   :message-delivery-persistent     ; [setfable] (boolean) does message persist should server be restarted?
   :message-priority                ; [setfable] (integer) in the range 0...9 - it doesn't say anywhere which end of the scale gets there first
   :message-timestamp               ; [setfable] (float) univeral-time with 3 decimal places
   :message-origin                  ; Defined as (format nil "~a/~a" message-reply-to message-id)
   ;;
   ;; Conditions
   :amqp-exception
   :connection-not-alive            ; Signalled if a connection is found not to be alive
   :connection-not-alive-connection ; Reader: the connection which was not alive (if known)
   :channel-not-alive               ; Signalled if a channel is found not to be alive
   :channel-not-alive-channel       ; Reader: the channel which was not alive (if known)
   :ignore-not-found-errors
   ))


;; A.  REFERENCES
;; [1] [org.levine.rabbitmq](http://www.nicklevine.org/cl-rabbit/)
;;
;; B.  HISTORY
;;
;; 2007-09-04 NDL Created.
;; 2010-01-10 JAA Emulation / de.setf.amqp.
;;
;;
;; C.  COPYRIGHT
;;
;; Copyright (c) 2007 Wiinz Limited. 
;; Copyright (c) 2010 james.anderson@setf.de 
;;
;; See `rabbitmq.asd` for the license terms for the original org.levine.rabbitmq package.

;;;  This file is part of the `de.setf.amqp.rabbitmq` library module.
;;;  (c) 2010 [james anderson](mailto:james.anderson@setf.de)
;;;
;;;  `de.setf.amqp.rabbitmq` is free software: you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation as version 3 of the License.
;;;
;;;  `de.setf.amqp.rabbitmq` is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with `de.setf.amqp.rabbitmq`. If not, see the GNU [site](http://www.gnu.org/licenses/).
