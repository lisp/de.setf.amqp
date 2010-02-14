;;; -*- Package: DE.SETF.AMQP.IMPLEMENTATION; -*-

(in-package "DE.SETF.AMQP.IMPLEMENTATION")

(document :file
  (description "This file defines buffer accessors for AMPQ version 0.9r1 components of the  `de.setf.amqp`
 library.")
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

;; nb. the spec has several type errors:
;; channel-max, frame-max and heartbeat are type short, which means
;; that the short, long, and longlong types are intended to be unsigned. for example,
;; qpid specifies #xffff for channel max, which would be a problem were it signed.

(def-encodings (:amqp-1-1-0-9-1)
  ;;amqp-0-9-1.pdf w/ clauses reordered for typecase type precedence
  (bit                amqp:bit)           ; promoted due to typecase
  (boolean            boolean              :line-code #\t)
  (short-short-uint   (unsigned-byte 8)    :line-code #\B)
  (short-short-int    (signed-byte 8)      :line-code #\b)
  (short-uint         (unsigned-byte 16)   :line-code #\u)
  (short-int          (signed-byte 16)     :line-code #\U)
  (long-uint          (unsigned-byte 32)   :line-code #\i)
  (long-int           (signed-byte 32)     :line-code #\I)
  (long-long-uint     (unsigned-byte 64)   :line-code #\l)
  (long-long-int      (signed-byte 64)     :line-code #\L)
  (float              short-float          :line-code #\f)
  (double             double-float         :line-code #\d)
  (decimal-value      amqp:decimal         :line-code #\D)
  (short-string       (amqp:string 8)      :line-code #\s)
  (long-string        (amqp:string 32)     :line-code #\S)
  (field-array        amqp:list            :line-code #\A  :codec amqp-1-1-0-9-1::buffer-list-codec)
  (timestamp          (unsigned-byte 64)   :line-code #\T)
  (field-table        amqp:table           :line-code #\F  :codec amqp-1-1-0-9-1::buffer-table-codec)
  ;; amqp-xml-doc0-9-1.pdf,p8 
  ;; ... pertains to argument types, which are not table fields
  (class-id           (unsigned-byte 16))
  (consumer-tag       (amqp:string 8))
  (delivery-tag       (unsigned-byte 64))
  (exchange-name      (amqp:string 8))
  (long               (unsigned-byte 32))
  (longlong           (unsigned-byte 64))
  (longstr            (amqp:string 32))
  (message-count      (unsigned-byte 32))
  (method-id          (unsigned-byte 16))
  (no-ack             amqp:bit)
  (no-local           amqp:bit)
  (no-wait            amqp:bit)
  (octet              (unsigned-byte 8))
  (path               (amqp:string 8))
  (peer-properties    amqp:table           :codec amqp-1-1-0-9-1::buffer-table-codec)
  (queue-name         (amqp:string 8))
  (redelivered        amqp:bit)
  (reply-code         (unsigned-byte 16))
  (reply-text         (amqp:string 8))
  (short              (unsigned-byte 16))
  (shortstr           (amqp:string 8))
  (table              amqp:table           :codec amqp-1-1-0-9-1::buffer-table-codec)
  (nil                nil                  :line-code #\V)
  )

