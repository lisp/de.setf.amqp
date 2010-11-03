;;; -*- Package: DE.SETF.AMQP.IMPLEMENTATION; -*-


(in-package :de.setf.amqp.implementation)

(:documentation "This file defines buffer accessors for AMPQ version 0.9r1 components of the  `de.setf.amqp`
 library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

;; nb. nowait renamed to no-wait to match other bits and
;;  later versions

(def-encodings (:amqp-1-1-0-8-0)
  ;;amqp-0-8-0.pdf w/ clauses reordered for typecase type precedence
  (bit                amqp:bit)
  (octet              (unsigned-byte 8))
  (short-int          (unsigned-byte 16)   )
  (long-int           (unsigned-byte 32)   :line-code #\I)
  (long-long-int      (unsigned-byte 64)   )
  (short-string       (amqp:string 8)      ) ;; actually utf
  (long-string        (amqp:string 32)     :line-code #\S)
  (timestamp          (unsigned-byte 64)   :line-code #\T)
  (decimal-value      amqp:decimal         :line-code #\D)
  (field-table        amqp:table           :line-code #\F  :codec amqp-1-1-0-8-0::buffer-table-codec)
  ;; ...  pertains to argument types, which are not table fields
  (access-ticket      (unsigned-byte 16))
  (class-id           (unsigned-byte 16))
  (consumer-tag       (amqp:string 8))
  (delivery-tag       (unsigned-byte 64))
  (exchange-name      (amqp:string 8))
  (long               (unsigned-byte 32))
  (longlong           (unsigned-byte 64))
  (longstr            (amqp:string 32))
  (message-count      (signed-byte 32))
  (method-id          (signed-byte 16))
  (known-hosts        (amqp:string 8))
  (no-ack             amqp:bit)
  (no-local           amqp:bit)
  (no-wait            amqp:bit)
  (path               (amqp:string 8))
  (peer-properties    amqp:table           :codec amqp-1-1-0-8-0::buffer-table-codec)
  (queue-name         (amqp:string 8))
  (redelivered        amqp:bit)
  (reply-code         (unsigned-byte 16))
  (reply-text         (amqp:string 8))
  (short              (unsigned-byte 16))
  (shortstr           (amqp:string 8))
  (table              amqp:table           :codec amqp-1-1-0-8-0::buffer-table-codec)
  )

