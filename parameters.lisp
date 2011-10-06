;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines global parameters for the 'de.setf.amqp' library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


;;;
;;; stream support

(defvar amqp:*standard-port* 5672
  "The port for connections to AMQP brokers/servers.")

(defvar *frame-size* 4096
  "The wire-level frame size. buffer sizes are net frame header and posible trailer.
 NB. this must exceed method argument and class header property lengths, as those cannot be
 split betweeen frames. an unelaborated basic.publish content header is alredy over 140.") 

(defvar amqp:*frame-size-maximum* (* 512 1024)
  "The maximum frame size permitted in connections negotiation.")

(defvar amqp:*default-version* :amqp-1-1-0-9-1
  "The initial version requested upon openening a connection to a broker.")

(defvar amqp.u:*version-headers* ()
  "An a-list which maps version inticator keywords to protocol version header buffers.
 The initial value is nil. Each loaded version sets its own header.")

(defvar amqp.u:*version*
  (load-time-value (format nil "de.setf.amqp-~a" (date:|yyyyMMddTHHmmssZZ| (get-universal-time)))))

(defvar amqp:*timestamp-epoch* (date:|yyyyMMddTHHmmssZZ| "19700101T000000Z"))

(defconstant +string-element-type+ 'character)

(defvar *connection* nil
  "Binds the current connection for specializing protocol operators.
 The global value, nil, reduces an effective operation to the default
 method, which is specialized on the respective class to which the
 command applies.")

(defvar *channel* nil
  "Binds the current channel, whan a channel-relative operation is in-progress.
 The global value, nil, appies outside of the dynamic extent of such
 operators.")

(defvar *connection-timeout* 10
  "Value in seconds to pass as time out duration for connection
 socket operations.")

(defparameter *default-locale* "en_US"
  "The default locale for connection negotiation")

(defparameter *default-mechanism* "PLAIN"
  "The default authentication mechanism for connection negotiation.")

(defparameter *max-channels* 256
  "The upper bounds on the channel count per connection.")

(defparameter *connection-classes* ()
  "Set by amqp:initialize to a list of defined amqp:connection specializations,
 sorted in descending version order.")


(defparameter *decimal-scale-factor* (expt 10 5))
(defparameter *decimal-scale* 5)


(def-mime-type-key :sexp :if-does-not-exist :create)
(def-mime-type (:application :sexp))

