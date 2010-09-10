;;; -*- Package: cl-user; -*-
;;;

(in-package :cl-user)

(:documentation "This file defines the package for AMPQ version 0.8 components of the
 `de.setf.amqp` library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

(defpackage :amqp-1-1-0-8-0
  (:documentation "Comprises the names for entities particular to version
 AMQP-TCP-0.8.0 of the protocol")
  (:use)
  (:export
   :channel
   :object
   :connection
   :frame
   :input-frame
   :output-frame
   :method))

