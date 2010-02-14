;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(document :file
 (description "This file defines global parameters for the 'de.setf.amqp' library.")
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

(defvar amqp:*standard-port* 5672)

(defvar *frame-size* 4096
  "The wire-level frame size. buffer sizes are net frame header and posible trailer.
 NB. this must exceed method argument and class header property lengths, as those cannot be
 split betweeen frames. an unelaborated basic.publish content header is alredy over 140.") 

(defvar amqp:*frame-size-maximum* (* 512 1024))

(defvar amqp:*default-version* :amqp-1-1-0-9-1)

(defvar amqp:*timestamp-epoch* (date:|yyyyMMddTHHmmssZZ| "19700101T00000000"))

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

(defparameter *default-locale* "en_US")

(defparameter *default-mechanism* "PLAIN")

(defparameter *max-channels* 256
  "The upper bounds on the channel count per connection.")

(defvar *supported-versions* ())

(defparameter *connection-classes* ()
  "Set by amqp:initialize to a list of defined amqp:connection specializations,
 sorted in descending version order.")


(defparameter *decimal-scale-factor* (expt 10 5))
(defparameter *decimal-scale* 5)


(def-mime-type-key :sexp :if-does-not-exist :create)
(def-mime-type (:application :sexp))

;;;
;;; constants and parameters used for codec operators
;;;
;;; floating point boundary constants
;;; define them where an implementation has not prepared them
;;;
;;; extended from corkill's openmcl addition

#+mcl
(unless (boundp 'double-float-positive-infinity)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defconstant double-float-positive-infinity
      (unwind-protect
        (progn
          (ccl::set-fpu-mode :division-by-zero nil)
          (funcall '/ 0d0))
        (ccl::set-fpu-mode :division-by-zero t)))
    
    (defconstant double-float-negative-infinity
      (unwind-protect
        (progn
          (ccl::set-fpu-mode :division-by-zero nil)
          (funcall '/ -0d0))
        (ccl::set-fpu-mode :division-by-zero t)))))

#+(or mcl (and clozure (not ccl-1.4)))
(unless (boundp 'double-float-nan)
  (defconstant double-float-nan
    (unwind-protect
      (locally (declare (special double-float-positive-infinity double-float-negative-infinity))
        (ccl::set-fpu-mode :invalid nil)
        (funcall '+ double-float-positive-infinity double-float-negative-infinity))
      (ccl::set-fpu-mode :invalid t))))

#+(or mcl clozure)
(unless (boundp 'single-float-positive-infinity)
  (eval-when (:compile-toplevel :load-toplevel :execute)
    (defconstant single-float-positive-infinity
      (unwind-protect
        (progn
          (ccl::set-fpu-mode :division-by-zero nil)
          (funcall '/ 0s0))
        (ccl::set-fpu-mode :division-by-zero t)))
    
    (defconstant single-float-negative-infinity
      (unwind-protect
        (progn
          (ccl::set-fpu-mode :division-by-zero nil)
          (funcall '/ -0s0))
        (ccl::set-fpu-mode :division-by-zero t)))))

#+(or mcl clozure)
(unless (boundp 'single-float-nan)
  (defconstant single-float-nan
    (unwind-protect
      (locally (declare (special single-float-positive-infinity single-float-negative-infinity))
        (ccl::set-fpu-mode :invalid nil)
        (funcall '+ single-float-positive-infinity single-float-negative-infinity))
      (ccl::set-fpu-mode :invalid t))))

#+sbcl  ;; does this work?
(unless (boundp 'single-float-nam)
  (defconstant single-float-nan
    (+ single-float-positive-infinity single-float-negative-infinity))
  (defconstant double-float-nan
    (+ double-float-positive-infinity double-float-negative-infinity)))