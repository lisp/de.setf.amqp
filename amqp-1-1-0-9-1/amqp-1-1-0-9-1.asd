;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user) 

;;; This file is the system definition for the AMQP version 0.9r1 elements of the 'de.setf.amqp'
;;; library.
;;;
;;; Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;; 'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
;;; of the GNU Affero General Public License as published by the Free Software Foundation.
;;;
;;; 'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
;;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the Affero General Public License for more details.
;;;
;;; A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
;;; If not, see the GNU [site](http://www.gnu.org/licenses/).
;;;
;;; In order to use the library, obtain its required libraries (see `../amqp.asd`), and load it as
;;;
;;;    (asdf:operate 'asdf:load-op :de.setf.amqp.AMQP-1-1-0-9-1)
;;;


(asdf:defsystem :de.setf.amqp.AMQP-1-1-0-9-1
  :serial t
  :version 20100111.1
  :depends-on (:de.setf.amqp)
  :components ((:file "package")
               ;; conformant
               ;; (:file "data-wire-coding")
               ;; rabbitmq-like
               (:file "data-wire-coding-rmq")
               (:file "abstract-classes")
               (:file "classes")
               (:file "device-level"))
  :description
  "This is the sub-library for AMQP/TCP 0.9r1.

 For notes about 0.9r1, see the protocol spec[1] and various posts[2]

 -------
 [1]: https://www.amqp.org/confluence/display/AMQP/AMQP+Specification

 [2]: http://dev.rabbitmq.com/wiki/Amqp08To091")



