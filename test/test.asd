;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

(document :file
  (description "This file is the system definition for tests for the `de.setf.amqp` Connon Lisp library.")
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License along with 'de.setf.amqp'.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


(asdf:defsystem :de.setf.amqp.AMQP-1-1-0-8-0
  :serial t
  :version 20100111.1
  :depends-on (:de.setf.amqp.AMQP-1-1-0-8-0
               :de.setf.amqp.AMQP-1-1-0-9-1
               :de.setf.utility.test)
  :components ((:module :utility
                :pathname #P"LIBRARY:;d;setf;utility;"
                (:components (:file "function-monitor")))
               (:file "amqp-uri")
               (:file "test")
               (:module :AMQP-1-1-0-8-0
                        (:file "test"))
               (:module :AMQP-1-1-0-9-1
                        (:file "test"))
               (:file "data-wire-coding")
               (:file "device-level")
               (:file "classes"))

  :description
  "This is the sub-library for testing :de.setf.amqp")



;;; (asdf:operate 'asdf:load-op :de.setf.amqp.AMQP-1-1-0-8-0)


