;;; -*- Mode: lisp; Syntax: ansi-common-lisp; Base: 10; Package: common-lisp-user; -*-

(in-package :common-lisp-user)

;;; This file is the system definition for the 'de.setf.amqp' Common Lisp library.
;;; 'de.setf.amqp' is a native Common Lisp wire-level implementation for the 'Advanced Message Queueing
;;; Protocol'.
;;;
;;; Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved
;;; 'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
;;; of the GNU Affero General Public License as published by the Free Software Foundation.
;;;
;;; 'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY;
;;; without even the ;;; implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;; See the Affero General Public License for more details.
;;;
;;; A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
;;; If not, see the GNU [site](http://www.gnu.org/licenses/).
;;;
;;; This file should reside in the root directory of the `de.setf.amqp` source files.
;;; That, in turn, should be a sibling directory to the `de.setf.utility` library, from which
;;; `pathnames.lisp` adds support for a system-specific logical host.
;;;
;;; In order to load the core system, obtain its required libraries (see below), and load it as
;;;
;;;    (asdf:operate 'asdf:load-op :de.setf.amqp)
;;;
;;; In order to use the library, one must load at least one concrete protocol version. for eaxmple,
;;;
;;;    (asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-9-1)
;;;
;;; Each supported versions are present as an individual sub-directory.
;;;


(unless (find-package :de.setf.utility)
  (load (merge-pathnames (make-pathname :directory '(:relative :up "utility")
                                        :name "pathnames")
                         *load-pathname*)))


(de.setf.utility:set-relative-logical-pathname-translations "AMQP")

;;; for sbcl simple streams should be here:
;;; "SYS:CONTRIB;SB-SIMPLE-STREAMS;SB-SIMPLE-STREAMS.ASD"
#+sbcl
(declaim (sb-ext:muffle-conditions sb-ext:compiler-note))

(asdf:defsystem :de.setf.amqp
  :nicknames (:setf.amqp)
  :description "An AMQP client library"
  :version "20100214-0.3"
  :serial t
  :depends-on (:net.common-lisp.usocket
               :net.common-lisp.closer-mop
               :net.common-lisp.bordeaux-threads
               :de.weitz.cl-ppcre
               :com.b9.puri.ppcre
               :de.setf.utility
               :de.setf.utility.mime
               #+sbcl :sb-simple-streams)
  :components ((:file "package")
               (:file "parameters")
               (:file "utilities")
               (:file "amqp-uri")
               #+(or mcl clozure)
               (:file "extremely-simple-stream")
               (:file "amqp-device")
               (:file "stream")
               (:file "data-wire-coding")
               (:file "states")
               (:file "macros")
               (:file "classes")
               (:file "frames")
               (:file "conditions")
               (:file "processing")
               (:file "commands")
               (:file "device-level")
               (:file "device-stream"))

  :long-description
  "`de.setf.amqp` implements a native Common Lisp client library for the 'Advanced Message Queueing
 Protocol'. The implementation comprises wire-level codecs, implementations
 for the standard protocol objects and methods, a functional interface for message-,
 stream- and object-based i/o, and a device-level simple-stream implementation.

 The library targets the revisions of the published AMQP protocol as of versions
 0.8, 0.9, and 0.10. This means that it should work with respective RabbitMQ,
 Apache ActiveMQ, and Qpid implementations. The implementation architecture
 should also accommodate a control structure appropriate for the prospective
 1.0 version - as least as described in preliminary drafts.
 For each version, a distinct package comprises the object and method
 definitions for protocol entities and codecs as generated from the respective
 specification documents.[1] Each collection is a
 complete projection, which means there is some amount of duplication.
 The package and directory names names follow more-or-less the naming conventions of the
 xml protocol documents[2]:
     ----------------- --------------- ---------------------------------------------
      AMQP-1-1-0-8-0    version 0.8     [amqp0-8.xml, amqp0-8.pdf (2006-06)]
      AMQP-1-1-0-9-0    version 0.9     [amqp0-9.xml, amqp0-9.pdf (2006-12)]
      AMQP-1-1-0-9-1    version 0.9r1   [amqp0-9-1.xml, amqp0-9-1.pdf (2008-11-24)]
      AMQP-1-1-0-10-0   version 0.10    [amqp.0-10.xml, amqp.0-10.pdf (2008-02-20)]

 In order to modify the translation and/or generate new codecs consult the `:de.setf.amqp.tools` component.

 All protocol versions are expressed through a common interface[3] which is specialized for the common
 abstract classes. The initial connection phase determines the correct concrete connection implementation
 to be used to communicate with the broker. Given which the other concrete object and method classes are
 elected from the same package. One determines the version support directly by loading the respective
 version's `.asd` file, which makes its connection class available for negotiation.

 ----------
 [1]: tools/spec.lisp
 [2]: http://www.amqp.org/confluence/display/AMQP/AMQP+Specification
 [3]: documentation/index.html
")




#+(or) ;; to graph the system descrition
(progn
  (asdf:operate 'asdf:load-op :de.setf.utility.asdf)

  (cl-user::encode-system-graph (asdf:find-system :de.setf.amqp) #p"LIBRARY:de;setf;amqp;amqp.dot"
                                ;; :rankdir "TB"
                                :name (format nil "AMQP-~/date::format-iso-time/" (get-universal-time))
                                :properties '(:file (:components nil))))