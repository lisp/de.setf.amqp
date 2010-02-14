;;; -*- Package: :cl-user; -*-

(in-package :cl-user)

(asdf:defsystem :de.setf.amqp.tools
  :serial t
  :depends-on (:de.setf.amqp
               :de.setf.xml)
  :components ((:file "spec")))

