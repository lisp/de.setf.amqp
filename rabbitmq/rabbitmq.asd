;;;-*  Package: cl-user; -*-
;; $Id: //info.ravenbrook.com/user/ndl/lisp/cl-rabbit/rabbitmq.asd#2 $

(in-package :cl-user)

;;                           RABBITMQ.ASD
;;           Nick Levine, Ravenbrook Limited, 2007-09-04
;;           James Anderson, setf.de, 2010-01-10
;; 
;; 1.  INTRODUCTION
;;
;; This document defines the :rabbitmq system.
;;
;; See Appendix C below for copyright and license.


(asdf:defsystem :de.setf.amqp.rabbitmq
  :description "RABBITMQ - interface to RabbitMQ"
  :author "james anderson <james.anderson@setf.de>"
  :depends-on (:de.setf.amqp.AMQP-1-1-0-8-0)
  :serial t
  :components ((:file "pkg")
               (:file "parameters")
               (:file "utilities")
               (:file "rabbitmq")
               (:file "errors")
               (:file "connection")
               (:file "channel")
               (:file "message")
               )
  :long-description
" `de.setf.amqp.rabbitmq` is emulates for Levine's Java-based library,
 [rabbitmq](org.nicklevine.rabbitmq) based on the `de.setf.amqp` library module.")


;; A.  REFERENCES
;; [1] [org.levine.rabbitmq](http://www.nicklevine.org/cl-rabbit/)
;;
;; B.  HISTORY
;;
;; 2007-09-04 NDL Created.
;; 2010-02-04 JAA Emulation / de.setf.amqp.
;;
;;
;; C.  COPYRIGHT
;;
;; Copyright (c) 2007 Wiinz Limited.
;; Copyright (c) 2010 james.anderson@setf.de 
;;
;; The license terms for the original org.levine.rabbitmq package[1]:
;;
;; Permission is hereby granted, free of charge, to any person
;; obtaining a copy of this software and associated documentation
;; files (the "Software"), to deal in the Software without
;; restriction, including without limitation the rights to use, copy,
;; modify, merge, publish, distribute, sublicense, and/or sell copies
;; of the Software, and to permit persons to whom the Software is
;; furnished to do so, subject to the following conditions:
;;
;; The above copyright notice and this permission notice shall be
;; included in all copies or substantial portions of the Software.
;; 
;; THE SOFTWARE IS PROVIDED "AS IS", WITHOUT WARRANTY OF ANY KIND,
;; EXPRESS OR IMPLIED, INCLUDING BUT NOT LIMITED TO THE WARRANTIES OF
;; MERCHANTABILITY, FITNESS FOR A PARTICULAR PURPOSE AND
;; NONINFRINGEMENT.  IN NO EVENT SHALL THE AUTHORS OR COPYRIGHT
;; HOLDERS BE LIABLE FOR ANY CLAIM, DAMAGES OR OTHER LIABILITY,
;; WHETHER IN AN ACTION OF CONTRACT, TORT OR OTHERWISE, ARISING FROM,
;; OUT OF OR IN CONNECTION WITH THE SOFTWARE OR THE USE OR OTHER
;; DEALINGS IN THE SOFTWARE.
;;

;;; The de.setf.amqp.rabbitmq license:
;;;
;;;  This file is part of the `de.setf.amqp.rabbitmq` library module.
;;;  It contains examples for simple interaction with a broker.
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

