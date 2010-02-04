;;;-*  Package: rabbitmq; -*-

(in-package :rabbitmq)

;;                           EXAMPLES.LISP
;;           Nick Levine, Ravenbrook Limited, 2007-09-04
;;           James Anderson, setf.de, 2010-02-04

;; 1.  INTRODUCTION
;;
;; The purpose of this document demonstrate the RABBITMQ package.
;;
;; See Appendix C below for copyright and license.

;; This transcript paraphrases Levine's original README, with results as per emulation.
;; It opens a connection and a channel, declares exchange and queue, binds them and
;; loops a message back to itself.

(defparameter *my-connection* nil)
(defparameter *my-channel* nil)
(defparameter *outgoing-message* nil)
(defparameter *incoming-message* nil)

(setq *my-connection* (new-connection "localhost" "/"))
;; #<AMQP-1-1-0-9-1:CLIENT-CONNECTION #x278956FE>

(setq *my-channel* (new-channel *my-connection*))
;; #<CHANNEL [#<URI amqp://localhost:5672/>].1 #x278C33FE>


(declare-exchange *my-channel* "my exchange" :direct)
;; #<AMQP-1-1-0-9-1:EXCHANGE #x27A3C2FE>

(declare-queue *my-channel* "my queue")
;; #<AMQP-1-1-0-9-1:QUEUE #x27C8E346>

(bind-queue *my-channel* "my queue" "my exchange" "my routing key")
;; #<AMQP-1-1-0-9-1:QUEUE #x27C8E346>


;;; - Send a message into the void:
(setq *outgoing-message* (new-message))
;; #<OUTGOING-MESSAGE #x27CBAFEE>


(setf (message-id *outgoing-message*) "42"
      (message-body *outgoing-message*) "Hello, World")
;; "Hello, World"


(publish *outgoing-message* *my-channel* "my exchange" "my routing key")
;; "Hello, World"


;;; - And get it back again:
(consume-queue *my-channel* "my queue")
;; #<CHANNEL [#<URI amqp://localhost:5672/>].1 #x2B5691F6>

(channel-arrived-count *my-channel*)
;; 1

(setq *incoming-message* (next-message *my-channel*))
;; #<RABBITMQ::QUEUEINGCONSUMER$DELIVERY #x2B7F9486>

(values (message-body *incoming-message*)
        (message-id *incoming-message*))
;; "Hello, World"
;; ""

(close *my-connection* :abort t)


;; A.  REFERENCES
;; [1] [org.levine.rabbitmq](http://www.nicklevine.org/cl-rabbit/)
;;
;; B.  HISTORY
;;
;; 2007-09-21 NDL Created.
;; 2009-02-04 james.anderson@setf.de portability
;;
;;
;; C.  COPYRIGHT
;;
;; Copyright (c) 2007 Wiinz Limited. 
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

;;;
;;;  This file is part of the `de.setf.amqp.rabbitmq` library module.
;;;  It contains examples for simple interaction with a broker.
;;;  (c) 2010 [james anderson](mailto:james.anderson@setf.de)
;;;
;;;  `de.setf.amqp.rabbitmq` is free software: you can redistribute it and/or modify
;;;  it under the terms of the GNU General Public License as published by
;;;  the Free Software Foundation, as version 3 of the License.
;;;
;;;  `de.setf.amqp.rabbitmq` is distributed in the hope that it will be useful,
;;;  but WITHOUT ANY WARRANTY; without even the implied warranty of
;;;  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
;;;  GNU General Public License for more details.
;;;
;;;  You should have received a copy of the GNU General Public License
;;;  along with `de.setf.amqp.rabbitmq`. If not, see the GNU [site](http://www.gnu.org/licenses/).


;;;  2010-02-03 [janderson](james.anderson@setf.de)
