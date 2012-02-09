;;; -*- Package: de.setf.amqp.user; -*-

(in-package :de.setf.amqp.user)

;;;  This file demonstrates examples of use of the 'de.setf.amqp' library.
;;;
;;;  Copyright 2010 [james anderson](mailto:james.anderson@setf.de
;;;  'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
;;;  of the GNU Affero General Public License as published by the Free Software Foundation.
;;;
;;;  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
;;;  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
;;;  See the Affero General Public License for more details.
;;;
;;;  You should have received a copy of the GNU Affero General Public License along with 'de.setf.amqp'.
;;;  If not, see the GNU [site](http://www.gnu.org/licenses/).

;;;  If you received this together with an MC image, place that image in a directory together with the MCL
;;;  [kernel](ftp://ftp.clozure.com/pub/MCL/MCL-5.2-Final3.dmg) and double-click it. 


(defun publish-get-loop (publish-channel get-channel data count
                                         &key (queue "q1") (exchange "ex")
                                         (routing-key "/")
                                         ;; specify :debug to observe the protocol exchange
                                         ((:log-level *log-level*) *log-level*))
  (let* ((publish-basic (amqp:basic publish-channel))
         (get-basic (amqp:basic get-channel))
         (exchange (amqp:exchange publish-channel :exchange exchange :type "direct"))
         (publish-queue (amqp:queue publish-channel :queue queue))
         (get-queue (amqp:queue get-channel :queue queue)))
    
    (amqp:request-declare publish-queue)
    (amqp:request-declare exchange)
    (amqp:bind publish-queue :exchange exchange :queue publish-queue :routing-key routing-key)

    (dotimes (i count)
      (dolist (datum data)
        (amqp:request-publish publish-basic :exchange (amqp:exchange-exchange exchange) :body datum :routing-key routing-key)
        (print (amqp:request-get get-basic :queue get-queue))))))
      

(defparameter *c* (make-instance 'amqp:connection :uri "amqp://guest:guest@localhost/"))
(defparameter *ch1* (amqp:channel *c* :uri (puri:uri "amqp:/")))
(defparameter *ch2* (amqp:channel *c* :uri (puri:uri "amqp:/")))

(publish-get-loop *ch1* *ch2* '("this is a test") 1)

(close *c* :abort t)

;;; (time (publish-get-loop *ch1* *ch2* '("a") 10000))

;;; os x, g5-2.5g, qpid-0.5
;;; sbcl : 52 seconds,  154 MB
;;; mcl  : 120 seconds, 11 MB
;;; ccl  : 52 seconds,  17.8 MB

;;; rabbitmq
;;; first w/o r8.0 classes
;;; (setq *log-level* :debug)
;;; [20100215T155724Z00] DEBUG #<CONNECTION #x4F1C566>: open-connection: requesting version: #(65 77 81 80 1 1 0 9)/:AMQP-1-1-0-9-1.
;;; [20100215T155724Z00] DEBUG #<CONNECTION #x4F1C566>: open-connection: [ok=1], updated class to: AMQP-1-1-0-9-1:CONNECTION.
;;; even thoug an r8 broker, it accepts the connection. ?
;;; (publish-get-loop *ch1* *ch2* '("a") 1 :log-level :debug)


;;; (defparameter *c* (make-instance 'amqp:connection :uri "amqp://guest:guest@ec2-175-41-174-180.ap-southeast-1.compute.amazonaws.com/"))

