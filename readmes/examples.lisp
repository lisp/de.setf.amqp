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

;;; These have been sporadically tests with
;;;
;;; - mcl, ccl, and sbcl
;;; - qpid
;;;
;;;    # install 0.5 (/Development/Downloads/qpid-0.5/ )
;;;    # set up environment variables
;;;    export QPID_HOME=/Development/Downloads/qpid-0.5/
;;;    export PATH=$PATH:/Development/Downloads/qpid-0.5/bin
;;;    # the first time through, only
;;;    cd /Development/Downloads/qpid-0.5/etc
;;;    create-example-ssl-stores.sh
;;;    # run the server
;;;    /Development/Downloads/qpid-0.5/bin/qpid-server"))
;;; - rabbitmq : see [README-rabbitmq.md](examples/README-rabbitmq.md)

;;; to observe the protocol exchange
;;; (setq *log-level* :debug)

(defparameter *c* (make-instance 'amqp:connection :uri "amqp://guest:guest@localhost/"))

(defparameter *ch1* (amqp:channel *c* :uri (uri "amqp:///")))
(defparameter *ch1.basic* (amqp:basic *ch1* :delivery-mode 1))
(defparameter *ch1.ex* (amqp:exchange *ch1*  :exchange "spocq.store" :type "direct"))
(defparameter *ch1.q*  (amqp:queue *ch1* :queue "spocq.store"))
;; don't redeclare if it exists (amqp:request-declare *ch1.ex*)
(amqp:request-declare *ch1.q*)
(amqp:request-bind *ch1.q* :exchange *ch1.ex* :queue *ch1.q* :routing-key "foaf")

(defparameter *ch2* (amqp:channel *c* :uri (uri "amqp:///")))
(defparameter *ch2.basic* (amqp:basic *ch2*))
(defparameter *ch2.q*  (amqp:queue *ch2* :queue "spocq.store"))

(list
 (amqp:request-publish *ch1.basic* :exchange *ch1.ex*
                       :body (format nil "a test message")
                       :routing-key "foaf")
 (amqp:request-get *ch2.basic* :queue *ch2.q*)
 (amqp:request-get *ch1.basic* :queue *ch1.q*))

#|
(defun handle-return (channel class method &rest args)
  (declare (dynamic-extent args)
           (ignore channel method))
  (let ((returned (apply #'amqp:respond-to-deliver class
                         :body #'(lambda (stream content-type)
                                   (declare (ignore content-type))
                                   (let ((buffer (make-array 10 :element-type 'character
                                                             :fill-pointer 0 :adjustable t)))
                                     (loop (let ((c (read-char stream nil nil)))
                                             (unless c (return buffer))
                                             (vector-push-extend c buffer)))))
                         args)))
    returned))
(setf (de.setf.amqp.implementation::channel-command *ch1* 'amqp:return)
      #'handle-return)
(amqp.u:process-connection-loop *c*)

(amqp:request-get *ch1.basic* :queue *ch1.q*)
(amqp:request-publish *ch1.basic* :exchange *ch1.ex*
                       :body "this is test 0"
                       :routing-key "xxxx"
                       :mandatory t
                       :immediate t)
|#

(close *c* :abort t)


(trace de.setf.amqp.implementation::write-frame)
(setq amqp:*log-level* :debug)
(in-package :amqp-user)

(defparameter *c* (make-instance 'amqp:connection :uri "amqp://guest:guest@localhost/"))

(defparameter *ch1* (amqp:channel *c* :uri (uri "amqp:///")))
(defparameter *ch1.basic* (amqp:basic *ch1* :delivery-mode 1))
(defparameter *ch1.ex* (amqp:exchange *ch1*  :exchange "exexchange" :type "direct"))
(defparameter *ch1.q*  (amqp:queue *ch1* :queue "exqueue"))
(amqp:request-declare *ch1.ex*)
(amqp:request-declare *ch1.q*)
(amqp:request-bind *ch1.q* :exchange *ch1.ex* :queue *ch1.q* :routing-key "foaf")

(defparameter *ch2* (amqp:channel *c* :uri (uri "amqp:///")))
(defparameter *ch2.basic* (amqp:basic *ch2*))
(defparameter *ch2.q*  (amqp:queue *ch2* :queue "exqueue"))

(amqp:request-publish *ch1.basic* :exchange *ch1.ex*
                       :body (format nil "this is ~a" (gensym "test #"))
                       :routing-key "testing")
