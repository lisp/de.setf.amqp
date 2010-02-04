;;;-*  Package: rabbitmq; -*-
;; $Id: //info.ravenbrook.com/user/ndl/lisp/cl-rabbit/connection.lisp#2 $

(in-package :rabbitmq)

;;                          CONNECTION.LISP
;;           Nick Levine, Ravenbrook Limited, 2007-09-20
;;           James Anderson, setf.de, 2010-02-04
;; 
;; 1.  INTRODUCTION
;;
;; The purpose of this document is to implement a lisp interface to AMQP connections consistent with the
;; RabbitMQ API. It emulates the original com.nicklevine.rabbitmq version, which was layered over
;; RabbitMQ/Java
;;
;; See Appendix C below for copyright and license.


;; 2.  OPEN & CLOSE

(defun new-connection (host vhost &rest args
                            &key (port amqp:*standard-port*)
                            (userinfo "guest:guest")
                            &allow-other-keys)
  (initialize-rabbitmq)
  (apply #'make-instance 'amqp:connection
    :uri (puri:uri :scheme :amqp :host host :port port
                   :userinfo userinfo
                   :path vhost)
    args))


(defmacro with-alive-connection ((connection &key (if-dead :error)) &body body)
  (rebinding (connection)   
    `(if (connection-alive ,connection)
         (progn ,@body)
       ,@(case if-dead
           ((:error)
            `((progn (connection-not-alive ,connection)
		     ;; prevent tail call, aid debugging
		     nil)))))))

(defun new-connection-parameters (vhost)
  (declare (ignore vhost))
  (error "new-connection-parameters: no autonomous properties are implemented."))

(defun connection-not-alive (connection)
  (error 'connection-not-alive :connection connection))

(define-condition connection-not-alive (error)
  ((connection :reader connection-not-alive-connection :initform nil :initarg :connection))
  (:report (lambda (condition stream)
             (format stream "Connection~@[ ~a~] is no longer alive"
                     (connection-not-alive-connection condition)))))

(defun check-connection-alive (connection)
  (with-alive-connection (connection)
    ()))

(defun destroy-connection (connection &key code message)
  (with-alive-connection (connection :if-dead nil)
    (handler-case
      (amqp:request-close connection
                          :reply-code code
                          :reply-test message)
      (connection-not-alive () ())))
  connection)


;; 3.  PROPERTIES

(defun connection-alive (connection)
  (open-stream-p connection))

(defun connection-client-property (connection property)
  (getf (amqp:connection-client-properties connection) property))

(defun connection-server-property (connection property)
  (getf (amqp:connection-server-properties connection) property))

(defun connection-server-product (connection)
  (connection-server-property connection :product))

(defun connection-server-platform (connection)
  (connection-server-property connection :platform))

(defun connection-server-version (connection)
  (connection-server-property connection :version))

(defun connection-server-copyright (connection)
  (connection-server-property connection :copyright))

(defun connection-server-info (connection)
  (connection-server-property connection :information))
    


;; A.  REFERENCES
;; [1] [org.levine.rabbitmq](http://www.nicklevine.org/cl-rabbit/)
;; [2] http://www.rabbitmq.com/releases/rabbitmq-java-client/v1.7.1/rabbitmq-java-client-javadoc-1.7.1/
;;
;; B.  HISTORY
;;
;; 2007-09-20 NDL Created.
;; 2010-02-04 JAA Emulation / de.setf.amqp.
;;
;;
;; C.  COPYRIGHT
;;
;; Copyright (c) 2007 Wiinz Limited. 
;; Copyright (c) 2010 james.anderson@setf.de 
;;
;; See `rabbitmq.asd` for the license terms for the original org.levine.rabbitmq package.

;;;  This file is part of the `de.setf.amqp.rabbitmq` library module.
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
