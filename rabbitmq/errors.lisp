;;;-*  Package: rabbitmq; -*-
;; $Id: //info.ravenbrook.com/user/ndl/lisp/cl-rabbit/errors.lisp#2 $

(in-package :rabbitmq)

;;                            ERRORS.LISP
;;           Nick Levine, Ravenbrook Limited, 2007-09-21
;;           James Anderson, setf.de, 2010-02-04
;; 
;; 1.  INTRODUCTION
;;
;; This document defines a RabbitMQ protocol class for errors and intergrates it into the de.setf.amqp
;; error class.
;;
;; See Appendix C below for copyright and license.

;;; define a protocol exception class for this interface and
;;; modify the base definition to reflect it.

(define-condition amqp-exception (#+jfli java-exception
                                  #-jfli simple-error)
  ())

;;; adjust the error class to fit with the jfli-based class
(interpose-superclass 'amqp-exception 'amqp:error)


(defun call-ignoring-not-found (operator)
  "Call the operator and suppress amqp not found exceptions.
 Serves as the functional implementation for IGNORE-NOT-FOUND-ERRORS."
  (declare (dynamic-extent operator))
  (handler-bind ((amqp:not-found-error
                  (lambda (e)
                    (return-from call-ignoring-not-found
                      (values nil e)))))
    (funcall operator)))

(defmacro ignore-not-found-errors (&body body)
  "Normal control flow returns the value(s) from the body.
 Iff an AMQP exception is signaled with a not-found error code,
 the error is ignored and the form returns two values, NIL and the
 signaled exception."
  (let ((operator (gensym (string 'ignore-not-found))))
    `(flet ((,operator () ,@body))
       (declare (dynamic-extent #',operator))
       (call-ignoring-not-found #',operator))))

(defmacro trapping-not-found (&body body)
  `(ignore-not-found-errors ,@body))

;; A.  REFERENCES
;; [1] [org.levine.rabbitmq](http://www.nicklevine.org/cl-rabbit/)
;; [2] http://www.rabbitmq.com/releases/rabbitmq-java-client/v1.7.1/rabbitmq-java-client-javadoc-1.7.1/
;;
;; B.  HISTORY
;;
;; 2007-09-21 NDL Created.
;; 2010-02-04 JAA Emulation / de.setf.amqp.
;;  factored out jfli dependency for portability
;;  trapping-not-found -> ignore-not-found-errors
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
