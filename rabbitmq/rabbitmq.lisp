;;; -*- Package: jfli; -*-

(in-package :jfli)

;;                            RABBITMQ.LISP
;;           Nick Levine, Ravenbrook Limited, 2007-09-03
;;           James Anderson, setf.de, 2010-02-04
;; 
;; 1.  INTRODUCTION
;;
;; The purpose of this document is to support initial library configuration
;;
;; See Appendix C below for copyright and license.

(defvar *rabbitmq-initialized* nil)

(defun initialize-rabbitmq (&rest args &key frame-size (timeout rabbitmq::*rabbitmq-timeout*))
  "Configure the AMQP interface in terms of
 :FRAME-SIZE : integer : the default frame size to negotiate with brokers
 :TIMEOUT : integer : duration, in seconds, for protocol time-outs."

  (declare (ignore frame-size))
  (unless *rabbitmq-initialized*
    (apply #'amqp:initialize
           :timeout timeout
           args)
    (setf *rabbitmq-initialized* t)))


;; A.  REFERENCES
;; [1] [org.levine.rabbitmq](http://www.nicklevine.org/cl-rabbit/)
;;
;; B.  HISTORY
;;
;; 2007-09-03 NDL Created.
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
