;;;-*  Package: rabbitmq; -*-

(in-package "RABBITMQ")

;;                            MESSAGE.LISP
;;           Nick Levine, Ravenbrook Limited, 2007-09-21
;;           James Anderson, setf.de, 2010-02-04
;;
;; 1.  INTRODUCTION
;;
;; This document is to implement a class to emulate RabbitMQ messages.
;;
;; See Appendix C below for copyright and license.


;; 2.  MESSAGE

(defclass message ()
  ((body
    :initform nil :initarg :body
    :accessor message-raw-body)
   (properties
    :initform nil :initarg :properties
    :accessor message-properties)
   (content-type
    :initform nil :initarg :content-type
    :accessor message-content-type
    :type (or (member t nil) mime:mime-type)
    :documentation "A MIME-TYPE instance specifies the encoding type.")
   (envelope
    :initform nil :initarg :envelope
    :accessor message-envelope)))


;;;  timestamp epoch shifts -
;;; Instance field/slot values are universal times and buffer accessors perform necessary
;;; epoch shift integral with the access. As auch, decoded property/argument values are
;;; universal times. If any code needs times in UNIX epoch, it shouldn't store or pass the values
;;; through here."

(defun message-timestamp (message)
  (getf (message-properties message) :timestamp))

(defun (setf message-timestamp) (new-value message)
  (setf (getf (message-properties message) :timestamp) new-value))


(defclass outgoing-message (message)
  ((properties
    :initform (new-basic-properties))))

(defclass incoming-message (message)
  ())

(defclass envelope ()
  ((exchange
    :initform nil :initarg :exchange
    :accessor envelope-exchange)
   (routing-key
    :initform nil :initarg :routing-key
    :accessor envelope-routing-key)
   (delivery-tag
    :initform nil :initarg :delivery-tag
    :accessor envelope-delivery-tag)))

(defun make-envelope (&key exchange routing-key delivery-tag &allow-other-keys)
  (make-instance 'envelope :exchange exchange
                 :routing-key routing-key
                 :delivery-tag delivery-tag))

(defun new-basic-properties ()
  `(:delivery-mode 1 :priority 0))

(defun new-message (&key timestamp)
  (let ((message (make-instance 'outgoing-message)))
    (setf (message-timestamp message)
          (case timestamp
            ((t) (get-universal-time))
            ((nil) 0)
            (otherwise timestamp)))
    message))



;; 3.  PROPERTIES
;;
;; TBD (if anyone ever wants them): clusterId and headers

(defun message-body (message)
  (etypecase (message-content-type message)
    (null nil)
    ((eql t) (message-raw-body message))
    (mime:text/* (message-body-string message))
    (mime:application/octet-stream (message-body-data message))))

(defun (setf message-body) (new-value message)
  (typecase new-value
    (null
     (setf (message-content-type message) nil)
     (setf (message-raw-body message) nil))
    (string
     (setf (message-content-type message) mime:text/plain)
     (setf (message-body-string message) new-value))
    (simple-vector
     (setf (message-content-type message) mime:application/octet-stream)
     (setf (message-body-data message) new-value))
    (otherwise
     (setf (message-content-type message) t)
     (setf (message-raw-body message) new-value))))


(defun message-body-size (message)
  (length (message-body message)))


(defmacro def-message-property (accessor property)
  `(progn
     (defmethod ,accessor ((message message))
       (getf (message-properties message) ',property))
     (defmethod (setf ,accessor) (value (message message))
       (setf (getf (message-properties message) ',property) value))
     ',accessor))

;; "The Basic class provides methods that support an industry-standard messaging model."

(def-message-property message-id :message-id)
(def-message-property message-application-id :app-id)
(def-message-property message-content-encoding :content-encoding)
(def-message-property message-correlation-id :correlation-id)
(def-message-property message-delivery-mode :delivery-mode)
(def-message-property message-expiration :expiration)
(def-message-property message-reply-to :reply-to)
(def-message-property message-priority :priority)
(def-message-property message-type :type)
(def-message-property message-user-id :userId)

(defun message-origin (message)
  (format nil "~a/~a"
          (message-reply-to message)
          (message-id message)))


(defun message-delivery-persistent (message)
  (eql (message-delivery-mode message) 2))

(defun (setf message-delivery-persistent) (new-value message)
  (setf (message-delivery-mode message)
        (if new-value 2 1))
  new-value)


(defun message-raw-message-content-type (message)
  (let ((type (message-content-type message)))
    (when type (symbol-name (type-of type)))))

(defun (setf message-raw-message-content-type) (type message)
  (setf (message-content-type message)
        (etypecase type
          ((or null (eql t)) type)
          (string (mime:mime-type type)))))



(defun message-exchange (message)
  (envelope-exchange (message-envelope message)))

(defun message-routing-key (message)
  (envelope-routing-key (message-envelope message)))

(defun message-delivery-tag (message)
  (envelope-delivery-tag (message-envelope message)))


;; 4.  METHODS

(defgeneric message-body-string (message)
  (:method ((message string)) message)
  (:method ((message vector))
    (map 'string #'code-char message))
  (:method ((message message))
    (message-body-string (message-raw-body message))))

(defmethod (setf message-body-string) (new-value (self outgoing-message))
  (setf (message-raw-body self)
        (map 'vector #'char-code new-value))
  new-value)

(defun message-body-data (message &key (element-type t))
  (let* ((body (or (message-raw-body message)
                   (return-from message-body-data
                     nil)))
         (data (make-array (length body) :element-type element-type)))
    (typecase data
      (simple-string (map-into data #'(lambda (x) (code-char x)) body))
      (t (replace data body)))
    data))

(defmethod (setf message-body-data) (new-value (self outgoing-message))
  (let* ((length (length new-value)))
    (setf (message-raw-body self)
          (typecase new-value
            (simple-vector (make-array length :element-type '(unsigned-byte 8)
                                       :initial-contents new-value))
            (simple-string (map 'vector #'char-code new-value))
            (t (map 'vector #'(lambda (x) (assert (typep x '(unsigned-byte 8))) x)
                    new-value)))))
  new-value)

(defun message-first-byte (message)
  (let ((raw-body (message-raw-body message)))
    (when raw-body
      (aref raw-body 0))))


(defun full-publish (message channel ticket exchange routing-key mandatory immediate)
  (declare (ignore ticket))
  (amqp:request-publish (amqp:basic channel)
                        :exchange exchange
                        :routing-key routing-key
                        :mandatory mandatory
                        :immediate immediate
                        :body (message-body message)))

(defun publish (message channel exchange routing-key)
  (full-publish message channel (channel-ticket channel) exchange routing-key nil nil))

(defun destroy-message (message)
  (declare (ignore message))
  nil)

(defun full-consume-queue (channel ticket queue consumer-tag no-local no-ack exclusive)
  (declare (ignore ticket))
  (amqp:request-consume (amqp:basic channel)
                        :queue queue
                        :consumer-tag consumer-tag
                        :no-local no-local
                        :no-ack no-ack
                        :exclusive exclusive))


(defun consume-queue (channel queue)
  (with-alive-channel (channel)
    (let ((consumer (make-instance 'queueingconsumer. :channel channel)))
      ;; allow server to generate the consumerTag
      (Channel.basicConsume channel (channel-ticket channel) queue nil consumer))))

(defun full-cancel-queue (channel consumer-tag)
  (amqp:request-cancel channel (amqp:basic channel)
                       :consumer-tag consumer-tag))

(defun cancel-queue (channel &key (consumer-tag (channel-consumer-tag channel)))
  (full-cancel-queue channel consumer-tag))


(defun acknowledge-delivery (channel message)
  (let ((delivery-tag (message-delivery-tag message)))
    (amqp:request-ack (amqp:basic channel)
                      :delivery-tag delivery-tag
                      :multiple nil)))


;; A.  REFERENCES
;; [1] [org.levine.rabbitmq](http://www.nicklevine.org/cl-rabbit/)
;; [2] http://www.rabbitmq.com/releases/rabbitmq-java-client/v1.7.1/rabbitmq-java-client-javadoc-1.7.1/
;;
;; B.  HISTORY
;;
;; 2007-09-21 NDL Created.
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
