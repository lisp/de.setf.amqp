;;;-*  Package: rabbitmq; -*-
;; $Id: //info.ravenbrook.com/user/ndl/lisp/cl-rabbit/channel.lisp#2 $

(in-package :rabbitmq)

;;                           CHANNEL.LISP
;;           Nick Levine, Ravenbrook Limited, 2007-09-20
;;           James Anderson, setf.de, 2010-02-04
;; 
;; 1.  INTRODUCTION
;;
;; The purpose of this document is to implement a lisp interface to AMQP channels, consistent with the
;; RabbitMQ API. It emulates the original com.nicklevine.rabbitmq version, which was layered over
;; RabbitMQ/Java
;;
;; See Appendix C below for copyright and license.


;; 2.  OPEN & CLOSE

(defun new-channel (connection)
  (amqp:connection.channel connection :number t))


(defmacro with-alive-channel ((channel &key (if-dead :error)) &body body)
  (rebinding (channel)   
    `(if (channel-alive ,channel)
         (progn ,@body)
       ,@(case if-dead
           ((:error)
            `((progn (channel-not-alive ,channel)
		     ;; prevent tail call, aid debugging
		     nil)))))))

(defun channel-not-alive (channel)
  (error 'channel-not-alive :channel channel))

(define-condition channel-not-alive (error)
  ((channel :reader channel-not-alive-channel :initform nil :initarg :channel))
  (:report (lambda (condition stream)
	     (format stream "Channel~@[ ~a~] is no longer alive"
		     (channel-not-alive-channel condition)))))

(defmacro with-channel ((channel connection) &body body)
  (rebinding (connection)
    `(multiple-value-prog1
         (let ((,channel (new-channel ,connection)))
           (unwind-protect
               (progn ,@body)
             (destroy-channel ,channel)))
       (check-connection-alive ,connection))))

(defun destroy-channel (channel &key (code 0) (message "closed by application"))
  (with-alive-channel (channel :if-dead nil)
    (handler-case (amqp:request-close channel
                                      :reply-code code
                                      :reply-text message)
      (channel-not-alive () ())))
  channel)


;; 3.  CHANNEL SUBCLASS

(defclass channel ()
  ((the-consumer
    :initform nil
    :reader the-consumer :writer setf-the-consumer)))

;;; adjust the amqp:channel class to fit with the jfli-based class
(interpose-superclass 'channel 'amqp:channel)

(defmethod channel-ticket ((channel channel))
  (amqp.utility:channel-ticket channel))

;;; define a consumer and a deilvery

(defclass queueingconsumer. ()
  ((queue :initform (make-instance 'amqp.utility:queue)
          :reader consumer-queue)
   (channel
    :initform nil :initarg :channel
    :accessor consumer-channel))
  (:documentation "The queueing consumer interacts with the channel to accept,
 parse and queue incoming messages"))


(defmethod consumer-next-delivery ((consumer queueingconsumer.))
  (unless (consumer-empty-p consumer)
    (amqp.utility:dequeue (consumer-queue consumer))))


;;;!!! this needs to look through the queue to see if there is any pending i/o
;;;!!! andd , if so, read it and push it through the processing pipeline
;;;!!! once there is nothing wating the count is up-to-date

(defmethod consumer-empty-p ((consumer QueueingConsumer.))
  (and (amqp.utility:collection-empty-p (consumer-queue consumer))
       (let ((count 0))
         (amqp:command-loop ((consumer-channel consumer) :wait nil)
           (amqp:deliver ((class amqp:basic) &key &allow-other-keys)
                         ;; just count, but don't handle
                         (print (incf count))
                         ;; allow the next handler to process and queue the result
                         nil))
         (zerop count))))
               

(defmethod consumer-empty-p ((consumer null))
  t)

(defmethod consumer-arrived-count ((consumer QueueingConsumer.))
  (if (consumer-empty-p consumer)
    0
    (amqp.utility:collection-size (consumer-queue consumer))))

(defmethod consumer-arrived-count ((consumer null))
  0)


(defclass queueingconsumer$delivery (incoming-message)
  ()
  (:documentation "The delivery class holds the delivered payload."))



(defmethod Channel.basicConsume ((channel AMQP-1-1-0-8-0:channel) ticket queue no-ack consumer)
  (check-type consumer queueingconsumer.)
  (amqp:request-consume (amqp:basic channel) :ticket ticket :no-ack no-ack :queue queue)
  (setf (de.setf.amqp.implementation::channel-command channel 'amqp:deliver)
        #'(lambda (channel class method &rest args)
            (let* ((body (apply #'amqp:respond-to-deliver class args))
                   (message (make-instance 'queueingconsumer$delivery
                              :body body
                              :content-type (amqp.utility:class-mime-type class)
                              :envelope (apply #'make-envelope
                                               (amqp.utility:method-arguments method))
                              :properties (amqp.utility:class-properties class)))
                   (consumer (the-consumer channel)))
              (when consumer
                (amqp.utility:enqueue message (consumer-queue consumer)))
              message)))
  (setf (consumer-channel consumer) channel)
  (setf-the-consumer consumer channel)
  channel)


(defmethod Channel.basicConsume ((channel AMQP-1-1-0-9-1:channel) (ticket t) queue no-ack consumer)
  (flet ((delivery-handler (channel class method &rest args)
           (print :handler)
           (let* ((body (apply #'amqp:respond-to-deliver class args))
                  (message (make-instance 'queueingconsumer$delivery
                             :body body
                             :content-type (amqp.utility:class-mime-type class)
                             :envelope (apply #'make-envelope
                                              (amqp.utility:method-arguments method))
                             :properties (amqp.utility:class-properties class)))
                  (consumer (the-consumer channel)))
             (amqp:log :error class "delivered: ~s" message)
             (when consumer
               (amqp.utility:enqueue message (consumer-queue consumer)))
             message)))
    (check-type consumer queueingconsumer.)
    (amqp:request-consume (amqp:basic channel) :no-ack no-ack :queue queue)
    (setf (de.setf.amqp.implementation::channel-command channel 'amqp:deliver)
          #'delivery-handler)
    (setf (consumer-channel consumer) channel)
    (setf-the-consumer consumer channel)
    channel))

(defmethod Channel.basicCancel ((channel channel) consumer-tag no-wait)
  (prog1 (amqp:request-cancel (amqp:basic channel) :consumer-tag consumer-tag :no-wait no-wait)
    (setf (slot-value channel 'the-consumer) nil)))


;; 4.  MESSAGES

(defun full-next-message (channel consumer nowait)
  (with-alive-channel (channel)
    (when nowait
      (when (consumer-empty-p consumer)
        (return-from full-next-message
          nil)))
    ;; TBD ?? -- shouldn't there be some waiting to do when nowait is
    ;; false?  I no longer remember my intent, and this function
    ;; doesn't have a mirror in libamq so I can't peek at that -- NDL
    ;; 2007-09-28
    (let ((delivery (consumer-next-delivery consumer))
          (basic (amqp:basic channel)))
      (unless (amqp:basic-no-ack basic)
        (acknowledge-delivery channel delivery))
      delivery)))

(defun next-message (channel)
  (full-next-message channel (the-consumer channel) t))

(defmethod consumer-empty-p ((channel channel))
  (consumer-empty-p (the-consumer channel)))

(defun channel-arrived-count (channel)
  (with-alive-channel (channel)
    (consumer-arrived-count (the-consumer channel))))

(defun channel-arrived-count-or-nil (channel)
  (let ((count (channel-arrived-count channel)))
    (when (plusp count)
      count)))

(defun channel-wait (channel timeout)
  (when (zerop timeout)
    (error "~s called with zerop timeout. If you really wanted that, call ~s instead"
           'channel-wait 'channel-wait-forever))
  (with-alive-channel (channel)
    (let ((consumer (the-consumer channel)))
      (assert consumer ()
              "No consumer present to satisfy wait criteria: ~s." channel)
      (let ((deadline (+ timeout (get-internal-run-time))))
        (loop (when (or (>= (get-internal-run-time) deadline)
                        (not (consumer-empty-p consumer)))
                (return))
              (sleep 0.01))))))

(defun channel-wait-forever (channel)
  (with-alive-channel (channel)
    (let ((consumer (the-consumer channel)))
      (assert consumer ()
              "No consumer present to satisfy wait criteria: ~s." channel)
      (loop (unless (consumer-empty-p consumer)
              (return))
            (sleep 0.01)))))


;; 5.  PROPERTIES

(defun channel-alive (channel)
  (open-stream-p channel))

(defun channel-consumer-count (channel)
  (values 1
          (list (the-consumer channel))))

(defun channel-consumer-tag (channel)
  (amqp:basic-consumer-tag (amqp:basic channel)))


;; 6.  EXCHANGE

(defmacro with-exchange-type ((type) &body body)
  `(let ((,type (ecase ,type
                  ((:fanout) "fanout")           ;; ignores routing-key
                  ((:direct) "direct")           ;; exact match on routing-key
                  ((:topic)  "topic")            ;; pattern matching on routing-key
                  )))
     ,@body))

(defgeneric full-declare-exchange (channel ticket exchange type passive durable auto-delete no-wait arguments)
  (:method ((channel AMQP-1-1-0-9-1:channel) ticket exchange type passive durable auto-delete no-wait arguments)
    (declare (ignore ticket auto-delete))
    (with-alive-channel (channel)
      (amqp:request-declare (amqp:exchange channel)
                            :exchange exchange
                            :type type
                            :passive passive
                            :durable durable
                            :no-wait no-wait
                            :arguments arguments)))
  (:method ((channel AMQP-1-1-0-8-0:channel) ticket exchange type passive durable auto-delete no-wait arguments)
    (declare (ignore no-wait))
    (with-alive-channel (channel)
      (amqp:request-declare (amqp:exchange channel)
                            :ticket ticket
                            :exchange exchange
                            :type type
                            :passive passive
                            :durable durable
                            :auto-delete auto-delete
                            :arguments arguments))))

(defun declare-exchange (channel exchange type)
  (with-exchange-type (type)
    (full-declare-exchange channel (channel-ticket channel) exchange type nil nil nil nil nil)))

(defun full-test-exchange (connection exchange type durable auto-delete arguments)
  (with-channel (channel connection)
    (trapping-not-found
      (full-declare-exchange channel (channel-ticket channel) exchange type t durable auto-delete nil arguments)
      t)))

(defun test-exchange (connection exchange type)
  (with-exchange-type (type)
    (full-test-exchange connection exchange type nil nil nil)))


(defgeneric full-delete-exchange (channel ticket exchange if-unused)
  (:method ((channel AMQP-1-1-0-9-1:channel) ticket exchange if-unused)
    (declare (ignore ticket))
    (with-alive-channel (channel)
      (amqp:request-delete (amqp:exchange channel) :exchange exchange :if-unused if-unused)))
  (:method ((channel AMQP-1-1-0-8-0:channel) ticket exchange if-unused)
    (with-alive-channel (channel)
      (amqp:request-delete (amqp:exchange channel) :ticket ticket :exchange exchange :if-unused if-unused))))

(defun delete-exchange (channel exchange)
  (full-delete-exchange channel (channel-ticket channel) exchange nil))


;; 7.  QUEUE

(defgeneric full-declare-queue (channel ticket queue passive durable exclusive auto-delete arguments)
  (:method ((channel AMQP-1-1-0-9-1:channel) ticket queue passive durable exclusive auto-delete arguments)
    (declare (ignore ticket))
    (with-alive-channel (channel)
      (amqp:request-declare (amqp:queue channel)
                            :queue queue
                            :passive passive
                            :durable durable
                            :exclusive exclusive
                            :auto-delete auto-delete
                            :arguments arguments)))
  (:method ((channel AMQP-1-1-0-8-0:channel) ticket queue passive durable exclusive auto-delete arguments)
    (with-alive-channel (channel)
      (amqp:request-declare (amqp:queue channel)
                            :ticket ticket
                            :queue queue
                            :passive passive
                            :durable durable
                            :exclusive exclusive
                            :auto-delete auto-delete
                            :arguments arguments))))

(defun declare-queue (channel queue)
  (full-declare-queue channel (channel-ticket channel) queue nil nil nil nil nil))

(defun full-test-queue (connection queue durable exclusive auto-delete arguments)
  (with-channel (channel connection)
    (trapping-not-found
      (amqp:request-declare (amqp:queue channel)
                            :ticket (channel-ticket channel)
                            :queue queue
                            :passive t
                            :durable durable
                            :exclusive exclusive
                            :auto-delete auto-delete
                            :arguments arguments)
      t)))

(defun test-queue (connection queue)
  (full-test-queue connection queue nil nil nil nil))

(defun full-delete-queue (channel ticket queue if-unused if-empty)
  (with-alive-channel (channel)
     (amqp:request-delete (amqp:queue channel)
                          :ticket ticket
                          :queue queue
                          :if-unused if-unused
                          :if-empty if-empty)))

(defun delete-queue (channel queue)
  (full-delete-queue channel (channel-ticket channel) queue nil nil))


(defgeneric full-bind-queue (channel ticket queue exchange routing-key arguments)
  (:method ((channel AMQP-1-1-0-9-1:channel) ticket queue exchange routing-key arguments)
    (declare (ignore ticket))
    (with-alive-channel (channel)
      (amqp:request-bind (amqp:queue channel)
                         :queue queue
                         :exchange exchange
                         :routing-key routing-key
                         :arguments arguments)))
  (:method ((channel AMQP-1-1-0-8-0:channel) ticket queue exchange routing-key arguments)
    (with-alive-channel (channel)
      (amqp:request-bind (amqp:queue channel)
                         :ticket ticket
                         :queue queue
                         :exchange exchange
                         :routing-key routing-key
                         :arguments arguments))))

(defun bind-queue (channel queue exchange routing-key)
  (full-bind-queue channel (channel-ticket channel) queue exchange routing-key nil))



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
