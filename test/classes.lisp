;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines model tests for the 'de.setf.amqp' library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License along with 'de.setf.amqp'.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


(test:test amqp/connection/instantiate
  (let ((amqp:*class.connection* 'amqp-1-1-0-9-1:connection))
    (with-test-connection (c)
      (typep c amqp:*class.connection*))))


             

(test:test amqp/connection/socket/1
  (let ((socket nil))
    (unwind-protect
      (ignore-errors
       (setf socket
             (usocket:socket-connect "127.0.0.1" *standard-port*
                                     :element-type 'unsigned-byte))
       t)
      (when socket (usocket:socket-close socket)))))

(test:test amqp/connection/socket/2
  (let ((socket nil))
    (unwind-protect
      (ignore-errors
       (setf socket
             (usocket:socket-connect #(127 0 0 1) *standard-port*
                                     :element-type 'unsigned-byte))
       t)
      (when socket (usocket:socket-close socket)))))

(test:test amqp/connection/socket/2
  (let ((socket nil))
    (unwind-protect
      (ignore-errors
       (setf socket
             (usocket:socket-connect "localhost" *standard-port*
                                     :element-type 'unsigned-byte))
       t)
      (when socket (usocket:socket-close socket)))))


(test:test amqp/connection/channel/connect
  (with-test-connection (connection 'amqp-1-1-0-9-1:connection)
    (with-test-channel (channel connection :number 1)
      (and (eq (channel-connection channel) connection)
           (typep (device-state channel) 'amqp.s:use-channel)
           (claim-input-frame channel)
           (claim-output-frame channel)))))


(:documentation amqp/connection/open
  "Some notes about first connections.

 The first tests were against a QPID-0.5 broker (:AMQP-1-1-0-9). It really does
 respond to the acceptable version token with the intial Connection.Start message.
 If it doesn't like the sent data it does one of several things:
 - a malformed respone, eg. a (spurious) cr/lf, caused it to try again with the version token
 - a misframed response caused it to close the connection
 - an unacceptable, but well-formed response, eg a bogus authentication mechanism, caused
   it to respond with a close operation.")


(test:test amqp/connection/open
  "Open a connection, and close it."
  (let ((connection nil))
    (unwind-protect
      (progn (setf connection
                   (make-instance 'amqp:connection :uri "amqp://guest:guest@127.0.0.1/"))
             (amqp:log :debug connection "state: ~a, uri: ~a, properties: ~s"
                       (connection-state connection)
                       (connection-uri connection)
                       (amqp:connection-server-properties connection))
             (assert-device-state connection amqp.s:use-connection)
             t)
      (when connection (close connection)))))


#+(or)
(progn
  ;; simple autonomous operations to try things by hand
  ;; see "AMQP:test;test.lisp" for the implementation as the test operation loopback-objects

(defparameter *c* (make-instance 'amqp:connection
                    :uri "amqp://guest:guest@192.168.1.25/"))

(defparameter *ch1* (make-instance 'amqp:channel
                      :connection *c*
                      :uri (uri "amqp:///?exchange=e1&queue=q1")))

(defparameter *ch2* (make-instance 'amqp:channel
                      :connection *c*
                      :direction :input
                      :uri (uri "amqp:///?queue=q1")))

(amqp:request-publish *ch1* :exchange "e1")

(let ((connection nil)
      (channel1 nil)
      (channel2 nil))
  (unwind-protect
    (progn (setf connection
                 (make-instance 'amqp:client-connection
                   :uri "amqp://guest:guest@192.168.1.25/"))
           (print (list :connection connection))
           (setf channel1
                 (amqp:channel connection :uri (amqp-uri "amqp:///?exchange=e1&queue=q1")))
           (print (list :channel1 channel1))
           (setf channel2
                 (amqp:channel connection :direction :input :uri (amqp-uri "amqp:///?queue=q1")))

           (print (list :channel2 channel2))
           (with-open-stream (stream (amqp:request-publish channel1 :exchange "e1"))
             (dotimes (x 32) (format stream "line: ~d~%" x)))
           (with-open-stream (stream (amqp:request-get channel2 :queue "q1"))
             (loop (let ((line (read-line stream nil nil)))
                     (if line (print line) (return))))))
    (when channel1 (close channel1))
    (when channel2 (close channel2))
    (when connection (close connection)))))
;; ! trying to declare a queue on channel 0 led to : "channel error [error code 504: channel error]"


;;; for reference
;;; negotiate by-hand : this reaches into the middle of device-open and is likely
;;; out-of-date wrt the current logic

#+(or )
(flet ((make-unnegotiated-connection (&key (host "127.0.0.1") (port *standard-port*)
                                           (version *default-version*))
         (let* ((connection (make-instance (amqp:find-protocol-class 'amqp:connection version)
                              :direction :probe))
                (open-socket (usocket:socket-connect host port :element-type 'unsigned-byte)))
           (with-slots (input-handle output-handle socket state external-format protocol-version
                                     frame-max)
                       connection
                   (unless (slot-boundp connection 'protocol-version)
                     (setf protocol-version version))
                   (setf state nil
                         input-handle (usocket:socket-stream open-socket)
                         output-handle (usocket:socket-stream open-socket)
                         socket open-socket))
                 connection)))

  ;; (test:test open-connection/simple
  ;;  "just open a connection and verify the version"
  (let ((c nil))
    (unwind-protect (progn (setf c (make-unnegotiated-connection))
                           (open-connection c :version :amqp-1-1-0-9-1)
                           )
      (when c (close c nil)))))
