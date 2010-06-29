;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)


(:documentation  "This file implements connection-related tests for streams based on AMQP connections for the
 'de.setf.amqp' library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License along with 'de.setf.amqp'.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


(:documentation (connection/open) "Test setup:

 testing w/ qpid
 installed 0.5 (/Development/Applications/AMQP/qpid-0.5/ )
 set up ssh
 # cd /Development/Applications/AMQP/qpid-0.5/etc
 # create-example-ssl-stores.sh
 set up environment variables
 # export QPID_HOME=/Development/Applications/AMQP/qpid-0.5/
 # export PATH=$PATH:/Development/Applications/AMQP/qpid-0.5/bin
 # /Development/Applications/AMQP/qpid-0.5/bin/qpid-server


 The first tests were against a QPID-0.5 broker (:AMQP-1-1-0-9). It really does
 respond to the acceptable version token with the intial Connection.Start message.
 If it doesn't like the sent data it does one of several things:
 - a malfored respone, eg. a (spurious) cr/lf, cause it to try again with the version token
 - a misframed response caused it to close the connection
 - an unacceptable, but well-formed response, eg a bogus authentication mechanism, caused
   it to respond with a close operation.")


(test:test connection/open
  "This will open a connection, and close it."
  (let ((connection nil))
    (unwind-protect
      (progn (setf connection
                   (make-instance 'amqp:connection
                     :uri "amqp://guest:guest@192.168.1.25/"))
             (values (connection-state connection)
                     (connection-uri connection)
                     (amqp:connection-server-properties connection)))
      (when connection (close connection) t))))


#|
The first long tests with qpid failed and left this in the terminal:

Using QPID_CLASSPATH /Development/Downloads/qpid-0.5//lib/qpid-all.jar:/Development/Downloads/qpid-0.5//lib/bdbstore-launch.jar
Info: QPID_JAVA_GC not set. Defaulting to JAVA_GC -XX:+UseConcMarkSweepGC -XX:+HeapDumpOnOutOfMemoryError
Info: QPID_JAVA_MEM not set. Defaulting to JAVA_MEM -Xmx1024m
Using configuration file /Development/Downloads/qpid-0.5/etc/config.xml
Configuring logger using configuration file /Development/Downloads/qpid-0.5/etc/log4j.xml
2010-01-11 01:17:06,168 INFO  [main] management.JMXManagedObjectRegistry (JMXManagedObjectRegistry.java:162) - JMX ConnectorServer using SSL keystore file /Development/Downloads/qpid-0.5/etc/qpid.keystore
2010-01-11 01:17:06,519 WARN  [main] management.JMXManagedObjectRegistry (JMXManagedObjectRegistry.java:187) - Starting JMX ConnectorServer on port '8999' (+9099) with SSL
2010-01-11 01:17:07,512 INFO  [main] server.Main (Main.java:279) - Starting Qpid Broker 0.5 build: exported
2010-01-11 01:17:07,746 INFO  [main] server.Main (Main.java:387) - Qpid.AMQP listening on non-SSL address 0.0.0.0/0.0.0.0:5672
2010-01-11 01:17:07,747 INFO  [main] server.Main (Main.java:409) - Qpid Broker Ready :0.5 build: exported
java.lang.OutOfMemoryError: Java heap space
Dumping heap to java_pid688.hprof ...
Heap dump file created [1079483534 bytes in 35.675 secs]
|#

#+mcl
(test:test amqp.device-level.test-live-object.mcl
  (let ((count (* 1)))
    (multiple-value-bind (results time count)
                         (test-live-objects (list "come here watson, ..."
                                                  (let ((i (char-code #\a))) (map-into (make-string 26) #'(lambda () (values (code-char i) (incf i)))))
                                                  (lisp-implementation-type)
                                                  (lisp-implementation-version))
                                            :log-level :error :verbose-p t
                                            :no-ack nil
                                            :element-type 'character
                                            :content-type mime:text/plain
                                            :count count)
      (values
       results
       (float (/ (/ time count) internal-time-units-per-second))
       (float (/ time internal-time-units-per-second))
       count))))

#+clozure
(test:test amqp.device-level.test-live-object.clozure
  (let ((count (* 1)))
    (multiple-value-bind (results time count)
                         (test-live-objects (list "a thing of beauty is a wonder to behold"
                                                  (let ((i (char-code #\0))) (map-into (make-string 10) #'(lambda () (values (code-char i) (incf i)))))
                                                  (lisp-implementation-type)
                                                  (lisp-implementation-version))
                                            :log-level :error :verbose-p t
                                            :no-ack nil
                                            :element-type 'character
                                            :content-type mime:text/plain
                                            :count count)
      (values
       results
       (float (/ (/ time count) internal-time-units-per-second))
       (float (/ time internal-time-units-per-second))
       count))))


;;; sbcl probes
#+(or)
(let ((remote-host "localhost"))
  (etypecase remote-host
    (string (let ((host (sb-bsd-sockets:get-host-by-name remote-host)))
              (setf remote-host (first (sb-bsd-sockets:host-ent-addresses host)))))
    (vector )))
           

(defmethod drain-connection ((data vector) (stream stream) &key (start 0) (end (length data)))
  (multiple-value-bind (null error)
                       (ignore-errors
                        (do ((i start (1+ i))
                             (byte (read-byte stream)
                                   (read-byte stream)))
                            ((or (>= i end) (null byte) (< byte 0))
                             (subseq data 0 i))
                          (format *trace-output* " ~2,'0d" byte)
                          (force-output *trace-output*)
                          (setf (aref data i) byte)))
    (cond ((null null)
           (princ error)
           (values data error))
          (t
           data))))
  
(defun probe-connection (&key (host "127.0.0.1") (port *standard-port*) (repeat 0))
  ;; open, write protocol token, read frame, write static frame, read complete to eof
  (let* ((socket (usocket:socket-connect host  port :element-type 'unsigned-byte))
         (stream (usocket:socket-stream socket))
         (data (make-frame-buffer 1024))
         (token (make-frame-buffer 8))
        (byte 0))
    (unwind-protect
      (progn
        (setf (buffer-protocol-header token) *default-version*)
        (write-sequence token stream)
        (dotimes (i repeat) 
          (write-sequence (map 'vector #'char-code #(#\return #\linefeed))
                          stream))
        (force-output stream)

        ;; read header
        (case (setf byte (read-byte stream))
          ;; the later protocols reply with a version to confirm, but
          ;; the early ones just send the start frame immediately
          (#.(char-code #\A)
           (setf (aref data 0) byte)
           (unless (= 8 (read-sequence token stream :start 1))
             (error "protocol token failed to read."))
           (buffer-protocol-header token))
          (t
           (setf (aref data 0) byte)
           (drain-connection data stream :start 1))))
      (when socket (usocket:socket-close socket)))))
