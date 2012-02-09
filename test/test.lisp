;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines test utilities for the 'de.setf.amqp' library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License along with 'de.setf.amqp'.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


;;;
;;; set up a delegating stream to act as a handle for test conection instances

(defclass binary-trace-stream ( #+ALLEGRO excl::fundamental-binary-output-stream
                                #+clisp gray:fundamental-binary-output-stream
                                #+CMU extensions:fundamental-binary-output-stream
                                #+CormanLisp stream
                                #+LispWorks stream:fundamental-stream
                                #+digitool ccl::output-binary-stream
                                #+clozure-common-lisp ccl:fundamental-binary-output-stream
                                #+sbcl sb-gray:fundamental-binary-output-stream
                                #+scl ext:binary-output-stream)
  ((stream :initform *trace-output* :initarg :stream
           :reader stream-stream))
  #+CormanLisp
  (:default-initargs :element-type '(unsigned-byte 8)))

(defclass binary-source-stream (#+ALLEGRO excl::fundamental-binary-input-stream
                                #+clisp gray:fundamental-binary-input-stream
                                #+CMU extensions:fundamental-binary-input-stream
                                #+CormanLisp stream
                                #+LispWorks stream:fundamental-stream
                                #+digitool ccl::input-binary-stream
                                #+clozure-common-lisp ccl:fundamental-binary-input-stream
                                #+sbcl sb-gray:fundamental-binary-input-stream
                                #+scl ext:binary-input-stream)
  ()
  #+CormanLisp
  (:default-initargs :element-type '(unsigned-byte 8)))

  

(defMethod stream-element-type ((stream binary-trace-stream))
  '(unsigned-byte 8))

(defmethod stream-direction ((stream binary-trace-stream))
  :output)

(defmethod stream-tyo ((stream binary-trace-stream) (datum integer))
  (format (stream-stream stream) " 0x~2,'0x" datum))

(defmethod stream-write-sequence ((vector vector) (stream binary-trace-stream) &key (start 0) (end nil))
  (setf end (or end (length vector)))
  (do ((I start (1+ i)))
      ((>= i end))
    (declare (fixnum i))
    (format (stream-stream stream) " 0x~2,'0x" (aref vector i)))
  (- end start))


(defmethod device-listen  ((stream binary-source-stream))
  ;; always return nil
  ;; this is needed when the connection starts as it tries to flush pending input on a stream
  nil)

(defmethod stream-eofp ((stream binary-source-stream))
  t)

(defmethod stream-tyi ((stream binary-trace-stream))
  nil)


(defparameter *binary-test-input* (make-instance 'binary-source-stream))
(defparameter *binary-trace-output* (make-instance 'binary-trace-stream))


(defmacro with-test-connection ((name &rest initargs) &rest body)
  `(let ((,name (amqp:connection ,@(or initargs `(t))
                  :uri #u"amqp:///"
                  :input-handle *standard-input*
                  :output-handle *binary-trace-output*)))
     ,@body))

(defmacro with-test-channel ((name connection &rest initargs) &rest body)
  `(let ((,name (amqp:connection.channel ,connection
                                         ,@initargs
                                         :number t
                                         :uri #u"amqp:///"
                                         :input-handle *binary-test-input*
                                         :output-handle *binary-trace-output*)))
     ;; as the input handles have been provided to suppress
     ;; real protocol negotiation, need to connect it by-hand
     (connect-channel ,connection ,name)
     ,@body))

(defmacro with-test-channels (bindings &rest body)
  (labels ((wrap-with-binding (bindings)
             (if bindings
               `(with-test-channel ,(first bindings)
                  ,(wrap-with-binding (rest bindings)))
               `(progn ,@body))))
    (wrap-with-binding bindings)))

(defgeneric test-class-method-codecs (object arguments &key verbose-p)
  (:documentation "Test the codec operators for a given object/method combination.

 object : (designator amqp:object) 
 method : (designator amqp:method)

 Resolve the object and method to instances, handling the context requirements at each level. Then encode and
 decode the arguments and test equivalence. Decoded reserved values are ignored.")

  (:method ((spec t) arguments &rest options)
    (let ((class (etypecase spec (cons (first spec)) (symbol spec)))
          (initargs (etypecase spec (cons (rest spec)) (symbol nil))))
      (cond ((subtypep class 'amqp:connection)
             (let ((amqp:*class.connection* class))
               (with-test-connection (connection)
                 (when initargs (apply #'reinitialize-instance connection initargs))
                 (apply #'test-class-method-codecs connection arguments options))))
          ((subtypep class 'amqp:channel)
           (with-test-connection (connection)
             (apply #'test-class-method-codecs (apply #'amqp:ensure-object connection class initargs)
                    arguments options)))
          (t
           (with-test-connection (connection)
             (with-test-channel (channel connection)
               (apply #'test-class-method-codecs (apply #'amqp:ensure-object channel class initargs)
                      arguments options)))))))

  (:method ((object amqp:object) arguments &rest options
            &key (verbose-p (eq test:*test-unit-mode* :verbose)))
    (every #'(lambda (method-name)
               (let ((method-entry (assoc method-name arguments)))
                 (cond (method-entry
                        (apply #'test-class-method-codecs (amqp:ensure-method object method-name)
                               (rest method-entry)
                               options))
                       (t
                        (when verbose-p
                          (format *trace-output* "No test entry for class method ~s." method-name))
                        nil))))
           (class-method-names object)))

  (:method ((method amqp:method) arguments &key (verbose-p (eq test:*test-unit-mode* :verbose)))
    (when (eq verbose-p :break) (break "test codec for method: ~s." method))
    (apply 'call-with-encoded-arguments
           #'(lambda (frame object method)
               (call-with-decoded-arguments #'(lambda (object method &rest decoded-arguments)
                                                (when verbose-p
                                                  (format *trace-output* "~&tcmc: ~a.~a: original ~s~%~7tdecoded ~s"
                                                          (type-of object)
                                                          (type-of method)
                                                          arguments
                                                          decoded-arguments))
                                                (equal (loop for (key value) on  decoded-arguments by #'cddr
                                                                    unless (search "reserved" (string key) :test #'char-equal)
                                                                    nconc (list key value))
                                                              arguments))
                                            object
                                            method
                                            frame))
           (method-object method) method
           arguments)))


;;; support for loopback testing
;;; define a channel which puts output frames direly into input
;;; and arrange for it  to be used by the connection

(defclass loopback-connection (amqp:connection)
  ((frame-map
    :initform nil :initarg :frame-map
    :accessor connection-frame-map)
   (loopback-enabled
    :initform t
    :accessor connection-loopback-enabled))
  (:documentation "A Specialized connection class which simulates broker interaction by mirroring
    each frames back into transformed input frames."))

(defgeneric print-queues (connection stream)
  (:method ((connection amqp:connection) stream)
    (format stream "~& [queues in[ ")
    (flet ((do-frame (frame) (format-frame-header frame stream) (write-char #\space stream))) 
      (map nil #'do-frame (collection-content (device-read-frames connection)))
      (format stream "] out[ ")
      (map nil #'do-frame (collection-content (device-encoded-frames connection)))
      (format stream "]]"))
    (format stream "]")))

(defgeneric loopback-frame (output-frame)
  (:method ((output-frame output-frame))
    (let* ((connection (frame-connection output-frame))
           (input-frame (claim-input-frame connection)))
      (rotatef (slot-value input-frame 'header) (slot-value output-frame 'header))
      (rotatef (slot-value input-frame 'data) (slot-value output-frame 'data))
      (release-frame output-frame)
      input-frame)))

    
(defmethod put-encoded-frame ((connection loopback-connection) output-frame)
  (when (connection-loopback-enabled connection)
    (amqp:log :debug output-frame "loopback output")

    (let ((frame-map (connection-frame-map connection)))
      (cond (frame-map
             (dolist (map-operator frame-map
                                   (amqp:log :warn output-frame "no mapping."))
               (multiple-value-bind (mapped handled)
                                    (funcall map-operator output-frame)
                 (when handled
                   (typecase mapped
                     (null
                      (amqp:log :debug nil "loopback suppressed"))
                     (amqp:frame
                      (amqp:log :debug mapped "loopback as mapped input")
                      (put-read-frame connection mapped))
                     (sequence
                      (dotimes (i (length mapped))
                        (let ((mapped (elt mapped i)))
                          (amqp:log :debug mapped "loopback as mapped input")
                          (put-read-frame connection mapped)))))
                    (return)))))
            (t
             (let ((input-frame (loopback-frame output-frame)))
               (amqp:log :debug input-frame "default loopback as input")
               (put-read-frame connection input-frame)))))

    (flet ((log-queues (stream)
             (print-queues connection stream)))
      (declare (dynamic-extent #'log-queues))
      (log-when :debug #'log-queues))))

;;; this has no effect as the channels pass through the connection queue in their
;;; own specialized method
(defmethod get-read-frame ((connection loopback-connection) &key (wait nil))
  (flet ((frame-matches-connection-p (frame)
           (eql (frame-channel-number frame) 0)))
    (declare (dynamic-extent #'frame-matches-connection-p))
    (or (dequeue (device-read-frames connection)
                 :test #'frame-matches-connection-p
                 :if-empty wait)
      (error "empty loopback queue."))))


;;;
;;; frequent loopback opreators

(defun make-publish-to-deliver-loopback (from-channel to-channel &key
                                         (redelivered nil)
                                         (delivery-tag 0)
                                         (message-count 0))
  (flet ((publish-to-deliver-loopback (frame)
           (when (and (eql (frame-channel-number frame) (channel-number from-channel))
                      (eq (frame-type-class-name frame) 'amqp:method)
                      (eq (frame-method-name frame) 'amqp:publish))
             (let ((to-basic (amqp:basic to-channel))
                   (from-basic (amqp:basic from-channel)))
               (labels ((encode-get-ok (class method &key routing-key exchange &allow-other-keys)
                          (declare (ignore class method))
                          (release-frame frame)
                          (call-with-encoded-arguments #'return-get-ok to-basic 'amqp:get-ok
                                 :delivery-tag (incf delivery-tag)
                                 :redelivered redelivered
                                 :exchange exchange
                                 :routing-key routing-key
                                 :message-count (incf message-count)))
                        (return-get-ok (frame class method)
                          (declare (ignore class method))
                          (return-from publish-to-deliver-loopback
                            (values (loopback-frame frame) t))))
                 (declare (dynamic-extent #'encode-get-ok #'return-get-ok))
                 (call-with-decoded-arguments #'encode-get-ok from-basic 'amqp:publish frame))))))
    #'publish-to-deliver-loopback))

(defun make-content-loopback (from-channel to-channel)
  (flet ((content-loopback (frame)
           (when (and (eql (frame-channel-number frame) (channel-number from-channel))
                      (member (frame-type-class-name frame) '(amqp:header amqp:body)))
             (let ((input-frame (loopback-frame frame)))
               (setf (frame-channel-number input-frame)
                     (channel-number to-channel))
               (values input-frame t)))))
    #'content-loopback))

(defun make-declare-queue-ok-loopback (channel &key
                                       (consumer-count 0)
                                       (message-count 0))
  (flet ((queue-ok-loopback (frame)
           (when (and (eql (frame-channel-number frame) (channel-number channel))
                      (eq (frame-type-class-name frame) 'amqp:method)
                      (eq (frame-method-name frame) 'amqp:declare)
                      (eq (frame-class-name frame) 'amqp:queue))
             (let ((queue (amqp:queue channel))
                   (ok-frame nil))
               (flet ((encode-ok (class method &key queue &allow-other-keys)
                        (declare (ignore class method))
                        (setf ok-frame
                              (encode-method 'amqp:declare-ok (amqp:queue channel)
                                             :queue queue
                                             :message-count (incf message-count)
                                             :consumer-count (incf consumer-count)))))
                 (declare (dynamic-extent #'encode-ok))
                 (call-with-decoded-arguments #'encode-ok queue (amqp:ensure-method queue 'amqp:declare) frame)
                 (release-frame frame)
                 (values (loopback-frame ok-frame) t))))))
    #'queue-ok-loopback))

(defun make-declare-exchange-ok-loopback (channel)
  (flet ((exchange-ok-loopback (frame)
           (when (and (eq (frame-type-class-name frame) 'amqp:method)
                      (eq (frame-method-name frame) 'amqp:declare)
                      (eq (frame-class-name frame) 'amqp:exchange))
             (let* ((exchange (amqp:exchange channel))
                    (ok-frame (encode-method 'amqp:declare-ok exchange)))
                 (release-frame frame)
                 (values (loopback-frame ok-frame) t)))))
    #'exchange-ok-loopback))

(defun make-consume-ok-loopback (channel)
  (flet ((consume-ok-loopback (frame)
           (when (and (eql (frame-channel-number frame) (channel-number channel))
                      (eq (frame-type-class-name frame) 'amqp:method)
                      (eq (frame-method-name frame) 'amqp:consume)
                      (eq (frame-class-name frame) 'amqp:basic))
             (let* ((exchange (amqp:exchange channel))
                    (ok-frame (encode-method 'amqp:declare-ok exchange)))
                 (release-frame frame)
                 (values (loopback-frame ok-frame) t)))))
    #'consume-ok-loopback))

(defun make-bind-ok-loopback  (channel)
  (flet ((bind-ok-loopback (frame)
           (when (and (eql (frame-channel-number frame) (channel-number channel))
                      (eq (frame-type-class-name frame) 'amqp:method)
                      (eq (frame-method-name frame) 'amqp:bind)
                      (eq (frame-class-name frame) 'amqp:queue))
             (let* ((queue (amqp:queue channel))
                    (ok-frame (encode-method 'amqp:bind-ok queue)))
                 (release-frame frame)
                 (values (loopback-frame ok-frame) t)))))
    #'bind-ok-loopback))
  
(defun make-command-sink (method-name)
  (flet ((method-sink (frame)
           (when (and (eq (frame-type-class-name frame) 'amqp:method)
                      (eq (frame-method-name frame) method-name))
             (release-frame frame)
             (values nil t))))
    #'method-sink))

(defun make-ok-reflector  (channel class out-method in-method)
  (flet ((ok-reflector (frame)
           (when (and (eql (frame-channel-number frame) (channel-number channel))
                      (eq (frame-type-class-name frame) 'amqp:method)
                      (eq (frame-method-name frame) out-method)
                      (eq (frame-class-name frame) class))
             (let* ((queue (amqp:ensure-object channel class))
                    (ok-frame (encode-method in-method queue)))
                 (release-frame frame)
                 (values (loopback-frame ok-frame) t)))))
    #'ok-reflector))

;;;
;;;


(defgeneric test-class-method-loopback (output-script input-constraints &key connection-class verbose-p)
  (:documentation "Test the output against the processed values 

 OUTPUT-SCRIPT : a list of forms, each (request-operator . args)
 INPUT-CONSTRAINTS : a list of forms, each (method-type class-type . args)

 the request is written to a loopback channel and the result is received until none are left.
 each is compared by type and argument list to the contraint form.")

  (:method ((output list) (input list) &key (connection-class (error "connection-class required."))
            (verbose-p nil))

    (let ((amqp:*class.connection* connection-class))
      (with-test-connection (connection)
        (with-test-channel (channel connection :number 1)
          (dolist (request output)
            (destructuring-bind (op class . args) request
              (setf class (amqp:ensure-object channel class))
              (apply op class args)))
          (let ((failure nil))
            (command-loop (channel :wait nil)
              (t ((class t)  &rest args)
                 (let ((test-response (pop input)))
                   (unless test-response
                     (setf failure `(:input ,amqp::method ,class ,args))
                     (return-from command-loop nil))
                   (destructuring-bind (method-type class-type . test-args) test-response
                     (when verbose-p
                       (format *trace-output* "~&~%received: (~s ~s)~%~8t. ~s~%constraint: (~s ~s) . ~s"
                               amqp::method class args  method-type class-type test-args))
                     (or (and (typep amqp::method method-type) (typep class class-type)
                              (loop for (key value) on test-args by #'cddr
                                    unless (equalp (getf args key) value)
                                    return nil
                                    finally (return t)))
                         (return-from command-loop
                           (progn (setf failure `(:input ,amqp::method ,class ,args)) nil)))))))
            (and (null failure) (null input))))))))

;;;
;;; test timing:
;;; time processing to send and receive a command. in order to test the pipeline per-se,
;;; create all markers by running one loop. then time repeated passes. the result indicates
;;; the total duration and memory usage and an average per echoed command. the minimum memory
;;; usage is that requird to service the queues.
;;;
;;; mcl5.2 g5-1.8ghz is .4ms per loopback
;;; (:loops 4000 :SECONDS 1.683 :BYTES 60416 :SECONDS/LOOPBACK 4.2075E-4 :BYTES/LOOPBACK 15.104)

(defun call-with-loopback (op &key (class 'amqp-1-1-0-9-1-loopback-connection))
  (let ((amqp:*class.connection* class)
        (command-count 0))
    (with-test-connection (connection)
      (with-test-channel (channel connection :number 1)
        (let ((basic (amqp:basic channel ))
              (exchange (amqp:exchange channel :exchange "" :type "direct"))
              (queue (amqp:queue channel :queue "" :message-count 1 :consumer-count 0))
              (tx (amqp:tx channel)))

          (setf (connection-frame-map connection)
                (list (make-publish-to-deliver-loopback channel channel)
                      (make-content-loopback channel channel)
                      (make-declare-queue-ok-loopback channel)
                      (make-declare-exchange-ok-loopback channel)
                      (make-bind-ok-loopback channel)
                      (make-consume-ok-loopback channel)
                      (make-command-sink 'amqp:get)
                      (make-ok-reflector  channel 'amqp:tx 'amqp:select 'amqp:select-ok)
                      (make-command-sink 'amqp:ack)))
          (flet ((basic.publish ()
                   ;; there is no intrinsic response
                   (amqp:publish basic :exchange exchange)
                   ;; clear the respose: get-ok,header.body
                   (setf (device-state channel) amqp.s:use-channel.body.input)
                   (dotimes (i 3)       ; command, header, body
                     (command-case (channel)
                       (t ((class t) &rest args)
                          (declare (dynamic-extent args))
                          (amqp:log :debug class "cwl in: ~a : ~s" amqp::method args)
                          t))))
                 (exchange.declare ()
                   ;; request op handles response
                   (amqp:declare exchange))
                 (queue.declare ()
                   ;; request op handles response
                   (amqp:declare queue))
                 (tx.select ()
                   ;; request op handles response
                   (amqp:select tx)))
            (flet ((run (&rest operations)
                     (declare (dynamic-extent operations))
                     (dolist (op (or operations '(basic.publish exchange.declare queue.declare tx.select)))
                       (ecase op
                         (basic.publish (basic.publish))
                         (exchange.declare (exchange.declare))
                         (queue.declare (queue.declare))
                         (tx.select (tx.select)))
                       (incf command-count))))
              (funcall op #'run))))))
    command-count))


(defun run-loopback (&key (passes 1) (operations nil) ((:log-level *log-level*) *log-level*)
                          (class 'amqp-1-1-0-9-1-loopback-connection))
  (labels ((run-loopback (run)
             (dotimes (i passes)
               (apply run operations))))
    
    (call-with-loopback #'run-loopback :class class)))
        

;;; about a 20% difference with the cache enabled
#+(or ) (time-loopback :passes  1000 :operations '() :cache-p t) ; :log-level :warn)
#+(or ) (time-loopback :passes  1000 :operations '() :cache-p nil) ; :log-level :warn)

(defun loopback-class-methods (combinations &key (class 'amqp-1-1-0-9-1-loopback-connection)
                                            ((:log-level *log-level*) *log-level*))
  (let ((amqp:*class.connection* class)
        (count 0))
    (with-test-connection (connection)
      (with-test-channel (channel connection :number 1)
        (loop for (class-name . methods) in combinations
              do (let ((class (amqp:ensure-object channel (cons-symbol :amqp class-name))))
                   (dolist (method methods)
                     (send-method (cons-symbol :amqp method) class)
                     (command-case (channel)
                       (t ((class t) &rest args)
                          (declare (dynamic-extent args))
                          (amqp:log :debug class "~a : ~s" amqp::method args)
                          (incf count))))))))
    count))

#+(or )
(loopback-class-methods '((basic publish) (exchange declare) (queue declare) (tx select))
                        :log-level :debug)



(defun loopback-objects (content &key (class 'amqp-1-1-0-9-1-loopback-connection)
                                (content-type mime:application/sexp)
                                (element-type 'standard-object)
                                 ((:log-level *log-level*) *log-level*))
  (let ((amqp:*class.connection* class))
    (with-test-connection (connection)
      (with-test-channels ((channel-publish connection :number 1 :element-type element-type :content-type content-type)
                           (channel-consume connection :number 2 :element-type element-type :content-type content-type))
        (let ((basic-publish (amqp:basic channel-publish))
              (basic-consume (amqp:basic channel-consume))
              (exchange-publish (amqp:exchange channel-publish :exchange "ex" :type "direct"))
              (queue-publish (amqp:queue channel-publish :queue "q1" :message-count 0 :consumer-count 0))
              (queue-consume (amqp:queue channel-consume :queue "q1" :message-count 0 :consumer-count 0)))

          (setf (connection-frame-map connection)
                (list (make-publish-to-deliver-loopback channel-publish channel-consume)
                      (make-content-loopback channel-publish channel-consume)
                      (make-declare-queue-ok-loopback channel-publish)
                      (make-declare-exchange-ok-loopback channel-publish)
                      (make-bind-ok-loopback channel-publish)
                      (make-declare-queue-ok-loopback channel-consume)
                      (make-consume-ok-loopback channel-consume)
                      (make-command-sink 'amqp:get)
                      (make-command-sink 'amqp:ack)))
            
          ;; declare and bind the queues
          (amqp:request-declare queue-publish)
          (amqp:request-declare exchange-publish)
          (amqp:request-bind queue-publish :exchange exchange-publish
                             :queue queue-publish
                             :routing-key "/")
            
          (mapcar #'(lambda (content)
                      (amqp:request-publish basic-publish :body content :exchange exchange-publish)
                      (amqp:request-get basic-consume :queue queue-consume))
                  content))))))

;;;
;;; simple class to use for object message tests

(defclass test-object ()
    ((number :initform 1 :initarg :number)
     (character :initform #\return :initarg :character)
     (string :initform "two" :initarg :string)
     (vector :initform #(1 2 3) :initarg :vector)))

(defmethod print-object ((object test-object) (stream t))
  (print-unreadable-object (object stream :identity t :type t)
    (dolist (sd (c2mop:class-direct-slots (class-of object)))
      (let ((name  (c2mop:slot-definition-name  sd)))
        (format stream " [~a ~s]"
                name (bound-slot-value object name '|#<unbound>|))))))

(defmethod make-load-form ((object test-object) &optional environment)
  (declare (ignore environment))
  (values `(allocate-instance (find-class ',(type-of object)))
          `(progn ,@(remove nil
                            (mapcar #'(lambda (sd)
                                        (let ((name  (c2mop:slot-definition-name  sd)))
                                          (when (slot-boundp object name)
                                            `(setf (slot-value ,object ',name)
                                                   ',(slot-value object name)))))
                                    (c2mop:class-direct-slots (class-of object)))))))


(defun test-live-objects (content &key (content-type mime:application/sexp)
                                  (verbose-p nil) (delay-p nil)
                                  (element-type 'standard-object)
                                  (uri "amqp://guest:guest@192.168.1.25/")
                                  ((:log-level *log-level*) *log-level*)
                                  (count 1) (no-ack t))
  (let ((connection (make-instance 'amqp:connection :uri uri)))
    (let ((channel-publish (amqp:connection.channel connection :uri #u"/?exchange=e1&queue=q1"
                                         :element-type element-type
                                         :content-type content-type))
          (channel-consume (amqp:connection.channel connection :direction :input :uri #u"/?queue=q1"
                                         :element-type element-type
                                         :content-type content-type)))
      (let ((basic-publish (amqp:channel.basic channel-publish))
            (basic-consume (amqp:channel.basic channel-consume :no-ack no-ack))
            (exchange-publish (amqp:channel.exchange channel-publish :exchange "ex" :type "direct"))
            (queue-publish (amqp:channel.queue channel-publish :queue "q1" :message-count 0 :consumer-count 0))
            (queue-consume (amqp:channel.queue channel-consume :queue "q1" :message-count 0 :consumer-count 0)))
        
        ;; declare and bind the queues
        (amqp:declare queue-publish)
        (amqp:request-declare exchange-publish)
        (amqp:request-bind queue-publish :exchange exchange-publish
                           :queue queue-publish
                           :routing-key "my key")
        (let ((start (get-internal-run-time))
              (get-count 0)
              (publish-count 0)
              (result nil))
          (values
           (dotimes (i count result)
             (mapcar #'(lambda (content)
                         (amqp:request-publish basic-publish :body content :exchange exchange-publish
                                               :routing-key "my key")
                         (incf publish-count)
                         (when delay-p (sleep (random 1.0)))
                         (setf result (amqp:request-get basic-consume :queue queue-consume))
                         (incf get-count)
                         (when verbose-p (print result)))
                     content))
           (- (get-internal-run-time) start)
           (+ publish-count get-count)))))))

;;;
;;; utilities

(defmethod drain-connection ((data vector) (stream stream) &key (start 0) (end (length data)))
  "read an return all bytes available on the stream."
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


#|
;;; generic dispatch and keyword processing overhead

(defgeneric test-keywords (context1 context2 &rest args &key key1 key2 key3 key4 key5)
  (:method ((context1 t) (context2 null) &rest args)
    (declare (dynamic-extent args))
    (every #'identity args))
  (:method ((context1 fixnum) (context2 cons) &key key1 key2 key3 key4 key5)
    (test-keywords (1+ context1) (rest context2)
                   :key1 key1
                   :key2 key2
                   :key3 key3
                   :key4 key4
                   :key5 key5)))

(defun test-required (context1 context2  key1 key2 key3 key4 key5)
  (if context2
    (test-required (1+ context1) (rest context2) key1 key2 key3 key4 key5)
    (and  key1 key2 key3 key4 key5)))

(defun test-calls (&key (count 1024) (data (make-list 1024)))
  (time (dotimes (x count)
          (test-keywords 0 data :key1 1 :key2 'two :key3 "three" :key4 4.0 :key5 #(5))))
  (time (dotimes (x count)
          (test-required 0 data 1 'two "three" 4.0 #(5)))))


;;; (test-calls :count (* 100 1024):data (make-list 10))

|#