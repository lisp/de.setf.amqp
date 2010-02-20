;;; -*- Package: de.setf.amqp.user; -*-

(in-package :de.setf.amqp.user)

;;;  This file demonstrates with-open-channel from the 'de.setf.amqp' library.
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


;;; to observe the protocol exchange
;;; (setq *log-level* :debug)

(defparameter *c* (make-instance 'amqp:connection :uri "amqp://guest:guest@localhost/"))

(amqp:connection-server-properties *c*)

(amqp:with-open-channel (output *c*  :exchange "ex" :type "direct" :queue "q1")
  (format output "~a, ~a, ~a~%"
          (lisp-implementation-type)
          (lisp-implementation-version)
          amqp.u:*version*))

(amqp:with-open-channel (input *c* :queue "q1")
  (read-line input))



(amqp:with-open-channel (output *c*  :exchange "ex" :type "direct" :queue "q1")
  (let ((message '("there" "comes" "a" "time" "when" "the" "mind" "takes" "a" "higher" "plane"
                   "of" "knowledge" "but" "can" "never" "prove" "how" "it" "got" "there")))
    (dotimes (x (- (length message) 4) x)
      (format output "~d.~{ ~a~}~:[.~;...~]~%" x (subseq message x (+ x 5)) (nth (+ x 5) message)))))

(amqp:with-open-channel (input *c* :queue "q1")
  (loop repeat 17 collect (read-line input)))

;; a small problem

(amqp:with-open-channel (input *c* :queue "q1")
  (loop
    (unless (print (read-line input nil nil))
      (return))))