;;;-*  Package: rabbitmq; -*-

(in-package :rabbitmq)

;;                            UTILITIES.LISP
;;           Nick Levine, Ravenbrook Limited, 2007-09-21
;;           James Anderson, setf.de, 2010-02-04

;; 1.  INTRODUCTION
;;
;; This document collects utilities for the RabbitMQ interface.
;;
;; See Appendix C below for copyright and license.

(defun whitespace-char-p (char)
  (find char #(#\space #\tab #\return #\linefeed)))

(defun simple-word-wrap (text &optional (start 0))
  (let* ((ideal 72)
         (where (+ start ideal))
         (length (length text)))
    (when (>= where length)
      (return-from simple-word-wrap
        text))
    (loop (when (whitespace-char-p (schar text where))
            (setf (schar text where) #\Newline)
            (return-from simple-word-wrap
              (simple-word-wrap text (1+ where))))
          (when (= (decf where) start)
            (return)))
    (setf where (+ start ideal 1))
    (loop (when (whitespace-char-p (schar text where))
            (setf (schar text where) #\Newline)
            (return
             (simple-word-wrap text (1+ where))))
          (when (= (incf where) length)
            (return text)))))
    

#+(or ) ;; by-hand
(defmacro rebinding (variables . body)
  (let ((rebindings (mapcar #'(lambda (v) (list (gensym (string v)) v)) variables)))
    `(list 'let (list ,@(mapcar #'(lambda (b) `(list (quote ,(first b)) ,(second b))) rebindings))
           ,@(mapcar #'(lambda (form)
                         `(let ((form ,form))
                            (loop for (new old) in (list ,@(mapcar #'(lambda (b) `(list (quote ,(first b)) ,(second b))) rebindings))
                                  do (setf form (subst new old form))
                                  return form)))
                     body))))

(defmacro rebinding (variables . body)
  (let ((rebindings (mapcar #'(lambda (v) (list (gensym (string v)) v)) variables)))
    `(list 'let (list ,@(mapcar #'(lambda (b) `(list (quote ,(first b)) ,(second b))) rebindings))
           (list 'symbol-macrolet ',(mapcar #'reverse rebindings)
                 ,@body))))

#+mcl
(defmacro defadvice ((function tag when) arglist . body)
  `(ccl:advise ,function (apply #'(lambda ,arglist ,@body) arglist) :when ,when :name ,tag))


(defgeneric interpose-superclass (add-class amqp-class)
  (:method ((add-class symbol) (amqp-class t))
    (interpose-superclass (find-class add-class) amqp-class))
  (:method ((add-class t) (amqp-class symbol))
    (assert (eq (symbol-package amqp-class) (find-package :amqp)) ()
            "Permitted for protocol classes only.")
    (interpose-superclass add-class (find-class amqp-class)))
  (:method ((add-class class) (amqp-class class))
    (let ((existing-supers (c2mop:class-direct-superclasses amqp-class)))
      (unless (find add-class existing-supers)
        (reinitialize-instance amqp-class
                               :direct-superclasses (cons add-class existing-supers))))
    amqp-class))


;; A.  REFERENCES
;;
;;
;; B.  HISTORY
;;
;; 2007-09-21 NDL Created.
;; 2010-02-04 james.anderson@setf.de portability
;;
;;
;; C.  COPYRIGHT
;;
;; Copyright (c) 2007 Wiinz Limited. 
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
