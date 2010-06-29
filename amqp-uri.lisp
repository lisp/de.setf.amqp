;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines the amqp-uri class to specialize `puri:uri` for use with the 'de.setf.amqp'
 library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (long-description "The prospective, standard format for AMQP uri is described in the 0.10 version spec[1],
 in addition to which QPID suggests to how to support unserinfo[2]. In addition to these, exchange and queue
 values are recognized as query parameters. QPID suggests[3] a syntax for binding URLs, but its interpreation
 of scheme values is suspect.

 [1]: http://jira.amqp.org/confluence/download/attachments/720900/amqp.0-10.pdf?version=1
 [2]: http://qpid.apache.org/url-format-proposal.html
 [3]: http://qpid.apache.org/bindingurlformat.html"))


(eval-when (:compile-toplevel :load-toplevel :execute)
  (let ((from (or (find-package :puri) (find-package :uri)
                  (error "No uri package found."))))
    (import (mapcar #'(lambda (s) (find-symbol (string s) from))
                    '(#:uri #:uri-scheme #:uri-host #:uri-port #:uri-path #:uri-query #:uri-fragment #:uri-plist
                      #:uri-user #:uri-password #:uri-userinfo #:merge-uris))
            *package*)))


(defclass amqp (uri)
  ()
  (:documentation "Extend the base uri class with support for exchange and queue query parameters an
to provide the default scheme throug a constructor."))


(defmethod shared-initialize ((uri amqp) (slots t) &rest initargs &key (scheme :amqp))
  (declare (dynamic-extent initargs))
  (apply #'call-next-method uri slots
         :scheme scheme
         initargs))

;; (uri 'amqp :host "1.2.3.4" :port 100)


(defmethod uri-query-plist ((uri uri))
  (or (getf (uri-plist uri) 'query-plist)
      (setf (getf (uri-plist uri) 'query-plist)
            (let ((string (uri-query uri)))
              (reduce #'nconc (mapcar #'(lambda (pair)
                                          (let ((eql-separator (position #\= pair)))
                                            (if eql-separator
                                              (list (intern (string-upcase (subseq pair 0 eql-separator)) :keyword)
                                                    (subseq pair (1+ eql-separator)))
                                              (list (intern (string-upcase pair) :keyword) t))))
                                      (split-string string "&;")))))))

(defmethod (setf uri-query-plist) (plist (uri uri))
  (setf (getf (uri-plist uri) 'query-plist) plist))

(defmethod uri-query-parameter ((uri uri) parameter)
  (getf (uri-query-plist uri) parameter))

(defmethod (setf uri-query-parameter) (value (uri uri) parameter)
  (setf (getf (uri-query-plist uri) parameter) value))


(defmethod uri-exchange ((uri uri))
  (uri-query-parameter uri :exchange))

(defmethod uri-queue ((uri uri))
  (uri-query-parameter uri :queue))

(defmethod uri-virtual-host ((uri uri))
  "Delegate to uri-path, but map a null path to '/'"
  (or (uri-path uri) "/"))


#+(or )
(inspect (merge-uris (amqp-uri (rest (parse-amqp-uri "/asdf/qwer.txt")))
                     (amqp-uri (rest (parse-amqp-uri "amqp://test.com/xxx/yyy.zzz")))))
