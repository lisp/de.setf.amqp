;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines tests for the amqp-uri class of the 'de.setf.amqp' library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License along with 'de.setf.amqp'.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))


(test:test amqp/amqp-uri
  (flet ((eqv (l1 l2) (and (null (set-difference l1 l2 :test 'equal)) (null (set-difference l2 l1 :test 'equal)))))
  
    (and (eqv (make-load-form (merge-uris (uri "/asdf/qwer.txt") (uri "amqp://test.com:23/xxx/yyy.zzz")))
              '(make-instance 'uri :scheme :amqp :host "test.com" :port 23 :path '"/asdf/qwer.txt" :query nil :fragment nil :plist 'nil :string nil :parsed-path '(:absolute "asdf" "qwer.txt")))
         (eqv (make-load-form (merge-uris (uri "qwer.txt") #u"amqp://test.com:23/asdf/qwer/xxx.yyy"))
              '(MAKE-INSTANCE 'URI :SCHEME :AMQP :HOST "test.com" :PORT 23 :PATH '"/asdf/qwer/qwer.txt" :QUERY NIL :FRAGMENT NIL :PLIST 'NIL :STRING NIL :PARSED-PATH 'NIL))
         (eqv (make-load-form (merge-uris (uri "./qwer.txt") (uri "amqp://test.com:23/asdf/qwer/xxx.yyy")))
              '(MAKE-INSTANCE 'URI :SCHEME :AMQP :HOST "test.com" :PORT 23 :PATH '"/asdf/qwer/qwer.txt" :QUERY NIL :FRAGMENT NIL :PLIST 'NIL :STRING NIL :PARSED-PATH 'NIL))
         (eqv (make-load-form (merge-uris (uri "../qwer.txt") (uri "amqp://test.com:23/asdf/qwer/xxx.yyy")))
              '(MAKE-INSTANCE 'URI :SCHEME :AMQP :HOST "test.com" :PORT 23 :PATH '"/asdf/qwer.txt" :QUERY NIL :FRAGMENT NIL :PLIST 'NIL :STRING NIL :PARSED-PATH 'NIL))
         
         (eqv (uri-query-plist (uri "amqp://test.com:23/xxx/yyy.zzz?exchange=e1&queue=q1"))
              '(:EXCHANGE "e1" :QUEUE "q1"))
         (equal "e1" (uri-exchange (uri "amqp://test.com:23/xxx/yyy.zzz?exchange=e1&queue=q1"))))))