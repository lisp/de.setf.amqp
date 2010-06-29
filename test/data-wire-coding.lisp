;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)

(:documentation "This file defines buffer-accessor tests for the 'de.setf.amqp' library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  You should have received a copy of the GNU Affero General Public License along with 'de.setf.amqp'.
  If not, see the GNU [site](http://www.gnu.org/licenses/)."))

;;; iff not in :define mode
;;; (test:execute-test :amqp.data-wire-coding.**)


(test:with-test-situation (:define)
  (test:test amqp.data-wire-coding.string.1
    (and (string-8-p "a")
         (not (string-8-p (make-string 256)))
         (string-16-p (make-string 256))
         (not (string-16-p (make-string (expt 2 16))))
         (string-32-p (make-string (expt 2 16)))
         (or (> (expt 2 32) array-dimension-limit)
             (not (string-32-p (make-string (expt 2 32)))))))
  
  (test:test amqp.data-wire-coding.binary.1
    (and (binary-8-p (make-array (floor 8 8) :element-type '(unsigned-byte 8)))
         (not (binary-8-p (make-array (1+ (floor 8 8)) :element-type '(unsigned-byte 8))))
         (binary-16-p (make-array (floor 16 8) :element-type '(unsigned-byte 8)))
         (not (binary-16-p (make-array (1+ (floor 16 8)) :element-type '(unsigned-byte 8))))
         (binary-32-p (make-array (floor 32 8) :element-type '(unsigned-byte 8)))
         (not (binary-32-p (make-array (1+ (floor 32 8)) :element-type '(unsigned-byte 8))))
         (binary-40-p (make-array (floor 40 8) :element-type '(unsigned-byte 8)))
         (not (binary-40-p (make-array (1+ (floor 40 8)) :element-type '(unsigned-byte 8))))
         (binary-48-p (make-array (floor 48 8) :element-type '(unsigned-byte 8)))
         (not (binary-48-p (make-array (1+ (floor 48 8)) :element-type '(unsigned-byte 8))))
         (binary-64-p (make-array (floor 64 8) :element-type '(unsigned-byte 8)))
         (not (binary-64-p (make-array (1+ (floor 64 8)) :element-type '(unsigned-byte 8))))
         (binary-128-p (make-array (floor 128 8) :element-type '(unsigned-byte 8)))
         (not (binary-128-p (make-array (1+ (floor 128 8)) :element-type '(unsigned-byte 8))))
         (binary-256-p (make-array (floor 256 8) :element-type '(unsigned-byte 8)))
         (not (binary-256-p (make-array (1+ (floor 256 8)) :element-type '(unsigned-byte 8))))
         (binary-512-p (make-array (floor 512 8) :element-type '(unsigned-byte 8)))
         (not (binary-512-p (make-array (1+ (floor 512 8)) :element-type '(unsigned-byte 8))))
         (binary-1024-p (make-array (floor 1024 8) :element-type '(unsigned-byte 8)))
         (not (binary-1024-p (make-array (1+ (floor 1024 8)) :element-type '(unsigned-byte 8))))))
  
  (test:test amqp.data-wire-coding.buffer-byte
    (let ((buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0))
          (contents '(#(0 0 0 0 0 0 0 1 0 0 0 0 0 0 0) #(0 0 0 0 0 0 0 255 255 255 255 255 255 255 255))))
      (mapcar #'(lambda (c)
                  (replace buffer c)
                  (list (mapcar #'(lambda (p) (buffer-unsigned-byte-8 buffer p)) '(7))
                        (mapcar #'(lambda (p) (buffer-unsigned-byte-16 buffer p)) '(6 7))
                        (mapcar #'(lambda (p) (buffer-unsigned-byte-32 buffer p)) '(4 5 6 7))
                        (mapcar #'(lambda (p) (buffer-unsigned-byte-64 buffer p)) '(0 1 2 3 4 5 6 7))
                        (mapcar #'(lambda (p) (buffer-signed-byte-8 buffer p)) '(7))
                        (mapcar #'(lambda (p) (buffer-signed-byte-16 buffer p)) '(6 7))
                        (mapcar #'(lambda (p) (buffer-signed-byte-32 buffer p)) '(4 5 6 7))
                        (mapcar #'(lambda (p) (buffer-signed-byte-64 buffer p)) '(0 1 2 3 4 5 6 7))
                        ))
              contents))
    '(((1) (1 256) (1 256 65536 16777216) (1 256 65536 16777216 4294967296 1099511627776 281474976710656 72057594037927936)
       (1) (1 256) (1 256 65536 16777216) (1 256 65536 16777216 4294967296 1099511627776 281474976710656 72057594037927936))
      ((255) (255 65535) (255 65535 16777215 4294967295) (255 65535 16777215 4294967295 1099511627775 281474976710655 72057594037927935 18446744073709551615)
       (-1) (255 -1) (255 65535 16777215 -1) (255 65535 16777215 4294967295 1099511627775 281474976710655 72057594037927935 -1))))
  
  (test:test amqp.data-wire-coding.buffer-string
    (let* ((buffer (fill (frame-buffer 32) (char-code #\.)))
           (good-strings (list "01234567"
                               (let ((i (char-code #\a))) (map-into (make-string (- (length buffer) 4)) #'(lambda () (values (code-char i) (incf i)))))))
           (accessors '(buffer-string-8 buffer-string-8-utf8
                        buffer-string-16 buffer-string-16-utf8
                        buffer-string-32 buffer-string-32-utf8)))
      (dolist (accessor accessors t)
        (unless (dolist (string good-strings t)
                  (unless (equal (funcall (fdefinition accessor) (nth-value 2 (funcall (fdefinition `(setf ,accessor)) string buffer 0)) 0)
                                 string)
                    (return nil)))
          (return nil)))))

  (test:test amqp.data-wire-coding.buffer-string.errors
    "Test the effect of access at the end: over-run should signal a type error."
    (let ((buffer (fill (frame-buffer 32) (char-code #\.)))
          (string "01234567")
          (accessors '((1 buffer-string-8) (1 buffer-string-8-utf8)
                       (2 buffer-string-16) (2 buffer-string-16-utf8)
                       (4 buffer-string-32) (4 buffer-string-32-utf8))))
      (loop for (length-bytes accessor) in accessors
            unless (and (typep (nth-value 1 (ignore-errors (funcall (fdefinition `(setf ,accessor)) string buffer
                                                                    (1+ (- (length buffer) (+ (length string) length-bytes))))))
                               'type-error)
                        (equal (funcall (fdefinition accessor) (nth-value 2 (funcall (fdefinition `(setf ,accessor)) string buffer
                                                                                     (- (length buffer) (+ (length string) length-bytes))))
                                        (- (length buffer) (+ (length string) length-bytes)))
                               string))
            return (values nil accessor)
            finally (return t))))
  
  (test:test amqp.data-wire-coding.buffer-string-err (let ((buffer (make-array 1024 :element-type '(unsigned-byte 8) :initial-element (char-code #\.))))
                                 (every #'(lambda (c) (typep c 'error))
                                        (list  (nth-value 1 (ignore-errors (setf (buffer-string-8 buffer 0) (make-string 257))))
                                               (nth-value 1 (ignore-errors (setf (buffer-string-8 buffer 1020) (make-string 10))))))))

  (test:test buffer-binary (let ((buffer (make-array 128 :element-type '(unsigned-byte 8) :initial-element 0)))
                             (dotimes (i (length buffer)) (setf (aref buffer i) i))
                             (and (equalp (buffer-binary-8 buffer 0) (subseq buffer 0 1))
                                  (equalp (buffer-binary-16 buffer 0) (subseq buffer 0 2))
                                  (equalp (buffer-binary-32 buffer 0) (subseq buffer 0 4))
                                  (equalp (buffer-binary-64 buffer 0) (subseq buffer 0 8))
                                  (equalp (buffer-binary-128 buffer 0) (subseq buffer 0 16))
                                  (equalp (buffer-binary-256 buffer 0) (subseq buffer 0 32))
                                  (equalp (buffer-binary-512 buffer 0) (subseq buffer 0 64))
                                  (equalp (buffer-binary-1024 buffer 0) (subseq buffer 0 128))
                                  (typep (nth-value 1 (ignore-errors (buffer-binary-1024 buffer 1)))
                                         'error)
                                  (typep (nth-value 1 (ignore-errors (setf (buffer-binary-32 buffer 0)
                                                                                  #(0 1 2 3 4))))
                                         'error)
                                  (typep (nth-value 1 (ignore-errors (setf (buffer-binary-32 buffer 125)
                                                                                  #(0 1 2 3 ))))
                                         'error))))

  (test:test amqp.data-wire-coding.with-property-encoders
    (equalp
     '((:V1 (:TEST 1 :TEST "two") :V2 2) ("10100000" "00000000" "00000000" "00000000" "00000000" "00010001" "00000100" "01010100" "01000101" "01010011" "01010100" "01000010" "00000001" "00000100" "01010100" "01000101" "01010011" "01010100" "01110011" "00000011" "01110100" "01110111" "01101111" "00000010" "00000000" "00000000" "00000000" "00000000" "00000000" "00000000" "00000000" "00000000"))
     (let ((buffer (make-array 32 :element-type '(unsigned-byte 8)))
           (v1 '(:test 1 :test "two")) (v2 2) (v-null nil))
       (with-property-encoders (buffer :start 0)
         (amqp:field v1 AMQP-1-1-0-9-1:table)
         (amqp:field v-null AMQP-1-1-0-9-1:short)
         (amqp:field v2 AMQP-1-1-0-9-1:octet))
       (let ((decoded (list :v1 nil :v-null t :v2 nil)))
         (with-property-decoders (buffer :start 0)
           (amqp:field AMQP-1-1-0-9-1:table decoded :v1)
           (amqp:field AMQP-1-1-0-9-1:short decoded :v-null)
           (amqp:field AMQP-1-1-0-9-1:octet decoded :v2))
         (list decoded
               (map 'list #'(lambda (x) (format nil "~8,'0b" x)) buffer))))))

  (test:test amqp.data-wire-coding.ieee-754-64
    "Test ieee-754-64 packed-integer/float conversion.
     See http://babbage.cs.qc.cuny.edu/IEEE-754/References.xhtml for the values."
    (null (remove t '(;; all NAN are encoded as positive silent
                      #xFFF0000000000000
                      #xFFEFFFFFFFFFFFFF #x8010000000000000 #x800FFFFFFFFFFFFF #x8000000000000001
                      #x8000000000000000 #x0000000000000000
                      #x0000000000000001 #x000FFFFFFFFFFFFF #x0010000000000000 #x7FEFFFFFFFFFFFFF
                      #x7FF8000000000000
                      #x4039000000000000 #xC039000000000000 #x3FF0000000000000 #xBFF0000000000000
                      #x4000000000000000 #xC000000000000000 #x3FD5555555555555 #xBFD5555555555555)
                  :key #'(lambda (x)
                           (cond ((eql (ieee-754-64-float-to-integer (ieee-754-64-integer-to-float x)) x))
                                 (t
                                  (warn "ieee-754-64 failed: #x~16,'0x -> ~d -> #x~16,'0x, ~d"
                                        x (ieee-754-64-integer-to-float x)
                                        (ieee-754-64-float-to-integer (ieee-754-64-integer-to-float x))
                                        (ieee-754-64-integer-to-float (ieee-754-64-float-to-integer (ieee-754-64-integer-to-float x))))
                                  x))))))

  (test:test amqp.data-wire-coding.ieee-754-32
    "Test ieee-754-32 packed-integer/float conversion.
     See http://babbage.cs.qc.cuny.edu/IEEE-754/References.xhtml for the values."
    (null (remove t '(;; all NAN are encoded as positive silent
                      #xFF800000
                      #xFF7FFFFF #x80800000 #x807FFFFF #x80000001
                      #x80000000 #x0000000
                      #x00000001 #x007FFFFF #x00800000 #x7F7FFFFF
                      #x7F800000  
                      ;; various numbers
                      #x41c80000 #xc1c80000 #x3f800000 #xbf800000
                      #x40000000 #xc0000000 #x3eaaaaab #xbeaaaaab)
                  :key #'(lambda (x)
                           (cond ((eql (ieee-754-32-float-to-integer (ieee-754-32-integer-to-float x)) x))
                                 (t
                                  (warn "ieee-754-32 failed: #x~8,'0x -> ~d -> #x~8,'0x, ~d"
                                        x (ieee-754-32-integer-to-float x)
                                        (ieee-754-32-float-to-integer (ieee-754-32-integer-to-float x))
                                        (ieee-754-32-integer-to-float (ieee-754-32-float-to-integer (ieee-754-32-integer-to-float x))))
                                  x))))))

  )

