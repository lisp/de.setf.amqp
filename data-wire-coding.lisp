;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)


(:documentation "This file defines buffer accessors for AMQP data as part of the 'de.setf.amqp' library."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (long-description "This file defines the general data buffer accessors for AMQP.[1]
 These encode/decode data between Lisp values and byte sequence buffers. All types required by the AMQP
 versions 0.8 through 0.10 are supported. Where the AMQP type corresponds directly to a designatable Lisp
 type, the general accessors reflect that name. Each type corresponds to an operator reader/writer pair of
 the form

    BUFFER-_type_ (buffer position)
    (SETF BUFFER-_type_) (value buffer position)
 Both expect a `(SIMPLE-ARRAY (UNSIGNED-BYTE 8))` typed buffer and an integer position within the buffer.
 Many of the AMQP typed operators map directly to atomic buffer operators with self-evident names.
 Sequence types require sized iterative coding. In some such cases, the AMQP type designation's size
 does not agree with the lisp type (eg str8 from 0.10 indicates the the length itself is an
 `(unsigned-byte 8)` value. In these cases an intermediate type serves to mediate terms.

 The self-describing encodings present a special case for each version, as the encoding structure and the
 type codes vary. As a consequence, those are not defined here. Instead a macro is defined to generate them
 for each version given the respective type codes.

 Each protocol specification includes a type table. The respective `data-encoding.lisp` file includes a
 transliterated version of this table to specify implementations for version-specific operators in terms of
 the general, and to inform the generation of the codecs for self-defining encodings.


 ----
 [1]: 'Advanced Message Queueing Protocol', amqp0-9-1.pdf, Section 4"))



(defvar *wire-level-type-map* ()
  "A bi-directional map between a primary lisp type and the types for a
 given protocol version. Each type map entry indicates a relation between a wire-level data type
 and a lisp data type, which can be used to generate the proper accessors for the wire-level
 type. Each protocol version recognizes different types and uses different indicators to mark
 them in-frame. In order to suport this, the accessor implementation for each version uses
 version-specific names for its operators and delegates them to general operators based on the
 entries in this table. One attribute, in particular, is whether a field type specializes `bit`,
 as they require special codec processing.

 NB. This is generated as a side-effect of translating the protocol specification, but is not
 used afterwards, as the equivalent relations are hard-wired into the generated definitions.")

(defun amqp:wire-level-type (type &optional (map *wire-level-type-map*))
  (gethash type map))

(defun (setf amqp:wire-level-type) (other-type key-type &optional (map *wire-level-type-map*))
  (when (and (symbolp key-type) (eq other-type 'amqp:bit))
    (setf (get key-type 'amqp:bit) t))
  (setf (gethash key-type map) other-type))





(:documentation (with-argument-decoders with-property-decoders)
  "The wire-level representation presents three patterns:

 - a fixed record structure for fields universally present - eg, frame type, channel, and size
 in this case the record fields are en/decoded with operations which reflect a fixed position/size/type
 map between lisp objects and the buffered data.

 - a fixed sequence of variable length fields for method arguments
 in this case macros are provided to en/decode a fixed sequence of values between lisp and buffered
 representations. varying sized data (eg sized strings) and self describing composite types are
 supported. As fields which are always present in the same order in the buffer the process is statically
 expressed in the source. The only variation is that of the length of sized elements. the macros establish
 an environment with a buffer and a position indicator which is maintained  through interaction with the
 primitive codecs for each field.

 - a fixed sequence of optional, variable length fields for class content header properties
 the encoded representation of a porperty set includes prefix flags to specify which fields are present.
 these are consulted/computed by macros to decode to a property list and encode from variables.
 the former mode is required in roder to construct keyword arguments based on presence. the latter
 relies on a null/not-null distinction, which will need to be revisited should bit property fields appear.
 The Property buffer codecs operate on a sequence of fields under control of an, initial bit flag
 sequence. Where the flag indicates presence value is decoded. otherwise, the field is skipped. The encoding
 performs the opposite projection, and skips null values. The property order and types are fixed, so use of
 the respective buffer accessors can be expressed in static code  in sequence to step through the fields.


 The macro operators are paired for decode/encode and argument/property functions:
  with-argument-decoders ((buffer &key (start 0)) &body body &environment env)
  with-argument-encoders ((buffer &key (start 0)) &body body &environment env)
  with-property-decoders ((buffer &key (start 0)) &body body &environment env)
  with-property-encoders ((buffer &key (start 0)) &body body &environment env)")


(defmacro with-argument-decoders ((buffer &key (start 0)) &body body &environment env)
  "Set up an argument decoding environment for the specified BUFFER. This includes a position indicator,
 which is initialized from the specified START value. Within the environment two operators
 are available
   field (type)  :  decodes a field of the specified type, updates the position based on its
     length and returns the value
   bit (bit-position &optional update-position) : decodes a single bit as a boolean value
     from the specified position in a bit field. if update-position is true, the position
     indicator is modified to reflect all immediately preceeding bits.
 VALUE : the length of the encoded data"

  (let* ((buffer-var (if (and (symbolp buffer) (eq (macroexpand-1 buffer env) buffer))
                       buffer (gensym (string :buffer-))))
         (position-var (gensym (string :position-))))
    `(macrolet ((amqp:field (type)
                  (list 'multiple-value-bind
                        '(value new-position)
                        (list (cons-symbol (symbol-package type) :buffer- type)
                              ',buffer-var
                              ',position-var)
                        (list 'setf ',position-var 'new-position)
                        'value))
                (amqp:bit (bit-position &optional (advance-position nil))
                  (let ((form (list 'buffer-bit ',buffer-var
                                    (list '+ ',position-var (floor bit-position 8))
                                    (mod bit-position 8))))
                    (if advance-position
                      (list 'prog1 form (list 'incf ',position-var (ceiling (1+ bit-position) 8)))
                      form))))
       (let ((,position-var ,start)
           ,@(unless (eq buffer buffer-var) `((,buffer-var ,buffer))))
         ,@body
         ,position-var))))


(defmacro with-argument-encoders ((buffer &key (start 0)) &body body &environment env)
  "Set up an argument encoding environment for the specified BUFFER. This includes a position indicator,
 which is initialized from the specified START value. Within the environment two operators
 are available:
   field (value type)  :  encodes the given value into a field of the specified type at the current
      position. Updates the position based on the value and returns the value
   bit (variable bit-position &optional update-position) : encodes a boolean as a single bit to the specified
      position in a bit field. if update-position is true, the position indicator is modified to reflect
      all immediately preceeding bits.
 VALUE : the length of the encoded data"

  (let* ((buffer-var (if (and (symbolp buffer) (eq (macroexpand-1 buffer env) buffer))
                       buffer (gensym (string :buffer-))))
         (position-var (gensym (string :position-))))
    `(macrolet ((amqp:field (value type)
                  (list 'multiple-value-bind
                        '(value new-position)
                        (list 'setf (list (cons-symbol (symbol-package type) :buffer- type)
                                          ',buffer-var
                                          ',position-var)
                              value)
                        (list 'setf ',position-var 'new-position)
                        'value))
                (amqp:bit (variable bit-position &optional (advance-position nil))
                  (let ((form (list 'setf (list 'buffer-bit ',buffer-var
                                                (list '+ ',position-var (floor bit-position 8))
                                                (mod bit-position 8))
                                    variable)))
                    (when (zerop (mod bit-position 8))
                      ;; first bit in an actet clears it
                      (setf form (list 'progn (list 'setf (list 'buffer-unsigned-byte-8 ',buffer-var 
                                                               (list '+ ',position-var (floor bit-position 8)))
                                                    0)
                                       form)))
                    (when advance-position
                      (setf form (list 'prog1 form (list 'incf ',position-var (ceiling (1+ bit-position) 8)))))
                    form)))
         (let ((,position-var ,start)
               ,@(unless (eq buffer buffer-var) `((,buffer-var ,buffer))))
           ,@(unless (eq buffer buffer-var) `((declare (ignorable ,buffer-var))))
           ,@body
           ,position-var))))



(defmacro with-property-decoders ((buffer &key (start 0)) &body body &environment env)
  "Set up a property decoding environment for the specified BUFFER. This includes a position indicator,
 which is initialized from the specified START value, and initial logic to extract a variable-length
 flag field. Within the environment a decoding operator is available which decodes the value:
   field (type &optional place keyword) : if the respective flag indicates presence, decodes a value of the
      given type, update the position, and returns the value. In addition, if a place and keyword are
      provided, the value is updated in the property list.
 VALUE : the length of the decoded data"

(let* ((buffer-var (if (and (symbolp buffer) (eq (macroexpand-1 buffer env) buffer))
                       buffer (gensym (string :buffer-))))
         (position-var (gensym (string :position-)))
         (flag-var (gensym (string :flags-)))
         (bit-count-var (gensym (string :bit-))))
    `(macrolet ((amqp:field (type &optional place keyword) 
                  (list* (if place 'if 'when)
                         (list 'logbitp (list 'decf ',bit-count-var) ',flag-var)
                         (list* 'multiple-value-bind
                                '(value new-position)
                                (list (cons-symbol (symbol-package type) :buffer- type)
                                      ',buffer-var
                                      ',position-var)
                                (list 'setf ',position-var 'new-position)
                                ;; even if a reserved value is present, ignore it
                                (if place
                                  (list (list 'setf (list 'getf place keyword) 'value))
                                  (list 'value)))
                         (when place (list (list 'remf place keyword))))))
       (let (,@(unless (eq buffer buffer-var) `((,buffer-var ,buffer)))
             (,bit-count-var 0))
         (declare (ignorable ,bit-count-var))
         (multiple-value-bind (,flag-var ,position-var) (buffer-property-flags-16 ,buffer-var ,start)
           (declare (ignorable ,flag-var))
           (setf ,bit-count-var (* (/ (- ,position-var ,start) 2) 15))
           ,@body
           ,position-var)))))


(defmacro with-property-encoders ((buffer &key (start 0)) &body body &environment env)
  "Set up a property decoding environment for the specified BUFFER. This includes a position indicator,
 which is initialized from the specified START value, and initial logic to encode a variable-length
 flag field based on the count of field operators in the body. Within the environment an operator is
 available which encodes values:
   field (value type) : if the value is not null, encodes in at the present position and updates same
      based on the value's encoded length. the presences is recored in the bit flags, which are set
      retrospectivelt at the conclusion.
 VALUE : the length of the encoded data

 nb. property flag chunking is handled when they are written into the buffer."

  (let* ((buffer-var (if (and (symbolp buffer) (eq (macroexpand-1 buffer env) buffer))
                       buffer (gensym (string :buffer-))))
         (position-var (gensym (string :position-)))
         (flag-var (gensym (string :flags-)))
         (value-var (gensym (string :value-)))
         (bit-count-var (gensym (string :bit-)))
         (start-var (gensym (string :start-)))
         (field-count 0)
         (bit-count 0)
         (short-count 0)
         (byte-count 0))
    (labels ((count-fields (x)
               (typecase x
                 (symbol (when (eq x 'amqp:field) (incf field-count)))
                 (cons (mapcar #'count-fields x))
                 (t ))))
      (count-fields body))
    (setf short-count (ceiling field-count 15)
          byte-count (* short-count 2)
          bit-count (* 15 short-count))
    `(macrolet ((amqp:field (value type)
                  (list 'let (list (list ',value-var value))
                        (list 'decf ',bit-count-var)    ; predecrement: highest possible bit is 14
                        #+amqp.debug-with-property-encoders
                        (list 'format '*trace-output* "~&~a=(~s) ~16,'0b @~d -> " `(quote ,value) value ',flag-var ',bit-count-var)
                        (list 'setf (list 'ldb (list 'byte 1 ',bit-count-var) ',flag-var)
                              (list 'if ',value-var 1 0))
                        #+amqp.debug-with-property-encoders
                        (list 'format '*trace-output* "~16,'0b" ',flag-var)
                        ;; don't worry about chunking/continuation here. it happens when written to the bufferr
                        (list 'if ',value-var
                              (list 'setf ',position-var
                                    (list 'nth-value 1
                                          (list 'setf (list (cons-symbol (symbol-package type) :buffer- type)
                                                            ',buffer-var
                                                            ',position-var)
                                                ',value-var)))
                              #+amqp.debug-with-property-encoders
                              (list 'format '*trace-output* "~&~a ! ~16,'0b  @~d" `(quote ,value)  ',flag-var ',bit-count-var)))))
       (let* (,@(unless (eq buffer buffer-var) `((,buffer-var ,buffer)))
              (,start-var ,start)
              (,position-var (+ ,start-var ,byte-count))
              (,flag-var 0)
              (,bit-count-var ,bit-count))
         (declare (ignorable ,bit-count-var))
         ,@body
         (setf (buffer-property-flags-16 ,buffer-var ,start-var ,short-count) ,flag-var)
         ,position-var))))




(:documentation "The individual AMQP field types all resolve to common lisp types. Some directly, but most in
 terms of custom type definitions. This applies, for example, to types where the AMQP size specifies the bit
 count of the respective size field rather than the length of the data. For example, string-8. These type
 definitions for these base types follow below. All names are in the :amqp package.

 Given these, the operator def-encodings (see below) defines version specific type predicates, elementary
 buffer accessors and composite codecs.")

#+digitool
(deftype amqp:frame-buffer (&optional length)
  (if length
    `(simple-array (unsigned-byte 8) (*))
    `(simple-array (unsigned-byte 8) (,length))))

#+(or clozure sbcl lispworks)
;; don't tell it more than it needs to know, otherwise shorter vectors conflict with declarations
(deftype amqp:frame-buffer (&optional length)
  (declare (ignore length))
  `(simple-array (unsigned-byte 8) (*)))

(defun make-frame-buffer (&optional (length *frame-size*))
  (make-array length :element-type '(unsigned-byte 8)))

(defun amqp:frame-buffer (length &key initial-contents)
  (let ((buffer (make-frame-buffer length)))
    (etypecase initial-contents
      (null buffer)
      (cons (map-into buffer (etypecase (first initial-contents)
                               (character #'char-code)
                               ((unsigned-byte 8) #'identity))
                      initial-contents))
      (string (map-into buffer #'char-code initial-contents))
      (vector (replace buffer initial-contents)))))

#+digitool
(deftype amqp::string-buffer (&optional length)
  (if length
    `(simple-array character (*))
    `(simple-array character (,length))))

#+(or clozure sbcl)
;; don't tell it more than it needs to know, otherwise shorter vectors conflict with declarations
(deftype amqp::string-buffer (&optional length)
  (declare (ignore length))
  `(simple-array character (*)))


(deftype amqp:bit ()
  "The bit type is a common lisp boolean which is coded to a bit array"
 'boolean)


(deftype amqp:iso-8859-character ()
  "names the subset of characters within the ISO-8859 domain."
  `(satisfies amqp:iso-8859-character-p))

(defun amqp:iso-8859-character-p (x)
  (and (characterp x)
       (<= 0 (char-code x) 255)))


(deftype amqp:utf32-character ()
  "names the subset of characters within the UTF-32 domain."
  `(satisfies utf32-character-p))

(defun amqp:utf32-character-p (x)
  (and (characterp x)
       (<= 0 (char-code x) #.(1- (expt 2 32)))))


(deftype  amqp:string (length-integer-length)
  "the AMQP string type designators are in terms of the size
 of the byte count, not the size of the string itself."
  (ecase length-integer-length
    (8 '(satisfies amqp:string-8-p))
    (16 '(satisfies amqp:string-16-p))
    (32 '(satisfies amqp:string-32-p))))

(macrolet ((def-string-predicate (length-integer-length)
             ;; define also the symbol form of type specifier
             (let* ((type-name (cons-symbol :amqp :string- (prin1-to-string length-integer-length)))
                    (predicate-name (cons-symbol :amqp type-name :-p))
                    (base (format nil "STRING with length less than ~s" length-integer-length))
                    (predicate-doc-string (format nil "Return true iff the argument is of type ~a." base))
                    (type-doc-string (format nil "The class of data of type ~a." base)))
               `(progn (eval-when (:compile-toplevel :load-toplevel :execute)
                         (export ',predicate-name :amqp)
                         (import ',predicate-name *package*)
                         (export ',type-name :amqp)
                         (import ',type-name *package*))
                       (deftype ,type-name () ,type-doc-string '(satisfies ,predicate-name))
                       (defun ,predicate-name (x)
                         ,predicate-doc-string
                         (and (stringp x)
                              (< (length x) ,(expt 2 length-integer-length))))))))
  (def-string-predicate 8)
  (def-string-predicate 16)
  (def-string-predicate 32))


(deftype  amqp:binary (length-in-bits)
  "the AMQP vector type designators are in terms of bit count."
  (ecase length-in-bits
    (8 '(satisfies amqp:binary-8))
    (16 '(satisfies amqp:binary-16))
    (32 '(satisfies amqp:binary-32))
    (40 '(satisfies amqp:binary-40))
    (48 '(satisfies amqp:binary-48))
    (64 '(satisfies amqp:binary-64))
    (72 '(satisfies amqp:binary-72))
    (128 '(satisfies amqp:binary-128))
    (256 '(satisfies amqp:binary-256))
    (512 '(satisfies amqp:binary-512))
    (1024 '(satisfies amqp:binary-1024))))

(macrolet ((def-binary-predicate (length-in-bits)
             ;; define also the symbol form of type specifier
             (let* ((type-name (cons-symbol :amqp :binary- (prin1-to-string length-in-bits)))
                    (predicate-name (cons-symbol :amqp type-name :-p))
                    (base (format nil "(vector (unsigned-byte 8)) with length less than ~s" (floor length-in-bits 8)))
                    (predicate-doc-string (format nil "Return true iff the argument is of type ~a." base))
                    (type-doc-string (format nil "The class of data of type ~a." base)))
               `(progn (eval-when (:compile-toplevel :load-toplevel :execute)
                         (export ',predicate-name :amqp)
                         (import ',predicate-name *package*)
                         (export ',type-name :amqp)
                         (import ',type-name *package*))
                       (deftype ,type-name () ,type-doc-string '(satisfies ,predicate-name))
                       (defun ,predicate-name (x)
                         ,predicate-doc-string
                         (and (typep x '(vector (unsigned-byte 8)))
                              (<= (length x) ,(floor length-in-bits 8))))))))
  (def-binary-predicate 8)
  (def-binary-predicate 16)
  (def-binary-predicate 32)
  (def-binary-predicate 40)
  (def-binary-predicate 48)
  (def-binary-predicate 64)
  (def-binary-predicate 128)
  (def-binary-predicate 256)
  (def-binary-predicate 512)
  (def-binary-predicate 1024))


(deftype amqp:table () `(satisfies amqp:table-p))

(defun amqp:table-p (x)
  (or (null x)
      (and (consp x)
           (keywordp (pop x))
           (consp x)
           (amqp:table-p (rest x)))))


(deftype amqp:array () 'vector)

(defun amqp:array-p (x) (typep x 'vector))


(deftype amqp:list () 'list)

(defun amqp:list-p (x) (typep x 'list))


(deftype amqp:decimal (&optional length)
  (declare (ignore length))
  '(and number (not complex)))

(defun amqp:decimal-p (x) (and (numberp x) (not (complexp x))))







(:documentation (compute-type-initform field-type-initform)
  "Where class slots definitions and codec keyword arguments require default values, these
 are imputed from the respective field type. This occurs as the specifications are translated into
 class and method definitions, at which point any version specific types are generalized and yield
 initial values, as below.")
 

(defun coerce-line-code (line-code)
  "Coerce a 'line code' into an integer.
 This allows for the variety of the code indicators which are carried over from the various xml
 specifications to the def-encodings elements."
  (etypecase line-code
    ((unsigned-byte 8) line-code)
    (character (char-code line-code))))


(eval-when (:compile-toplevel :load-toplevel :execute)
  ;; define macro and expansion operators to map respective standard's types to
  ;; initform values. these implement the generic types. each version's encoding
  ;; definition generates methods for its own types.
  (defgeneric compute-type-initform (type)
    (:documentation "Given a type, return an appropriate initform value.")

    (:method ((type cons))
      (compute-type-initform (first type)))
    
    (:method ((type null))                            nil)
    (:method ((type (eql 'amqp:array)))               #())
    (:method ((type (eql 'amqp:bit)))                 nil)
    (:method ((type (eql 'amqp:binary)))              #())
    (:method ((type (eql 'amqp:decimal)))             0)
    (:method ((type (eql 'amqp:iso-8859-character)))  #\null)
    (:method ((type (eql 'amqp:list)))                ())
    (:method ((type (eql 'amqp:string)))              "")
    (:method ((type (eql 'amqp:struct)))              ())
    (:method ((type (eql 'amqp:table)))               ())
    (:method ((type (eql 'boolean)))                  nil)
    (:method ((type (eql 'double-float)))             0.0d0)
    (:method ((type (eql 'short-float)))              0.0s0)
    (:method ((type (eql 'signed-byte)))              0)
    (:method ((type (eql 'unsigned-byte)))            0)
    (:method ((type (eql 'amqp:utf32-character)))     #\null)
    (:method ((type (eql 'amqp:vbinary)))             #())
    (:method ((type t))
      (error "No default known for type: ~s." type)))
  
  
  (defgeneric compute-field-type-initform (name type)
    (:documentation "return a value form to produce the initial value for
 the named (slot x type) combination.
 NB. the present version return NIL for ever field.")
    
    (:method ((name symbol) (type t))
      "The default version ignores the field."
      (compute-type-initform type))))
  

(defmacro field-type-initform (field type)
  (compute-field-type-initform field type))


(:documentation (def-encodings def-byte-accessors def-string-accessors)
  "The codecs implement transformations between lisp objects and byte sequences. The buffer type,
frame-buffer, is defined as (vector (unsigned-byte 8) (*)). It serves as a declaration and an argument
constraint. Each version's codecs are are expressed in terms of that version's types and its operators.
Each version-specific field type resolves to a lisp type, and the version-specific buffer accessors
are implemented, in turn, in terms of the lisp-type frame-buffer accessors. This permits
type names in one fersion to designate a different base implementation type than some other version.

The lisp-type accessors are defined in the def-byte-accessors, def-string-accessors, etc.

Each version includes a `data-wire-coding` file, in which a `def-encodings` form declares the type relation.
That declaration compiles into the several things for each entry:

 - a type definition
 - a method to compute an initform
 - protocol-specific buffer-accessors; for which, if a line code is included, they are intended to be used
 in self-describing data (tables)

In addition compound buffer accessors are defined for the types

 - list
 - array
 - table")
 
 

(defmacro def-encodings ((protocol-version) &rest type-specifications
                         &aux void-line-code)
  
  "Compile a protocol type specification into buffer codec operators for
 the respectively defined types and type codes. Also generate a map
 specific to that protocol version between lisp type specifiers and the
 protocol's. The operators include respective reader and writers for:
 - atomic data
 - name-value pair data
 - table/map data
 - list 
 - array (with mixed and uniform types)
 - structure (NYI)"
  
  (flet ((protocol-buffer-op (type)
           (cons-symbol protocol-version :buffer- type))
         (lisp-buffer-op (type)
           (cons-symbol *package* :buffer (format nil "~{-~a~}" (if (consp type) type (list type)))))
         (array-type-spec ()
           (find 'amqp:array type-specifications :key #'second))
         (list-type-spec ()
           (find 'amqp:list type-specifications :key #'second))
         (table-type-spec ()
           (find 'amqp:table type-specifications :key #'second))
         )
    
    (when (setf void-line-code
                (getf (find nil type-specifications :key #'first) :line-code))
      (setf type-specifications (remove nil type-specifications :key #'first))
      (setf void-line-code (coerce-line-code void-line-code)))

    (let ((wire-level-type-map (intern (string :*wire-level-type-map*) protocol-version)))
    
      `(progn
         (defparameter ,wire-level-type-map (make-hash-table :test 'equal))
         (macrolet ((optionally-set-type (line-code)
                      `(when type-code-p
                         (setf (aref buffer position) ,(coerce-line-code line-code))
                         (incf position))))
           
           ;; generate the field and table encoders such that they reference each other
           ,@(let ((buffer-table-op (protocol-buffer-op 'table-codec))
                   (buffer-array-op (protocol-buffer-op 'array-codec))
                   (buffer-list-op (protocol-buffer-op 'list-codec))
                   (buffer-field-value-op (protocol-buffer-op 'field-value))
                   (buffer-field-value-pair-op (protocol-buffer-op 'field-value-pair))
                   (buffer-setf-field-value-pair-op (protocol-buffer-op 'setf-field-value-pair))
                   (type-code-of-op (intern (string :type-code-of) protocol-version)))
               `((defun ,type-code-of-op (datum)
                   (etypecase datum
                     ,@(remove nil
                               (mapcar #'(lambda (spec)
                                           (destructuring-bind (amqp-type lisp-type &key line-code &allow-other-keys)
                                                               spec
                                             (declare (ignore amqp-type))
                                             (when line-code
                                               `(,lisp-type
                                                 ,(coerce-line-code line-code)))))
                                       type-specifications))))
                 
                 (defun ,buffer-field-value-op (buffer position &optional line-code)
                   (ecase (or line-code (aref buffer (shiftf position (1+ position))))
                     (,void-line-code nil)
                     ,@(remove nil
                               (mapcar #'(lambda (spec)
                                           (destructuring-bind (amqp-type lisp-type
                                                                          &key line-code
                                                                          (codec (lisp-buffer-op lisp-type)))
                                                               spec
                                             (declare (ignore amqp-type))
                                             (when line-code
                                               `(,(coerce-line-code line-code)
                                                 (,codec buffer position)))))
                                       type-specifications))))
                 (defun (setf ,buffer-field-value-op) (value buffer position &optional (type-code-p nil))
                   (etypecase value
                     ,@(remove nil
                               (remove-duplicates
                                (mapcar #'(lambda (spec)
                                            (destructuring-bind (amqp-type lisp-type &key (codec (protocol-buffer-op amqp-type))
                                                                           (line-code nil)
                                                                           &allow-other-keys)
                                                                spec
                                              (when line-code
                                                `(,lisp-type
                                                  (setf position
                                                        (nth-value 1 (setf (,codec buffer position
                                                                                   ,@(when line-code '(type-code-p)))
                                                                           value)))))))
                                        type-specifications)
                                :key #'first :from-end t :test #'equalp)))
                   (values value position))
                 
                 
                 (defun ,buffer-field-value-pair-op (buffer position)
                   (let ((namestring (buffer-string-8 buffer position)))
                     (incf position (1+ (length namestring)))
                     (multiple-value-bind (value position)
                                          (,buffer-field-value-op buffer position)
                       (values (list (intern namestring :keyword) value)
                               position))))
                 
                 (defun ,buffer-setf-field-value-pair-op (name value buffer position &optional type-code-p)
                   "Write a name-value pair. !! coerce a keyword name to a string to allow p-lists."
                   (setf position (nth-value 1 (setf (buffer-string-8 buffer position) (string name))))
                   (setf position (nth-value 1 (setf (,buffer-field-value-op buffer position type-code-p) value)))
                   (values value position))
                 
                 (defsetf ,buffer-field-value-pair-op (buffer position &optional type-code-p) (name value)
                   (list ',buffer-setf-field-value-pair-op name value buffer position type-code-p))
                 
                 
                 ,@(let ((type-spec (table-type-spec)))
                     (when type-spec
                       `((defun ,buffer-table-op (buffer position)
                           (let* ((length (buffer-unsigned-byte-32 buffer position))
                                  (result ())
                                  (end (+ position 4 length)))
                             (incf position 4)
                             (loop (when (>= position end)
                                     (return))
                                   (multiple-value-bind (pair new-position)
                                                        (,buffer-field-value-pair-op buffer position)
                                     (push pair result)
                                     (setf position new-position)))
                             (values (reduce 'nconc (nreverse result)) end)))
                         (defmethod (setf ,buffer-table-op) ((table list) buffer position &optional type-code-p)
                           (optionally-set-type ,(getf type-spec :line-code))
                           (let ((base position))
                             (incf position 4)
                             (loop for (field-name field-value) on table by #'cddr
                                   do (setf position
                                            (nth-value 1
                                                       #+ignore (setf (,buffer-field-value-pair-op buffer position t)
                                                                      (values field-name field-value))
                                                       (,buffer-setf-field-value-pair-op field-name field-value buffer position t))))
                             (setf (buffer-unsigned-byte-32 buffer base) (- (- position base) 4))
                             (values table position))))))
                 
                 ,@(let ((type-spec (list-type-spec)))
                     (when type-spec
                       `((defun ,buffer-list-op (buffer position)
                           (let* ((length (buffer-unsigned-byte-32 buffer position))
                                  (result ())
                                  (end (+ position 4 length)))
                             (incf position 4)
                             (loop (when (>= position end)
                                     (return))
                                   (multiple-value-bind (value new-position)
                                                        (,buffer-field-value-op buffer position)
                                     (push value result)
                                     (setf position new-position)))
                             (values (nreverse result) end)))
                         (defmethod (setf ,buffer-list-op) ((list list) buffer position &optional (type-code-p nil))
                           (optionally-set-type ,(getf type-spec :line-code))
                           (let ((base position))
                             (incf position 4)
                             (dolist (value list)
                               (setf position
                                     (nth-value 1 (setf (,buffer-field-value-op buffer position t) value))))
                             (setf (buffer-unsigned-byte-32 buffer base) (- (- position base) 4))
                             (values list position))))))
                 
                 ;;; !!! needs to take account of the 0.10 change to include a count filed after
                 ;;; the length and type
                 ,@(let ((type-spec (array-type-spec)))
                     (when type-spec
                       `((defun ,buffer-array-op (buffer position)
                           (let* ((length (buffer-unsigned-byte-32 buffer position))
                                  (result (make-array 8 :adjustable t :fill-pointer 0))
                                  (end (+ position 4 length))
                                  (count 0)
                                  (type-code (buffer-unsigned-byte-8 buffer (+ 4 position))))
                             (incf position 5)
                             (loop (when (>= position end)
                                     (return))
                                   (multiple-value-bind (value new-position)
                                                        (,buffer-field-value-op buffer position type-code)
                                     (vector-push-extend value result)
                                     (setf position new-position)))
                             (values result end)))
                         (defmethod (setf ,buffer-array-op) ((array vector) buffer position &optional (type-code-p nil))
                           (optionally-set-type ,(getf type-spec :line-code))
                           (let ((base position)
                                 (code (if (> (length array) 0)
                                         (,type-code-of-op (elt array 0))
                                         ,void-line-code)))
                             (incf position 4)
                             (setf (buffer-unsigned-byte-8 buffer position) code)
                             (incf position)
                             (loop for value across array
                                   do (setf position
                                            (nth-value 1
                                                       (setf (,buffer-field-value-op buffer position nil)
                                                             value))))
                             (setf (buffer-unsigned-byte-32 buffer base) (- (- position base) 4))
                             (values array position))))))))
           
           
           ;; generate the atomic encoders
           ,@(mapcar #'(lambda (spec)
                         (destructuring-bind (amqp-type lisp-type
                                                        &key line-code
                                                        (codec (lisp-buffer-op lisp-type)))
                                             spec
                           (setf amqp-type (cons-symbol protocol-version amqp-type))
                           (let ((p-op (protocol-buffer-op amqp-type))
                                 (l-op codec))
                             `(progn
                                (export ',amqp-type ,protocol-version)
                                (deftype ,amqp-type () ',lisp-type)
                                ,@(unless (equalp amqp-type lisp-type)
                                    `((eval-when (:compile-toplevel :load-toplevel :execute)
                                        (defmethod compute-type-initform ((type (eql ',amqp-type)))
                                          (compute-type-initform ',lisp-type)))))
                                (setf (amqp:wire-level-type ',amqp-type ,wire-level-type-map) ',lisp-type)
                                (unless (amqp:wire-level-type ',lisp-type ,wire-level-type-map)
                                  (setf (amqp:wire-level-type ',lisp-type ,wire-level-type-map) ',amqp-type))
                                (defun ,p-op (buffer position)
                                  (,l-op buffer position ,@(when (eq l-op 'buffer-bit) '(0))))
                                ;; if the line code is specified, the protocol-specific encoder
                                ;; should add it if necessary and then call the primtiive.
                                ;; method arguments have no line-code and are never encoded
                                ;; in a context which needs one.
                                ,@(unless (eq p-op l-op)
                                    (if line-code
                                      `((defun (setf ,p-op) (value buffer position &optional type-code-p)
                                          (optionally-set-type ,line-code)
                                          (setf (,l-op buffer position ,@(when (eq l-op 'buffer-bit) '(0))) value)))
                                      `((defun (setf ,p-op) (value buffer position)
                                          (setf (,l-op buffer position ,@(when (eq l-op 'buffer-bit) '(0))) value)))))))))
                     type-specifications))))))



(:documentation ieee-754-32-integer-to-float ieee-754-64-integer-to-float
                ieee-754-32-float-to-integer ieee-754-64-float-to-integer
  "The protocol data domain names vary from version to version, but they
 resolve to a limited number of lisp types, mostly

    string
    (unsigned-byte 8, 16, 32, 64)

 for each an encoding and a decoding operator is defined to pack/unpack the
 value from a byte buffer. The operators are not generic as the entity codecs
 all operate on data which fits in a single frame buffer - and (at least
 through 0.10) operations were defined to be communicated in single frame.

 Each buffered type requires two operators, one to encode and one to decode.
 they are paired as a reader operator and the respective setf. In the latter
 case the operator accepts an addition optional argument to specify the
 type code. Each protocol version reuqires its own frame codecs as the type
 codes vary.")


;;;
;;; floating point is brute force.

(defun ieee-754-32-integer-to-float (integer)
  (let* ((negative-p (logbitp 31 integer))
         (sign (if negative-p -1 +1))
         (raw-exponent (ash (logand #x7f800000 integer) -23))
         (exponent (- raw-exponent 127))
         (fraction (logand #x007fffff integer)))
    (case raw-exponent
      (#xff
       (if (zerop fraction)
         (if negative-p single-float-negative-infinity single-float-positive-infinity)
         #-sbcl single-float-nan
         #+sbcl (eval 'single-float-nan)))
      (#x00
       ;; (print (list :to-float sign raw-exponent exponent fraction))
       (if (zerop fraction)
         (if negative-p -0.0f0 0.0f0)
         (float (* sign (* fraction (expt 2 (- exponent 22)))) single-float-epsilon)))
      (t
       ;; (print (list :to-float sign raw-exponent exponent fraction))
       (float (* sign (1+ (* fraction #.(expt 2 -23))) (expt 2 exponent))
              single-float-epsilon)))))

(defun ieee-754-64-integer-to-float (integer)
  (let* ((negative-p (logbitp 63 integer))
         (sign (if negative-p -1 +1))
         (raw-exponent (ash (logand #x7ff0000000000000 integer) -52))
         (exponent (- raw-exponent 1023))
         (fraction (logand #x000fffffffffffff integer)))
    (case raw-exponent
      (#x7ff
       (if (zerop fraction)
         (if negative-p double-float-negative-infinity double-float-positive-infinity)
         #-sbcl double-float-nan
         #+sbcl (eval 'double-float-nan)))
      (#x000
       ;; (print (list :to-float sign raw-exponent exponent fraction))
       (if (zerop fraction)
         (if negative-p -0.0d0 0.0d0)
         (float (* sign (* fraction (expt 2 (- exponent 51)))) double-float-epsilon)))
      (t
       ;; (print (list :to-float sign raw-exponent exponent fraction))
       (float (* sign (1+ (* fraction #.(expt 2 -52))) (expt 2 exponent))
              double-float-epsilon)))))

;; (eql (ieee-754-32-integer-to-float #b00111110001000000000000000000000) 0.15625)
;; (eql (ieee-754-32-integer-to-float #b11000010111011010100000000000000) -118.625)

(defun raw-deconstruct-single-float (float)
  (etypecase float
    (single-float )
    (double-float (setf float (float float 1.0f0))))
  #+ccl (multiple-value-bind (fraction exponent sign)
                             (ccl::fixnum-decode-short-float float)
          (values fraction exponent (plusp sign)))
  ;; from sbcl:src;code;float.lisp
  #+sbcl (let* ((bits (sb-kernel::single-float-bits (abs float)))
                (exp (ldb sb-vm:single-float-exponent-byte bits))
                (sig (ldb sb-vm:single-float-significand-byte bits))
                (sign (minusp (float-sign float))))
           (values sig exp sign))
  #-(or ccl sbcl) (error "NYI: raw-deconstruct-single-float"))

(defun raw-deconstruct-double-float (float)
  (etypecase float
    (single-float (setf float (float float 1.0d0)))
    (double-float ))
  #+ccl (multiple-value-bind (hi lo exp sign) (ccl::%integer-decode-double-float float)
          (values (logior (ash hi 28) lo) exp (minusp sign)))
  #+sbcl (let* ((abs (abs float))
                (hi (sb-kernel::double-float-high-bits abs))
                (lo (sb-kernel::double-float-low-bits abs))
                (exp (ldb sb-vm:double-float-exponent-byte hi))
                ;(sig (ldb sb-vm:double-float-significand-byte hi))
                (sign (minusp (float-sign float))))
           (values
            (logior (ash (logior (ldb sb-vm:double-float-significand-byte hi)
                                 sb-vm:double-float-hidden-bit)
                         32)
                    lo)
            exp sign))
  #-(or ccl sbcl) (error "NYI: raw-deconstruct-double-float"))


(defun ieee-754-32-float-to-integer (float)
  (cond ((= float single-float-negative-infinity)
         #xff800000)
        ((= float single-float-positive-infinity)
         #x7f800000)
        ;; allow for sbcl inability to compile code with nan constants 
        (#-sbcl (eql float single-float-nan)
         #+sbcl (sb-ext:float-nan-p float)
         ;; http://en.wikipedia.org/wiki/NaN#Encodings
         ;; http://java.sun.com/javase/6/docs/api/java/lang/Double.html#doubleToLongBits(double)
         #x7fc00000)
        ((= float 0.0f0)
         (if (minusp (float-sign float)) #x80000000 #x00000000))
        (t
         (multiple-value-bind (fraction exponent sign)
                              (raw-deconstruct-single-float float)
           (if (zerop exponent)
             (logior (if sign #x80000000 0)
                     (logand fraction #x007fffff))
             (logior (if sign #x80000000 0)
                     (ash exponent 23)
                     (logand fraction #x007fffff)))))))

(defun ieee-754-64-float-to-integer (float)
  (cond ((= float double-float-negative-infinity)
         #xfff0000000000000)
        ((= float double-float-positive-infinity)
         #x7ff0000000000000)
        ;; allow for sbcl inability to compile code with nan constants                                                                                
        (#-sbcl (eql float double-float-nan)
         #+sbcl (sb-ext:float-nan-p float)
         ;; http://en.wikipedia.org/wiki/NaN#Encodings
         ;; http://java.sun.com/javase/6/docs/api/java/lang/Double.html#doubleToLongBits(double)
         #x7ff8000000000000)        
        ((= float 0.0d0)
         (if (minusp (float-sign float)) #x8000000000000000 #x0000000000000000))
        (t
         (multiple-value-bind (fraction exponent sign)
                              (raw-deconstruct-double-float float)
           (if (zerop exponent)
             (logior (if sign #x8000000000000000 0)
                     (logand fraction #x000fffffffffffff))
             (logior (if sign #x8000000000000000 0)
                     (ash exponent 52)
                     (logand fraction #x000fffffffffffff)))))))


#+ignore                                ; not used as the logic is protocol-specific
(defgeneric amqp:type-code (type)
  (:method ((value string))
    (let ((length (length string)))
      (if (<= length 255) (gethash 'string-8 *type-codes*)
          (if (<= length 65535) (gethash 'string-16 *type-codes*)
              (gethash 'string-32 *type-codes*)))))
  (:method ((value double-float))
    (gethash 'double-float *type-codes*))
  (:method ((value short-float))
    (gethash 'short-float *type-codes*))
  (:method ((value integer))
    (if (minusp value)
      (cond ((typep value '(signed-byte 8))
             (gethash 'signed-byte-8 *type-codes*))
            ((typep value '(signed-byte 16))
             (gethash 'signed-byte-16 *type-codes*))
            ((typep value '(signed-byte 32))
             (gethash 'signed-byte-32 *type-codes*))
            (t
             (gethash 'signed-byte-64 *type-codes*)))
      (cond ((typep value '(unsigned-byte 8))
             (gethash 'unsigned-byte-8 *type-codes*))
            ((typep value '(unsigned-byte 16))
             (gethash 'unsigned-byte-16 *type-codes*))
            ((typep value '(unsigned-byte 32))
             (gethash 'unsigned-byte-32 *type-codes*))
            (t
             (gethash 'unsigned-byte-64 *type-codes*))))))


(defun buffer-character (buffer position)
  (values (code-char (aref buffer position))
          (1+ position)))

(defun (setf buffer-character) (value buffer position)
  (setf (aref buffer position)
        (char-code value))
  (values value (1+ position)))


(defun buffer-iso-8859-character (buffer position)
  (values (code-char (aref buffer position))
          (1+ position)))

(defun (setf buffer-iso-8859-character) (value buffer position)
  (setf (aref buffer position)
        (char-code value))
  (values value (1+ position)))


(defun buffer-utf32-character (buffer position)
  (buffer-integer buffer position 4))

(defun (setf buffer-utf32-character) (value buffer position)
  (setf (buffer-integer buffer position 4) value))


(defun buffer-boolean (buffer position)
  (values (not (zerop (aref buffer position))) (1+ position)))

(defun (setf buffer-boolean) (value buffer position)
  (setf (aref buffer position)
        (if value 1 0))
  (values value (1+ position)))


(defun buffer-property-flags-16 (buffer position)
  "Retrieve chunked property flags from the byte BUFFER at the given POSITION.
 Each chunk is 15 bits long, with the low bit to indicate continuation."
  (let ((result 0))
    (loop
      (multiple-value-bind (segment new-position)
                           (buffer-unsigned-byte-16 buffer position)
        (setf result (logior (ash result 15) (ash segment -1)))
        (if (logbitp 0 segment)
          (setf position new-position)
          (return (values result new-position)))))))

(defun (setf buffer-property-flags-16) (flags buffer position count)
  "Store chunked property flages into the byte BUFFER at the given POSITION.
 Each chunk is 15 bits long, with the low bit to indicate continuation."
  (dotimes (i count)
    (let ((segment (ldb (byte 15 (* 15 (1- (- count i)))) flags)))
      (setf segment (ash segment 1))
      (when (< i (1- count))
        (setf segment (logior segment 1)))
      (setf position (nth-value 1 (setf (buffer-unsigned-byte-16 buffer position) segment)))))
  (values flags position))



(defun buffer-decimal (buffer position)
  (let ((scale (aref buffer position))
        (value (buffer-integer buffer (1+ position) 4)))
    (values (if (plusp scale)
              (/ value (expt 10 scale))
              value)
            (+ position 5))))


(defun (setf buffer-decimal) (value buffer position)
  (let ((scaled (floor (* value *decimal-scale-factor*))))
    (setf (aref buffer position) *decimal-scale*)
    (setf (buffer-unsigned-byte-32 buffer (1+ position)) scaled)
    (values value (+ position 5))))


(defun buffer-short-float (buffer position)
  (values (ieee-754-32-integer-to-float (buffer-integer buffer position 4))
          (+ position 4)))

(defun (setf buffer-short-float) (value buffer position)
  (setf (buffer-integer buffer position) (ieee-754-32-float-to-integer value))
  (values value (+ position 4)))


(defun buffer-double-float (buffer position)
  (values (ieee-754-64-integer-to-float (buffer-integer buffer position 8))
          (+ position 8)))

(defun (setf buffer-double-float) (value buffer position)
  (setf (buffer-integer buffer position) (ieee-754-64-float-to-integer value))
  (values value (+ position 8)))


#+(or )
(defun buffer-bit (buffer position bit-position)
  (let ((byte (buffer-unsigned-byte-8 buffer position)))
    (values (plusp (logand byte (ash 1 bit-position)))
            (+ position 1))))

(defun buffer-bit (buffer position bit-position)
  (values (ldb-test (byte 1 bit-position) (buffer-unsigned-byte-8 buffer position))
          (+ position 1)))

(defun (setf buffer-bit) (value buffer position bit-position)
  (let ((byte (buffer-unsigned-byte-8 buffer position)))
    (setf byte (dpb (if value 1 0) (byte 1 bit-position) byte))
    (setf (buffer-unsigned-byte-8 buffer position) byte)
    ;; advance the position by 1/8. in fact, this never advances the position for
    ;; known protocols, as they never have more than 8 bits. the caller must +1
    ;; ths total for a bit string
    (values value (+ position (ash bit-position -1)))))


(macrolet ((signed-byte (datum length)
             `(if (>= ,datum ,(1- (expt 2 (1- length))))          ;  convert
                (- (logxor ,(1- (expt 2 length)) (1- ,datum)))
                ,datum)))

  (defun signed-byte-8 (byte) (signed-byte byte 8))
  (defun signed-byte-16 (byte) (signed-byte byte 16))
  (defun signed-byte-32 (byte) (signed-byte byte 32))
  (defun signed-byte-64 (byte) (signed-byte byte 64))
  )


(macrolet ((def-byte-accessors (length)
             (let ((buffer-signed-name (intern (format nil "~a~d" :buffer-signed-byte- length)
                                               :de.setf.amqp.implementation))
                   (signed-name (intern (format nil "~a~d" :signed-byte- length)
                                        :de.setf.amqp.implementation))
                   (buffer-unsigned-name (intern (format nil "~a~d" :buffer-unsigned-byte- length)
                                                 :de.setf.amqp.implementation))
                   (bytes (floor length 8)))
               `(progn (defun ,buffer-unsigned-name (buffer position &optional (assert-conditions t))
                         (declare (type amqp:frame-buffer buffer))
                         (declare (type fixnum position))
                         (when assert-conditions
                           (assert-argument-type ,buffer-unsigned-name buffer frame-buffer)
                           (assert-condition (and (typep position 'fixnum) (<= (+ position ,bytes) (length buffer)))
                                             ,buffer-unsigned-name "value overflows buffer: (~s + ~s), ~s"
                                             position ,bytes (length buffer)))
                         (let ((value 0))
                           (declare (type (unsigned-byte ,length) value))
                           ,@(loop for i from 1 to bytes
                                   append `((setf value ,(if (= i 1)
                                                           '(aref buffer position)
                                                           '(+ (ash value 8) (aref buffer position))))
                                            (incf position)))
                           (values value position)))
                       
                       (defun (setf ,buffer-unsigned-name) (value buffer position &optional (assert-conditions t))
                         (declare (type amqp:frame-buffer buffer))
                         (declare (type fixnum position)
                                  (type (unsigned-byte ,length) value))
                         (assert-condition (typep value '(unsigned-byte ,length))
                                           (setf ,buffer-unsigned-name) "Invalid byte value, exceeds domain: ~s."
                                           value)
                         (when assert-conditions
                           (assert-argument-type (setf ,buffer-unsigned-name) buffer frame-buffer)
                           (assert-condition (and (typep position 'fixnum) (<= (+ position ,bytes) (length buffer)))
                                             (setf ,buffer-unsigned-name) "value overflows buffer: (~s + ~s), ~s"
                                             position ,bytes (length buffer)))
                         (values value
                                 (progn ,@(loop for i from (1- bytes) downto 0
                                                append `((setf (aref buffer (+ position ,i)) (logand #xff value))
                                                         (setf value (ash value -8))))
                                        (+ position ,bytes))))
                       
                       ;; the signed read can reuse the unsigned version, but the writer has different type constraints
                       (defun ,buffer-signed-name (buffer position  &optional (assert-conditions t))
                         (values (,signed-name (,buffer-unsigned-name buffer position assert-conditions))
                                 (+ position ,bytes)))
                       
                       (defun (setf ,buffer-signed-name) (value buffer position &optional (assert-conditions t))
                         (declare (type amqp:frame-buffer buffer))
                         (declare (type fixnum position)
                                  (type (signed-byte ,length) value))
                         (assert-condition (typep value '(signed-byte ,length))
                                           (setf ,buffer-signed-name) "Invalid byte value, exceeds domain: ~s."
                                           value)
                         (when assert-conditions
                           (assert-argument-type (setf ,buffer-signed-name) buffer frame-buffer)
                           (assert-condition (and (typep position 'fixnum) (<= (+ position ,bytes) (length buffer)))
                                             (setf ,buffer-signed-name) "value overflows buffer: (~s + ~s), ~s"
                                             position ,bytes (length buffer)))
                         (values value
                                 (progn ,@(loop for i from (1- bytes) downto 0
                                                append `((setf (aref buffer (+ position ,i)) (logand #xff value))
                                                         (setf value (ash value -8))))
                                        (+ position ,bytes))))))))
  
  (def-byte-accessors 8)
  (def-byte-accessors 16)
  (def-byte-accessors 32)
  (def-byte-accessors 64))


;;; the variable integer operator packs/unpacks an integer value of a given
;;; integer length.
;;; NB. this is not used as the protocols all specify constant length fields

(defun buffer-integer (buffer &optional (position 0) (length 4))
  (ecase length
    (8 (buffer-unsigned-byte-8 buffer position))
    (16 (buffer-unsigned-byte-16 buffer position))
    (32 (buffer-unsigned-byte-32 buffer position))
    (64 (buffer-unsigned-byte-64 buffer position))))


(defun (setf buffer-integer) (value buffer &optional (position 0) (length 4))
  (ecase length
    (8 (setf (buffer-unsigned-byte-8 buffer position) value))
    (16 (setf (buffer-unsigned-byte-16 buffer position) value))
    (32 (setf (buffer-unsigned-byte-32 buffer position) value))
    (64 (setf (buffer-unsigned-byte-64 buffer position) value))))


(:documentation (buffer-timestamp (setf buffer-stimestamp))
  "Timestamps are '64-bit POSIX time_t format with an accuracy of one second[1].
 The UNIX epoch is 1970-01-01T00:00:00Z. This is specified by the amqp:*timestamp-epoch*,
 which the buffer accessors use to shift to/from universal time.
 ---
 [1] amqp0-9-1.pdf, 4.2.5.4
 [2] http://en.wikipedia.org/wiki/Unix_time")

(defun buffer-timestamp (buffer position)
  (+ amqp:*timestamp-epoch*
     (buffer-unsigned-byte-64 buffer position)))

(defun (setf buffer-timestamp) (value buffer position)
  (setf (buffer-unsigned-byte-64 buffer position) (- value amqp:*timestamp-epoch*)))

(defun buffer-offset (buffer position)
  (buffer-unsigned-byte-64 buffer position))

(defun (setf buffer-offset) (value buffer position)
  (setf (buffer-unsigned-byte-64 buffer position) value))


(macrolet ((def-string-accessors (length-bits)
             ;; for a given bit size fo the length field,
             ;; generate iso8859, utf8, utf16, and utf32 buffer operators
             
             (let* ((buffer-iso-name (intern (format nil "~a-~d" :buffer-string length-bits)
                                             :de.setf.amqp.implementation))
                    (buffer-utf8-name (intern (format nil "~a-~d-~a" :buffer-string length-bits :utf8)
                                              :de.setf.amqp.implementation))
                    (buffer-utf16-name (intern (format nil "~a-~d-~a" :buffer-string length-bits :utf16)
                                               :de.setf.amqp.implementation))
                    (buffer-utf32-name (intern (format nil "~a-~d-~a" :buffer-string length-bits :utf32)
                                               :de.setf.amqp.implementation))
                    (buffer-unsigned-name (intern (format nil "~a-~d" :buffer-unsigned-byte length-bits)
                                                  :de.setf.amqp.implementation))
                    (length-bytes (floor length-bits 8)))
               (declare (ignore buffer-utf16-name buffer-utf32-name))
               `(progn (defun ,buffer-iso-name (buffer position)
                         (declare (type amqp:frame-buffer buffer))
                         (declare (type fixnum position))
                         (assert-argument-type ,buffer-iso-name buffer frame-buffer)
                         (assert-condition (and (typep position 'fixnum) (<= (+ position ,length-bytes) (length buffer)))
                                           ,buffer-iso-name "size field overflows buffer: (~s + ~s), ~s"
                                           position ,length-bytes (length buffer))
                         (let* ((length (,buffer-unsigned-name buffer position nil)))
                           (declare (type fixnum length))
                           (incf position ,length-bytes)
                           (if (plusp length)
                             (let ((result (make-array length :element-type +string-element-type+)))
                               (declare (type amqp::string-buffer result))
                               (assert-condition (<= (+ position length) (length buffer))
                                                 ,buffer-iso-name "string overflows buffer: (~s + ~s), ~s"
                                                 position length (length buffer))
                               (dotimes (i length)
                                 (setf (aref result i)
                                       (code-char (aref buffer position)))
                                 (incf position))
                               (values result position))
                             (values "" position))))
                       (defun (setf ,buffer-iso-name) (value buffer position)
                         (unless value (setf value ""))
                         (locally
                           (declare (type amqp:frame-buffer buffer))
                           (declare (type fixnum position)
                                    (type (or string null) value))
                           (assert-argument-type (setf ,buffer-iso-name) buffer frame-buffer)
                           (assert-argument-type (setf ,buffer-iso-name) value string)
                           (let* ((length (etypecase value (string (length value)) (null 0))))
                             ; (print (list length value buffer (+ position length ,length-bytes) (length buffer)))
                             (assert-condition (< length ,(expt 2 length-bits))
                                               (setf ,buffer-iso-name) "String overflows the size constraint")
                             (assert-condition (and (typep position 'fixnum) (<= (+ position length ,length-bytes) (length buffer)))
                                               (setf ,buffer-iso-name) "value overflows buffer: (~s + ~s), ~s"
                                               position (+ length ,length-bytes) (length buffer))
                             (setf (,buffer-unsigned-name buffer position nil) length)
                             (incf position ,length-bytes)
                             (dotimes (i length)
                               ; (print (list length value buffer position i (aref value i)))
                               (setf (aref buffer position) (char-code (aref value i)))
                               (incf position))
                             (values value position buffer))))

                       (defun ,buffer-utf8-name (buffer position)
                         (declare (type amqp:frame-buffer buffer))
                         (declare (type fixnum position))
                         (assert-argument-type ,buffer-iso-name buffer frame-buffer)
                         (assert-condition (and (typep position 'fixnum) (<= (+ position ,length-bytes) (length buffer)))
                                           ,buffer-iso-name "size field overflows buffer: (~s + ~s), ~s"
                                           position ,length-bytes (length buffer))
                         (let* ((length (,buffer-unsigned-name buffer position nil))
                                (end (+ position ,length-bytes))
                                (decoder (load-time-value (content-encoding-byte-decoder (content-encoding :utf-8)))))
                           (declare (type fixnum length))
                           (incf position ,length-bytes)
                           (if (plusp length)
                             (let ((result (make-array length :element-type +string-element-type+)))
                               (declare (type amqp::string-buffer result))
                               (assert-condition (<= (setf end (+ position length)) (length buffer))
                                                 ,buffer-iso-name "string size overflows buffer: (~s + ~s), ~s"
                                                 position length (length buffer))
                               (flet ((buffer-extract-byte (buffer)
                                        (declare (type amqp::frame-buffer buffer))
                                        (assert-condition (< position end)
                                                          ,buffer-iso-name "string overflows own size: ~s, ~s"
                                                          position end)
                                        (prog1 (aref buffer position)
                                          (incf position))))
                                 (declare (dynamic-extent #'buffer-extract-byte))         ; just in case
                                 (dotimes (i length)
                                   (setf (aref result i) (funcall decoder #'buffer-extract-byte buffer))))
                               (values result end))
                             (values "" end))))
                       (defun (setf ,buffer-utf8-name) (value buffer position)
                         (unless value (setf value ""))
                         (locally
                           (declare (type amqp:frame-buffer buffer))
                           (declare (type fixnum position)
                                    (type string value))
                           (assert-argument-type (setf ,buffer-utf8-name) buffer frame-buffer)
                           (assert-argument-type (setf ,buffer-utf8-name) value string)
                           (let* ((length (length value))
                                  (max-position 0)
                                  (start position)
                                  (encoder (load-time-value (content-encoding-byte-encoder (content-encoding :utf-8)))))
                             ;; can't check bounds here as the object length does not signify, but still limit
                             ;; to the maximum buffer size
                             (incf position ,length-bytes)
                             (setf max-position (min (+ position ,(expt 2 length-bits)) (length buffer)))
                             ; (print (list length value buffer (+ position length ,length-bytes) (length buffer) max-position))
                             (assert-condition (< length ,(expt 2 length-bits))
                                               (setf ,buffer-utf8-name) "String overflows the size constraint")
                             (flet ((buffer-insert-byte (buffer byte)
                                      (declare (type amqp:frame-buffer buffer))
                                      (declare (type (unsigned-byte 8) byte))
                                      ;; check bounds here as it's finally the encoded positioning
                                      (assert-condition (< position max-position)
                                                        (setf ,buffer-utf8-name) "String overflows size constraint: ~s, ~s"
                                                        position max-position)
                                      (setf (aref buffer position) byte)
                                      (incf position)))
                               (declare (dynamic-extent #'buffer-insert-byte))    ; just in case
                               (dotimes (i length)        ; can't check bounds here either
                                 ; (print (list length value buffer position i (aref value i)))
                                 (funcall encoder (char value i) #'buffer-insert-byte buffer))
                               ;; update the length prefix after the fact
                               (setf (,buffer-unsigned-name buffer start nil) (- position (+ start ,length-bytes)))
                               (values value position buffer)))))))))
  
  (def-string-accessors 8)
  (def-string-accessors 16)
  (def-string-accessors 32))
;;; (buffer-string-8-utf8 (nth-value 2 (setf (buffer-string-8-utf8 (frame-buffer 32) 0) "testing")) 0)

(macrolet ((def-binary-accessors (length-bits)
             ;; for a given data bit count generate binary vector codecs
             
             (let ((buffer-binary-name (intern (format nil "~a-~d" :buffer-binary length-bits)
                                               :de.setf.amqp.implementation))
                   (length-bytes (floor length-bits 8)))
               `(progn (defun ,buffer-binary-name (buffer position)
                         (let* ((result (make-array ,length-bytes :element-type '(unsigned-byte 8)))
                                (end (+ position ,length-bytes))
                                (length (length buffer)))
                           (assert (<= end length) ()
                                   "~s: size exceeds buffer: (~s + ~s), ~s"
                                   ',buffer-binary-name position ,length-bytes length)
                           (replace result buffer :start2 position :end2 end)
                           (values result end)))
                       (defun (setf ,buffer-binary-name) (value buffer position)
                         (let* ((length (length value))
                                (end (+ position ,length-bytes))
                                (value-end (+ position length)))
                           (assert (<= length ,length-bytes) ()
                                   "~s: Binary value length exceeds the size constraint: ~s"
                                   '(setf ,buffer-binary-name) length)
                           (assert (< end (length buffer)) ()
                                    "~s: value overflows buffer: (~s + ~s), ~s"
                                    '(setf ,buffer-binary-name) position ,length-bytes (length buffer))
                           (replace buffer value :start1 position :end1 value-end)
                           (when (< value-end end)
                             (fill buffer 0 :start value-end :end end))
                           (values value (+ position end))))))))
  
  (def-binary-accessors 8)
  (def-binary-accessors 16)
  (def-binary-accessors 32)
  (def-binary-accessors 40)
  (def-binary-accessors 48)
  (def-binary-accessors 64)
  (def-binary-accessors 72)
  (def-binary-accessors 128)
  (def-binary-accessors 256)
  (def-binary-accessors 512)
  (def-binary-accessors 1024))


(macrolet ((def-vbinary-accessors (length-bits)
             ;; for a given bit size of the length field generate binary vector codecs
             
             (let ((buffer-binary-name (intern (format nil "~a-~d" :buffer-vbinary length-bits)
                                               :de.setf.amqp.implementation))
                   (buffer-unsigned-name (intern (format nil "~a-~d" :buffer-unsigned-byte length-bits)
                                                 :de.setf.amqp.implementation))
                   (length-bytes (floor length-bits 8)))
               `(progn (defun ,buffer-binary-name (buffer position)
                         (let* ((length (,buffer-unsigned-name buffer position))
                                (result (make-array length :element-type '(unsigned-byte 8)))
                                (end (+ position length)))
                           (incf position ,length-bytes)
                           (replace result buffer :start2 position :end2 end)
                           (values result end)))
                       (defun (setf ,buffer-binary-name) (value buffer position)
                         (let* ((length (length value))
                                (end (+ position length)))
                           (assert (< length ,(expt 2 length-bits)) ()
                                   "Binary overflows the size constraint")
                           (assert (< end (length buffer)) ()
                                   "Binary overflows buffer")
                           (setf (buffer-integer buffer position ,length-bytes) length)
                           (incf position ,length-bytes)
                           (replace buffer value :start1 position :end1 end)
                           (values value end)))))))
  
  (def-vbinary-accessors 8)
  (def-vbinary-accessors 16)
  (def-vbinary-accessors 32))

;;; these two manifest an unrealistic structural relation between the version elements and
;;; the protocol headers. in fact, the relation is conventional and is recorded in
;;; amqp.u:*version-headers* by each version as it loads.

#+(or )
(progn
(defgeneric buffer-protocol-header (buffer)
  (:documentation "Extract a protocol header from a buffer.
 Return it as as keyword. (see make-version-keyword)")

  (:method ((buffer vector))
    (make-version-keyword :name (map-into (make-string 4) #'code-char buffer)
                          :class (aref buffer 4)
                          :instance (aref buffer 5)
                          :major (aref buffer 6)
                          :minor (aref buffer 7))))
(defgeneric (setf buffer-protocol-header) (header buffer)
  (:documentation "Store a protocol header into a buffer.")

  (:method ((header symbol) (buffer t))
    (setf (buffer-protocol-header buffer) (string header)))
  (:method ((header string) (buffer t))
    (setf (buffer-protocol-header buffer) (parse-version-keyword header)))
  (:method ((header cons) (buffer vector))
    "Store the header cookie and the version numbers in the first eight bytes of the buffer"
    (map-into buffer #'char-code (string (first header)))
    (replace buffer (rest header) :start1 4 :end2 4)
    header))
)


(defun (setf buffer-protocol-header-version) (version buffer)
  "Store a protocol header into a buffer.
 Accept a version keyword and set the version header as registered in the list of supported versions."
  (let ((encoded-version (version-protocol-header version)))
    (cond (encoded-version)
          ((null amqp.u:*version-headers*)
           (error "No AMQP version implementation loaded."))
          (t
           (error "Invalid version : ~s." version)))

    (replace buffer encoded-version :start1 0 :end1 8)
    version))


(defun buffer-protocol-header-version (buffer &optional (error-p t))
  "Extract a protocol header from a buffer.
 Return the respective version keyword as registered in the list of supported versions."

  (cond ((protocol-header-version (if (= (length buffer) 8) buffer (setf buffer (subseq buffer 0 8)))))
        ((null amqp.u:*version-headers*)
         (error "No AMQP version implementation loaded."))
        (error-p
         (error "Invalid version : ~s." buffer))
        (t
         nil)))
