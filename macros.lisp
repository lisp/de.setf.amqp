;;; -*- Package: de.setf.amqp.implementation; -*-


(in-package :de.setf.amqp.implementation)

(:documentation "This file defines the macros to declare protocol objects and methods for the
 'de.setf.amqp' library."

 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de) All Rights Reserved"
  "'de.setf.amqp' is free software: you can redistribute it and/or modify it under the terms of version 3
  of the GNU Affero General Public License as published by the Free Software Foundation.

  'setf.amqp' is distributed in the hope that it will be useful, but WITHOUT ANY WARRANTY; without even the
  implied warranty of MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.
  See the Affero General Public License for more details.

  A copy of the GNU Affero General Public License should be included with 'de.setf.amqp' as `AMQP:agpl.txt`.
  If not, see the GNU [site](http://www.gnu.org/licenses/).")

 (long-description "Several macros are used to define protocol entities

 - `def-ensure-object` operates with a protocol object to retrieve or create the respective dependent
 object, where the type is given abstractly while the instance is apways specific to the context's
 version
 - `def-ensure-method` operates with a protocol object similar to `def-ensure-object`, but with respect to
 method instances.
 - `def-amqp-abstract-class` defines a general protocol class. (see `classes.lisp`)
 - `def-amqp-class` defines a version-specific object class. This includes the codecs and the constructors.
 - `def-amqp-method` defines a version-specific method class. This includes the codecs and the constructors.
 - `def-amqp-command` defines a combined request/response command suite."))


(defmacro def-ensure-instance ((context-class-name scoped-class-name type) &optional keyword-arguments &rest options)
  "Define an AMQP interface method, named 'context-class-name.scoped-class-name' which ensures an instance of
 the scoped class exist for the context class. The operator manages a cache to arrange that there is one of
 each named type for the class instance. If keyargs are specified, they serve to index a set of cached
 methods, otherwise the operator arrange that at most one method exist. If initialization arguments other than
 any designators are passed, an existing instance is reinitialized to reflect the new values.
 When no (matching) instance exists, the abstract type is resolved wrt to a version-specific concrete class
 wrt the context, instantiated with any passed initialization arguments, and cached.
 In addition to the interface operator, define elementary slot accessors which use the respective
 valence and index keys to implement the index.

 The macro serves to implement def-ensure-object and def-insure-method."
  
  (let* ((keyargs (mapcar #'(lambda (arg) (etypecase arg (cons (first arg)) (symbol arg))) keyword-arguments))
         (keytypes (mapcar #'(lambda (arg) (etypecase arg (cons (second arg)) (symbol t))) keyword-arguments))
         (method-name.impl (cons-symbol *package* context-class-name "." scoped-class-name))
         (method-name.amqp (cons-symbol :amqp method-name.impl))
         (reader-name (cons-symbol *package* :get- context-class-name :- scoped-class-name
                                   (ecase type (object nil) (method :-method))
                                   (when (consp keyargs) :s)))
         (writer-name (cons-symbol *package* :setf- context-class-name :- scoped-class-name
                                   (ecase type (object nil) (method :-method))
                                   (when (consp keyargs) :s)))
         (ensure-name (cons-symbol :amqp :ensure- type))
         (documentation (or (second (assoc :documentation options))
                            (format nil "Ensure an ~:(~a.~a~) instance~@[, cached by ~:(~a~)~]."
                                    context-class-name scoped-class-name keyargs)))
         (find-class-operator (cons-symbol *package* :class-find- type :-class))
         #+(or ) ;; don't name a consturctor directly by class/method name
         ;; the name should be used for the request operator
         (constuctor (assoc :constructor options))
         (reader (assoc :reader options))
         (writer (assoc :writer options))
         (ensure-object (assoc :ensure-object options))
         (class.type (assoc (ecase type (object :class.object) (method :class.method)) options)))
    `(progn
       ;; (declare (ftype ... (setf x))) had no effect within the function
       ;; the type here cannot be made specific, as some methods apply to
       ;; more than one class and the eventual actual accessor definitions are
       ;; non-specific. otherwise sbcl nortices the anamoly and complains
       (declaim (ftype (function (t) t) ,reader-name)
                (ftype (function (t t) t) ,writer-name))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export '(,method-name.amqp ,ensure-name) :amqp))

       ,(if reader                      ; define a standard function - it delegates to a generic
          `(defun ,method-name.impl ,@(rest reader))
          (if keyargs
            `(defun ,method-name.impl (_::context &key ,@keyargs)
               ,@(loop for name in keyargs
                       for type in keytypes
                       unless (eq type t)
                       collect `(assert-argument-type ,method-name.impl ,name ,type))
               ,(if (and (= (length keytypes) 1) (subtypep (first keytypes) 'fixnum))
                  `(let ((_::cache (,reader-name _::context)))
                     (assert (and (>= ,(first keyargs) 0) (< ,(first keyargs) (length _::cache))) ()
                             "Invalid cache reference: ~d, ~a" ,(first keyargs) (type-of _::cache))
                     (aref _::cache ,(first keyargs)))
                  `(let ((key ,(if (cdr keyargs) `(list ,@keyargs) (first keyargs))))
                     ,@(when (cdr keyargs) `((declare (dynamic-extent key))))
                     (rest (assoc key (,reader-name _::context) :test #'equal)))))
            `(defun ,method-name.impl (_::context &key)
               (,reader-name _::context))))

       ,(if writer                      ; define a standard function - it delegates to a generic
          `(defun (setf ,method-name.impl) ,@(rest writer))
          (if keyargs
            `(defun (setf ,method-name.impl) (_::value _::context &key ,@keyargs)
               ,@(loop for name in keyargs
                       for type in keytypes
                       unless (eq type t)
                       collect `(assert-argument-type (setf ,method-name.impl) ,name ,type))
               (let* ((_::cache (,reader-name _::context)))
                 ,(if (and (= (length keytypes) 1) (subtypep (first keytypes) 'fixnum))
                    `(progn
                       (assert (and (>= ,(first keyargs) 0) (< ,(first keyargs) (length _::cache))) ()
                               "Invalid cache reference: ~d, ~a" ,(first keyargs) (type-of _::cache))
                       (setf (aref _::cache ,(first keyargs)) _::value))
                    `(let* ((key ,(if (cdr keyargs) `(list ,@keyargs) (first keyargs)))
                            (entry (assoc key _::cache :test #'equal)))
                       ,@(when (cdr keyargs) `((declare (dynamic-extent key))))
                       (if entry
                         (setf (rest entry) _::value)
                         (prog1 _::value
                           (,writer-name (acons ,(if (cdr keyargs) `(copy-list key) 'key)
                                                _::value
                                                _::cache)
                                         _::context)))))))
            `(defun (setf ,method-name.impl) (_::value _::context &key)
               (,writer-name _::value _::context))))
       
       ,(if ensure-object
          `(defmethod ,ensure-name ,@(rest ensure-object))
          `(defmethod ,ensure-name ((_::context ,context-class-name) (type (eql ',scoped-class-name))
                                    &rest _::initargs)
             (declare (dynamic-extent _::initargs))
             (apply #',method-name.amqp _::context _::initargs)))

       ,(if class.type
          `(defmethod ,method-name.amqp ,@(rest class.type))
          `(defmethod ,method-name.amqp ((_::context ,context-class-name) &rest _::initargs
                                         &key ,@keyargs &allow-other-keys)
             ,documentation
             (declare (dynamic-extent _::initargs))
             (let* ((_::instance
                     (,method-name.impl _::context ,@(loop for name in keyargs
                                                         append `(,(cons-symbol :keyword name) ,name)))))
               (if _::instance
                 (if ,(if keyargs
                        `(loop for key in _::initargs by #'cddr
                               unless ,(if (cdr keyargs)
                                         `(member key ',(mapcar #'(lambda (name)
                                                                    (cons-symbol :keyword name))
                                                                keyargs))
                                         `(eq key ',(cons-symbol :keyword (first keyargs))))
                               return t)
                        '_::initargs)
                   (apply #'reinitialize-instance _::instance _::initargs)
                   _::instance)
                 (setf _::instance
                       (apply #'make-instance (,find-class-operator _::context ',scoped-class-name)
                              :context _::context
                              _::initargs)
                       (,method-name.impl _::context ,@(loop for name in keyargs
                                                           append `(,(cons-symbol :keyword name) ,name)))
                       _::instance)))))
       )))

(defmacro def-ensure-object ((context-class-name class-name) &optional keyargs &rest options)
  "Define an AMQP interface method, named 'class-name.method-name' which ensures an instance of the given
 method exist for the respective class. See def-ensure-instance."

  `(def-ensure-instance (,context-class-name ,class-name object) ,keyargs
     ,@options))


(defmacro def-ensure-method ((context-class-name method-name) &rest options)
  "Define an AMQP interface method, named 'class-name.method-name' which ensures an instance of the given
 method exist for the respective class. See def-ensure-instance. The methods are indexed in the object by
 name only."

  `(def-ensure-instance (,context-class-name ,method-name method) ()
     ,@options))



(defmacro def-amqp-abstract-class (name supers slots &rest options)
  (unless (assoc :documentation options)
    (push `(:documentation ,(format nil "The AMQP protocol class ~:(~a~)." name))
          options))
  `(progn
     (eval-when (:compile-toplevel :load-toplevel :execute)
       (export '(,name) (symbol-package ',name)))
     (defclass ,name ,supers ,slots ,@options)
     (def-class-constructor ,name
       (:method ((context amqp:object) &rest initargs)
         (declare (dynamic-extent initargs))
         (apply #'amqp:ensure-object context ',name initargs)))
     (find-class ',name)))


(defmacro def-amqp-class (name supers slots properties arguments &rest options)
  (let* ((exports `((export ',name ,(symbol-package name))))
         (class-code (or (getf (rest (assoc 'id slots)) :initform)
                         (error "no class id code present")))
         (abstract-version-class (first supers))
         (abstract-protocol-class (second supers))
         (version-package (symbol-package name))
         (connection-class (cons-symbol version-package :connection))
         (method-names (second (getf (rest (assoc 'method-names slots)) :initform)))
         (length-var '_::length)
         (buffer-var '_::buffer)
         (frame-var '_::frame)
         (reserved-slot-names nil))

    ;; coerce protocol slots names to the :amqp package
    (flet ((coerce-slot-name (sd)
             (cons (cons-symbol :amqp (first sd)) (rest sd))))
      (setf properties (mapcar #'coerce-slot-name properties)
            arguments (mapcar #'coerce-slot-name arguments)))
    (setf reserved-slot-names (remove nil (mapcar #'(lambda (sd)
                                                      (let ((name (first sd)))
                                                        (when (search "reserved" (string name) :test #'char-equal)
                                                          name)))
                                                  (append properties arguments))))
    
    (setf slots (append slots
                        `((property-slot-names :initform ',(mapcar #'first properties)
                                               :allocation :class)
                          (argument-slot-names :initform ',(mapcar #'first arguments)
                                               :allocation :class))
                        (unless (assoc 'protocol-version slots)
                          `((protocol-version :initform ,(cons-symbol :keyword (package-name (symbol-package name)))
                                              :reader class-protocol-version
                                              :allocation :class)))
                        (mapcar #'(lambda (method-name)
                                    `(,(cons-symbol :amqp method-name :-method)
                                      :initform nil
                                      :reader ,(cons-symbol *package* :get-  name :- method-name :-method)
                                      :writer ,(cons-symbol *package* :setf-  name :- method-name :-method)))
                                method-names)
                        (mapcar #'(lambda (sd)
                                    (destructuring-bind (slot-name &key accessor reader writer
                                                                   &allow-other-keys)
                                                        sd
                                      ;; coerce name to :amqp and generate accessors
                                      (list* slot-name
                                             :initarg (cons-symbol :keyword slot-name)
                                             (if (or accessor reader writer)
                                               (rest sd)
                                               (let ((accessor (cons-symbol :amqp name :- slot-name)))
                                                 (push `(export ',accessor :amqp) exports)
                                                 `(:accessor ,accessor ,@(rest sd)))))))
                                ;; allow that the class itself and  method share a property/argument
                                (remove-duplicates (append properties arguments) :key #'first :from-end nil))))
   
    
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,name ,supers ,slots ,@options))
       ,@(nreverse exports)

       (defmethod connection-class-code-class-name ((connection ,connection-class) (class-code (eql ,class-code)))
         ',abstract-protocol-class)

       (defmethod connection-class-name-class-code ((connection ,connection-class) (class-code (eql ',abstract-protocol-class)))
         ,class-code)

       (defmethod class-find-object-class ((context-class ,abstract-version-class) (class-code (eql ,class-code)))
         (find-class ',name))
       (defmethod class-find-object-class ((context-class ,abstract-version-class) (class-name (eql ',abstract-protocol-class)))
         (find-class ',name))

       (defmethod amqp:ensure-object ((class ,abstract-version-class) (class-code (eql ,class-code)) &rest args)
         (declare (dynamic-extent args))
         (apply #'amqp:ensure-object class ',abstract-protocol-class args))

       #+(or )
       (defmethod class-initialize-class ((_::context-class amqp:object) (_::context ,name) &key
                                           ,@(mapcar #'(lambda (name)
                                                         `(,name nil ,(cons-symbol *package* name :-s)))
                                                     unreserved-property-names))
               ,@(mapcar #'(lambda (name)
                             `(when ,(cons-symbol *package* name :-s)
                                (setf (slot-value _::context ',name) ,name)))
                         unreserved-property-names)
               (call-next-method))

       (defmethod call-with-decoded-properties (op (class ,name) buffer &rest args) 
         ,(format nil "Decode ~a properties into a buffer." name)
         (declare (dynamic-extent args))
         ;; stack-allocate the initial list of the full property complement
         (let ((decoded-args (list* ,@(reduce #'append (mapcar #'(lambda (sd)
                                                                  (let ((name (first sd)))
                                                                    (unless (find name reserved-slot-names)
                                                                      ;; the initial value is nil. either it is
                                                                      ;; set from the properties or it is removed
                                                                      (list (cons-symbol :keyword name) nil))))
                                                              properties))
                                    :class ',name
                                    :weight (content-weight buffer)
                                    :body-size (content-body-size buffer)
                                    args)))
           (declare (dynamic-extent decoded-args))
           (assert (eql (content-header-class-id buffer) ,class-code) ()
                   "Invalid content header for class: ~d, ~s" (content-header-class-id buffer) ',name)
           ;; massage the property list based on the buffer content
           (with-property-decoders (buffer :start (class-property-offset class))
             ,@(mapcar #'(lambda (sd)
                           (destructuring-bind (name &key (type (error "No type present: ~s . ~s" name sd))
                                                     &allow-other-keys) sd
                             ;; nb. must check the reserveds
                             ;; in order to maintain the flag position
                             (if (find name reserved-slot-names)
                               `(amqp:field ,type)      ; just decoded to keep place
                               `(amqp:field ,type decoded-args ,(cons-symbol :keyword name)))))
                       properties))
           (apply op class decoded-args)))

       (defmethod call-with-encoded-properties (op (class ,name) 
                                                   &key (body-size (class-body-size class))
                                                   (weight (class-weight class))
                                                   ,@(mapcar #'(lambda (sd)
                                                                 (destructuring-bind (name &key  &allow-other-keys) sd
                                                                   ;; reserved properties are nil to suppress encoding
                                                                   (if (search "reserved" (string name) :test #'char-equal)
                                                                     `(,name nil ,(cons-symbol (symbol-package name) name :-s))
                                                                     `(,name (slot-value class ',name)))))
                                                             properties)) 
         ,(format nil "Encode ~a properties into a buffer." name)
         ,@(let ((reserved-names (remove nil (mapcar  #'(lambda (sd)
                                                          (destructuring-bind (name &key &allow-other-keys) sd
                                                            (when (search "reserved" (string name) :test #'char-equal)
                                                              (list name (cons-symbol (symbol-package name) name :-s)))))
                                                      properties))))
             (loop for (var var-s) in reserved-names
                   collect `(when ,var-s
                              (error ,(format nil "~s is reserved and may not be specified." var)))))
         (amqp:log :debug class "encoding: (~@{~s~^ ~})"
                   ,@(reduce #'append (mapcar #'(lambda (sd)
                                                  (destructuring-bind (name &key &allow-other-keys) sd
                                                    (unless (search "reserved" (string name) :test #'char-equal)
                                                      `(,(cons-symbol :keyword name) ,name))))
                                              properties)))
         (let* ((,length-var 0)
                (,frame-var (claim-output-frame class))
                (,buffer-var (frame-data ,frame-var)))
           (declare (ignorable ,buffer-var))
           (setf ,length-var
                 (with-property-encoders (,buffer-var :start (class-property-offset class))
                   ,@(mapcar #'(lambda (sd)
                                (destructuring-bind (name &key type &allow-other-keys) sd
                                  (cond ((or (eq type 'amqp:bit)
                                             (get type 'amqp:bit))
                                         (error "Bit type not supported for properties: ~s." name))
                                        (t
                                         `(amqp:field ,name ,type)))))
                            properties)))
           
           (setf-content-header-class-id ,class-code ,buffer-var)
           (setf-content-weight weight ,buffer-var)
           (setf-content-body-size body-size ,buffer-var)
           (setf-frame-size ,length-var ,frame-var)
           (funcall op ,frame-var class))))))

(defmacro def-amqp-method ((class method-name) (amqp:method-name &rest supers) slots arguments &rest options)
  (let* ((exports ())
         (method-code (or (getf (rest (assoc 'id slots)) :initform)
                          (error "no id slot present: ~a." class)))
         (length-var (gensym "length"))
         (buffer-var (gensym "buffer"))
         (frame-var (gensym "frame"))
         (version-package (symbol-package class))
         (connection-class (cons-symbol version-package :connection))
         (class.method-name (cons-symbol version-package class "." method-name))
         (amqp::class (cons-symbol :amqp class))
         (amqp::class.method-name (cons-symbol :amqp class.method-name))
         (decoded-operator (cons-symbol version-package :call-with-decoded- class.method-name :-arguments))
         (encoded-operator (cons-symbol version-package :call-with-encoded- class.method-name :-arguments))
         (unreserved-arguments nil))
    (push `(export ',class.method-name (symbol-package ',class.method-name)) exports)
    (flet ((coerce-slot-name (sd)
             (if (eq (getf (rest sd) :allocation) :class)
               sd
               (cons (cons-symbol :amqp (first sd)) (rest sd)))))
      (setf arguments (mapcar #'coerce-slot-name arguments)))
    (setf unreserved-arguments (remove-if #'(lambda (sd) (search "reserved" (string (first sd)) :test #'char-equal)) arguments))
    `(progn
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (defclass ,class.method-name (,amqp::class.method-name ,@supers)
           ,(list* `(argument-slot-names :initform ',(mapcar #'first arguments)
                                         :allocation :class)
                   ;; initialize to quoted name; it's defined later
                   `(request-function :initform ',(cons-symbol :amqp :channel-request- amqp:method-name) ) ;:allocation :class)
                   `(response-function :initform (response-function ',(cons-symbol :amqp :channel-respond-to- amqp:method-name))
                                       :allocation :instance)
                   `(name :initform ',amqp:method-name :allocation :class)
                   '(header :initform nil)
                   '(data :initform nil)
                   (append slots
                           (mapcar #'(lambda (sd)
                                       (destructuring-bind (slot-name &key accessor reader writer
                                                                      &allow-other-keys)
                                                           sd
                                         (list* slot-name
                                                :initarg (cons-symbol :keyword slot-name)
                                                (if (or accessor reader writer)
                                                  (rest sd)
                                                  (let ((accessor (cons-symbol :amqp amqp:method-name :- slot-name)))
                                                    (push `(export ',accessor :amqp) exports)
                                                    `(:accessor ,accessor ,@(rest sd)))))))
                                   arguments)))
           ,@options))
       (c2mop:ensure-class ',amqp::class.method-name :direct-superclasses '(,amqp:method-name))
       ;; (format *trace-output* "~&method defined: ~s" ',class.method-name)
       ;; (pushnew ',class.method-name (amqp:class-methods (find-class ',class)))
       ,@(nreverse exports)
       
       ;; initialize unspecified slots from the respective class
       ,@(when unreserved-arguments
           `((defmethod class-initialize-method ((class ,class) (method ,class.method-name) &rest initargs &key
                                                 ,@(mapcar #'(lambda (sd)
                                                               (destructuring-bind (name &key &allow-other-keys) sd
                                                                 `(,name (slot-value class ',name))))
                                                           unreserved-arguments))
               (declare (dynamic-extent initargs))
               (apply #'call-next-method class method
                      ,@(apply #'nconc (mapcar #'(lambda (sd)
                                                   (destructuring-bind (name &key &allow-other-keys) sd
                                                     `(,(cons-symbol :keyword name) ,name)))
                                               unreserved-arguments))
                      initargs))))
       
       ;; needs to be the specific, class.method combined name
       (defmethod class-method-code-method-name ((class ,class) (method-code (eql ,method-code)))
         ',class.method-name)
       
       (defmethod class-method-name-method-code ((class ,class) (method-name (eql ',amqp:method-name)))
         ,method-code)
       
       (defmethod connection-method-code-method-name ((connection ,connection-class)
                                                      (class-name (eql ',amqp::class))
                                                      (method-code (eql ,method-code)))
         ',amqp:method-name)
       
       (defmethod connection-method-name-method-code ((connection ,connection-class)
                                                 (class-name (eql ',amqp::class))
                                                 (method-code (eql ',amqp:method-name)))
         ',method-code)
       
       ;; designators map to the generic name, but return the concrete class
       (defmethod amqp:ensure-method ((class ,class) (method-code (eql ,method-code)) &rest initargs)
         (declare (dynamic-extent initargs))
         (apply #'amqp:ensure-method class ',amqp::method-name initargs))
       
       
       (defmethod class-find-method-class ((class ,class) (method-code (eql ,method-code)))
         (find-class ',class.method-name))
       #+(or )                          ; map just the general, not the context-specific
       (defmethod class-find-method-class ((class ,class) (concrete-class (eql ',amqp::class.method-name)))
         (find-class ',class.method-name))
       
       (defmethod class-find-method-class ((class ,class) (concrete-class (eql ',amqp::method-name)))
         (find-class ',class.method-name))
       
       
       ;; decoding
       
       (defmethod call-with-decoded-arguments (op (class ,class) (id (eql ,method-code)) buffer &rest args)
         (declare (dynamic-extent args))
         (apply #',decoded-operator op class (amqp:ensure-method class ',amqp::class.method-name) buffer
                args))
       
       (defmethod call-with-decoded-arguments (op (class ,class) (id (eql ',amqp:method-name)) buffer &rest args)
         (declare (dynamic-extent args))
         (apply #',decoded-operator op class (amqp:ensure-method class ',amqp::class.method-name) buffer
                args))
       
       (defmethod call-with-decoded-arguments (op (class ,class) (method ,class.method-name) buffer &rest args)
         (declare (dynamic-extent args))
         (apply #',decoded-operator op class method buffer
                args))
       
       (defun ,decoded-operator (op class method buffer &rest args) 
         (declare (dynamic-extent args)
                  (type frame-buffer buffer))
         (assert-argument-type ,decoded-operator buffer frame-buffer)
         (let ,(mapcar #'(lambda (sd) `(,(first sd) nil)) arguments)
           ,@(let ((unusables (remove nil (mapcar #'(lambda (sd)
                                                      (let ((name (first sd)))
                                                        (when (search "reserved" (string name) :test #'char-equal)
                                                          name)))
                                                  arguments))))
               (when unusables `((declare (ignorable ,@unusables)))))
           (with-argument-decoders (buffer :start (method-argument-offset method))
             ,@(let ((bits 0) (forms nil) (last-bits nil))
                 (mapcar #'(lambda (sd)
                             (destructuring-bind (name &key (type (error "No type present: ~s . ~s" class sd))
                                                       &allow-other-keys) sd
                               (cond ((or (eq type 'amqp:bit)
                                          (get type 'amqp:bit))
                                      (setf last-bits `(setf ,name (amqp:bit ,bits)))
                                      (push last-bits forms)
                                      (incf bits))
                                     ;; nb. must (at least initially) decode the reserveds
                                     ;; in order to maintain the position
                                     (t
                                      (when last-bits
                                        (setf (cdr (last (third last-bits))) (list t))
                                        (setf bits 0 last-bits nil))
                                      (push `(setf ,name (amqp:field ,type)) forms)))))
                         arguments)
                 (when last-bits
                   (setf (cdr (last (third last-bits))) (list t))
                   (setf bits 0 last-bits nil))
                 (nreverse forms)))
           (apply op class method
                  ,@(apply #'nconc
                           (mapcar #'(lambda (sd)
                                       (destructuring-bind (name &key (initarg (intern (string name) :keyword))
                                                                 &allow-other-keys) sd
                                         (unless (search "reserved" (string name) :test #'char-equal)
                                           `(,initarg ,name))))
                                   arguments))
                  args)))
       
       
       ;; encode and send method application to server
       ,(let ((sender (cons-symbol :amqp :send- amqp:method-name)))
          `(progn
             (eval-when (:compile-toplevel :load-toplevel :execute)
               (export ',sender :amqp))
             (defmethod ,sender ((class ,class) &rest args)
               (declare (dynamic-extent args))
               ;; must use the generic name, to permit caching
               (apply #'send-method ',amqp:method-name class args))))
       
       
       ;; encoding
       
       (defmethod call-with-encoded-arguments (op (class ,class) (id (eql ,method-code)) &rest args)
         (declare (dynamic-extent args))
         (apply #',encoded-operator op class (amqp:ensure-method class ',amqp::method-name) args))
       
       (defmethod call-with-encoded-arguments (op (class ,class) (name (eql ',amqp:method-name)) &rest args)
         (declare (dynamic-extent args))
         (apply #',encoded-operator op class (amqp:ensure-method class ',amqp::method-name) args))
       
       (defmethod call-with-encoded-arguments (op (class ,class) (method ,class.method-name) &rest args)
         (declare (dynamic-extent args))
         (apply #',encoded-operator op class method args))
       
       (defun ,encoded-operator (op class method &key ,@(mapcar #'(lambda (sd)
                                                                    (destructuring-bind (name &key initform &allow-other-keys) sd
                                                                      (if (search "reserved" (string name) :test #'char-equal)
                                                                        `(,name ,initform ,(cons-symbol (symbol-package name) name :-s))
                                                                        `(,name (,(cons-symbol :amqp class "-" name) class)))
                                                                      #+(or )
                                                                      (if (search "reserved" (string name) :test #'char-equal)
                                                                        `(,name (slot-value class ',name) ,(cons-symbol (symbol-package name) name :-s))
                                                                        `(,name (slot-value class ',name)))))
                                                                arguments))
         ;; generate an form to encode the argument fields into the buffer.
         ;; arguments are defaulted to the class instance's fiedl values
         ;; bit fields are combined when contiguous
         ,@(let ((reserved-names (remove nil (mapcar  #'(lambda (sd)
                                                          (destructuring-bind (name &key &allow-other-keys) sd
                                                            (when (search "reserved" (string name) :test #'char-equal)
                                                              (list name (cons-symbol (symbol-package name) name :-s)))))
                                                      arguments))))
             (loop for (var var-s) in reserved-names
                   collect `(when ,var-s
                              (error ,(format nil "~s is reserved and may not be specified." var)))))
         (amqp:log :debug class "encoding: ~a . (~@{~s~^ ~})"
                   ',class.method-name
                   ,@(reduce #'append (mapcar #'(lambda (sd)
                                                  (destructuring-bind (name &key &allow-other-keys) sd
                                                    (unless (search "reserved" (string name) :test #'char-equal)
                                                      `(,(cons-symbol :keyword name) ,name))))
                                              arguments)))
         (let* ((,length-var 0)
                (,frame-var (claim-output-frame class))
                (,buffer-var (frame-data ,frame-var)))
           (declare (ignorable ,buffer-var))
           (setf ,length-var
                 (with-argument-encoders (,buffer-var :start (method-argument-offset method))
                   ,@(let ((bits 0) (forms nil) (last-bit nil))
                       (mapcar #'(lambda (sd)
                                   (destructuring-bind (name &key type &allow-other-keys) sd
                                     (cond ((or (eq type 'amqp:bit)
                                                (get type 'amqp:bit))
                                            (setf last-bit (list 'amqp:bit name bits))
                                            (push last-bit forms)
                                            (incf bits))
                                           (t
                                            (when last-bit
                                              (setf (cdr (last last-bit)) (list t))
                                              (setf bits 0 last-bit nil))
                                            (push `(amqp:field ,name ,type) forms)))))
                               arguments)
                       (when last-bit (setf (cdr (last last-bit)) (list t))
                             (setf bits 0 last-bit nil))
                       (nreverse forms))))
           (setf-frame-type-class-name 'amqp:method ,frame-var)
           (setf-frame-channel-number (channel-number class) ,frame-var)
           (setf-frame-track-number (channel-track class) ,frame-var)
           (setf-frame-size ,length-var ,frame-var)
           (setf-frame-class-code (amqp:class-id class) ,frame-var)
           (setf-frame-method-code ,method-code ,frame-var)
           (funcall op ,frame-var class method)))
       
       )))

#+mcl
;;; 20100214: sbcl-1.0.35 decided today, that this passage shouldn't modify the "standard pprint dispatch table"
;;; ok. hmmm. as the purpose of this is to print them reasonably for top-level debugging, which happens
;;; in mcl, that's the way it is.
(progn
  (defun pprint-def-amqp-method (xp list &rest args)
    (declare (ignore args))
    (funcall (formatter "~:<~1I~W~^ ~@_~W~^ ~@_~:/pprint-fill/~^ ~@:_~:/pprint-fill/~^ ~@:_(~{(~{~s~@{~%~3t~s ~s~}~})~^~%~2t~})~^~@{ ~_~W~^~}~:>")
	     xp list))

  (set-pprint-dispatch '(cons (member def-amqp-class)) (pprint-dispatch '(defclass) nil))
  (set-pprint-dispatch '(cons (member def-amqp-method)) 'pprint-def-amqp-method))


(defmacro def-amqp-command (name lambda-list &rest options)
  "Define the generic class and operator for an amqp method.

 NAME : symbol : The abstract protocol method name. Each version specializes it
  to implement the specific codecs and behaviour. It serves to define an
  amqp:method specialization and a generic function.
 LAMBDA-LIST : list : The lambda specifies generic function arguments. The
  initial, required argument serves to specialze the method's class. The
  remainder, keyword arguments, comprise the union of the fields from all
  versions' methods.

 The operator implements the protocol behaviour for a class to perform a
 method. When applied to an input command stream, the static definition may
 be combined with others, as a sequence of filters. In those cases, processing
 continues until some operator returns a true value.

 NB. As this macro constructs and exports the operator names on-the-fly, any cross-references -
 send-*, in particular must be coded as internal symbols for the first compilation to succeed."
  
  (let* ((class-var (first lambda-list))
         (qualified-lambda-list (cons 'channel lambda-list))
         (response nil)
         (response-op nil)
         (qualified-response nil)
         (qualified-response-op nil)
         (request nil)
         (request-op nil)
         (qualified-request nil)
         (qualified-request-op nil)
         (send nil)
         (send-op nil)
         (doc nil)
         (exports ()))

    ;; collect the operator names, based on the clauses present
    ;; if there is a respose definition, ensure a send.
    (dolist (option options)
      (destructuring-bind (keyword . option-value) option
        (case keyword
          (:documentation
           (setf doc option))
          (:send
           (setf send option-value
                 send-op (or send-op (cons-symbol :amqp :send- name))))                 
          (:request
           (setf request option-value
                 request-op (cons-symbol :amqp :request- name)
                 qualified-request-op (cons-symbol :amqp.i :channel- request-op)
                 send-op (or send-op (cons-symbol :amqp :send- name))))
          (:response
           (setf response option-value
                 response-op (cons-symbol :amqp :respond-to- name)
                 qualified-response-op (cons-symbol :amqp.i :channel- response-op)))
          (t
           (error "Option not permitted in command definition: ~s, ~s."
                  keyword name)))))
           
    (when send-op
      (unless send
        (setf send `((:method (,class-var &rest args)
                       (declare (dynamic-extent args))
                       (apply #'send-method ',name ,class-var args)))))
      (unless (assoc :documentation send)
        (push `(:documentation ,(format nil "A convenience send operator for ~a." name))
              send)))

    (when request-op
      (unless (assoc :documentation request)
        (push `(:documentation ,(format nil "The base protocol request operator for ~a." name))
              request))
      (setf qualified-request
            (list* (first request)      ; the documentation
                   `(:method :before ((channel t) (class t) &rest args)
                             "A before method logs the request-to-be and updates the class instance."
                             (declare (dynamic-extent args))
                             (amqp:log* ,request-op class args))
                   (mapcar #'(lambda (method)
                               (destructuring-bind (keyword parameters &rest body) method
                                 `(,keyword ((amqp:channel amqp:channel) ,@parameters)
                                   ,@body)))
                           (rest request)))))

    (when response-op
      (unless (assoc :documentation response)
        (push `(:documentation ,(format nil "The base protocol response operator for ~a." name))
              response))
      (setf qualified-response
            (list* (first response)
                   `(:method :before ((channel t) (class t) &rest args)
                             "A before method logs the response-to-be and updates the class instance."
                             (declare (dynamic-extent args))
                             (amqp:log* ,response-op class args))
                    (mapcar #'(lambda (method)
                               (destructuring-bind (keyword parameters &rest body) method
                                 `(,keyword ((amqp:channel amqp:channel) ,@parameters)
                                   ,@body)))
                            (rest response)))))

    (setf exports (remove nil (list name send-op response-op request-op)))
    (export exports :amqp)
    `(progn (defclass ,name (amqp:method)
              ((name :initform ',name :allocation :class)
               (request-function :initform ',qualified-request-op :allocation :class)
               (header :initform nil :allocation :class)
               (data :initform nil :allocation :class))
              ,@(when doc (list doc)))
       (eval-when (:compile-toplevel :load-toplevel :execute)
         (export ',exports :amqp))
       ,@(when send-op
           `((defgeneric ,send-op ,lambda-list
               ,@send)))
       ,@(when response-op
           `((defgeneric ,qualified-response-op ,qualified-lambda-list
               ,@qualified-response)

             (defun ,response-op (class &rest args)
               (declare (dynamic-extent args))
               (apply #',qualified-response-op (object-channel class) class args))))
       ,@(when request-op
           `((defgeneric ,qualified-request-op ,qualified-lambda-list
               ,@qualified-request)
             (defun ,request-op (class &rest args)
               (declare (dynamic-extent args))
               (apply #',qualified-request-op (object-channel class) class args))
             ;; this claims the simple name for the request operator
             ;; note that the approach precludes the simple constructor
             (defgeneric ,name (class &rest args)
               (:method ((object amqp:object) &rest args)
                 (declare (dynamic-extent args))
                 (apply ',request-op object args))))))))
