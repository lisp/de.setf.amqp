;;; -*- Package: de.setf.amqp.implementation; -*-

(in-package :de.setf.amqp.implementation)


(:documentation "This file generates protocol classes and wire-level codecs for AMQP based on the
  xml-encoded protocol specification."

 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)"
  "'setf.amqp' is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  'setf.amqp' is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with 'setf.amqp'.  If not, see the GNU [http://www.gnu.org/licenses/ site].")

 (long-description "Two operators translate the AMQP specification into common lisp:
 - generate-amqp-definitions (source destination &key package verbose-p version)
   Transfroms an xml specification into lisp class and method definitions.
 - amqp-specification-model (specification version)
   Performs the single step of extracting the class/method definitions from a dom."))


;;; with-package
;;; amqp-specification-version
;;; amqp-specification-model (sppecification version)
;;; 

(defparameter *canonical-field-name-map*
  '((noack . no-ack) (nowait . no-wait)))

(defmacro with-package (designator &body body)
  `(let ((*package* (or (find-package ,designator) (error "invalid package: ~s." ,designator))))
     ,@body))

(defparameter *license*
  '(:documentation :file
 (description "This file contains generated protocol classes and wire-level codecs for AMQP based on the
  xml-encoded protocol specification.")
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)"
  "'setf.amqp' is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  'setf.amqp' is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with 'setf.amqp'.  If not, see the GNU [http://www.gnu.org/licenses/ site].")))



(defgeneric generate-amqp-definitions (source destination &key package verbose-p version)
  (:documentation "Generate AMQP wire-level implementation(s) given as SOURCE
 the specification(s) and as DESTINATION as source location.
 SOURCE : pathname : to designate an xml specification or a directory of same
 DESTINATION : pathname : to designate the source file location(s)
 :PACKAGE : (designator string) : designates the source package
 :VERBOSE-P : boolean : enable trace output
 :VERSION : keyword : overrides the specification file's indicated version
 VALUES : keyword : the specification version
        : pathname : the destination pathname

 Transfroms the specification model from the xml expression through a document
 object model, into class and method definitions, and writes that to file/stream a destination.
 Implemented as a network of methods, each of which performs an individual step in the transformation.

 Each respective specification version is expressed in a slightly different
 xml schema. The common features are captured in a specification model of
 the form
   (:version version
    :classes ((:class (def-amq-class name ...)  ; class definition
               :methods ((def-amq-method name ...)  ; method definitions
                         ...))
              ...))
 This model is then output to the given destination with appropriate
 file documentation.

 Where the destination names a directory, the result is a list of the
 respective results.")
  
  (:method ((source pathname) (destination t) &rest args
            &key (verbose-p nil) &allow-other-keys)
    "Given a pathname source, given a directory, iterate over the xml
 specification files and generate the source for each. If it is an xml file,
 parse it accordingly, and generate the source from that model."

    (cond  ((directory-pathname-p source)
            (when verbose-p
              (format *trace-output* "~&; generate AMQP definitions~%;;;~:[ from ~a to ~a~; in ~a~]"
                      (equalp source destination) (truename source) (truename destination)))
            (mapcar #'(lambda (specification)
                        (multiple-value-list
                         (apply #'generate-amqp-definitions specification destination args)))
                    (directory (make-pathname :name :wild :type "xml" :defaults source))))
           (t
            (when verbose-p
              (format *trace-output* "~&;;; generate AMQP definitions from ~a"
                      (truename source)))
            (apply #'generate-amqp-definitions (xmlp:document-parser source) destination args))))
  
  (:method ((source xqdm:doc-node) (destination t) &rest args
            &key (package :de.setf.amqp.implementation) (verbose-p nil)
            (version (amqp-specification-version source) version-p))
    "Given a parsed specification document, determine the version if necessary,
 extract the model for the respective document structure, and generate the
 source for that model."
    (when verbose-p
      (format *trace-output* "~&;;; generate AMQP definitions for version ~a ~:[(from spec)~;(overridden)~]"
              version version-p))
    (apply #'generate-amqp-definitions (with-package package (amqp-specification-model source version))
           destination
           args))
  
  (:method ((source cons) (destination pathname) &rest args)
    (when (directory-pathname-p destination)
      (setf destination (merge-pathnames (make-pathname :name "classes"
                                                        :type "lisp"
                                                        :directory `(:relative ,(string (getf source :version))))
                                         destination)))
    (ensure-directories-exist destination)
    (with-open-file (stream destination :direction :output
                            :if-exists :supersede
                            :if-does-not-exist :create)
      (apply #'generate-amqp-definitions source stream args)))
  
  (:method ((model cons) (destination stream)
            &key (package :de.setf.amqp.implementation) (verbose-p nil)
            &allow-other-keys)
    (when (or verbose-p *compile-verbose*)
      (format *trace-output* "~&;;; generate AMQP definitions to ~:[?~;~:*~a~]"
              (ignore-errors (truename destination))))
    (let ((*print-case* :downcase)
          (*print-right-margin* 120))
      (with-package package
        (destructuring-bind (&key version classes package version-list) model
          (format destination ";;; -*- Package: ~a; -*-~%" (package-name *package*))
          (format destination ";;;~%")
          (format destination ";;; version: ~a~%;;; generated ~a"
                  version (date:iso (get-universal-time)))
          (format destination "~%~%(in-package ~s)" (package-name *package*))
          (format destination "~%~:w~%" *license*)
          (let ((amqp-exports ()) (version-exports ()))
            (map nil  #'(lambda (c)
                          ;; class names should have exported homonyms in :amqp and the version's package
                          (pushnew (second (getf c :class)) amqp-exports)
                          (pushnew (second (getf c :class)) version-exports)
                          ;; abstract methods should be external in amqp only
                          ;; version class.method should be external in the version package only
                          (dolist (m (getf c :methods))
                            (destructuring-bind ((class method-name) (method-function-name &rest supers) . ignore) (rest m)
                              (declare (ignore supers ignore))
                              (pushnew method-function-name amqp-exports)
                              (pushnew (cons-symbol nil class "." method-name) version-exports))))
                 classes)
            (format destination "~%~%(eval-when (:compile-toplevel :load-toplevel :execute)~%  (macrolet ((ensure-export (x) `(export (intern ,(string x) :amqp) :amqp))) ~{~%    (ensure-export #:~a)~}))"
                    (sort amqp-exports #'string-lessp :key #'string))
            #+ignore
            (format destination "~%~%(eval-when (:compile-toplevel :load-toplevel :execute)~%  (macrolet ((import-export (x) `(let ((sym (find-symbol ,(string x) :amqp))) (import sym :~a) (export sym :~a)))) ~{~%    (import-export #:~a)~}))"
                    package package transports)
            (format destination "~%~%(eval-when (:compile-toplevel :load-toplevel :execute)~%  (macrolet ((ensure-export (x) `(export (intern ,(string x) :~a) :~a))) ~{~%    (ensure-export #:~a)~}))"
                    package package
                    (sort version-exports #'string-lessp :key #'string)))

          #+(or )
          (progn ;; these are defined in abstract-classes.lisp to permit use of
                 ;; the object class as the mixin there, rather than requiring
                 ;; a definition for the concrete connection class
            (format destination "~%~%(defclass ~a:object (amqp:object)~%  ((version~%   :initform '~s~%   :allocation :class)))"
                    version version-list)
            (format destination "~%~%(defclass ~a:method (amqp:method) ())" version))
          (map nil #'(lambda (definition)
                       (destructuring-bind (&key class methods) definition
                         (destructuring-bind (defclass name supers slots &rest options) class
                           (declare (ignore defclass supers options))
                           (flet ((describe-definitions (stream)
                                    (format stream "~%~%;;; class: ~a~@[ [~{~a~^ ~}]~]~{~{~%;;;   ~a~@[ [~{~a~^ ~}]~]~}~}"
                                            name
                                            (mapcar #'first slots)
                                            (mapcar #'(lambda (method)
                                                        (destructuring-bind (defclass (class method) supers slots arguments &rest options) method
                                                          (declare (ignore defclass supers slots options))
                                                          `(,(cons-symbol nil class "." method) ,(mapcar #'first arguments))))
                                                    methods))))
                             (describe-definitions destination)
                             (when verbose-p
                               (describe-definitions *trace-output*)))
                           (format destination "~%~%~:w~%~{~%~%~:w~}" class methods))))
               classes)
          (values version (ignore-errors (pathname destination))))))))



(defmethod amqp-specification-version ((source xqdm:doc-node))
  "Extract a revision identifier from the document.
 This comprises the four fields required in the version specification in the
 wire-level protocol header _plus_ thing which have appeared in the
 specification documents. note that 0-8 documents exhibit bogus major/minor
 values and are presumed to refer to version 0-8-0"

  (flet ((attribute-value (node name &optional (default nil default-p))
           (let ((attribute (xqdm:./@-value node name)))
             (cond (attribute (parse-integer attribute))
                   (default-p default)
                   (t (error "attribute missing: ~s" name))))))
    (let* ((root (xqdm:root source)))
      (if (string-equal (xqdm:./@-value root "comment") "AMQ protocol 0.80")
        :amqp-1-1-0-8-0
        (let ((class (attribute-value root "class" 1))
              (instance (attribute-value root "instance" 1))
              (major (attribute-value root "major"))
              (minor (attribute-value root "minor"))
              (revision (attribute-value root "revision" 0)))
          (values (make-version-keyword :name (xqdm:local-part (xqdm:name root))
                                        :class class
                                        :instance instance
                                        :major major
                                        :minor minor
                                        :revision revision)
                  (list class instance major minor revision)))))))

(labels ((compact-string (string)
           (let ((buffer (make-array (length string) :element-type 'character :fill-pointer 0))
                 (eol-count 0))
             (do ((i 0 (1+ i))
                  (last #\null))
                 ((>= i (length string))
                  (substitute #\' #\" (string-trim #(#\tab #\space #\return #\linefeed) buffer)))
               (case last
                 (#\space
                  (unless (char= #\space (setf last (char string i)))
                    (vector-push last buffer)))
                 ((#\linefeed #\return)
                  (case (setf last (char string i))
                    (#\space (setf last #\return))
                    ((#\linefeed #\return )
                     (when (= eol-count 1)
                       (vector-push last buffer)
                       (incf eol-count)))
                    (t (vector-push #\space buffer) (vector-push last buffer))))
                 (t
                  (vector-push (setf last (char string i)) buffer)
                  (case last
                    ((#\linefeed #\return) (setf eol-count 1))
                    (t (setf eol-count 0))))))))
         (amqp-role-implementations (node element-name role-attribute status-attribute)
                 ;; as of 0.10 the encoding changed 
                 (mapcar #'(lambda (node)
                             (let* ((role (xqdm:./@-value node role-attribute))
                                    (status (xqdm:./@-value node status-attribute)))
                               `(,role ,(or status "-"))))
                         (xqdm:./* node element-name)))
         (amqp-class-name (name package)
           ;; abstract class names are exported from the general interface package
           (if (eql package :amqp)
             (intern (string-upcase name) package)
             ;; but version-specific class names areexported from the version package only
             (let ((symbol (intern (string-upcase name) package)))
               (export symbol package)
               symbol)))
         (amqp-method-name (class-name name package)
           ;; method names are exported from the general interface package and
           ;; transported through the version package
           (intern (format nil "~:@(~a.~a~)" class-name name) package))
         (amqp-field-name (name &optional (package :de.setf.amqp.implementation))
           (setf name (or (rest (assoc name *canonical-field-name-map* :test #'string-equal))
                          (string-upcase name)))
           (intern (substitute #\- #\space (string name)) package))
         (amqp-type-name (name &optional (package :de.setf.amqp))
           (intern (string-upcase (substitute #\- #\space name)) package))
         (amqp-field-slot-definition  (field-node package)
           (let ((name (amqp-field-name (xqdm:./@-value field-node "name")))
                 ;; 0.9 used domain rather than type
                 (type (amqp-type-name (or (xqdm:./@-value field-node "domain") (xqdm:./@-value field-node "type"))
                                       package))
                 (doc (compact-string (format nil "~@[~a~%~]  ~a"
                                              (xqdm:./@-value field-node "label")
                                              (or (xqdm:./-value field-node "doc")
                                                  (xqdm:value-string field-node))))))
             `(,name :initform (field-type-initform ,name ,type)
                     ,@(when type `(:type ,type))
                     ,@(when (plusp (length doc)) `(:documentation ,doc))
                     ,@(when (search "reserved" (string name) :test #'char-equal) '(:allocation :class))))))


  (defgeneric amqp-specification-model (specification version)
    (:documentation "Extract the class definitions from the specification document
 presuming the given format.
 SPECIFICATION : xmlp:doc-node : a specification document instance, parsed from
  an AMQP xml specification.
 VERSION : (or (member t) keyword) : designates the protocol version
   (see amqp-specification-version)
  VALUES : list : a property list which describes the specification components

 If the protocol version is not specified, it is infered from the document root
 element. The version is synonymous with the package into which all names are interned.

 The various versions' specifications exhibit various document structures. each is
 implemented in its own method. In general:
 - reserved fields are included
 - it is presumed that methods have the same signature and operator names can
   be common between versions.

")
    
    (:method ((source xqdm:doc-node) (version (eql t)))
      (amqp-specification-model source (amqp-specification-version source)))
    
    (:method ((source xqdm:doc-node) (version t))
      (error "Unsupported AMQP version: '~a'" version))
    
    (:method ((source xqdm:doc-node) (version (eql :AMQP-1-1-0-8-0)))
      (let* ((package (or (find-package version)
                          (make-package version :use ())))
             (object-symbol (intern (string :object) package))
             (method-symbol (intern (string :method) package)))
        (export (list object-symbol method-symbol) package)
        (labels ((amqp-element-id (node)
                   (parse-integer (xqdm:./@-value node "index"))))
          `(:version ,version
                     :version-list ,(nth-value 1 (amqp-specification-version source))
                     :package ,version
                     :classes ,(mapcar #'(lambda (class-node)
                                           (let ((class-name (amqp-class-name (xqdm:./@-value class-node "name") version)))
                                             `(:class (def-amqp-class ,class-name
                                                        (,object-symbol ,(amqp-class-name (xqdm:./@-value class-node "name") :amqp))
                                                        ((id :initform ,(amqp-element-id class-node)
                                                             :allocation :class)
                                                         (method-names :initform ',(mapcar #'(lambda (method-node)
                                                                                               (cons-symbol :amqp (xqdm:./@-value method-node "name")))
                                                                                           (xqdm:.//* class-node "method"))
                                                                       :allocation :class))
                                                        ;; properties
                                                        ,(mapcar #'(lambda (field)
                                                                     (amqp-field-slot-definition field version))
                                                                 (xqdm:./* class-node "field"))
                                                        ;; method arguments
                                                        ,(remove-duplicates 
                                                          (apply 'nconc
                                                                 (mapcar #'(lambda (method-node)
                                                                             (mapcar #'(lambda (field)
                                                                                         (amqp-field-slot-definition field version))
                                                                                     (xqdm:./* method-node "field")))
                                                                         (xqdm:.//* class-node "method")))
                                                          :key #'first)
                                                        (:documentation
                                                         ,(format nil "roles:~@[~{~{ ~a ~:[-~;~:*~a~]~}~^;~}.~]~%~{~a~}"
                                                                  (amqp-role-implementations class-node "chassis" "name" "implement")
                                                                  (mapcar #'xqdm:value-string (xqdm:./* class-node "doc")))))
                                                      :methods ,(mapcar
                                                                 #'(lambda (method-node)
                                                                     (let ((name (xqdm:./@-value method-node "name")))
                                                                       (setf name (cons-symbol :amqp name))
                                                                       `(def-amqp-method (,class-name ,name)
                                                                          ;; the test class has method naes which coincide with types.
                                                                          ;; if this turns onerous, they universal types will need their own package
                                                                          (,(cons-symbol :amqp  (when (string-equal class-name "test") :test-) name)
                                                                           ,method-symbol)
                                                                          ((id :initform ,(amqp-element-id method-node)))
                                                                          ,(mapcar #'(lambda (field)
                                                                                       (amqp-field-slot-definition field version))
                                                                                   (xqdm:./* method-node "field"))
                                                                          (:documentation
                                                                           ,(format nil "roles:~@[~{~{ ~a ~:[-~;~:*~a~]~}~^;~}.~]~@[~%~a~]"
                                                                                    (amqp-role-implementations method-node "chassis" "name" "implement")
                                                                                    (xqdm:./-value method-node "doc"))))))
                                                                 (xqdm:.//* class-node "method")))))
                                       (xqdm:.//* source "class"))))))
    
    (:method ((source xqdm:doc-node) (version (eql :AMQP-1-1-0-9-0)))
      (let* ((package (or (find-package version)
                          (make-package version :use ())))
             (object-symbol (intern (string :object) package))
             (method-symbol (intern (string :method) package)))
        (export (list object-symbol method-symbol) package)
        (labels ((amqp-element-id (node)
                   (parse-integer (xqdm:./@-value node "index"))))
          `(:version ,version
                     :version-list ,(nth-value 1 (amqp-specification-version source))
                     :package ,version
                     :classes ,(mapcar #'(lambda (class-node)
                                           (let ((class-name (amqp-class-name (xqdm:./@-value class-node "name") version)))
                                             `(:class (def-amqp-class ,class-name
                                                        (,object-symbol ,(amqp-class-name (xqdm:./@-value class-node "name") :amqp))
                                                        ((id :initform ,(amqp-element-id class-node)
                                                             :allocation :class)
                                                         (method-names :initform ',(mapcar #'(lambda (method-node)
                                                                                               (cons-symbol :amqp (xqdm:./@-value method-node "name")))
                                                                                           (xqdm:.//* class-node "method"))
                                                                       :allocation :class))
                                                        ;; properties
                                                        ,(mapcar #'(lambda (field)
                                                                     (amqp-field-slot-definition field version))
                                                                 (xqdm:./* class-node "field"))
                                                        ;; method arguments
                                                        ,(remove-duplicates 
                                                          (apply 'nconc
                                                                 (mapcar #'(lambda (method-node)
                                                                             (mapcar #'(lambda (field)
                                                                                         (amqp-field-slot-definition field version))
                                                                                     (xqdm:./* method-node "field")))
                                                                         (xqdm:.//* class-node "method")))
                                                          :key #'first)
                                                        (:documentation
                                                         ,(format nil "roles:~@[~{~{ ~a ~:[-~;~:*~a~]~}~^;~}.~]~%~{~a~}"
                                                                  (amqp-role-implementations class-node "chassis" "name" "implement")
                                                                  (mapcar #'xqdm:value-string (xqdm:./* class-node "doc")))))
                                                      :methods ,(mapcar
                                                                 #'(lambda (method-node)
                                                                     (let ((name (xqdm:./@-value method-node "name")))
                                                                       (setf name (cons-symbol :amqp name))
                                                                       `(def-amqp-method (,class-name ,name)
                                                                          (,(cons-symbol :amqp  (when (string-equal class-name "test") :test-) name)
                                                                           ,method-symbol)
                                                                          ((id :initform ,(amqp-element-id method-node)))
                                                                          ,(mapcar #'(lambda (field)
                                                                                       (amqp-field-slot-definition field version))
                                                                                   (xqdm:./* method-node "field"))
                                                                          (:documentation
                                                                           ,(format nil "roles:~@[~{~{ ~a ~:[-~;~:*~a~]~}~^;~}.~]~@[~%~a~]"
                                                                                    (amqp-role-implementations method-node "chassis" "name" "implement")
                                                                                    (xqdm:./-value method-node "doc"))))))
                                                                 (xqdm:.//* class-node "method")))))
                                       (xqdm:.//* source "class"))))))
    
    (:method ((source xqdm:doc-node) (version (eql :AMQP-1-1-0-9-1)))
      (let* ((package (or (find-package version)
                          (make-package version :use ())))
             (object-symbol (intern (string :object) package))
             (method-symbol (intern (string :method) package)))
        (export (list object-symbol method-symbol) package)
        (labels ((amqp-element-id (node)
                   (parse-integer (xqdm:./@-value node "index"))))
          `(:version ,version
                     :version-list ,(nth-value 1 (amqp-specification-version source))
                     :package ,version
                     :classes ,(mapcar #'(lambda (class-node)
                                           (let ((class-name (amqp-class-name (xqdm:./@-value class-node "name") version)))
                                             `(:class (def-amqp-class ,class-name
                                                        (,object-symbol ,(amqp-class-name (xqdm:./@-value class-node "name") :amqp))
                                                        ((id :initform ,(amqp-element-id class-node)
                                                             :allocation :class)
                                                         (method-names :initform ',(mapcar #'(lambda (method-node)
                                                                                               (cons-symbol :amqp (xqdm:./@-value method-node "name")))
                                                                                           (xqdm:.//* class-node "method"))
                                                                       :allocation :class))
                                                        ;; properties
                                                        ,(mapcar #'(lambda (field)
                                                                     (amqp-field-slot-definition field version))
                                                                 (xqdm:./* class-node "field"))
                                                        ;; arguments
                                                        ,(remove-duplicates 
                                                          (apply 'nconc
                                                                 (mapcar #'(lambda (method-node)
                                                                             (mapcar #'(lambda (field)
                                                                                         (amqp-field-slot-definition field version))
                                                                                     (xqdm:./* method-node "field")))
                                                                         (xqdm:.//* class-node "method")))
                                                          :key #'first)
                                                        (:documentation
                                                         ,(format nil "roles:~@[~{~{ ~a ~:[-~;~:*~a~]~}~^;~}.~]~%~{~a~}"
                                                                  (amqp-role-implementations class-node "chassis" "name" "implement")
                                                                  (mapcar #'xqdm:value-string (xqdm:./* class-node "doc")))))
                                                      :methods ,(mapcar #'(lambda (method-node)
                                                                            (let ((name (xqdm:./@-value method-node "name")))
                                                                              (setf name (cons-symbol :amqp name))
                                                                              `(def-amqp-method (,class-name ,name)
                                                                                 (,(cons-symbol :amqp  (when (string-equal class-name "test") :test-) name)
                                                                                  ,method-symbol)
                                                                                 ((id :initform ,(amqp-element-id method-node)))
                                                                                 ,(mapcar #'(lambda (field)
                                                                                              (amqp-field-slot-definition field version))
                                                                                          (xqdm:./* method-node "field"))
                                                                                 (:documentation
                                                                                  ,(format nil "roles:~@[~{~{ ~a ~:[-~;~:*~a~]~}~^;~}.~]~@[~%~a~]"
                                                                                           (amqp-role-implementations method-node "chassis" "name" "implement")
                                                                                           (xqdm:./-value method-node "doc"))))))
                                                                        (xqdm:.//* class-node "method")))))
                                       (xqdm:.//* source "class"))))))
    
    (:method ((source xqdm:doc-node) (version (eql :AMQP-1-1-0-10-0)))
      (let* ((package (or (find-package version)
                          (make-package version :use ())))
             (object-symbol (intern (string :object) package))
             (method-symbol (intern (string :method) package)))
        (export (list object-symbol method-symbol) package)
        (labels ((amqp-element-id (node)
                   (parse-integer (xqdm:./@-value node "code") :start 2 :radix 16)))
          `(:version ,version
                     :version-list ,(nth-value 1 (amqp-specification-version source))
                     :package ,version
                     :classes ,(mapcar #'(lambda (class-node)
                                           (let ((class-name (amqp-class-name (xqdm:./@-value class-node "name") version)))
                                             `(:class (def-amqp-class ,class-name
                                                        (,object-symbol ,(amqp-class-name (xqdm:./@-value class-node "name") :amqp))
                                                        ((id :initform ,(amqp-element-id class-node) :allocation :class)
                                                         (method-names :initform ',(mapcar #'(lambda (method-node)
                                                                                               (cons-symbol :amqp (xqdm:./@-value method-node "name")))
                                                                                           (xqdm:.//* class-node "method"))
                                                                       :allocation :class))
                                                        ;; properties
                                                        ,(mapcar #'(lambda (field)
                                                                     (amqp-field-slot-definition field version))
                                                                 (xqdm:./* class-node "field"))
                                                        ;; arguments
                                                        ,(remove-duplicates 
                                                          (apply 'nconc
                                                                 (mapcar #'(lambda (method-node)
                                                                             (mapcar #'(lambda (field)
                                                                                         (amqp-field-slot-definition field version))
                                                                                     (xqdm:./* method-node "field")))
                                                                         (xqdm:.//* class-node "method")))
                                                          :key #'first)
                                                        (:documentation
                                                         ,(format nil "roles:~@[~{~{ ~a ~:[-~;~:*~a~]~}~^;~}.~]~%~{~a~}"
                                                                  (amqp-role-implementations class-node "role" "name" "implement")
                                                                  (mapcar #'xqdm:value-string (xqdm:./* class-node "doc")))))
                                                      :methods ,(mapcar #'(lambda (method-node)
                                                                            (let ((name (xqdm:./@-value method-node "name")))
                                                                              (setf name (cons-symbol :amqp name))
                                                                              `(def-amqp-method (,class-name ,name)
                                                                                 (,(cons-symbol :amqp  (when (string-equal class-name "test") :test-) name)
                                                                                  ,method-symbol)
                                                                                 ((id :initform ,(amqp-element-id method-node)))
                                                                                 ,(mapcar #'(lambda (field)
                                                                                              (amqp-field-slot-definition field version))
                                                                                          (xqdm:./* method-node "field"))
                                                                                 (:documentation
                                                                                  ,(format nil "roles:~@[~{~{ ~a ~:[-~;~:*~a~]~}~^;~}.~]~@[~%~a~]"
                                                                                           (amqp-role-implementations method-node "implement" "role" "handle")
                                                                                           (xqdm:./-value method-node "doc"))))))
                                                                        ;; as of .10, they're called 'controls'
                                                                        (append (xqdm:.//* class-node "control") (xqdm:.//* class-node "command"))))))
                                       (xqdm:.//* source "class"))))))))

  
;;;
;;; generation examples
;;;
;;; for everything
;;; (defparameter *spec-root* #p"LIBRARY:de;setf;amqp;specification;")
;;; (defparameter *source-root* #p"LIBRARY:de;setf;amqp;")
;;; (generate-amqp-definitions *spec-root* *source-root* :verbose-p t)


;;; individual steps for individual versions

;;; (defparameter *amqp-spec-0-8-0.xml* #p"LIBRARY:de;setf;amqp;specification;amqp0-8.xml")
;;; (generate-amqp-definitions *amqp-spec-0-8-0.xml* *source-root*)
;;; (defparameter *amqp-spec-0-8-0* (xmlp:document-parser *amqp-spec-0-8.xml*))
;;; (amqp-specification-version *amqp-spec-0-8-0*)
;;; (amqp-specification-model *amqp-spec-0-8-0* t)
;;; (multiple-value-bind (v path) (generate-amqp-definitions *amqp-spec-0-8-0* *spec-root*) (values v (load (compile-file path))))

;;; (defparameter *amqp-spec-0-9-0.xml* #p"LIBRARY:de;setf;amqp;specification;amqp0-9.xml")
;;; (generate-amqp-definitions *amqp-spec-0-9-0.xml* *source-root*)
;;; (defparameter *amqp-spec-0-9-0* (xmlp:document-parser *amqp-spec-0-9.xml*))
;;; (amqp-specification-version *amqp-spec-0-9-0*)
;;; (amqp-specification-model *amqp-spec-0-9-0* t)
;;; (multiple-value-bind (v path) (generate-amqp-definitions *amqp-spec-0-9-0* *spec-root*) (values v (load (compile-file path))))

;;; (defparameter *amqp-spec-0-9-1.xml* #p"LIBRARY:de;setf;amqp;specification;amqp0-9-1.xml")
;;; (generate-amqp-definitions *amqp-spec-0-9-1.xml* *source-root*)
;;; (defparameter *amqp-spec-0-9-1* (xmlp:document-parser *amqp-spec-0-9-1.xml*))
;;; (amqp-specification-version *amqp-spec-0-9-1*)
;;; (amqp-specification-model *amqp-spec-0-9-1* t)
;;; (multiple-value-bind (v path) (generate-amqp-definitions *amqp-spec-0-9-1* *source-root*) (values v (load (compile-file path))))

;;; (defparameter *amqp-spec-0-10.xml* #p"LIBRARY:de;setf;amqp;specification;amqp.0-10;amqp¶.0-10.xml")
;;; (generate-amqp-definitions *amqp-spec-0-10.xml* *source-root*)
;;; (defparameter *amqp-spec-0-10* (xmlp:document-parser *amqp-spec-0-10.xml*))
;;; (amqp-specification-version *amqp-spec-0-10*)
;;; (amqp-specification-model *amqp-spec-0-10* t)
;;; (generate-amqp-definitions *amqp-spec-0-10* *spec-root*)

