;;;
;;; 2010-02-08  janderson
;;;
;;; a production build template
;;; - it expects to be in the root directory of a source hierarchy with a tree structure
;;;   which mirrors the system name hierarchy.
;;; - if no asdf is present, it attempts to load it from net/common-lisp/asdf
;;; - it attempts to load de/setf/utility/pathname.lisp and de/setf/utility/asdf/hierarchical-names.lisp
;;;   in order to support systems which express dependencies as hierarchical names.
;;; it expects to find the pathname utilities and an asdf in the tree.

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;; universal comment-reader

(defun |universal-comment-reader| (stream char)
  (declare (ignore char))
  (loop (case (read-char stream nil nil)
          ((#\return #\linefeed nil) (return))
          (t )))
  (values))

#-abcl                                  ; the abcl reader does not do (values) correctly
(set-macro-character #\; '|universal-comment-reader|)
(shadow '(#:save-image #:save-system #:load-system #:leave-lisp #:print-backtrace) :cl-user)
#+cmu
(setq ext:*gc-verbose* nil)
(setq *load-verbose* (setq *compile-verbose* t))

(defparameter *build-init-pathname*
  (or *load-pathname*
      *load-truename*
      *default-pathname-defaults*
      (error "Indeterminate load pathname...")))
(setq *build-init-pathname* (truename *build-init-pathname*))

(when *load-verbose*
  (format *trace-output* "~%;Build root: ~s." *build-init-pathname*))

;;;
;;; load the relative asdf version for building images
;;; in a dev tree, this mens to go upwards to look for the production tree

(defun compile-and-load-file (source-pathname)
  (let ((binary-pathname (compile-file-pathname source-pathname)))
    (if (probe-file binary-pathname)
      (if (probe-file source-pathname)
        (if (> (file-write-date binary-pathname) (file-write-date source-pathname))
          (load binary-pathname)
          (load (compile-file source-pathname)))
        (load binary-pathname))
      (load (compile-file source-pathname)))))

(defparameter *asdf-pathname*
  (make-pathname :directory (append (pathname-directory *build-init-pathname*)
                                    '("net" "common-lisp" "asdf"))
                 :name "asdf" :type "lisp"
                 :defaults *build-init-pathname*))

(cond ((probe-file *asdf-pathname*)
         (when *load-verbose*
           (format *trace-output* "~&;Incorporating asdf anew from ~s." *asdf-pathname*))
         (compile-and-load-file *asdf-pathname*)
         #+ecl
         (compile-and-load-file (make-pathname :name "asdf-ecl" :defaults *asdf-pathname*)))
        (t
         (cerror "Continue anyway." "ASDF is missing: ~s." *asdf-pathname*)))


;;;
;;; incorporate support for hierarchical names
#+(or :clozure :allegro sbcl) ;; for now
(unless (fboundp (find-symbol (string :sysdef-hierarchical-search-function) :asdf))
  (loop for (path name) in '((("de" "setf" "utility") "package")
                             (("de" "setf" "utility") "pathnames")
                             (("de" "setf" "utility" "asdf") "hierarchical-names"))
        do (let ((pathname (make-pathname :directory (append (pathname-directory *build-init-pathname*) path)
                                          :name name :type "lisp"
                                          :defaults *build-init-pathname*)))
             (if (probe-file pathname)
               (compile-and-load-file pathname)
               (cerror "Continue anyway." "Hierarchical name component is missing: ~s." pathname)))))

(or (ignore-errors (logical-pathname-translations "LIBRARY"))
    (de.setf.utility:define-library-host (or *compile-file-pathname* *load-pathname*) "LIBRARY"))


;;; search first the dev sources, then production
;;; in order to load module cross-references, dedicated registration
;;; are unavoidable - check them last, in order that hierarchic system
;;; designators be generated as nicknames
(map nil
     #'(lambda (pathname)
         (setf pathname (make-pathname :name nil :type nil :defaults pathname))
         (when (#-clisp probe-file #+clisp probe-directory pathname)
           (pushnew (truename pathname) asdf:*central-registry* :test #'equalp)))
     (list (make-pathname :directory (append (pathname-directory *build-init-pathname*)
                                          '("asdf-registry"))
                          :defaults *build-init-pathname*)
           *build-init-pathname*))


;;; from asdf/test/script-support.lisp
(defun leave-lisp (&optional message (return 0))
  (when message
    (format *error-output* message))
  #+abcl
  (ext:quit :status return)
  #+allegro
  (excl:exit return)
  #+clisp
  (ext:quit return)
  #+(or cmu scl)
  (unix:unix-exit return)
  #+ecl 
  (si:quit return)
  #+gcl
  (lisp:quit return)
  #+lispworks
  (lispworks:quit :status return :confirm nil :return nil :ignore-errors-p t)
  #+(or openmcl mcl)
  (ccl::quit return)
  #+sbcl
  (sb-ext:quit :unix-status return)

  (error "Don't know how to quit Lisp; wanting to use exit code ~a" return))

(defun save-image (pathname &optional system)
  (when *load-verbose*
    (format *trace-output* "~&saving ~@[~a ~] image to ~a." system pathname))
  #+abcl
  (warn "cannot save images.")

  #+allegro
  (excl:dumplisp :name pathname)
  
  #+ccl
  (ccl:save-application pathname)

  #+clisp
  (ext:saveinitmem pathname)

  #+cmu
  (extensions:save-lisp pathname :load-init-file nil :site-init nil)

  #+ecl
  (progn
    (asdf:make-build system :type :fasl :monolithic t :move-here pathname)
    ;; still need to copy the file once it is found
    )

  #+lispworks
  (warn "cannot save images.")

  #+sbcl
  (sb-ext:save-lisp-and-die pathname)
  )

(defun save-system (&rest args) (apply #'save-image args))

(defun print-backtrace (&optional (stream *standard-output*))
  #+abcl
  (format stream "~&~{ ~a~%~}" (system:backtrace))

  #+allegro
  (let ((*terminal-io* stream))
    (tpl::zoom-print-stack nil nil))

  #+clozure
  (let ((ccl::*debug-io* stream))
    (ccl::print-call-history :process *current-process* :start-frame-number 0 :detailed-p t))

  #+clisp
  (system::print-backtrace :out stream :limit most-positive-fixnum)

  #+cmu
  (debug:backtrace most-positive-fixnum stream)
  
  #+ecl
  (let ((*standard-output* stream))
    (si::tpl-backtrace))

  #+lispworks
  (dbg::output-backtrace :bug-form stream)

  #+sbcl
  (sb-debug:backtrace most-positive-fixnum stream)
  )

(defun load-system (system)
  (handler-case (asdf:load-system system)
    (error (c)
           (warn "Build (~a) failed with error: ~a." system c)
           (cl-user::print-backtrace)
           (cl-user::leave-lisp "Build failed" 255))))

#|

;;; cl-ppcre
;;; http://weitz.de/files/cl-ppcre.tar.gz

;;; clx
cd /development/source/library/org/cl-http
ccl --no-init --load /development/source/library/build-init.lisp \
    --eval "(asdf:load-system :org.cl-http)"

(http::set-local-context "http://ip-10-251-70-19.ec2.internal:8082")
(http::initialize-server-authentication)
;; no! this does not set the context
;;(http::start-serving "ip-10-251-70-19.ec2.internal" 8082 :type :single)

;; locally
(http::start :hostname "yoda.setf.de" :port 8082 :type :not-yet)
(http::load-examples)
;; (http::local-context)
;; "http://yoda.setf.de:8082"
(http::start-serving "yoda.setf.de" 8082 :type :stupid-multi)

> Error: value NIL is not of the expected type STRING.
 implies the:
1 > (http::local-context)
NIL
1 > 

;;; de.setf.documentation
(load "/Development/Source/dev/Library/de/setf/utility/asdf/hierarchical-names.lisp")
(asdf:load-system :org.cl-http)
(asdf:load-system :de.setf.documentation)

|#


