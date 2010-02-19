;;;
;;; 2010-02-08  janderson
;;;
;;; sbcl build file for de.setf.amqp

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

;; universal comment-reader

(defun |universal-comment-reader| (stream char)
  (declare (ignore char))
  (loop (case (read-char stream nil nil)
          ((#\return #\linefeed nil) (return))
          (t )))
  (values))

(set-macro-character #\; '|universal-comment-reader|)


;;; (setq *load-verbose* (setq *compile-verbose* t))

;;; adjust these paths to your environment
(load #p"/Development/Source/production/Library/net/common-lisp/asdf/asdf")
(load #p"/Development/Source/dev/Library/de/setf/utility/asdf/hierarchical-names.lisp")

;;; search first the dev sources, then production
;;; in order to load module cross-references, dedicated registration
;;; are unavoidable - check them last, in order that hierarchic system
;;; designators be generated as nicknames
(map nil
     #'(lambda (directory)
         (when (probe-file directory)
           (pushnew directory asdf:*central-registry* :test #'equalp)))
     '(#p"/Development/Source/production/Library/asdf-registry/"
       #p"/Development/Source/dev/Library/asdf-registry/"
       #p"/Development/Source/production/Library/"
       #p"/Development/Source/dev/Library/"))


#+(or )
(progn
  (asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-8-0)
  (asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-9-0)
  (asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-9-1))

