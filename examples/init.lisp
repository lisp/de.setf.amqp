;;;
;;; 2010-02-14 janderson
;;; special minimal to test release build on mcl

;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;;

(in-package :cl-user)

(defvar *patches-loaded* nil)

(when (and (not *patches-loaded*) (y-or-n-dialog "load patches? "))
  (ccl:load-patches "ccl:patches;" t)
  (setq *patches-loaded* t))

;;; (mapc #'(lambda (p) (print (load p))) (directory "CCL:*-patch.lisp"))


;; mac-encoding comment-reader

(defun |universal-comment-reader| (stream char)
  (declare (ignore char))
  (loop (case (read-char stream nil nil)
          ((#\return #\linefeed nil) (return))
          (t )))
  (values))

(set-macro-character #\; '|universal-comment-reader|)

(load #p"marbella-backup:Development:Source:dev:Library:net:common-lisp:asdf:asdf.lisp")
(load #p"marbella-backup:Development:Source:dev:Library:de:setf:utility:asdf:hierarchical-names.lisp")
(pushnew  #p"marbella-backup:Development:Source:dev:Library:" asdf:*central-registry* :test #'equalp)
(pushnew  #p"marbella-backup:Development:Source:dev:Library:asdf-registry:" asdf:*central-registry* :test #'equalp)
(setq asdf::*output-translations* (list nil))

(progn
  (setq ccl::*short-site-name* "marbella")
  (setq *PREFERRED-EOL-CHARACTER* #\linefeed)
  (setq *TRANSFORM-CRLF-TO-PREFERRED-EOL* t))

(setq *load-verbose*
      (setq *compile-verbose* t))
