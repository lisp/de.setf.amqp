
DE.SETF.AMQP: how to build it with Steel Bank Common Lisp
------------

In order to use asdf with sbcl, 

The system can be built and saved from the command line

    $ sbcl --userinit readmes/build-init.lisp \
      --eval "(asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-8-0)" \
      --eval "(asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-9-0)" \
      --eval "(asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-9-1)" \
      --eval '(sb-ext:save-lisp-and-die "sbcl-amqp.core")'

Start it with the core

    $ sbcl --core sbcl-amqp.core
    This is SBCL 1.0.35, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.
    
    SBCL is free software, provided as is, with absolutely no warranty.
    It is mostly in the public domain; some portions are provided under
    BSD-style licenses.  See the CREDITS and COPYING files in the
    distribution for more information.
    * (defparameter *c* (make-instance 'amqp:connection :uri "amqp://guest:guest@localhost/"))
    
    *C*
    * (amqp:with-open-channel (output *c*  :exchange "ex" :type "direct" :queue "q1")
        (format output "~a, ~a, ~a~%"
                (lisp-implementation-type)
                (lisp-implementation-version)
                amqp.u:*version*))
    
    NIL
    * (amqp:with-open-channel (input *c* :queue "q1")
        (read-line input))
    
    "SBCL, 1.0.35, de.setf.amqp-20100219T020947Z00"
    * 

A snap-shot core is present in the downloads section.
