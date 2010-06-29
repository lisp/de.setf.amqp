
DE.SETF.AMQP: build it with Clozure Common Lisp
------------

The system can be built and saved as a run-time image from the command line

    $ export CCL=/Development/Applications/LISP/ccl-1-4/dppccl
    $ $CCL --no-init --load readmes/build-init.lisp \
      --eval "(asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-8-0)" \
      --eval "(asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-9-0)" \
      --eval "(asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-9-1)" \
      --eval '(ccl:save-application "ccl-amqp.image")'

Start it with the core

    $ $CCL -I ccl-amqp.image
    ;Loading #P"P-LIBRARY:net;common-lisp;asdf;asdf..newest"...
    Welcome to Clozure Common Lisp Version 1.4-r13119  (DarwinPPC32)!
    ? (defparameter *c* (make-instance 'amqp:connection :uri "amqp://guest:guest@localhost/"))
    *C*
    ? (amqp:with-open-channel (output *c*  :exchange "ex" :type "direct" :queue "q1")
      (format output "~a, ~a, ~a~%"
              (lisp-implementation-type)
              (lisp-implementation-version)
              amqp.u:*version*))
    NIL
    ? (amqp:with-open-channel (input *c* :queue "q1")
      (read-line input))
    "Clozure Common Lisp, Version 1.4-r13119  (DarwinPPC32), de.setf.amqp-20100218T233140Z00"
    ? 

A snap-shot image is present in the downloads section.
