 `de.setf.amqp` implements a client library for the 'Advanced Message Queueing
 Protocol'. The implementation comprises wire-level codecs, implementations
 for the standard protocol objects and methods, a functional interface for message-,
 stream- and object-based i/o, and a device-level simple-stream implementation.

 The library targets the revisions of the published AMQP protocol as of versions
 0.8, 0.9, and 0.10. This means that it should work with respective RabbitMQ,
 Apache ActiveMQ, and Qpid implementations. The implementation architecture
 should also accommodate a control structure appropriate for the prospective
 1.0 version - as least as described in preliminary drafts.
 For each version, a distinct package comprises the object and method
 definitions for protocol entities and codecs as generated from the respective
 specification documents.[1] Each collection is a
 complete projection, which means there is some amount of duplication.
 The package and directory names names follow more-or-less the naming conventions of the
 xml protocol documents[2]:
     ----------------- --------------- ---------------------------------------------
      AMQP-1-1-0-8-0    version 0.8     [amqp0-8.xml, amqp0-8.pdf (2006-06)]
      AMQP-1-1-0-9-0    version 0.9     [amqp0-9.xml, amqp0-9.pdf (2006-12)]
      AMQP-1-1-0-9-1    version 0.9r1   [amqp0-9-1.xml, amqp0-9-1.pdf (2008-11-24)]
      AMQP-1-1-0-10-0   version 0.10    [amqp.0-10.xml, amqp.0-10.pdf (2008-02-20)]

 In order to modify the translation and/or generate new codecs consult the `:de.setf.amqp.tools` component.

 All protocol versions are expressed through a common interface[3] which is specialized for the common
 abstract classes. The initial connection phase determines the correct concrete connection implementation
 to be used to communicate with the broker. Given which the other concrete object and method classes are
 elected from the same package. One determines the version support directly by loading the respective
 version's `.asd` file, which makes its connection class available for negotiation.

 ----------
 [1]: tools/spec.lisp
 [2]: http://www.amqp.org/confluence/display/AMQP/AMQP+Specification
 [3]: documentation/index.html
