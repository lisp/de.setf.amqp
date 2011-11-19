

# DE.SETF.AMQP: a Common Lisp client library for AMQP


`de.setf.amqp` implements a native Common Lisp client library for the 'Advanced Message Queuing
 Protocol' ([AMQP](http://en.wikipedia.org/wiki/AMQP)). The implementation comprises wire-level codecs, implementations
 for the standard protocol objects and methods, a functional interface for message-,
 stream- and object-based i/o, and a device-level simple-stream implementation.

 The library targets the revisions of the published AMQP protocol as of versions
 0.8, 0.9, and 0.10. This means that it should work with respective [RabbitMQ](http://www.rabbitmq.com/),
 OpenAMQ, and [Qpid](http://qpid.apache.org/) implementations. The implementation architecture
 should also accommodate a control structure appropriate for the prospective
 1.0 version - as least as described in preliminary drafts.
 For each version, a distinct package comprises the object and method
 definitions for protocol entities and codecs as generated from the respective
 specification documents.[[1]] Each collection is a
 complete projection, which means there is some amount of duplication.
 The names - classes, operators, fields, packages and directories all follow more-or-less the naming conventions of the
 xml protocol documents[[2]]:

<table>
<tr><td>AMQP-1-1-0-8-0</td>  <td>version 0.8</td>   <td>[amqp0-8.xml, amqp0-8.pdf (2006-06)]</tr>
<tr><td>AMQP-1-1-0-9-0</td>  <td>version 0.9</td>   <td>[amqp0-9.xml, amqp0-9.pdf (2006-12)]</tr>
<tr><td>AMQP-1-1-0-9-1</td>  <td>version 0.9r1</td> <td>[amqp0-9-1.xml, amqp0-9-1.pdf (2008-11-24)]</tr>
<tr><td>AMQP-1-1-0-10-0</td> <td>version 0.10</td>  <td>[amqp.0-10.xml, amqp.0-10.pdf (2008-02-20)]</tr>
</table>

 In order to modify the translation and/or generate new codecs consult the `:de.setf.amqp.tools` module.

 All protocol versions are expressed through a common interface[[3]] which is specialized for the common
 abstract classes. The initial connection phase determines the correct concrete connection implementation
 to be used to communicate with the broker. Given which the other concrete object and method classes are
 elected from the same package. One determines the version to be supported directly by loading the respective
 version's `.asd` file, which makes its connection class available for negotiation.

 [1]: tools/spec.lisp
 [2]: http://www.amqp.org/confluence/display/AMQP/AMQP+Specification
 [3]: documentation/index.html


## Status

This is intended as the base for a distributed semantic store.
What one has here is a reasonably complete engineering prototype.
It consumes phenomenal amounts of memory and runs astonishingly slowly:
5 - 10 milliseconds and 1 to 10 thousand bytes per round trip.
A request entails ten levels of generic function dispatch, half of which require keyword processing.
A response call stack is about as deep, but with somewhat less keyword processing.
As almost everything between the interface commands and the frame buffers is generated code, once it is clear
which aspects should remain available for specialization and/or optional arguments, the protocol call
stack would benefit from recasting uninteresting elements as ordinary functions of fixed arguments - depending
on implementation type, by a factor of ten to sixty.

It would also be nice to generate a table similar to [RabbitMQ's](http://www.rabbitmq.com/specification.html) to
record protocol  conformance  and compatibility with brokers. Eventually.
The present tests are limited to

- [codec](test/AMQP-1-1-0-9-1/test.lisp) unit tests which validate the respective version's codecs for default
  argument values.
- in-memory loop-back [tests](test/test.lisp), used to ensure (for a recent version) that a round-trip
  does not cons
- simple data exchanges with a broker.


### An Example

The library has been built and [probed](file:///examples/examples.lisp) in the following combinations

<table>
<tr><td style='text-align: right;'>AMQP broker<br/>lisp implementation</td><th>RabbittMQ</th><th>QPID</th></tr>
<tr><th>MCL</th><td>MCL-5.2, RabbitMQ 1.7.1, AMQP-0.8r0</td><td>MCL-5.2, QPID-0.5, AMQP-0.9r1</td></tr>
<tr><th>CCL</th><td/><td>CCL-1.4, QPID-0.5, AMQP-0.9r1</td></tr>
<tr><th>SBCL</th><td/><td>SBCL-1.0.35, QPID-0.5, AMQP-0.9r1</td></tr>
</table>

For example,

    $ sbcl
    This is SBCL 1.0.35, an implementation of ANSI Common Lisp.
    More information about SBCL is available at <http://www.sbcl.org/>.

    * (in-package :amqp.i)

    #<PACKAGE "DE.SETF.AMQP.IMPLEMENTATION">
    * (defparameter *c* (make-instance 'amqp:connection :uri "amqp://guest:guest@localhost/"))

    *C*
    * (defparameter *ch1* (amqp:channel *c* :uri (uri "amqp:///")))

    *CH1*
    * (defparameter *ch1.basic* (amqp:basic *ch1*))

    *CH1.BASIC*
    * (defparameter *ch1.ex* (amqp:exchange *ch1*  :exchange "ex" :type "direct"))

    *CH1.EX*
    * (defparameter *ch1.q*  (amqp:queue *ch1* :queue "q1"))

    *CH1.Q*
    * (amqp:request-declare *ch1.q*)

    #<AMQP-1-1-0-9-1:QUEUE {11D7A0F1}>
    * (amqp:request-bind *ch1.q* :exchange *ch1.ex* :queue *ch1.q* :routing-key "/")

    #<AMQP-1-1-0-9-1:QUEUE {11D7A0F1}>
    * (defparameter *ch2* (amqp:channel *c* :uri (uri "amqp:///")))

    *CH2* 
    * (defparameter *ch2.basic* (amqp:basic *ch2*))

    *CH2.BASIC*
    * (defparameter *ch2.q*  (amqp:queue *ch2* :queue "q1"))

    *CH2.Q*
    * (amqp:request-declare *ch2.q*)

    #<AMQP-1-1-0-9-1:QUEUE {11DA6891}>
    * (list
        (amqp:request-publish *ch1.basic* :exchange *ch1.ex*
                              :body (format nil "this is ~a" (gensym "test #"))
                              :routing-key "/")
        (amqp:request-get *ch2.basic* :queue *ch2.q*))

    ("this is test #1282" "this is test #1624")
    * (amqp:request-get *ch2.basic* :queue *ch2.q*)

    "this is test #1820"
    * (amqp:request-get *ch2.basic* :queue *ch2.q*)

    "this is test #1998"
    * (amqp:request-get *ch2.basic* :queue *ch2.q*)

    "this is test #2011"
    * (amqp:request-get *ch2.basic* :queue *ch2.q*)

    "this is test #2014"
    * (amqp:request-get *ch2.basic* :queue *ch2.q*)

    "this is test #2022"
    * (amqp:request-get *ch2.basic* :queue *ch2.q*)

    "this is test #2028"
    * (amqp:request-get *ch2.basic* :queue *ch2.q*)

    "this is test #2179"
    * (amqp:request-get *ch2.basic* :queue *ch2.q*)

    "this is test #1282"
    * (amqp:request-get *ch2.basic* :queue *ch2.q*)

    NIL
    * 

Which, as an aside, illustrates that brokered messages persist between connections until they have been consumed.
Of which a default QPID broker with no persistence support was observed to cache only about 500k bytes
(ca. 25,000 messages of 20 bytes each).

An introduction to the [API](./documentation.md) is taking shape.
In the meantime, there is a package [index](./documentation/package_AMQP.html), there are a few [examples](./readmes/) for the
library proper and for the RabbitMQ library [emulation](./rabbitmq/examples.lisp).

common-lisp.net hosts a mailing [list](http://www.common-lisp.net/mailman/listinfo/de-setf-amqp-devel)
and a [trac](http://trac.common-lisp.net/de-setf-amqp/) instance for discussions and/or reports.


### Changes

 * 2010-06-10 : corrected and completed float codecs
 * 2011-11-20 : added user-id default for publish to cooperate with rabbitmq @2.6

### Downloading

A [.dmg](http://github.com/downloads/lisp/de.setf.amqp/amqp-20100214-0.3.dmg) is available for MCL-5.2.
Saved images are available for [ccl](http://github.com/downloads/lisp/de.setf.amqp/ccl-amqp.image.zip)
and [sbcl](http://github.com/downloads/lisp/de.setf.amqp/sbcl-amqp.core.zip).


### Building

In principle, `de.setf.amqp` is built with [`asdf`](http://www.common-lisp.net/projects/asdf).
Once one has the sources and the `asdf` configuration in place, evaluate

    (asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-8-0)
    (asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-9-0)
    (asdf:operate 'asdf:load-op :de.setf.amqp.amqp-1-1-0-9-1)

to load the respective version. The versions intend to be compatible and connections should
negotiate automatically to use the correct implementation.
Please consult the detailed instructions for the respective [runtime](./readmes/README-build.md) for more information.

 
## Licensing

This version is released under version 3 of the GNU Affero license (GAL).[[5]]
The required components are included as per the respective licenses and covered,
in this combined form,  under the GAL as well

- [alexandria](mailto:alexandria-devel@common-lisp.net) : Public Domain
- [usocket](mailto:usocket-devel@common-lisp.net) : MIT, through 2007. later work unspecified
  - 2003 Erik Enge
  - 2006-2007 Erik Huelsmann 
- closer-mop :  MIT-style
  - 2005 - 2010 [Pascal Costanza](http://p-cos.net)
- [bordeaux-threads](mailto:bordeaux-threads-devel@common-lisp.net) : MIT-style, with additional attributions undated.[[6]]
  - -2006 Greg Pfeil
- cl-ppcre : equivalent to MIT
  - 2002-2008, [Dr. Edmund Weitz](http://www.weitz.de)
- com.b9.puri : LLGPL, by which com.b9.puri.ppcre is also covered by the LLGPL
  - 1999-2001 [Franz, Inc](mailto:opensource@franz.com).
  - 2003 [Kevin Rosenberg](mailto:kevin@rosenberg.net)


 [5]: agpl.txt
 [6]: http://common-lisp.net/project/bordeaux-threads/darcs/bordeaux-threads/CONTRIBUTORS

--------
![made with mcl](http://www.digitool.com/img/mcl-made-1.gif "Made With MCL")


