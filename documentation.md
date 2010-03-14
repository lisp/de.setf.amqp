
DE.SETF.AMQP : the API
------------


### Introduction : On Portability and COmpatibility

The de.set.amqp library offers three views into the [AMQP](http://www.amq.org) world:

- as streams: The application can use Common Lisp streams to write arbitrary lisp data to
 publish it to through an exchange and to get or consume it from a queue.
- as messages: The application can perform message-based  operations through standard AMQP (class x method) operation
 interface. At this level, the interaction can be syncronous or asynchronous.
- as framed data: The low-level codecs and frame operations are also exposed, should it be necessary to implement wire-level
 interaction with a broker.

All three aspects are portable across Common Lisp implementations and 
suitable for interoperation with the respective facilities to transfer data.
The intent is to improve upon the prevailing notion of 'portability' as it concerns, that a program
operate as specified in different settings to include conformance to and compatibiltiy with
respective input/output interfaces.
Where one is concerned with application portability in the setting of an operating system or an
application environment, this is a closed-world problem.
An application relies on and suffices with user the interfaces for user interaction, while
data management, and communication interfaces are dictated by the respective host operating system.
Where, as is the case with de.setf.amqp, the software is a Lisp library, an additional concern appears:
it must be suited to interoperate with other interfaces used by applications which are written
in the Lisp implementations to which it is purports to be portable.
This concerns interface standardization and conformance.
That is, the amqp library must either itself establish a standard interface
for its domain, or it must implement an existing standard.
At the framing and messaging levels, it realizes the AMQP standard directly in the library's interfaces and, as such
establishes a standard interace binding for Common Lisp.
At the stream level, the task is less well defined, as there are several interfaces.
Although this is not customarily viewed as a portability issue, but rather held as orthogonal, 
from the perspective of the users for a Common Lisp library, this isolation is not helpful.

As a matter of circumstance, as the language standard does not address
numerous significant issues which concern development of input-output interfaces,
each run-time affords itself particular extensions to address those issues.
Each of which makes available to the programmer its own interface to manipulate its own resources.
In this situation, it is not sufficient that a library  rely on either standard language features or
other portable libraries in order that it, itself is to be portable.
As there is no standard, universally avaialable API for streaming input/output to network resources with
arbitrary content types and content encoding, it is essential that the it support
these operations in a manner which is compatible with the interfaces which are otherwise available
in respective implementations.

It would make no sense for a portable library to offer a different
interface for analogous functionality on different run-times.
Likewise, there is little advantage for the library's users if it does not
support interfaces for related resources in the respective runtimes.

That is, in order to be useful, a library must fulfill two requirements: it must be both portable and compatible.
First, it must manipulate resources and produce equivalent results in different implementation.
Second, it must be compatible with those interfaces to equivalent operations
which are otherwise available in the respective run times.

de.setf.amqp addresses these concerns as follows.
Portability is adressed by implmenting the network transport layer via  the
usocket library. For the moment, this dependency is hard-wired and does not achieve complete run time portability,
but, as it requires only open, close, listen, and vector/byte-read/write operators, this could be refactored
to include both more portability via libio or more specificity via a run-time's specific network
interface.
Compatibility is address by presenting both a simple-stream and gray stream interface for its
stream based operations, providing a Lisp binding for the standard AMQP APIs.

The next section describes the library architecture and introduces the three interface levels. The follows
a section to decribe each level's operators. Finally two simple examples are annotated to demonstrate
basic stream and message operations.


### Library Architecture : functional components and interface layers

An AMQP library realizes or dismisses options in response to two issues

 - What is the process model? Does the client respond to commands from a broker synchronously or asynchronously?
 - What is the entity model? Are commands autonomous messages exchanged between peers or are message exchanges as
 affintive side-effect of commands applied to local proxies for remote entities?

Various librariy implementation demonstrate alternative responses to these questions.

#### [AS3 AMQP](http://hopper.squarespace.com/blog/category/as3)

The AS3 AMQP library implements the asynchronous / autonomous approach.
For example, in order to declare and bind an exchange and a queue once a connection is established,
the program creates the respective autonomus commands, sends them to the broker, and registers a listener
to process the expected `BindOk` response.

    public function onOpenOk(event:ProtocolEvent):void {
        sessionHandler = sessionManager.create();

        var open:Open = new Open();

        var accessRequest:Request = new Request();
        accessRequest._realm = realm;
        accessRequest._passive = true;
        accessRequest._active = true;
        accessRequest._read = true;
        accessRequest._write = true;

        var exchange:org.amqp.methods.exchange.Declare 
        = new org.amqp.methods.exchange.Declare();
        exchange._exchange = x;
        exchange._type = x_type;

        var queue:org.amqp.methods.queue.Declare 
        = new org.amqp.methods.queue.Declare();
        queue._queue = q;

        var bind:Bind = new Bind();
        bind._exchange = x;
        bind._queue = q;
        bind._routingkey = bind_key;

        var onBindOk:Function = function(event:ProtocolEvent):void{
            trace("onBindOk called");
        };

        sessionHandler.dispatch(new Command(open));                 
        sessionHandler.dispatch(new Command(accessRequest));
        sessionHandler.dispatch(new Command(exchange));
        sessionHandler.dispatch(new Command(queue));
        sessionHandler.dispatch(new Command(bind));

        sessionHandler.addEventListener(new BindOk(), onBindOk);
    }

The library projects the asynchronous messaging protocol directly through the interface.
The examples  express the process to declare and bind a queue with an exchange as declaring a handler for incoming comamnds,
instantiating the respective command instances, and sending them in the correct sequence to the broker: open, accessRequest, exchange, queue, bind.
Followed by registering an event handler of the subsequence bindOk.
The approach imposes a continuation passing model where none is necessary.
It requires that the application must either correctly register and then remove handlers, or suffice with a single static control pattern.
The connection establishment process demostrates that there are alternative, as connection establishment is performed through a synchrous recursive control flow.

#### [Qpid Ruby](http://qpid.apache.org/amqp-ruby-messaging-client.html)

The Qpid Ruby library demonstrates the opposite approach, in that commands are expressed
as combinations of entity and method and are performed synchronously.
The channel implements basic_publish and basic_consume operators.
The example, [ehllo-world.rb](https://svn.apache.org/repos/asf/qpid/trunk/qpid/ruby/examples/hello-world.rb)
and the [excerpt](http://somic.org/2008/06/24/ruby-amqp-rabbitmq-example/) below indicate the proxied
consume request and the subsequent synchronous processing loop.

    def consumer(client, ch)
        myqueue = ch.queue_declare()
        ch.queue_bind(:queue=>myqueue.queue, :exchange=>'amq.topic',
                        :routing_key=>'disttailf.#')
        cons = ch.basic_consume(:queue=>myqueue.queue, :no_ack => true)
        ruby_queue = client.queue(cons.consumer_tag)
 
        while true
            raise "Rabbitmq broker disconnected" if client.closed?
            begin
              msg = ruby_queue.pop(non_block=true)
              puts "== #{msg.content.headers[:headers]} " \
                    "#{msg.routing_key.split('.')[-1]}"
              puts msg.content.body
            rescue
              sleep(0.5)
            end
        end
    end

#### [Qpid Jaa](http://qpid.apache.org/amqp-java-jms-messaging-client.html)

 The Qpid Java is based on the javax.jms classes, which it uses to implement asynchronous affinitive processing.
 The use pattern is to open a Connection, use it to create a Session, then create MessageConsumer and
 MessageProducer instances within the Session. Message instances are sent through the
 producer to consumers.

#### [py-amqplib](http://code.google.com/p/py-amqplib/)

The python AMQP library is semi-synchronous / affintive.
The [example](http://blogs.digitar.com/jjww/2009/01/rabbits-and-warrens/) illustrates the
proxy operations and the polled callback processing.

    def recv_callback(msg):
         print 'Received: ' + msg.body
    chan.basic_consume(queue='po_box', no_ack=True,
                    callback=recv_callback, consumer_tag="testtag")
    while True:
         chan.wait()
    chan.basic_cancel("testtag")

As demonstrated by the feature [request](http://code.google.com/p/py-amqplib/issues/detail?id=10), the
callback mechanism does not always suited benefit the application structure.


#### [RabbitMQ Java](http://www.jarvana.com/jarvana/view/com/rabbitmq/amqp-client/1.3.0/amqp-client-javadoc-1.3.0.jar!/com/rabbitmq/client/impl/package-tree.html)


---

### Library Interface

The de.setf.amqp implementation supports all four possible combinations.
The AS3 example demonstrates that the protocol event sequences for entity creation and configuration
would be much better realized with a synchronous process model as there is no real use case to justify
pipelined queue creation.

de.setf.amqp presents interfaces for stream, message, and framed data input/output
in both synchronous and asynchronous modes. 

At the stream level, it provides `with-open-channel` to open a channel as a stream for use with
standard read/write operators.
At the message level it implements the standard AMQP commands to create connection, channel, exchange, and
queue instances, and to publish, get, and subscribe to messages.
At the framed data level, it provides operators to encode/decode framed commands and to exchange them with a broker.

The stream and framed operations are all synchronous, by definition.
Message operations can be performed asynchronously to the extent that the broker initiates them as such
in response to eaarlier client commands.

#### Frame Data

 -  [send-method]((documentation/processing.html#send-method)
 -  [call-with-encoded-arguments](documentation/processing.html#call-with-encoded-arguments)
 -  [call-with-decoded-arguments](documentation/processing.html#call-with-decoded-arguments)
 -  [put-encoded-frame](documentation/processing.html#put-encoded-frame)
 -  [get-read-frame](documentation/processing.html#get-read-frame)
 -  [process-frame](documentation/processing.html#process-frame)

#### Message

de.setf.amqp supports both asynchronous an synchronous processing models.

synchronous procesing is performed with operators which read, parse, and dispatch successive commands in an (object x method) typecase.

 -  [command-case](documentation/processing.html#command-case)
 -  [command-loop](documentation/processing.html#command-loop)

The first form processes just the next command, the '-loop' form iterates over read commands indefinitely.
Each successively read frame is first filtered through the command clauses and passed to the first matching
clause for processing. If it declines, then processing succeeds with the handlers registered for the channel.
If all decline, then the [static operator](documentation/commands.html) for that command is invoked.

In addition to synchronous processing, an application can create a thread to manage a connection and
run a processing loop

  [connection-top-level](documentation/processing.html#connection-top-level)

In this case, that process read the connection frames and dispatches input to each channel.
If the respective channel has bound filters,
then it is asynchronously interrupted to filter the frame. One it starts, it can elect
to read further frames synchronously, or to retain the event-based processing.
The application can register method handlers for each channel

  [(setf channel-command)](documentation/processing.html#setf_channel-command)

to bins a function to process specific methods. The default method processes commands with the static operators.

The [static operators](documentation/commands.html) implements those commands which are required of the client as `respond-to-`.
For broker operations, the client side operator, `request-` implements the
immediate request as well is any immediate synchronous interaction.

#### Streams

The stream interface supports both the standard / gray stream interface and the simple-stream interface

 -  with-open-stream
 -  read
 -  write
 -  [stream-clear-output](documentation/device-level.html#stream-clear-output)
 -  [stream-finish-output](documentation/device-level.html#stream-finish-output)
 -  [stream-force-output](documentation/device-level.html#stream-force-output)
 -  [stream-listen](documentation/device-level.html#stream-listen)
 -  [stream-peek-char](documentation/device-level.html#stream-peek-char)
 -  [stream-read-byte](documentation/device-level.html#stream-read-byte)
 -  [stream-read-char](documentation/device-level.html#stream-read-char)
 -  [stream-read-line](documentation/device-level.html#stream-read-line)
 -  [stream-read-sequence](documentation/device-level.html#stream-read-sequence)
 -  [stream-write-byte](documentation/device-level.html#stream-write-byte)
 -  [stream-write-char](documentation/device-level.html#stream-write-char)
 -  [stream-write-sequence](documentation/device-level.html#stream-write-sequence)
 -  [stream-write-string](documentation/device-level.html#stream-write-string)

 -  [device-open](documentation/device-level.html#device-open) (stream #-sbcl slots initargs)
 -  [device-close](documentation/device-level.html#device-close) (stream abort)
 -  [device-read](documentation/device-level.html#device-read) (stream buffer start end blocking)
 -  [device-clear-input](documentation/device-level.html#device-clear-input) (stream buffer-only)
 -  [device-write](documentation/device-level.html#device-write) (stream buffer start end blocking)
 -  [device-clear-output](documentation/device-level.html#device-clear-output) (stream)
 -  [device-flush](documentation/device-level.html#device-flush) (device)
 -  [device-read-content](documentation/device-level.html#device-read-content) (device &rest content-arguments)
 -  [device-write-content](documentation/device-level.html#device-write-content) (device body &rest content-arguments)
    
---

### References

 - [Wikipedia](http://en.wikipedia.org/wiki/Software_portability) on software portability
 - [Wikipedia](http://en.wikipedia.org/wiki/Advanced_Message_Queuing_Protocol) on AMQP
 - [InfoQ](http://www.infoq.com/articles/AMQP-RabbitMQ) articles on RabbitMQ; includes examples and references.
 - [Get Started](http://www.rabbitmq.com/how.html) at RabbitMQ; links to implementations and documentation
 - [Ruby](http://amqp.rubyforge.org/)
 - [openamq.org](http://www.openamq.org/) : tutorials
   - [ESB](http://www.openamq.org/tutorial:soa)
   - [chatroom](http://www.openamq.org/tutorial:chatroom-exmaple)
   - [file transfer](http://www.openamq.org/tutorial:simple-file-transfer)
   - [load balancing](http://www.openamq.org/tutorial:load-balancing)
 - [Vinsoky IEEE](http://steve.vinoski.net/pdf/IEEE-Advanced_Message_Queuing_Protocol.pdf) :
   Steve Vinsoky's IEEE Towards Interation article about AMQP's launch.

#### Implementations

  - [AMQP Ruby/EventMachine driver]('http://amqp.rubyforge.org/)
  - [Qpid Ruby](http://qpid.apache.org/qpid-java-client-refactoring.data/java_amqp_client_design.pdf)
  - [py-amqp](http://barryp.org/software/py-amqplib/' id='pederson-amqplib) :
     Barry Pederson's AMQP library for python. 
     Jason@DigiTag's [discussion](http://blogs.digitar.com/jjww/2009/01/rabbits-and-warrens/' id='jason-2009) of py-amqplinb
  - [txAMQP](http://www.apparatusproject.org/blog/2009/04/creating-a-simple-amqp-client-using-txamqp/) :
     a multi-threaded AMQP client for python
  - [AS3-AMQP](http://hopper.squarespace.com/blog/2008/3/24/as3-amqp-client-first-cut.html) : AMQP client for Adobe's S3-FLEX environment 
    
#### Discussions

Matt Heitzenroder argues [txAMQP] the client libraries should necessarily be event-based in order to support multi-threaded applications.
His particular example is queueing messages. His argument fails to mention two issues. First, the socket imposes a complete order
on connection frames. Second, the protocol imposes a complete order on channel frames. In this situation, a channel user should
block for any synchronous commands. For asynchrounous output, it is only important that operations queue rather than block.
For asynchronous input, it is only important that it be routed to the channel's process. It is not clear whether this needs to
happen as an asynchronous event, or in the course of successive input processing, since the input in the channel is ordered.
Especially once links are supported, the latter will be the case.

Ben Hood's [describes](http://hopper.squarespace.com/blog/2008/6/21/build-your-own-amqp-client.html)
the basic structure of an AMPQ client. He suggests two organizational aspects:
functional components and interface layers. The suggested functional components
    
 - data-type stream codecs
 - command parsing and generation
 - command frame composition/decomposition
 - command socket output
 - command socket input, decoding, and handler dispatch
 - workflow functionality which effect the protocol state progressions
   
actually combine distinct functional aspects. The interface layers:
    
 - AMQP method codecs, most of which should be generated from the specifications.
 - Mid-layer operations which provide for method ordering
 - Convenience application operations with defaults and functional abstraction
   (his terms: dependency injection and inversion of control) to minimze
   application code.
 - templates for application patterns, like consumers or asynchronous RPC
 - Sensible defaults for method attributes
   
are also indistinct, and there is no argument to expose the asynchronous operations
on commands which require responses in an external interface. The correct implementation
of declaration and binding operations overrides any possible concern for high-throughput
execution.
