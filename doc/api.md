
DE.SETF.AMQP : the API
------------


=== Introduction : On Portability and COmpatibility

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


=== Library Architecture : functional components and interface layers

Discuss the choice of processing architecture : asynchronous, synchronous; explicit call-back registration.
Several documents exist to describe AMQP client library features, arrchitecture
and implementation[<a href='#hood-2008'>hood-2008</a>], ...
Some of them even name an "AMQP client API", (eg [qpid-2010-amqp]) but none references a description of one.
Neither do the specification documents.

Two dimensions emerge from the various alternative implementations.
<ul>
 <li>The processing models : asynchronous or synchronous<br />
  </li>
 <li>The protocol entity model : affinitive or literal<br />
 </li>
<ul>
 <li>AMQP R/EventMachine: asyncronous literal</li>
 <li>AS3 AMQP : asynchronous literal
  <code>
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

public function publish():void {
    var data:ByteArray = new ByteArray();
    data.writeUTF("hello, world");
    var publish:Publish = new Publish();
    publish._exchange = x;
    publish._routingkey = routing_key;
    var props:BasicProperties = Properties.getBasicProperties();
    var cmd:Command = new Command(publish, props, data);             
    sessionHandler.dispatch(cmd);       
}
  </code>
 </li>
 <li>QPid Ruby : synchronous affinitive : channel implements basic_publish and basic_consume operators.
  <code>
 c = Qpid::Content.new({:headers=>h}, line)
 ch.basic_publish(:routing_key=>rkey, :content=>c,
                  :exchange=>'amq.topic')
  </code>
 </li>
 <li>Qpid Java: asynchronous affinitive : 
  <p>The 0.5 model is simple. It depends on javax.jms classes and defines five primary API classes
   <ul>
    <li>Connection</li>
    <li>Message</li>
    <li>MessageConsumer</li>
    <li>MessageProducer</li>
    <li>Session</li>
   </ul>
   The use pattern is to open a Connection, use it to create a Session, then create MessageConsumer and
 MessageProducer instances within the Session and then create Message instances to send through the
 producer to consumers. The content type and encoding are specified for the producers, and the adressing
 is handled by inherited javax.jms behavior.</p>
   <p>The dev version, which intend to implement version-specific support for 0-8 through 0-10,
 are much more elaborate. They not only
 differentiate entities by version, they also 
  </p>
 <li>RabbitMQ : <a href='http://www.jarvana.com/jarvana/view/com/rabbitmq/amqp-client/1.3.0/amqp-client-javadoc-1.3.0.jar!/com/rabbitmq/client/impl/package-tree.html>class tree</a>

<ul>
 
</ul>

=== Library Interface

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

==== Frame Data

    send-method
    call-with-encoded-arguments
    call-with-decoded-arguments
    put-encoded-frame
    get-read-frame
    process-frame

==== Message

rather than a single control model, it supports both asynchronous an synchronous forms

Read frames are handled as follows
if read synchronously, an explicit handler is provided, which as the first opportunity to process.
If it declines, then processing succeeds with the handlers registered for the channel.
If all decline, then the status operator for that command is invoked.
This process applies to all frames for the channel to be read.

In addition, one can initiate an event generator, which manages the input to all channels.
Rather than permit any given channel to read-through to the input frame stream, This 
process reads all input and dispatches it. If the respective channel has bound filters,
then it is asynchronously interrupted to filter the frame. One it starts, it can elect
to read further frames synchronously, or to retain the event-based processing.

register handler or allow the channel to process read frames with the static operators.

 The library implements those commands which are required of the client as `respond-to-`.
For those which the server must implement, the client side operator, `request-` implements the
immeidate request as well is any immediate synchronus interaction.

==== Streams

    with-open-stream
    read
    write
    stream-read- byte char string sequence vector
    stream-write- byte char string sequence vector
    finish-output
    force-output
    stream-listen
    stream-peek-char
    stream-clear-output

    device-open (stream #-sbcl slots initargs)
    device-close (stream abort)
    device-read (stream buffer start end blocking)
    device-clear-input (stream buffer-only)
    device-write (stream buffer start end blocking)
    device-clear-output (stream)
    device-flush (device)
    device-read-content (device &rest content-arguments)
    device-write-content (device body &rest content-arguments)
    

=== References
 <h4>AMQP: specifications</h4>
 <h4>AMQP: implementations</h4>
  <ul>
   <li>AMQP Ruby/EventMachine driver:<br />
    <a href='http://amqp.rubyforge.org/'>doc</a></li>
   <li>QPid
    <ul>
     <li> Ruby :</li>
     <hr />
     <li>[<a href='http://qpid.apache.org/qpid-java-client-refactoring.data/java_amqp_client_design.pdf' id='qpid-2010-amqp'>qpid-2010-amqp</a>]
    
    </ul>
   <li>py-amqplib : AMPQ client for python<br />
     <a href='http://barryp.org/software/py-amqplib/' id='pederson-amqplib'>pederson-amqplib</a> :
     Barry Pederson's AMQP library for python.</li> <br />
      <a href='http://blogs.digitar.com/jjww/2009/01/rabbits-and-warrens/' id='jason-2009'>jason-2009</a> :
Jason@DigiTag's discussion of py-amqplinb</li>
   <li>txAMQP : AMQP client for python; multi-threaded :<br />
    See [<a href='#heitzenroder-2009-performance'>heitzenroder-2009-performance</a>].
    </li>
   <li>AS3-AMQP : AMQP client for Adobe's S3-FLEX environment :<br />
     Ben Hood's library projects the asynchronous messaging protocol directly through the interface.
     His examples [<a href='#hood-2009-as3'>hood-2009-as3</a>] express the process to declare and bind a queue with an exchange
     as declaring a handler for incoming comamnds, instantiating the respective command instances, and sending them in the correct sequence to the broker, in the
     correct order : open, accessRequest, exchange, queue, bind. Followed by registering an event handler of the subsequence bindOk.
     It exhibits several deficiencies.
     It imposes a continuation passing model where none is necessary.
     It implies that the apllication must either correctly register and then remove handlers, or suffice with a single static control pattern.
     This is not even completely uniform, as connection establishment is performed through a synchrous recursive control flow.
     <verbatim>
      
      </verbatim>

     <hr>
     [<a href='http://hopper.squarespace.com/blog/2008/3/24/as3-amqp-client-first-cut.html' id='hood-2009-as3'>hood-2009-as3</a>] : 'AS3 AMQP Client: First Cut'

   </li>
  </ul>
    
 <h4>AMQP: discussions</h4>
  <ul>
   <li><a href='http://en.wikipedia.org/wiki/Advanced_Message_Queuing_Protocol'>Wikipedia</a></li>
   <li><a href='http://www.infoq.com/articles/AMQP-RabbitMQ'>InfoQ</a> article includes examples and references.</li>
   <li><a href='http://www.rabbitmq.com/how.html'>rabbitmq-how</a> : RabbitMQ "Get Started" page with links
 to implementations and documentation</li>
   <li><a href='http://amqp.rubyforge.org/'>Ruby</a><li>
   <li><a href='http://hopper.squarespace.com/blog/2008/6/21/build-your-own-amqp-client.html' id='hood-2008'>hood-2008</a> :
    Ben Hood's description
    of the structure of an AMPQ client. This suggests two organizational aspects:
    functional components and interface layers. The functional components
    <ul><li>data-type stream codecs</li>
     <li>command parsing and generation</li>
     <li>command frame composition/decomposition</li>
     <li>command socket output</li>
     <li>command socket input, decoding, and handler dispatch</li>
     <li>workflow functionality which effect the protocol state progressions</li>
    </ul>
    actually combine distinct functional aspects.
    The interface layers:
    <ul>
     <li>AMQP method codecs, mos of which should be generated from the specifications.</li>
     <li>Mid-layer operations which provide for method ordering</li>
     <li>Convenience application operations with defaults and functional abstraction
      (his terms: dependency injection and inversion of control) to minimze
      application code.</li>
     <li>templates fro application patterns, like consumers or asynchronous RPC.</li>
     <li>Sensible defaults for method attributes</li>
    </ul>
    are also indistinct, and there is no argument to expose the asynchronous operations
    on commands which require responses in an external interface. The correct implementation
    of declaration and binding operations overrides any possible concern for high-throughput
    execution.
    </li>
    <li>[<a href='http://www.apparatusproject.org/blog/2009/04/creating-a-simple-amqp-client-using-txamqp/' id='heitzenroder-2009-performance'>heitzenroder-2009-performance</a>] :
     Matt Heitzenroder argues the client libraries whould necessarily be event-based in order to support multi-threaded applications.
     His particular example is queueing messages. His argument fails to mention two issues. First, the socket imposes a complete order
     on connection frames. Second, the protocol imposes a complete order on channel frames. In this situation, a channel user should
     block for any synchronous commands. For asynchrounous output, it is only important that operations queue rather than block.
     For asynchronous input, it is only important that it be routed to the channel's process. It is not clear whether this needs to
     happen as an asynchronous event, or in the course of successive input processing, since the input in the channel is ordered.
     Especially once links are supported, the latter will be the case.</li>
   <li>[<a href='http://steve.vinoski.net/pdf/IEEE-Advanced_Message_Queuing_Protocol.pdf' id='vinoski-2007-amqp'>vinoski-2007-amqp</a>] :
    Steve Vinsoky's IEEE Towards Interation article about AMQP's launch.</li>
   <li><a href='http://www.openamq.org/'>openamq.org</a> tutorials :
    <ul>
     <li><a href='http://www.openamq.org/tutorial:soa'>ESB</a></li>
     <li><a href='http://www.openamq.org/tutorial:chatroom-exmaple'>chatroom</a></li>
     <li><a href='http://www.openamq.org/tutorial:simple-file-transfer'>file transfer</a></li>
     <li><a href='http://www.openamq.org/tutorial:load-balancing'>load balancing</a></li>
    </ul>
   </li>
  </ul>
 <h4>General</h4>
 <ul>
  <li><a href='http://en.wikipedia.org/wiki/Software_portability'>Wikipedia</a></li>
 </ul>
