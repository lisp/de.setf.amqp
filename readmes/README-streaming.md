
DE.SETF.AMQP: Attempts at streaming.
-------

Given an open connection bound to `*c*`, in this first case, RabbitMQ, it would be nice to have a channel amenable to some `stream` interface or other.
At least `write-char` and `read-char` equivalents. `de.setf.amqp` intende to support something like that. For example
    ? (amqp:connection-server-properties *c*)
    (:|product| "RabbitMQ" :|version| "1.7.1" :|platform| "Erlang/OTP" :|copyright| "Copyright (C) 2007-2009 LShift Ltd., Cohesive Financial Technologies LLC., and Rabbit Technologies Ltd." :|information| "Licensed under the MPL.  See http://www.rabbitmq.com/")
    ? (amqp:with-open-channel (output *c*  :exchange "ex" :type "direct" :queue "q1")
        (dotimes (x 7 x)
          (format output "this is a test : ~d~%" x)))
    7
    ? (amqp:with-open-channel (input *c* :queue "q1")
        (loop repeat 7  collect (read-line input)))
    ("this is a test : 0" "this is a test : 1" "this is a test : 2" "this is a test : 3" "this is a test : 4" "this is a test : 5" "this is a test : 6")
    ?  

Ok, but what happens whne the reader does not know how muc to read. That is why the interface offers `eof-error-p`. 
For a closed system - for example, files, that is straight-forward.
In a message-based system, how does one know what the last message has arrived.
Absent awarenes of the issue, the reader hangs ,,,

    ? (amqp:with-open-channel (output *c*  :exchange "ex" :type "direct" :queue "q1")
      (dotimes (x 7 x)
        (format output "this is a test : ~d~%" x)))
    7
    ? (amqp:with-open-channel (input *c* :queue "q1")
        (loop
          (unless (print (read-line input nil nil))
            (return))))

    "this is a test : 0" 
    "this is a test : 1" 
    "this is a test : 2" 
    "this is a test : 3" 
    "this is a test : 4" 
    "this is a test : 5" 
    "this is a test : 6" 
    > Break in process Listener 2: 
    > While executing: #<Anonymous Function #x39E46DE>
    > Type Command-/ to continue, Command-. to abort.
    > If continued: Return from BREAK.
    See the RestartsÉ menu item for further choices.
    1 > 

... until interrupted. The writer must somehow single the reader out-of-band, that the data is complete.
In the AMQP case, the messages must augment the body content with an indicator. The options are limited.
A suitable carrier would be the command's header argument. If only that were not already on the line by the time the writer discovers that it has no more data to send.
It has been suggested, that one specify two message types, "to be continued" and "the end", but it seems not quite right to need to send an extra message, just to
say that the previous one was the last one. [HTTP](rfc) has managed this with the `chunked` content encoding, but it is not a framed protocol. That is the chunking
mechanism is used exactly to releive the writer of the requirement to specify the stream length in the header.
Since AMQP does not relax that requirement, the indicator requires some other means.
The essence of the chunking mechanism, is that some entity can independntly, unambiguously mark the end of the stream.
For a stream of AMQP frames, once they have commenced, the writer has few places to indicate that.
Data is in-band and the frame itself is off-limits.
Other than the data, the only value the writer can set is the frame size.
Well, actually that suffices.
In order to end the stream, the writer emits a zero-length frame and then completes the current message by padding the remaing body length.
* If this happens at the start of a message, the consequence is a sequence (publish[n], header, data[0], data[n])
* If this happens mid-message, at the start of a frame, the consequence is a sequence (publish[n], header, data[i], data[0], data[n-i])

It does not matter whther it occurs on a frame module.
It cannot happen at a "message" module, without that a new (publish, header) sequence has already been emitted.

Ok. But. This works only if the broker does not drop the zero-length frame. Which RabbitMQ appears to do. Here, output from an attempt analogous to that above, but with debugging enabled.
The important bits are tagged `write-frame` and `read-frame`. We see, that the writer emits the frame sequence (method, header, body[630], body[0], body[3458]).
Which sum to message size, and include the zero-length frame. Note also the `transfer-encoding`.
The reader, for its part, notices the transfer encoding and prepares to continue to read messages until a zero-length frame appears.
Which ever happens. Two data frames arrive, body[630] and body[3458]. the body[0] evaporated.
As this is a transcript of the first interaction above, the absence leads to no problem, as the reader closed and flushed the input stream.
In the second case, above, however, the stream reamins in a chunked state, reading continues, and the reader hangs.

    [20100217T171655Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: send-method: OPEN . NIL
    [20100217T171655Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: encoding: CHANNEL.OPEN . (:OUT-OF-BAND "")
    [20100217T171655Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: send-method: #<CHANNEL.OPEN #x207CBB46>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{CHANNEL.OPEN}[5/4088: 0 20 0 10 0] #x207CBC46>
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{CHANNEL.OPEN}[5/4088: 0 20 0 10 0] #x207CBC46>
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.22|t.0].{CHANNEL.OPEN-OK}[4/4088: 0 20 0 11] #x2077B56E>
    [20100217T171655Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: process-command: #<CHANNEL [amqp://localhost/].22 #x207CB74E> #<CHANNEL.OPEN-OK #x207CCFFE> . NIL
    [20100217T171655Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: Opened: NIL
    [20100217T171655Z00] DEBUG #<EXCHANGE #x207CD2CE>: send-method: DECLARE . NIL
    [20100217T171655Z00] DEBUG #<EXCHANGE #x207CD2CE>: encoding: EXCHANGE.DECLARE . (:TICKET 0 :EXCHANGE "ex" :TYPE "direct" :PASSIVE NIL :DURABLE NIL :AUTO-DELETE NIL :INTERNAL NIL :NO-WAIT NIL :ARGUMENTS NIL)
    [20100217T171655Z00] DEBUG #<EXCHANGE #x207CD2CE>: send-method: #<EXCHANGE.DECLARE #x207CD3F6>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{EXCHANGE.DECLARE}[21/4088: 0 40 0 10 0 0 2 101 ... 101 99 116 0 0 0 0 0] #x207CBC46>
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{EXCHANGE.DECLARE}[21/4088: 0 40 0 10 0 0 2 101 ... 101 99 116 0 0 0 0 0] #x207CBC46>
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.22|t.0].{EXCHANGE.DECLARE-OK}[4/4088: 0 40 0 11] #x2077B56E>
    [20100217T171655Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: process-command: #<EXCHANGE #x207CDA06> #<EXCHANGE.DECLARE-OK #x207CDA46> . NIL
    [20100217T171655Z00] DEBUG #<QUEUE #x207CD25E>: send-method: DECLARE . NIL
    [20100217T171655Z00] DEBUG #<QUEUE #x207CD25E>: encoding: QUEUE.DECLARE . (:TICKET 0 :QUEUE "q1" :PASSIVE NIL :DURABLE NIL :EXCLUSIVE NIL :AUTO-DELETE NIL :NO-WAIT NIL :ARGUMENTS NIL)
    [20100217T171655Z00] DEBUG #<QUEUE #x207CD25E>: send-method: #<QUEUE.DECLARE #x207CDC46>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{QUEUE.DECLARE}[14/4088: 0 50 0 10 0 0 2 113 49 0 0 0 0 0] #x207CBC46>
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{QUEUE.DECLARE}[14/4088: 0 50 0 10 0 0 2 113 49 0 0 0 0 0] #x207CBC46>
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.22|t.0].{QUEUE.DECLARE-OK}[15/4088: 0 50 0 11 2 113 49 0 0 0 2 0 0 0 0] #x2077B56E>
    [20100217T171655Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: process-command: #<QUEUE #x207CE2E6> #<QUEUE.DECLARE-OK #x207CE32E> . (:QUEUE "q1" :MESSAGE-COUNT 2 :CONSUMER-COUNT 0)
    [20100217T171655Z00] DEBUG q1: queue declared: q1 2 0
    [20100217T171655Z00] DEBUG #<QUEUE #x207CD25E>: send-method: BIND . (:EXCHANGE "ex" :QUEUE "q1" :EXCHANGE #<AMQP-1-1-0-8-0:EXCHANGE #x207CD2CE> :QUEUE #<AMQP-1-1-0-8-0:QUEUE #x207CD25E> :ROUTING-KEY "/")
    [20100217T171655Z00] DEBUG #<QUEUE #x207CD25E>: encoding: QUEUE.BIND . (:TICKET 0 :QUEUE "q1" :EXCHANGE "ex" :ROUTING-KEY "/" :NO-WAIT NIL :ARGUMENTS NIL)
    [20100217T171655Z00] DEBUG #<QUEUE #x207CD25E>: send-method: #<QUEUE.BIND #x207CE63E>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{QUEUE.BIND}[19/4088: 0 50 0 20 0 0 2 113 ... 120 1 47 0 0 0 0 0] #x207CBC46>
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{QUEUE.BIND}[19/4088: 0 50 0 20 0 0 2 113 ... 120 1 47 0 0 0 0 0] #x207CBC46>
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.22|t.0].{QUEUE.BIND-OK}[4/4088: 0 50 0 21] #x2077B56E>
    [20100217T171655Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: process-command: #<QUEUE #x207CE2E6> #<QUEUE.BIND-OK #x207CEC1E> . NIL
    [20100217T171655Z00] DEBUG #<QUEUE #x207CE2E6>: bound.
    [20100217T171655Z00] DEBUG #<BASIC #x207CEECE>: send-method: PUBLISH . (:EXCHANGE "ex" :EXCHANGE #<AMQP-1-1-0-8-0:EXCHANGE #x207CD2CE> :ROUTING-KEY "/")
    [20100217T171655Z00] DEBUG #<BASIC #x207CEECE>: encoding: BASIC.PUBLISH . (:TICKET 0 :EXCHANGE "ex" :ROUTING-KEY "/" :MANDATORY NIL :IMMEDIATE NIL)
    [20100217T171655Z00] DEBUG #<BASIC #x207CEECE>: send-method: #<BASIC.PUBLISH #x207CF056>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{BASIC.PUBLISH}[12/4088: 0 60 0 40 0 0 2 101 120 1 47 0] #x207CBC46>
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{BASIC.PUBLISH}[12/4088: 0 60 0 40 0 0 2 101 120 1 47 0] #x207CBC46>
    [20100217T171655Z00] DEBUG #<BASIC #x207CEECE>: encoding: (:CONTENT-TYPE "TEXT/PLAIN" :CONTENT-ENCODING "ISO-8859-1" :HEADERS (:ELEMENT-TYPE "CHARACTER" :PACKAGE "DE.SETF.AMQP.USER" :TRANSFER-ENCODING "chunked") :DELIVERY-MODE 0 :PRIORITY 0 :CORRELATION-ID "" :REPLY-TO "" :EXPIRATION "" :MESSAGE-ID "" :TIMESTAMP 0 :TYPE "" :USER-ID "" :APP-ID "" :CLUSTER-ID "")
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)HEADER|0|c.22|t.0].[145/4088: 0 60 0 0 0 0 0 0 ... 0 0 0 0 0 0 0 0] #x207CBC46>
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)BODY|0|c.22|t.0].[630/4088: 116 104 105 115 32 105 115 32 ... 115 116 32 58 32 51 49 10] #x207CBC46>
    [20100217T171655Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: Ending chunking. padding 3458 bytes...
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)BODY|0|c.22|t.0].[0/4088:] #x207CBC46>
    [20100217T171655Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)BODY|0|c.22|t.0].[3458/4088: 0 0 0 0 0 0 0 0 ... 0 0 0 0 0 0 0 0] #x207CBC46>
    [20100217T171656Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: Ended padding.
    [20100217T171656Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: Close in state: #<AMQP.S:USE-CHANNEL.BODY.OUTPUT #x196B2EAE>
    [20100217T171656Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: send-method: CLOSE . (:REPLY-CODE 0 :REPLY-TEXT "" :CLASS-ID 0 :METHOD-ID 0)
    [20100217T171656Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: encoding: CHANNEL.CLOSE . (:REPLY-CODE 0 :REPLY-TEXT "" :CLASS-ID 0 :METHOD-ID 0)
    [20100217T171656Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: send-method: #<CHANNEL.CLOSE #x207D002E>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{CHANNEL.CLOSE}[11/4088: 0 20 0 40 0 0 0 0 0 0 0] #x207CBC46>
    [20100217T171656Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{CHANNEL.CLOSE}[11/4088: 0 20 0 40 0 0 0 0 0 0 0] #x207CBC46>
    [20100217T171656Z00] DEBUG #<CONNECTION #x1F8AA2D6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.22|t.0].{CHANNEL.CLOSE-OK}[4/4088: 0 20 0 41] #x2077B56E>
    [20100217T171656Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207CB74E>: process-command: #<CHANNEL [amqp://localhost/].22 #x207CB74E> #<CHANNEL.CLOSE-OK #x207D056E> . NIL
    32
    ? (amqp:with-open-channel (input *c* :queue "q1")
        (loop repeat 32  collect (read-line input)))
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: send-method: OPEN . NIL
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: encoding: CHANNEL.OPEN . (:OUT-OF-BAND "")
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: send-method: #<CHANNEL.OPEN #x207D3F06>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{CHANNEL.OPEN}[5/4088: 0 20 0 10 0] #x207D4006>
    [20100217T171659Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{CHANNEL.OPEN}[5/4088: 0 20 0 10 0] #x207D4006>
    [20100217T171659Z00] DEBUG #<CONNECTION #x1F8AA2D6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.22|t.0].{CHANNEL.OPEN-OK}[4/4088: 0 20 0 11] #x207D529E>
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: process-command: #<CHANNEL [amqp://localhost/].22 #x207D3B0E> #<CHANNEL.OPEN-OK #x207D63F6> . NIL
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: Opened: NIL
    [20100217T171659Z00] DEBUG #<QUEUE #x207D6636>: send-method: DECLARE . NIL
    [20100217T171659Z00] DEBUG #<QUEUE #x207D6636>: encoding: QUEUE.DECLARE . (:TICKET 0 :QUEUE "q1" :PASSIVE NIL :DURABLE NIL :EXCLUSIVE NIL :AUTO-DELETE NIL :NO-WAIT NIL :ARGUMENTS NIL)
    [20100217T171659Z00] DEBUG #<QUEUE #x207D6636>: send-method: #<QUEUE.DECLARE #x207D675E>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{QUEUE.DECLARE}[14/4088: 0 50 0 10 0 0 2 113 49 0 0 0 0 0] #x207D4006>
    [20100217T171659Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{QUEUE.DECLARE}[14/4088: 0 50 0 10 0 0 2 113 49 0 0 0 0 0] #x207D4006>
    [20100217T171659Z00] DEBUG #<CONNECTION #x1F8AA2D6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.22|t.0].{QUEUE.DECLARE-OK}[15/4088: 0 50 0 11 2 113 49 0 0 0 3 0 0 0 0] #x207D529E>
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: process-command: #<QUEUE #x207D6DFE> #<QUEUE.DECLARE-OK #x207D6E46> . (:QUEUE "q1" :MESSAGE-COUNT 3 :CONSUMER-COUNT 0)
    [20100217T171659Z00] DEBUG q1: queue declared: q1 3 0
    [20100217T171659Z00] DEBUG #<BASIC #x207D712E>: send-method: GET . (:QUEUE "q1" :QUEUE #<AMQP-1-1-0-8-0:QUEUE #x207D6636>)
    [20100217T171659Z00] DEBUG #<BASIC #x207D712E>: encoding: BASIC.GET . (:TICKET 0 :QUEUE "q1" :NO-ACK NIL)
    [20100217T171659Z00] DEBUG #<BASIC #x207D712E>: send-method: #<BASIC.GET #x207D726E>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{BASIC.GET}[10/4088: 0 60 0 70 0 0 2 113 49 0] #x207D4006>
    [20100217T171659Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{BASIC.GET}[10/4088: 0 60 0 70 0 0 2 113 49 0] #x207D4006>
    [20100217T171659Z00] DEBUG #<CONNECTION #x1F8AA2D6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.22|t.0].{BASIC.GET-OK}[22/4088: 0 60 0 71 0 0 0 0 ... 101 120 1 47 0 0 0 2] #x207D529E>
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: process-command: #<BASIC #x207D712E> #<BASIC.GET-OK #x207D785E> . (:DELIVERY-TAG 1 :REDELIVERED T :EXCHANGE "ex" :ROUTING-KEY "/" :MESSAGE-COUNT 2)
    [20100217T171659Z00] DEBUG #<BASIC #x207D712E>: respond-to-get, get-ok: (:DELIVERY-TAG 1 :REDELIVERED T :EXCHANGE "ex" :ROUTING-KEY "/" :MESSAGE-COUNT 2)
    [20100217T171659Z00] DEBUG #<CONNECTION #x1F8AA2D6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)HEADER|0|c.22|t.0].[145/4088: 0 60 0 0 0 0 0 0 ... 0 0 0 0 0 0 0 0] #x207D7AA6>
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: process-command: #<BASIC #x207D712E> #<HEADER #x3ED3B16> . (:FRAME #<7-BYTE-HEADER-INPUT-FRAME [(+)HEADER|0|c.22|t.0].[145/4088: 0 60 0 0 0 0 0 0 ... 0 0 0 0 0 0 0 0] #x207D7AA6> :CONTENT-TYPE "TEXT/PLAIN" :CONTENT-ENCODING "ISO-8859-1" :HEADERS (:ELEMENT-TYPE "CHARACTER" :PACKAGE "DE.SETF.AMQP.USER" :TRANSFER-ENCODING "chunked") :DELIVERY-MODE 0 :PRIORITY 0 :CORRELATION-ID "" :REPLY-TO "" :EXPIRATION "" :MESSAGE-ID "" :TIMESTAMP 0 :TYPE "" :USER-ID "" :APP-ID "" :CLUSTER-ID "" :CLASS AMQP-1-1-0-8-0:BASIC :WEIGHT 0 :BODY-SIZE 4088)
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: device-read-content: in (CHARACTER #<MIME:TEXT/PLAIN #x3D43A36>) in state #<AMQP.S:USE-CHANNEL.BODY.INPUT.CHUNKED #x196B2626> x4088
    [20100217T171659Z00] DEBUG #<CONNECTION #x1F8AA2D6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)BODY|0|c.22|t.0].[630/4088: 116 104 105 115 32 105 115 32 ... 115 116 32 58 32 51 49 10] #x207D7AA6>
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: device-clear-input: drain expected frames: state: #<USE-CHANNEL.BODY.INPUT.CHUNKED #x196B2626>, at 630 of 4088
    [20100217T171659Z00] DEBUG #<CONNECTION #x1F8AA2D6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)BODY|0|c.22|t.0].[3458/4088: 0 0 0 0 0 0 0 0 ... 0 0 0 0 0 0 0 0] #x207D7AA6>
    [20100217T171659Z00] DEBUG #<BASIC #x207D712E>: send-method: ACK . (:DELIVERY-TAG 1)
    [20100217T171659Z00] DEBUG #<BASIC #x207D712E>: encoding: BASIC.ACK . (:DELIVERY-TAG 1 :MULTIPLE NIL)
    [20100217T171659Z00] DEBUG #<BASIC #x207D712E>: send-method: #<BASIC.ACK #x207D9DFE>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{BASIC.ACK}[13/4088: 0 60 0 80 0 0 0 0 0 0 0 1 0] #x207D4006>
    [20100217T171659Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{BASIC.ACK}[13/4088: 0 60 0 80 0 0 0 0 0 0 0 1 0] #x207D4006>
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: Close in state: #<AMQP.S:USE-CHANNEL.BODY.INPUT.CHUNKED #x196B2626>
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: send-method: CLOSE . (:REPLY-CODE 0 :REPLY-TEXT "" :CLASS-ID 0 :METHOD-ID 0)
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: encoding: CHANNEL.CLOSE . (:REPLY-CODE 0 :REPLY-TEXT "" :CLASS-ID 0 :METHOD-ID 0)
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: send-method: #<CHANNEL.CLOSE #x207DA40E>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{CHANNEL.CLOSE}[11/4088: 0 20 0 40 0 0 0 0 0 0 0] #x207D4006>
    [20100217T171659Z00] DEBUG #<CONNECTION #x1F8AA2D6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.22|t.0].{CHANNEL.CLOSE}[11/4088: 0 20 0 40 0 0 0 0 0 0 0] #x207D4006>
    [20100217T171659Z00] DEBUG #<CONNECTION #x1F8AA2D6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.22|t.0].{CHANNEL.CLOSE-OK}[4/4088: 0 20 0 41] #x207D529E>
    [20100217T171659Z00] DEBUG #<CHANNEL [amqp://localhost/].22 #x207D3B0E>: process-command: #<CHANNEL [amqp://localhost/].22 #x207D3B0E> #<CHANNEL.CLOSE-OK #x207DA94E> . NIL
    ("this is a test : 0" "this is a test : 1" "this is a test : 2" "this is a test : 3" "this is a test : 4" "this is a test : 5" "this is a test : 6" "this is a test : 7" "this is a test : 8" "this is a test : 9" "this is a test : 10" "this is a test : 11" "this is a test : 12" "this is a test : 13" "this is a test : 14" "this is a test : 15" "this is a test : 16" "this is a test : 17" "this is a test : 18" "this is a test : 19" "this is a test : 20" "this is a test : 21" "this is a test : 22" "this is a test : 23" "this is a test : 24" "this is a test : 25" "this is a test : 26" "this is a test : 27" "this is a test : 28" "this is a test : 29" "this is a test : 30" "this is a test : 31")
    ? 
    
If we look at the behavior of a different server: 

    ? (defparameter *c* (make-instance 'amqp:connection :uri "amqp://guest:guest@localhost/"))
    *C*
    ? (amqp:connection-server-properties *c*)
    NIL
    ? ;;; i ommit the writer
    (amqp:with-open-channel (input *c* :queue "q1")
      (loop
        (unless (print (read-line input nil nil))
          (return))))
    [20100217T191524Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: send-method: OPEN . NIL
    [20100217T191524Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: encoding: CHANNEL.OPEN . ()
    [20100217T191524Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: send-method: #<CHANNEL.OPEN #x20D856A6>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.1|t.0].{CHANNEL.OPEN}[5/4088: 0 20 0 10 0] #x20D857AE>
    [20100217T191524Z00] DEBUG #<CONNECTION #x20D791B6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.1|t.0].{CHANNEL.OPEN}[5/4088: 0 20 0 10 0] #x20D857AE>
    [20100217T191524Z00] DEBUG #<CONNECTION #x20D791B6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.1|t.0].{CHANNEL.OPEN-OK}[24/4088: 0 20 0 11 0 0 0 16 ... 189 32 238 228 119 85 207 129] #x20D86A5E>
    [20100217T191524Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: process-command: #<CHANNEL [amqp://localhost/].1 #x20D852A6> #<CHANNEL.OPEN-OK #x20D87C86> . NIL
    [20100217T191524Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: Opened: NIL
    [20100217T191524Z00] DEBUG #<QUEUE #x20D87EFE>: send-method: DECLARE . NIL
    [20100217T191524Z00] DEBUG #<QUEUE #x20D87EFE>: encoding: QUEUE.DECLARE . (:QUEUE "q1" :PASSIVE NIL :DURABLE NIL :EXCLUSIVE NIL :AUTO-DELETE NIL :NO-WAIT NIL :ARGUMENTS NIL)
    [20100217T191524Z00] DEBUG #<QUEUE #x20D87EFE>: send-method: #<QUEUE.DECLARE #x20D88026>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.1|t.0].{QUEUE.DECLARE}[14/4088: 0 50 0 10 0 0 2 113 49 0 0 0 0 0] #x20D857AE>
    [20100217T191524Z00] DEBUG #<CONNECTION #x20D791B6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.1|t.0].{QUEUE.DECLARE}[14/4088: 0 50 0 10 0 0 2 113 49 0 0 0 0 0] #x20D857AE>
    [20100217T191524Z00] DEBUG #<CONNECTION #x20D791B6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.1|t.0].{QUEUE.DECLARE-OK}[15/4088: 0 50 0 11 2 113 49 0 0 0 1 0 0 0 0] #x20D86A5E>
    [20100217T191524Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: process-command: #<QUEUE #x20D886F6> #<QUEUE.DECLARE-OK #x20D8873E> . (:QUEUE "q1" :MESSAGE-COUNT 1 :CONSUMER-COUNT 0)
    [20100217T191524Z00] DEBUG q1: queue declared: q1 1 0
    [20100217T191524Z00] DEBUG #<BASIC #x20D88A36>: send-method: GET . (:QUEUE "q1" :QUEUE #<AMQP-1-1-0-9-1:QUEUE #x20D87EFE>)
    [20100217T191524Z00] DEBUG #<BASIC #x20D88A36>: encoding: BASIC.GET . (:QUEUE "q1" :NO-ACK NIL)
    [20100217T191524Z00] DEBUG #<BASIC #x20D88A36>: send-method: #<BASIC.GET #x20D88B7E>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.1|t.0].{BASIC.GET}[10/4088: 0 60 0 70 0 0 2 113 49 0] #x20D857AE>
    [20100217T191524Z00] DEBUG #<CONNECTION #x20D791B6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.1|t.0].{BASIC.GET}[10/4088: 0 60 0 70 0 0 2 113 49 0] #x20D857AE>
    [20100217T191525Z00] DEBUG #<CONNECTION #x20D791B6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.1|t.0].{BASIC.GET-OK}[22/4088: 0 60 0 71 0 0 0 0 ... 101 120 1 47 0 0 0 1] #x20D86A5E>
    [20100217T191525Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: process-command: #<BASIC #x20D88A36> #<BASIC.GET-OK #x20D8919E> . (:DELIVERY-TAG 1 :REDELIVERED NIL :EXCHANGE "ex" :ROUTING-KEY "/" :MESSAGE-COUNT 1)
    [20100217T191525Z00] DEBUG #<BASIC #x20D88A36>: respond-to-get, get-ok: (:DELIVERY-TAG 1 :REDELIVERED NIL :EXCHANGE "ex" :ROUTING-KEY "/" :MESSAGE-COUNT 1)
    [20100217T191525Z00] DEBUG #<CONNECTION #x20D791B6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)HEADER|0|c.1|t.0].[135/4088: 0 60 0 0 0 0 0 0 ... 0 0 0 0 0 0 0 0] #x20D893F6>
    [20100217T191525Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: process-command: #<BASIC #x20D88A36> #<HEADER #x3ED3B16> . (:FRAME #<7-BYTE-HEADER-INPUT-FRAME [(+)HEADER|0|c.1|t.0].[135/4088: 0 60 0 0 0 0 0 0 ... 0 0 0 0 0 0 0 0] #x20D893F6> :CONTENT-TYPE "TEXT/PLAIN" :CONTENT-ENCODING "ISO-8859-1" :HEADERS (:ELEMENT-TYPE "CHARACTER" :PACKAGE "DE.SETF.AMQP.USER" :TRANSFER-ENCODING "chunked") :DELIVERY-MODE 0 :PRIORITY 0 :CORRELATION-ID "" :REPLY-TO "" :EXPIRATION "" :MESSAGE-ID "" :TIMESTAMP 0 :TYPE "" :USER-ID "" :APP-ID "" :CLASS AMQP-1-1-0-9-1:BASIC :WEIGHT 0 :BODY-SIZE 4088)
    [20100217T191525Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: device-read-content: in (CHARACTER #<MIME:TEXT/PLAIN #x3D43A36>) in state #<AMQP.S:USE-CHANNEL.BODY.INPUT.CHUNKED #x196B2626> x4088
    [20100217T191525Z00] DEBUG #<CONNECTION #x20D791B6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)BODY|0|c.1|t.0].[133/4088: 116 104 105 115 32 105 115 32 ... 101 115 116 32 58 32 54 10] #x20D893F6>
    "this is a test : 0" 
    "this is a test : 1" 
    "this is a test : 2" 
    "this is a test : 3" 
    "this is a test : 4" 
    "this is a test : 5" 
    "this is a test : 6" 
    [20100217T191525Z00] DEBUG #<CONNECTION #x20D791B6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)BODY|0|c.1|t.0].[0/4088:] #x20D893F6>
    [20100217T191525Z00] DEBUG #<CONNECTION #x20D791B6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)BODY|0|c.1|t.0].[3955/4088: 0 0 0 0 0 0 0 0 ... 0 0 0 0 0 0 0 0] #x20D893F6>
    "" 
    NIL 
    [20100217T191525Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: device-clear-input: drain expected frames: state: #<USE-CHANNEL.BODY.INPUT #x196B200E>, at -1 of 133
    [20100217T191525Z00] DEBUG #<BASIC #x20D88A36>: send-method: ACK . (:DELIVERY-TAG 1)
    [20100217T191525Z00] DEBUG #<BASIC #x20D88A36>: encoding: BASIC.ACK . (:DELIVERY-TAG 1 :MULTIPLE NIL)
    [20100217T191525Z00] DEBUG #<BASIC #x20D88A36>: send-method: #<BASIC.ACK #x20D8B266>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.1|t.0].{BASIC.ACK}[13/4088: 0 60 0 80 0 0 0 0 0 0 0 1 0] #x20D857AE>
    [20100217T191525Z00] DEBUG #<CONNECTION #x20D791B6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.1|t.0].{BASIC.ACK}[13/4088: 0 60 0 80 0 0 0 0 0 0 0 1 0] #x20D857AE>
    [20100217T191525Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: Close in state: #<AMQP.S:USE-CHANNEL.BODY.INPUT #x196B200E>
    [20100217T191525Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: send-method: CLOSE . (:REPLY-CODE 0 :REPLY-TEXT "" :CLASS-ID 0 :METHOD-ID 0)
    [20100217T191525Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: encoding: CHANNEL.CLOSE . (:REPLY-CODE 0 :REPLY-TEXT "" :CLASS-ID 0 :METHOD-ID 0)
    [20100217T191525Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: send-method: #<CHANNEL.CLOSE #x20D8B8A6>  #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.1|t.0].{CHANNEL.CLOSE}[11/4088: 0 20 0 40 0 0 0 0 0 0 0] #x20D857AE>
    [20100217T191525Z00] DEBUG #<CONNECTION #x20D791B6>: write-frame: (NIL,NIL) #<7-BYTE-HEADER-OUTPUT-FRAME [(+)METHOD|0|c.1|t.0].{CHANNEL.CLOSE}[11/4088: 0 20 0 40 0 0 0 0 0 0 0] #x20D857AE>
    [20100217T191525Z00] DEBUG #<CONNECTION #x20D791B6>: read-frame: (NIL,NIL) #<7-BYTE-HEADER-INPUT-FRAME [(+)METHOD|0|c.1|t.0].{CHANNEL.CLOSE-OK}[4/4088: 0 20 0 41] #x20D86A5E>
    [20100217T191525Z00] DEBUG #<CHANNEL [amqp://localhost/].1 #x20D852A6>: process-command: #<CHANNEL [amqp://localhost/].1 #x20D852A6> #<CHANNEL.CLOSE-OK #x20D8BE16> . NIL
    NIL
    ? 

We see that the zero-length frame arrives, the channel can recognize the end of the stream, change from the state `AMQP.S:USE-CHANNEL.BODY.INPUT.CHUNKED`,
recognize the premature end of the message body,
and signal signal an EOF to the application.

This is very useful.
Why does RabbtMQ drop the zero-length frames?
