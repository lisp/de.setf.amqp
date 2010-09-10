;;; -*- Package: DE.SETF.AMQP.IMPLEMENTATION; -*-
;;;
;;; version: amqp-1-1-0-8-0
;;; generated 20100129T191413

(in-package "DE.SETF.AMQP.IMPLEMENTATION")

(:documentation "This file contains generated protocol classes and wire-level codecs for AMQP based on the
  xml-encoded protocol specification."
 (copyright
  "Copyright 2010 [james anderson](mailto:james.anderson@setf.de)"
  "'setf.amqp' is free software: you can redistribute it and/or modify
  it under the terms of the GNU Lesser General Public License as published by
  the Free Software Foundation, either version 3 of the License, or
  (at your option) any later version.

  'setf.amqp' is distributed in the hope that it will be useful,
  but WITHOUT ANY WARRANTY; without even the implied warranty of
  MERCHANTABILITY or FITNESS FOR A PARTICULAR PURPOSE.  See the
  GNU Lesser General Public License for more details.

  You should have received a copy of the GNU Lesser General Public License
  along with 'setf.amqp'.  If not, see the GNU [http://www.gnu.org/licenses/ site]."))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((ensure-export (x) `(export (intern ,(string x) :amqp) :amqp))) 
    (ensure-export #:access)
    (ensure-export #:ack)
    (ensure-export #:alert)
    (ensure-export #:basic)
    (ensure-export #:bind)
    (ensure-export #:bind-ok)
    (ensure-export #:cancel)
    (ensure-export #:cancel-ok)
    (ensure-export #:channel)
    (ensure-export #:close)
    (ensure-export #:close-ok)
    (ensure-export #:commit)
    (ensure-export #:commit-ok)
    (ensure-export #:connection)
    (ensure-export #:consume)
    (ensure-export #:consume-ok)
    (ensure-export #:declare)
    (ensure-export #:declare-ok)
    (ensure-export #:delete)
    (ensure-export #:delete-ok)
    (ensure-export #:deliver)
    (ensure-export #:dtx)
    (ensure-export #:exchange)
    (ensure-export #:file)
    (ensure-export #:flow)
    (ensure-export #:flow-ok)
    (ensure-export #:get)
    (ensure-export #:get-empty)
    (ensure-export #:get-ok)
    (ensure-export #:open)
    (ensure-export #:open-ok)
    (ensure-export #:publish)
    (ensure-export #:purge)
    (ensure-export #:purge-ok)
    (ensure-export #:qos)
    (ensure-export #:qos-ok)
    (ensure-export #:queue)
    (ensure-export #:recover)
    (ensure-export #:redirect)
    (ensure-export #:reject)
    (ensure-export #:request)
    (ensure-export #:request-ok)
    (ensure-export #:return)
    (ensure-export #:rollback)
    (ensure-export #:rollback-ok)
    (ensure-export #:secure)
    (ensure-export #:secure-ok)
    (ensure-export #:select)
    (ensure-export #:select-ok)
    (ensure-export #:stage)
    (ensure-export #:start)
    (ensure-export #:start-ok)
    (ensure-export #:stream)
    (ensure-export #:test)
    (ensure-export #:test-content)
    (ensure-export #:test-content-ok)
    (ensure-export #:test-integer)
    (ensure-export #:test-integer-ok)
    (ensure-export #:test-string)
    (ensure-export #:test-string-ok)
    (ensure-export #:test-table)
    (ensure-export #:test-table-ok)
    (ensure-export #:tune)
    (ensure-export #:tune-ok)
    (ensure-export #:tunnel)
    (ensure-export #:tx)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((ensure-export (x) `(export (intern ,(string x) :amqp-1-1-0-8-0) :amqp-1-1-0-8-0))) 
    (ensure-export #:access)
    (ensure-export #:access.request)
    (ensure-export #:access.request-ok)
    (ensure-export #:basic)
    (ensure-export #:basic.ack)
    (ensure-export #:basic.cancel)
    (ensure-export #:basic.cancel-ok)
    (ensure-export #:basic.consume)
    (ensure-export #:basic.consume-ok)
    (ensure-export #:basic.deliver)
    (ensure-export #:basic.get)
    (ensure-export #:basic.get-empty)
    (ensure-export #:basic.get-ok)
    (ensure-export #:basic.publish)
    (ensure-export #:basic.qos)
    (ensure-export #:basic.qos-ok)
    (ensure-export #:basic.recover)
    (ensure-export #:basic.reject)
    (ensure-export #:basic.return)
    (ensure-export #:channel)
    (ensure-export #:channel.alert)
    (ensure-export #:channel.close)
    (ensure-export #:channel.close-ok)
    (ensure-export #:channel.flow)
    (ensure-export #:channel.flow-ok)
    (ensure-export #:channel.open)
    (ensure-export #:channel.open-ok)
    (ensure-export #:connection)
    (ensure-export #:connection.close)
    (ensure-export #:connection.close-ok)
    (ensure-export #:connection.open)
    (ensure-export #:connection.open-ok)
    (ensure-export #:connection.redirect)
    (ensure-export #:connection.secure)
    (ensure-export #:connection.secure-ok)
    (ensure-export #:connection.start)
    (ensure-export #:connection.start-ok)
    (ensure-export #:connection.tune)
    (ensure-export #:connection.tune-ok)
    (ensure-export #:dtx)
    (ensure-export #:dtx.select)
    (ensure-export #:dtx.select-ok)
    (ensure-export #:dtx.start)
    (ensure-export #:dtx.start-ok)
    (ensure-export #:exchange)
    (ensure-export #:exchange.declare)
    (ensure-export #:exchange.declare-ok)
    (ensure-export #:exchange.delete)
    (ensure-export #:exchange.delete-ok)
    (ensure-export #:file)
    (ensure-export #:file.ack)
    (ensure-export #:file.cancel)
    (ensure-export #:file.cancel-ok)
    (ensure-export #:file.consume)
    (ensure-export #:file.consume-ok)
    (ensure-export #:file.deliver)
    (ensure-export #:file.open)
    (ensure-export #:file.open-ok)
    (ensure-export #:file.publish)
    (ensure-export #:file.qos)
    (ensure-export #:file.qos-ok)
    (ensure-export #:file.reject)
    (ensure-export #:file.return)
    (ensure-export #:file.stage)
    (ensure-export #:queue)
    (ensure-export #:queue.bind)
    (ensure-export #:queue.bind-ok)
    (ensure-export #:queue.declare)
    (ensure-export #:queue.declare-ok)
    (ensure-export #:queue.delete)
    (ensure-export #:queue.delete-ok)
    (ensure-export #:queue.purge)
    (ensure-export #:queue.purge-ok)
    (ensure-export #:stream)
    (ensure-export #:stream.cancel)
    (ensure-export #:stream.cancel-ok)
    (ensure-export #:stream.consume)
    (ensure-export #:stream.consume-ok)
    (ensure-export #:stream.deliver)
    (ensure-export #:stream.publish)
    (ensure-export #:stream.qos)
    (ensure-export #:stream.qos-ok)
    (ensure-export #:stream.return)
    (ensure-export #:test)
    (ensure-export #:test.content)
    (ensure-export #:test.content-ok)
    (ensure-export #:test.integer)
    (ensure-export #:test.integer-ok)
    (ensure-export #:test.string)
    (ensure-export #:test.string-ok)
    (ensure-export #:test.table)
    (ensure-export #:test.table-ok)
    (ensure-export #:tunnel)
    (ensure-export #:tunnel.request)
    (ensure-export #:tx)
    (ensure-export #:tx.commit)
    (ensure-export #:tx.commit-ok)
    (ensure-export #:tx.rollback)
    (ensure-export #:tx.rollback-ok)
    (ensure-export #:tx.select)
    (ensure-export #:tx.select-ok)))


;;; class: connection [id method-names]
;;;   connection.start [version-major version-minor server-properties mechanisms locales]
;;;   connection.start-ok [client-properties mechanism response locale]
;;;   connection.secure [challenge]
;;;   connection.secure-ok [response]
;;;   connection.tune [channel-max frame-max heartbeat]
;;;   connection.tune-ok [channel-max frame-max heartbeat]
;;;   connection.open [virtual-host capabilities insist]
;;;   connection.open-ok [known-hosts]
;;;   connection.redirect [host known-hosts]
;;;   connection.close [reply-code reply-text class-id method-id]
;;;   connection.close-ok

(def-amqp-class amqp-1-1-0-8-0:connection (amqp-1-1-0-8-0:object amqp:connection)
  ((id :initform 10 :allocation :class)
   (method-names :initform
    '(amqp:start amqp:start-ok amqp:secure amqp:secure-ok amqp:tune amqp:tune-ok amqp:open amqp:open-ok amqp:redirect
      amqp:close amqp:close-ok)
    :allocation :class))
  ()
  ((version-major :initform (field-type-initform version-major amqp-1-1-0-8-0::octet) :type amqp-1-1-0-8-0::octet
    :documentation "protocol major version

 The protocol major version that the server agrees to use, which
 cannot be higher than the client's major version.")
   (version-minor :initform (field-type-initform version-minor amqp-1-1-0-8-0::octet) :type amqp-1-1-0-8-0::octet
    :documentation "protocol major version

 The protocol minor version that the server agrees to use, which
 cannot be higher than the client's minor version.")
   (server-properties :initform (field-type-initform server-properties amqp-1-1-0-8-0::peer-properties) :type
    amqp-1-1-0-8-0::peer-properties :documentation "server properties")
   (mechanisms :initform (field-type-initform mechanisms amqp-1-1-0-8-0::longstr) :type amqp-1-1-0-8-0::longstr
    :documentation "available security mechanisms

 A list of the security mechanisms that the server supports, delimited
 by spaces. Currently ASL supports these mechanisms: PLAIN.")
   (locales :initform (field-type-initform locales amqp-1-1-0-8-0::longstr) :type amqp-1-1-0-8-0::longstr
    :documentation "available message locales

 A list of the message locales that the server supports, delimited
 by spaces. The locale defines the language in which the server
 will send reply texts.

 All servers MUST support at least the en_US locale.")
   (client-properties :initform (field-type-initform client-properties amqp-1-1-0-8-0::peer-properties) :type
    amqp-1-1-0-8-0::peer-properties :documentation "client properties")
   (mechanism :initform (field-type-initform mechanism amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "selected security mechanism

 A single security mechanisms selected by the client, which must be
 one of those specified by the server.

 The client SHOULD authenticate using the highest-level security
 profile it can handle from the list provided by the server.

 The mechanism field MUST contain one of the security mechanisms
 proposed by the server in the Start method. If it doesn't, the
 server MUST close the socket.")
   (locale :initform (field-type-initform locale amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "selected message locale

 A single message local selected by the client, which must be one
 of those specified by the server.")
   (challenge :initform (field-type-initform challenge amqp-1-1-0-8-0::longstr) :type amqp-1-1-0-8-0::longstr
    :documentation "security challenge data

 Challenge information, a block of opaque binary data passed to
 the security mechanism.")
   (response :initform (field-type-initform response amqp-1-1-0-8-0::longstr) :type amqp-1-1-0-8-0::longstr
    :documentation "security response data

 A block of opaque data passed to the security mechanism. The contents
 of this data are defined by the SASL security mechanism.")
   (channel-max :initform (field-type-initform channel-max amqp-1-1-0-8-0::short) :type amqp-1-1-0-8-0::short
    :documentation "negotiated maximum channels

 The maximum total number of channels that the client will use
 per connection. May not be higher than the value specified by
 the server.

 The server MAY ignore the channel-max value or MAY use it for
 tuning its resource allocation.")
   (frame-max :initform (field-type-initform frame-max amqp-1-1-0-8-0::long) :type amqp-1-1-0-8-0::long :documentation
    "negotiated maximum frame size

 The largest frame size that the client and server will use for
 the connection. Zero means that the client does not impose any
 specific limit but may reject very large frames if it cannot
 allocate resources for them. Note that the frame-max limit
 applies principally to content frames, where large contents
 can be broken into frames of arbitrary size.

 Until the frame-max has been negotiated, both peers must accept
 frames of up to 4096 octets large. The minimum non-zero value for
 the frame-max field is 4096.")
   (heartbeat :initform (field-type-initform heartbeat amqp-1-1-0-8-0::short) :type amqp-1-1-0-8-0::short
    :documentation "desired heartbeat delay

 The delay, in seconds, of the connection heartbeat that the client
 wants. Zero means the client does not want a heartbeat.")
   (virtual-host :initform (field-type-initform virtual-host amqp-1-1-0-8-0::path) :type amqp-1-1-0-8-0::path
    :documentation "virtual host name

 The name of the virtual host to work with.

 If the server supports multiple virtual hosts, it MUST enforce a
 full separation of exchanges, queues, and all associated entities
 per virtual host. An application, connected to a specific virtual
 host, MUST NOT be able to access resources of another virtual host.

 The server SHOULD verify that the client has permission to access
 the specified virtual host.

 The server MAY configure arbitrary limits per virtual host, such
 as the number of each type of entity that may be used, per
 connection and/or in total.")
   (capabilities :initform (field-type-initform capabilities amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "required capabilities

 The client may specify a number of capability names, delimited by
 spaces. The server can use this string to how to process the
 client's connection request.")
   (insist :initform (field-type-initform insist amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "insist on connecting to server

 In a configuration with multiple load-sharing servers, the server
 may respond to a Connection.Open method with a Connection.Redirect.
 The insist option tells the server that the client is insisting on
 a connection to the specified server.

 When the client uses the insist option, the server SHOULD accept
 the client connection unless it is technically unable to do so.")
   (host :initform (field-type-initform host amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr :documentation
    "server to connect to

 Specifies the server to connect to. This is an IP address or a
 DNS name, optionally followed by a colon and a port number. If
 no port number is specified, the client should use the default
 port number for the protocol.")
   (known-hosts :initform (field-type-initform known-hosts amqp-1-1-0-8-0::known-hosts) :type
    amqp-1-1-0-8-0::known-hosts)
   (reply-code :initform (field-type-initform reply-code amqp-1-1-0-8-0::reply-code) :type amqp-1-1-0-8-0::reply-code)
   (reply-text :initform (field-type-initform reply-text amqp-1-1-0-8-0::reply-text) :type amqp-1-1-0-8-0::reply-text)
   (class-id :initform (field-type-initform class-id amqp-1-1-0-8-0::class-id) :type amqp-1-1-0-8-0::class-id
    :documentation "failing method class

 When the close is provoked by a method exception, this is the
 class of the method.")
   (method-id :initform (field-type-initform method-id amqp-1-1-0-8-0::class-id) :type amqp-1-1-0-8-0::class-id
    :documentation "failing method ID

 When the close is provoked by a method exception, this is the
 ID of the method."))
  (:documentation "roles: server MUST; client MUST.

  The connection class provides methods for a client to establish a
  network connection to a server, and for both peers to operate the
  connection thereafter.

    connection          = open-connection *use-connection close-connection
    open-connection     = C:protocol-header
                          S:START C:START-OK
                          *challenge
                          S:TUNE C:TUNE-OK
                          C:OPEN S:OPEN-OK | S:REDIRECT
    challenge           = S:SECURE C:SECURE-OK
    use-connection      = *channel
    close-connection    = C:CLOSE S:CLOSE-OK
                        / S:CLOSE C:CLOSE-OK
"))


(def-amqp-method (amqp-1-1-0-8-0:connection amqp:start) (amqp:start amqp-1-1-0-8-0:method)
  ((id :initform 10))
  ((version-major
   :initform (field-type-initform version-major amqp-1-1-0-8-0::octet)
   :type amqp-1-1-0-8-0::octet
   :documentation "protocol major version

 The protocol major version that the server agrees to use, which
 cannot be higher than the client's major version.")
  (version-minor
   :initform (field-type-initform version-minor amqp-1-1-0-8-0::octet)
   :type amqp-1-1-0-8-0::octet
   :documentation "protocol major version

 The protocol minor version that the server agrees to use, which
 cannot be higher than the client's minor version.")
  (server-properties
   :initform (field-type-initform server-properties amqp-1-1-0-8-0::peer-properties)
   :type amqp-1-1-0-8-0::peer-properties
   :documentation "server properties")
  (mechanisms
   :initform (field-type-initform mechanisms amqp-1-1-0-8-0::longstr)
   :type amqp-1-1-0-8-0::longstr
   :documentation "available security mechanisms

 A list of the security mechanisms that the server supports, delimited
 by spaces. Currently ASL supports these mechanisms: PLAIN.")
  (locales
   :initform (field-type-initform locales amqp-1-1-0-8-0::longstr)
   :type amqp-1-1-0-8-0::longstr
   :documentation "available message locales

 A list of the message locales that the server supports, delimited
 by spaces. The locale defines the language in which the server
 will send reply texts.

 All servers MUST support at least the en_US locale."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:connection amqp:start-ok) (amqp:start-ok amqp-1-1-0-8-0:method)
  ((id :initform 11))
  ((client-properties
   :initform (field-type-initform client-properties amqp-1-1-0-8-0::peer-properties)
   :type amqp-1-1-0-8-0::peer-properties
   :documentation "client properties")
  (mechanism
   :initform (field-type-initform mechanism amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "selected security mechanism

 A single security mechanisms selected by the client, which must be
 one of those specified by the server.

 The client SHOULD authenticate using the highest-level security
 profile it can handle from the list provided by the server.

 The mechanism field MUST contain one of the security mechanisms
 proposed by the server in the Start method. If it doesn't, the
 server MUST close the socket.")
  (response
   :initform (field-type-initform response amqp-1-1-0-8-0::longstr)
   :type amqp-1-1-0-8-0::longstr
   :documentation "security response data

 A block of opaque data passed to the security mechanism. The contents
 of this data are defined by the SASL security mechanism. For the
 PLAIN security mechanism this is defined as a field table holding
 two fields, LOGIN and PASSWORD.")
  (locale
   :initform (field-type-initform locale amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "selected message locale

 A single message local selected by the client, which must be one
 of those specified by the server."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:connection amqp:secure) (amqp:secure amqp-1-1-0-8-0:method)
  ((id :initform 20))
  ((challenge
   :initform (field-type-initform challenge amqp-1-1-0-8-0::longstr)
   :type amqp-1-1-0-8-0::longstr
   :documentation "security challenge data

 Challenge information, a block of opaque binary data passed to
 the security mechanism."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:connection amqp:secure-ok) (amqp:secure-ok amqp-1-1-0-8-0:method)
  ((id :initform 21))
  ((response
   :initform (field-type-initform response amqp-1-1-0-8-0::longstr)
   :type amqp-1-1-0-8-0::longstr
   :documentation "security response data

 A block of opaque data passed to the security mechanism. The contents
 of this data are defined by the SASL security mechanism."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:connection amqp:tune) (amqp:tune amqp-1-1-0-8-0:method)
  ((id :initform 30))
  ((channel-max
   :initform (field-type-initform channel-max amqp-1-1-0-8-0::short)
   :type amqp-1-1-0-8-0::short
   :documentation "proposed maximum channels

 The maximum total number of channels that the server allows
 per connection. Zero means that the server does not impose a
 fixed limit, but the number of allowed channels may be limited
 by available server resources.")
  (frame-max
   :initform (field-type-initform frame-max amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "proposed maximum frame size

 The largest frame size that the server proposes for the
 connection. The client can negotiate a lower value. Zero means
 that the server does not impose any specific limit but may reject
 very large frames if it cannot allocate resources for them.

 Until the frame-max has been negotiated, both peers MUST accept
 frames of up to 4096 octets large. The minimum non-zero value for
 the frame-max field is 4096.")
  (heartbeat
   :initform (field-type-initform heartbeat amqp-1-1-0-8-0::short)
   :type amqp-1-1-0-8-0::short
   :documentation "desired heartbeat delay

 The delay, in seconds, of the connection heartbeat that the server
 wants. Zero means the server does not want a heartbeat."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:connection amqp:tune-ok) (amqp:tune-ok amqp-1-1-0-8-0:method)
  ((id :initform 31))
  ((channel-max
   :initform (field-type-initform channel-max amqp-1-1-0-8-0::short)
   :type amqp-1-1-0-8-0::short
   :documentation "negotiated maximum channels

 The maximum total number of channels that the client will use
 per connection. May not be higher than the value specified by
 the server.

 The server MAY ignore the channel-max value or MAY use it for
 tuning its resource allocation.")
  (frame-max
   :initform (field-type-initform frame-max amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "negotiated maximum frame size

 The largest frame size that the client and server will use for
 the connection. Zero means that the client does not impose any
 specific limit but may reject very large frames if it cannot
 allocate resources for them. Note that the frame-max limit
 applies principally to content frames, where large contents
 can be broken into frames of arbitrary size.

 Until the frame-max has been negotiated, both peers must accept
 frames of up to 4096 octets large. The minimum non-zero value for
 the frame-max field is 4096.")
  (heartbeat
   :initform (field-type-initform heartbeat amqp-1-1-0-8-0::short)
   :type amqp-1-1-0-8-0::short
   :documentation "desired heartbeat delay

 The delay, in seconds, of the connection heartbeat that the client
 wants. Zero means the client does not want a heartbeat."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:connection amqp:open) (amqp:open amqp-1-1-0-8-0:method)
  ((id :initform 40))
  ((virtual-host
   :initform (field-type-initform virtual-host amqp-1-1-0-8-0::path)
   :type amqp-1-1-0-8-0::path
   :documentation "virtual host name

 The name of the virtual host to work with.

 If the server supports multiple virtual hosts, it MUST enforce a
 full separation of exchanges, queues, and all associated entities
 per virtual host. An application, connected to a specific virtual
 host, MUST NOT be able to access resources of another virtual host.

 The server SHOULD verify that the client has permission to access
 the specified virtual host.

 The server MAY configure arbitrary limits per virtual host, such
 as the number of each type of entity that may be used, per
 connection and/or in total.")
  (capabilities
   :initform (field-type-initform capabilities amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "required capabilities

 The client may specify a number of capability names, delimited by
 spaces. The server can use this string to how to process the
 client's connection request.")
  (insist
   :initform (field-type-initform insist amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "insist on connecting to server

 In a configuration with multiple load-sharing servers, the server
 may respond to a Connection.Open method with a Connection.Redirect.
 The insist option tells the server that the client is insisting on
 a connection to the specified server.

 When the client uses the insist option, the server SHOULD accept
 the client connection unless it is technically unable to do so."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:connection amqp:open-ok) (amqp:open-ok amqp-1-1-0-8-0:method)
  ((id :initform 41))
  ((known-hosts
   :initform (field-type-initform known-hosts amqp-1-1-0-8-0::known-hosts)
   :type amqp-1-1-0-8-0::known-hosts))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:connection amqp:redirect) (amqp:redirect amqp-1-1-0-8-0:method)
  ((id :initform 50))
  ((host
   :initform (field-type-initform host amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "server to connect to

 Specifies the server to connect to. This is an IP address or a
 DNS name, optionally followed by a colon and a port number. If
 no port number is specified, the client should use the default
 port number for the protocol.")
  (known-hosts
   :initform (field-type-initform known-hosts amqp-1-1-0-8-0::known-hosts)
   :type amqp-1-1-0-8-0::known-hosts))
  (:documentation "roles: client MAY."))

(def-amqp-method (amqp-1-1-0-8-0:connection amqp:close) (amqp:close amqp-1-1-0-8-0:method)
  ((id :initform 60))
  ((reply-code
   :initform (field-type-initform reply-code amqp-1-1-0-8-0::reply-code)
   :type amqp-1-1-0-8-0::reply-code)
  (reply-text
   :initform (field-type-initform reply-text amqp-1-1-0-8-0::reply-text)
   :type amqp-1-1-0-8-0::reply-text)
  (class-id
   :initform (field-type-initform class-id amqp-1-1-0-8-0::class-id)
   :type amqp-1-1-0-8-0::class-id
   :documentation "failing method class

 When the close is provoked by a method exception, this is the
 class of the method.")
  (method-id
   :initform (field-type-initform method-id amqp-1-1-0-8-0::class-id)
   :type amqp-1-1-0-8-0::class-id
   :documentation "failing method ID

 When the close is provoked by a method exception, this is the
 ID of the method."))
  (:documentation "roles: client MUST; server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:connection amqp:close-ok) (amqp:close-ok amqp-1-1-0-8-0:method)
  ((id :initform 61))
  ()
  (:documentation "roles: client MUST; server MUST."))

;;; class: channel [id method-names]
;;;   channel.open [out-of-band]
;;;   channel.open-ok
;;;   channel.flow [active]
;;;   channel.flow-ok [active]
;;;   channel.alert [reply-code reply-text details]
;;;   channel.close [reply-code reply-text class-id method-id]
;;;   channel.close-ok

(def-amqp-class amqp-1-1-0-8-0:channel (amqp-1-1-0-8-0:object amqp:channel)
  ((id :initform 20 :allocation :class)
   (method-names :initform '(amqp:open amqp:open-ok amqp:flow amqp:flow-ok amqp:alert amqp:close amqp:close-ok)
    :allocation :class))
  ()
  ((out-of-band :initform (field-type-initform out-of-band amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "out-of-band settings

 Configures out-of-band transfers on this channel. The syntax and
 meaning of this field will be formally defined at a later date.")
   (active :initform (field-type-initform active amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "current flow setting

 Confirms the setting of the processed flow method: 1 means the
 peer will start sending or continue to send content frames; 0
 means it will not.")
   (details :initform (field-type-initform details amqp-1-1-0-8-0::table) :type amqp-1-1-0-8-0::table :documentation
    "detailed information for warning

 A set of fields that provide more information about the
 problem. The meaning of these fields are defined on a
 per-reply-code basis (TO BE DEFINED).")
   (reply-code :initform (field-type-initform reply-code amqp-1-1-0-8-0::reply-code) :type amqp-1-1-0-8-0::reply-code)
   (reply-text :initform (field-type-initform reply-text amqp-1-1-0-8-0::reply-text) :type amqp-1-1-0-8-0::reply-text)
   (class-id :initform (field-type-initform class-id amqp-1-1-0-8-0::class-id) :type amqp-1-1-0-8-0::class-id
    :documentation "failing method class

 When the close is provoked by a method exception, this is the
 class of the method.")
   (method-id :initform (field-type-initform method-id amqp-1-1-0-8-0::method-id) :type amqp-1-1-0-8-0::method-id
    :documentation "failing method ID

 When the close is provoked by a method exception, this is the
 ID of the method."))
  (:documentation "roles: server MUST; client MUST.

  The channel class provides methods for a client to establish a virtual
  connection - a channel - to a server and for both peers to operate the
  virtual connection thereafter.

    channel             = open-channel *use-channel close-channel
    open-channel        = C:OPEN S:OPEN-OK
    use-channel         = C:FLOW S:FLOW-OK
                        / S:FLOW C:FLOW-OK
                        / S:ALERT
                        / functional-class
    close-channel       = C:CLOSE S:CLOSE-OK
                        / S:CLOSE C:CLOSE-OK
"))


(def-amqp-method (amqp-1-1-0-8-0:channel amqp:open) (amqp:open amqp-1-1-0-8-0:method)
  ((id :initform 10))
  ((out-of-band
   :initform (field-type-initform out-of-band amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "out-of-band settings

 Configures out-of-band transfers on this channel. The syntax and
 meaning of this field will be formally defined at a later date."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:channel amqp:open-ok) (amqp:open-ok amqp-1-1-0-8-0:method)
  ((id :initform 11))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:channel amqp:flow) (amqp:flow amqp-1-1-0-8-0:method)
  ((id :initform 20))
  ((active
   :initform (field-type-initform active amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "start/stop content frames

 If 1, the peer starts sending content frames. If 0, the peer
 stops sending content frames."))
  (:documentation "roles: server MUST; client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:channel amqp:flow-ok) (amqp:flow-ok amqp-1-1-0-8-0:method)
  ((id :initform 21))
  ((active
   :initform (field-type-initform active amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "current flow setting

 Confirms the setting of the processed flow method: 1 means the
 peer will start sending or continue to send content frames; 0
 means it will not."))
  (:documentation "roles: server MUST; client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:channel amqp:alert) (amqp:alert amqp-1-1-0-8-0:method)
  ((id :initform 30))
  ((reply-code
   :initform (field-type-initform reply-code amqp-1-1-0-8-0::reply-code)
   :type amqp-1-1-0-8-0::reply-code)
  (reply-text
   :initform (field-type-initform reply-text amqp-1-1-0-8-0::reply-text)
   :type amqp-1-1-0-8-0::reply-text)
  (details
   :initform (field-type-initform details amqp-1-1-0-8-0::table)
   :type amqp-1-1-0-8-0::table
   :documentation "detailed information for warning

 A set of fields that provide more information about the
 problem. The meaning of these fields are defined on a
 per-reply-code basis (TO BE DEFINED)."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:channel amqp:close) (amqp:close amqp-1-1-0-8-0:method)
  ((id :initform 40))
  ((reply-code
   :initform (field-type-initform reply-code amqp-1-1-0-8-0::reply-code)
   :type amqp-1-1-0-8-0::reply-code)
  (reply-text
   :initform (field-type-initform reply-text amqp-1-1-0-8-0::reply-text)
   :type amqp-1-1-0-8-0::reply-text)
  (class-id
   :initform (field-type-initform class-id amqp-1-1-0-8-0::class-id)
   :type amqp-1-1-0-8-0::class-id
   :documentation "failing method class

 When the close is provoked by a method exception, this is the
 class of the method.")
  (method-id
   :initform (field-type-initform method-id amqp-1-1-0-8-0::method-id)
   :type amqp-1-1-0-8-0::method-id
   :documentation "failing method ID

 When the close is provoked by a method exception, this is the
 ID of the method."))
  (:documentation "roles: client MUST; server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:channel amqp:close-ok) (amqp:close-ok amqp-1-1-0-8-0:method)
  ((id :initform 41))
  ()
  (:documentation "roles: client MUST; server MUST."))

;;; class: access [id method-names]
;;;   access.request [realm exclusive passive active write read]
;;;   access.request-ok [ticket]

(def-amqp-class amqp-1-1-0-8-0:access (amqp-1-1-0-8-0:object amqp:access)
  ((id :initform 30 :allocation :class) (method-names :initform '(amqp:request amqp::request-ok) :allocation :class))
  ()
  ((realm :initform (field-type-initform realm amqp-1-1-0-8-0::path) :type amqp-1-1-0-8-0::path :documentation
    "name of requested realm

 If the specified realm is not known to the server, the server
 must raise a channel exception with reply code 402 (invalid
 path).")
   (exclusive :initform (field-type-initform exclusive amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "request exclusive access

 Request exclusive access to the realm. If the server cannot grant
 this - because there are other active tickets for the realm - it
 raises a channel exception.")
   (passive :initform (field-type-initform passive amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "request passive access

 Request message passive access to the specified access realm.
 Passive access lets a client get information about resources in
 the realm but not to make any changes to them.")
   (active :initform (field-type-initform active amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "request active access

 Request message active access to the specified access realm.
 Acvtive access lets a client get create and delete resources in
 the realm.")
   (write :initform
          (field-type-initform write amqp-1-1-0-8-0::bit)
          :type
          amqp-1-1-0-8-0::bit
          :documentation
          "request write access

 Request write access to the specified access realm. Write access
 lets a client publish messages to all exchanges in the realm.")
   (read :initform
         (field-type-initform read amqp-1-1-0-8-0::bit)
         :type
         amqp-1-1-0-8-0::bit
         :documentation
         "request read access

 Request read access to the specified access realm. Read access
 lets a client consume messages from queues in the realm.")
   (ticket :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket) :type amqp-1-1-0-8-0::access-ticket))
  (:documentation "roles: server MUST; client MUST.

  The protocol control access to server resources using access tickets.
  A client must explicitly request access tickets before doing work.
  An access ticket grants a client the right to use a specific set of
  resources - called a \"realm\" - in specific ways.

    access              = C:REQUEST S:REQUEST-OK
"))


(def-amqp-method (amqp-1-1-0-8-0:access amqp:request) (amqp:request amqp-1-1-0-8-0:method)
  ((id :initform 10))
  ((realm
   :initform (field-type-initform realm amqp-1-1-0-8-0::path)
   :type amqp-1-1-0-8-0::path
   :documentation "name of requested realm

 If the specified realm is not known to the server, the server
 must raise a channel exception with reply code 402 (invalid
 path).")
  (exclusive
   :initform (field-type-initform exclusive amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request exclusive access

 Request exclusive access to the realm. If the server cannot grant
 this - because there are other active tickets for the realm - it
 raises a channel exception.")
  (passive
   :initform (field-type-initform passive amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request passive access

 Request message passive access to the specified access realm.
 Passive access lets a client get information about resources in
 the realm but not to make any changes to them.")
  (active
   :initform (field-type-initform active amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request active access

 Request message active access to the specified access realm.
 Acvtive access lets a client get create and delete resources in
 the realm.")
  (write
   :initform (field-type-initform write amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request write access

 Request write access to the specified access realm. Write access
 lets a client publish messages to all exchanges in the realm.")
  (read
   :initform (field-type-initform read amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request read access

 Request read access to the specified access realm. Read access
 lets a client consume messages from queues in the realm."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:access amqp::request-ok) (amqp::request-ok amqp-1-1-0-8-0:method)
  ((id :initform 11))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket))
  (:documentation "roles: client MUST."))

;;; class: exchange [id method-names]
;;;   exchange.declare [ticket exchange type passive durable auto-delete internal no-wait arguments]
;;;   exchange.declare-ok
;;;   exchange.delete [ticket exchange if-unused no-wait]
;;;   exchange.delete-ok

(def-amqp-class amqp-1-1-0-8-0:exchange (amqp-1-1-0-8-0:object amqp:exchange)
  ((id :initform 40 :allocation :class)
   (method-names :initform '(amqp:declare amqp:declare-ok amqp:delete amqp:delete-ok) :allocation :class))
  ()
  ((type :initform (field-type-initform type amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr :documentation
    "exchange type

 Each exchange belongs to one of a set of exchange types implemented
 by the server. The exchange types define the functionality of the
 exchange - i.e. how messages are routed through it. It is not valid
 or meaningful to attempt to change the type of an existing exchange.

 amq_exchange_16
 If the exchange already exists with a different type, the server
 MUST raise a connection exception with a reply code 507 (not allowed).

 amq_exchange_18
 If the server does not support the requested exchange type it MUST
 raise a connection exception with a reply code 503 (command invalid).")
   (passive :initform (field-type-initform passive amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "do not create exchange

 If set, the server will not create the exchange. The client can use
 this to check whether an exchange exists without modifying the server
 state.

 amq_exchange_05
 If set, and the exchange does not already exist, the server MUST
 raise a channel exception with reply code 404 (not found).")
   (durable :initform (field-type-initform durable amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "request a durable exchange

 If set when creating a new exchange, the exchange will be marked as
 durable. Durable exchanges remain active when a server restarts.
 Non-durable exchanges (transient exchanges) are purged if/when a
 server restarts.

 amq_exchange_24
 The server MUST support both durable and transient exchanges.

 The server MUST ignore the durable field if the exchange already
 exists.")
   (auto-delete :initform (field-type-initform auto-delete amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit
    :documentation "auto-delete when unused

 If set, the exchange is deleted when all queues have finished
 using it.

 amq_exchange_02
 The server SHOULD allow for a reasonable delay between the point
 when it determines that an exchange is not being used (or no longer
 used), and the point when it deletes the exchange. At the least it
 must allow a client to create an exchange and then bind a queue to
 it, with a small but non-zero delay between these two actions.

 amq_exchange_25
 The server MUST ignore the auto-delete field if the exchange already
 exists.")
   (internal :initform (field-type-initform internal amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "create internal exchange

 If set, the exchange may not be used directly by publishers, but
 only when bound to other exchanges. Internal exchanges are used to
 construct wiring that is not visible to applications.")
   (arguments :initform (field-type-initform arguments amqp-1-1-0-8-0::table) :type amqp-1-1-0-8-0::table
    :documentation "arguments for declaration

 A set of arguments for the declaration. The syntax and semantics
 of these arguments depends on the server implementation. This
 field is ignored if passive is 1.")
   (ticket :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket) :type amqp-1-1-0-8-0::access-ticket
    :documentation "The client MUST provide a valid access ticket giving 'active'
 access rights to the exchange's access realm.")
   (exchange :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name) :type amqp-1-1-0-8-0::exchange-name
    :documentation "amq_exchange_11
 The exchange MUST exist. Attempting to delete a non-existing exchange
 causes a channel exception.")
   (if-unused :initform (field-type-initform if-unused amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "delete only if unused

 If set, the server will only delete the exchange if it has no queue
 bindings. If the exchange has queue bindings the server does not
 delete it but raises a channel exception instead.

 amq_exchange_12
 If set, the server SHOULD delete the exchange but only if it has
 no queue bindings.

 amq_exchange_13
 If set, the server SHOULD raise a channel exception if the exchange is in
 use.")
   (no-wait :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception."))
  (:documentation "roles: server MUST; client MUST.

  Exchanges match and distribute messages across queues.  Exchanges can be
  configured in the server or created at runtime.

    exchange            = C:DECLARE  S:DECLARE-OK
                        / C:DELETE   S:DELETE-OK
"))


(def-amqp-method (amqp-1-1-0-8-0:exchange amqp:declare) (amqp:declare amqp-1-1-0-8-0:method)
  ((id :initform 10))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "When a client defines a new exchange, this belongs to the access realm
 of the ticket used. All further work done with that exchange must be
 done with an access ticket for the same realm.

 The client MUST provide a valid access ticket giving 'active' access
 to the realm in which the exchange exists or will be created, or
 'passive' access if the if-exists flag is set.")
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "amq_exchange_15
 Exchange names starting with 'amq.' are reserved for predeclared
 and standardised exchanges. If the client attempts to create an
 exchange starting with 'amq.', the server MUST raise a channel
 exception with reply code 403 (access refused).")
  (type
   :initform (field-type-initform type amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "exchange type

 Each exchange belongs to one of a set of exchange types implemented
 by the server. The exchange types define the functionality of the
 exchange - i.e. how messages are routed through it. It is not valid
 or meaningful to attempt to change the type of an existing exchange.

 amq_exchange_16
 If the exchange already exists with a different type, the server
 MUST raise a connection exception with a reply code 507 (not allowed).

 amq_exchange_18
 If the server does not support the requested exchange type it MUST
 raise a connection exception with a reply code 503 (command invalid).")
  (passive
   :initform (field-type-initform passive amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not create exchange

 If set, the server will not create the exchange. The client can use
 this to check whether an exchange exists without modifying the server
 state.

 amq_exchange_05
 If set, and the exchange does not already exist, the server MUST
 raise a channel exception with reply code 404 (not found).")
  (durable
   :initform (field-type-initform durable amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request a durable exchange

 If set when creating a new exchange, the exchange will be marked as
 durable. Durable exchanges remain active when a server restarts.
 Non-durable exchanges (transient exchanges) are purged if/when a
 server restarts.

 amq_exchange_24
 The server MUST support both durable and transient exchanges.

 The server MUST ignore the durable field if the exchange already
 exists.")
  (auto-delete
   :initform (field-type-initform auto-delete amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "auto-delete when unused

 If set, the exchange is deleted when all queues have finished
 using it.

 amq_exchange_02
 The server SHOULD allow for a reasonable delay between the point
 when it determines that an exchange is not being used (or no longer
 used), and the point when it deletes the exchange. At the least it
 must allow a client to create an exchange and then bind a queue to
 it, with a small but non-zero delay between these two actions.

 amq_exchange_25
 The server MUST ignore the auto-delete field if the exchange already
 exists.")
  (internal
   :initform (field-type-initform internal amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "create internal exchange

 If set, the exchange may not be used directly by publishers, but
 only when bound to other exchanges. Internal exchanges are used to
 construct wiring that is not visible to applications.")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception.")
  (arguments
   :initform (field-type-initform arguments amqp-1-1-0-8-0::table)
   :type amqp-1-1-0-8-0::table
   :documentation "arguments for declaration

 A set of arguments for the declaration. The syntax and semantics
 of these arguments depends on the server implementation. This
 field is ignored if passive is 1."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:exchange amqp:declare-ok) (amqp:declare-ok amqp-1-1-0-8-0:method)
  ((id :initform 11))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:exchange amqp:delete) (amqp:delete amqp-1-1-0-8-0:method)
  ((id :initform 20))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "The client MUST provide a valid access ticket giving 'active'
 access rights to the exchange's access realm.")
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "amq_exchange_11
 The exchange MUST exist. Attempting to delete a non-existing exchange
 causes a channel exception.")
  (if-unused
   :initform (field-type-initform if-unused amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "delete only if unused

 If set, the server will only delete the exchange if it has no queue
 bindings. If the exchange has queue bindings the server does not
 delete it but raises a channel exception instead.

 amq_exchange_12
 If set, the server SHOULD delete the exchange but only if it has
 no queue bindings.

 amq_exchange_13
 If set, the server SHOULD raise a channel exception if the exchange is in
 use.")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:exchange amqp:delete-ok) (amqp:delete-ok amqp-1-1-0-8-0:method)
  ((id :initform 21))
  ()
  (:documentation "roles: client MUST."))

;;; class: queue [id method-names]
;;;   queue.declare [ticket queue passive durable exclusive auto-delete no-wait arguments]
;;;   queue.declare-ok [queue message-count consumer-count]
;;;   queue.bind [ticket queue exchange routing-key no-wait arguments]
;;;   queue.bind-ok
;;;   queue.purge [ticket queue no-wait]
;;;   queue.purge-ok [message-count]
;;;   queue.delete [ticket queue if-unused if-empty no-wait]
;;;   queue.delete-ok [message-count]

(def-amqp-class amqp-1-1-0-8-0:queue (amqp-1-1-0-8-0:object amqp:queue)
  ((id :initform 50 :allocation :class)
   (method-names :initform
    '(amqp:declare amqp:declare-ok amqp:bind amqp:bind-ok amqp:purge amqp:purge-ok amqp:delete amqp:delete-ok)
    :allocation :class))
  ()
  ((passive :initform (field-type-initform passive amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "do not create queue

 If set, the server will not create the queue. The client can use
 this to check whether a queue exists without modifying the server
 state.

 amq_queue_05
 If set, and the queue does not already exist, the server MUST
 respond with a reply code 404 (not found) and raise a channel
 exception.")
   (durable :initform (field-type-initform durable amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "request a durable queue

 If set when creating a new queue, the queue will be marked as
 durable. Durable queues remain active when a server restarts.
 Non-durable queues (transient queues) are purged if/when a
 server restarts. Note that durable queues do not necessarily
 hold persistent messages, although it does not make sense to
 send persistent messages to a transient queue.

 amq_queue_03
 The server MUST recreate the durable queue after a restart.

 amq_queue_36
 The server MUST support both durable and transient queues.

 amq_queue_37
 The server MUST ignore the durable field if the queue already
 exists.")
   (exclusive :initform (field-type-initform exclusive amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "request an exclusive queue

 Exclusive queues may only be consumed from by the current connection.
 Setting the 'exclusive' flag always implies 'auto-delete'.

 amq_queue_38
 The server MUST support both exclusive (private) and non-exclusive
 (shared) queues.

 amq_queue_04
 The server MUST raise a channel exception if 'exclusive' is specified
 and the queue already exists and is owned by a different connection.")
   (auto-delete :initform (field-type-initform auto-delete amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit
    :documentation "auto-delete queue when unused

 If set, the queue is deleted when all consumers have finished
 using it. Last consumer can be cancelled either explicitly or because
 its channel is closed. If there was no consumer ever on the queue, it
 won't be deleted.

 amq_queue_02
 The server SHOULD allow for a reasonable delay between the point
 when it determines that a queue is not being used (or no longer
 used), and the point when it deletes the queue. At the least it
 must allow a client to create a queue and then create a consumer
 to read from it, with a small but non-zero delay between these
 two actions. The server should equally allow for clients that may
 be disconnected prematurely, and wish to re-consume from the same
 queue without losing messages. We would recommend a configurable
 timeout, with a suitable default value being one minute.

 amq_queue_31
 The server MUST ignore the auto-delete field if the queue already
 exists.")
   (consumer-count :initform (field-type-initform consumer-count amqp-1-1-0-8-0::long) :type amqp-1-1-0-8-0::long
    :documentation "number of consumers

 Reports the number of active consumers for the queue. Note that
 consumers can suspend activity (Channel.Flow) in which case they
 do not appear in this count.")
   (exchange :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name) :type amqp-1-1-0-8-0::exchange-name
    :documentation "The name of the exchange to bind to.

 amq_queue_14
 If the exchange does not exist the server MUST raise a channel
 exception with reply code 404 (not found).")
   (routing-key :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "message routing key

 Specifies the routing key for the binding. The routing key is
 used for routing messages depending on the exchange configuration.
 Not all exchanges use a routing key - refer to the specific
 exchange documentation. If the routing key is empty and the queue
 name is empty, the routing key will be the current queue for the
 channel, which is the last declared queue.")
   (arguments :initform (field-type-initform arguments amqp-1-1-0-8-0::table) :type amqp-1-1-0-8-0::table
    :documentation "arguments for binding

 A set of arguments for the binding. The syntax and semantics of
 these arguments depends on the exchange class.")
   (ticket :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket) :type amqp-1-1-0-8-0::access-ticket
    :documentation "The client provides a valid access ticket giving 'active'
 access rights to the queue's access realm.")
   (queue :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name) :type amqp-1-1-0-8-0::queue-name
    :documentation "Specifies the name of the queue to delete. If the queue name is
 empty, refers to the current queue for the channel, which is the
 last declared queue.

 If the client did not previously declare a queue, and the queue
 name in this method is empty, the server MUST raise a connection
 exception with reply code 530 (not allowed).

 The queue must exist. Attempting to delete a non-existing queue
 causes a channel exception.")
   (if-unused :initform (field-type-initform if-unused amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "delete only if unused

 If set, the server will only delete the queue if it has no
 consumers. If the queue has consumers the server does does not
 delete it but raises a channel exception instead.

 amq_queue_29
 amq_queue_30
 The server MUST respect the if-unused flag when deleting a queue.")
   (if-empty :initform (field-type-initform if-empty amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "delete only if empty
 	amq_queue_27

 If set, the server will only delete the queue if it has no
 messages. If the queue is not empty the server raises a channel
 exception.")
   (no-wait :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception.")
   (message-count :initform (field-type-initform message-count amqp-1-1-0-8-0::long) :type amqp-1-1-0-8-0::long
    :documentation "number of messages purged

 Reports the number of messages purged."))
  (:documentation "roles: server MUST; client MUST.

  Queues store and forward messages.  Queues can be configured in the server
  or created at runtime.  Queues must be attached to at least one exchange
  in order to receive messages from publishers.

    queue               = C:DECLARE  S:DECLARE-OK
                        / C:BIND     S:BIND-OK
                        / C:PURGE    S:PURGE-OK
                        / C:DELETE   S:DELETE-OK
"))


(def-amqp-method (amqp-1-1-0-8-0:queue amqp:declare) (amqp:declare amqp-1-1-0-8-0:method)
  ((id :initform 10))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "When a client defines a new queue, this belongs to the access realm
 of the ticket used. All further work done with that queue must be
 done with an access ticket for the same realm.

 The client provides a valid access ticket giving 'active' access
 to the realm in which the queue exists or will be created, or
 'passive' access if the if-exists flag is set.")
  (queue
   :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name)
   :type amqp-1-1-0-8-0::queue-name
   :documentation "amq_queue_10
 The queue name MAY be empty, in which case the server MUST create
 a new queue with a unique generated name and return this to the
 client in the Declare-Ok method.

 amq_queue_32
 Queue names starting with 'amq.' are reserved for predeclared and
 standardised server queues. If the queue name starts with 'amq.'
 and the passive option is zero, the server MUST raise a connection
 exception with reply code 403 (access refused).")
  (passive
   :initform (field-type-initform passive amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not create queue

 If set, the server will not create the queue. The client can use
 this to check whether a queue exists without modifying the server
 state.

 amq_queue_05
 If set, and the queue does not already exist, the server MUST
 respond with a reply code 404 (not found) and raise a channel
 exception.")
  (durable
   :initform (field-type-initform durable amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request a durable queue

 If set when creating a new queue, the queue will be marked as
 durable. Durable queues remain active when a server restarts.
 Non-durable queues (transient queues) are purged if/when a
 server restarts. Note that durable queues do not necessarily
 hold persistent messages, although it does not make sense to
 send persistent messages to a transient queue.

 amq_queue_03
 The server MUST recreate the durable queue after a restart.

 amq_queue_36
 The server MUST support both durable and transient queues.

 amq_queue_37
 The server MUST ignore the durable field if the queue already
 exists.")
  (exclusive
   :initform (field-type-initform exclusive amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request an exclusive queue

 Exclusive queues may only be consumed from by the current connection.
 Setting the 'exclusive' flag always implies 'auto-delete'.

 amq_queue_38
 The server MUST support both exclusive (private) and non-exclusive
 (shared) queues.

 amq_queue_04
 The server MUST raise a channel exception if 'exclusive' is specified
 and the queue already exists and is owned by a different connection.")
  (auto-delete
   :initform (field-type-initform auto-delete amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "auto-delete queue when unused

 If set, the queue is deleted when all consumers have finished
 using it. Last consumer can be cancelled either explicitly or because
 its channel is closed. If there was no consumer ever on the queue, it
 won't be deleted.

 amq_queue_02
 The server SHOULD allow for a reasonable delay between the point
 when it determines that a queue is not being used (or no longer
 used), and the point when it deletes the queue. At the least it
 must allow a client to create a queue and then create a consumer
 to read from it, with a small but non-zero delay between these
 two actions. The server should equally allow for clients that may
 be disconnected prematurely, and wish to re-consume from the same
 queue without losing messages. We would recommend a configurable
 timeout, with a suitable default value being one minute.

 amq_queue_31
 The server MUST ignore the auto-delete field if the queue already
 exists.")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception.")
  (arguments
   :initform (field-type-initform arguments amqp-1-1-0-8-0::table)
   :type amqp-1-1-0-8-0::table
   :documentation "arguments for declaration

 A set of arguments for the declaration. The syntax and semantics
 of these arguments depends on the server implementation. This
 field is ignored if passive is 1."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:queue amqp:declare-ok) (amqp:declare-ok amqp-1-1-0-8-0:method)
  ((id :initform 11))
  ((queue
   :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name)
   :type amqp-1-1-0-8-0::queue-name
   :documentation "Reports the name of the queue. If the server generated a queue
 name, this field contains that name.")
  (message-count
   :initform (field-type-initform message-count amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "number of messages in queue

 Reports the number of messages in the queue, which will be zero
 for newly-created queues.")
  (consumer-count
   :initform (field-type-initform consumer-count amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "number of consumers

 Reports the number of active consumers for the queue. Note that
 consumers can suspend activity (Channel.Flow) in which case they
 do not appear in this count."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:queue amqp:bind) (amqp:bind amqp-1-1-0-8-0:method)
  ((id :initform 20))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "The client provides a valid access ticket giving 'active'
 access rights to the queue's access realm.")
  (queue
   :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name)
   :type amqp-1-1-0-8-0::queue-name
   :documentation "Specifies the name of the queue to bind. If the queue name is
 empty, refers to the current queue for the channel, which is
 the last declared queue.

 If the client did not previously declare a queue, and the queue
 name in this method is empty, the server MUST raise a connection
 exception with reply code 530 (not allowed).

 If the queue does not exist the server MUST raise a channel exception
 with reply code 404 (not found).")
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "The name of the exchange to bind to.

 amq_queue_14
 If the exchange does not exist the server MUST raise a channel
 exception with reply code 404 (not found).")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "message routing key

 Specifies the routing key for the binding. The routing key is
 used for routing messages depending on the exchange configuration.
 Not all exchanges use a routing key - refer to the specific
 exchange documentation. If the routing key is empty and the queue
 name is empty, the routing key will be the current queue for the
 channel, which is the last declared queue.")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception.")
  (arguments
   :initform (field-type-initform arguments amqp-1-1-0-8-0::table)
   :type amqp-1-1-0-8-0::table
   :documentation "arguments for binding

 A set of arguments for the binding. The syntax and semantics of
 these arguments depends on the exchange class."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:queue amqp:bind-ok) (amqp:bind-ok amqp-1-1-0-8-0:method)
  ((id :initform 21))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:queue amqp:purge) (amqp:purge amqp-1-1-0-8-0:method)
  ((id :initform 30))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "The access ticket must be for the access realm that holds the
 queue.

 The client MUST provide a valid access ticket giving 'read' access
 rights to the queue's access realm. Note that purging a queue is
 equivalent to reading all messages and discarding them.")
  (queue
   :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name)
   :type amqp-1-1-0-8-0::queue-name
   :documentation "Specifies the name of the queue to purge. If the queue name is
 empty, refers to the current queue for the channel, which is
 the last declared queue.

 If the client did not previously declare a queue, and the queue
 name in this method is empty, the server MUST raise a connection
 exception with reply code 530 (not allowed).

 The queue must exist. Attempting to purge a non-existing queue
 causes a channel exception.")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:queue amqp:purge-ok) (amqp:purge-ok amqp-1-1-0-8-0:method)
  ((id :initform 31))
  ((message-count
   :initform (field-type-initform message-count amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "number of messages purged

 Reports the number of messages purged."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:queue amqp:delete) (amqp:delete amqp-1-1-0-8-0:method)
  ((id :initform 40))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "The client provides a valid access ticket giving 'active'
 access rights to the queue's access realm.")
  (queue
   :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name)
   :type amqp-1-1-0-8-0::queue-name
   :documentation "Specifies the name of the queue to delete. If the queue name is
 empty, refers to the current queue for the channel, which is the
 last declared queue.

 If the client did not previously declare a queue, and the queue
 name in this method is empty, the server MUST raise a connection
 exception with reply code 530 (not allowed).

 The queue must exist. Attempting to delete a non-existing queue
 causes a channel exception.")
  (if-unused
   :initform (field-type-initform if-unused amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "delete only if unused

 If set, the server will only delete the queue if it has no
 consumers. If the queue has consumers the server does does not
 delete it but raises a channel exception instead.

 amq_queue_29
 amq_queue_30
 The server MUST respect the if-unused flag when deleting a queue.")
  (if-empty
   :initform (field-type-initform if-empty amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "delete only if empty
 	amq_queue_27

 If set, the server will only delete the queue if it has no
 messages. If the queue is not empty the server raises a channel
 exception.")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:queue amqp:delete-ok) (amqp:delete-ok amqp-1-1-0-8-0:method)
  ((id :initform 41))
  ((message-count
   :initform (field-type-initform message-count amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "number of messages purged

 Reports the number of messages purged."))
  (:documentation "roles: client MUST."))

;;; class: basic [id method-names content-type content-encoding headers delivery-mode priority correlation-id reply-to expiration message-id timestamp type user-id app-id cluster-id]
;;;   basic.qos [prefetch-size prefetch-count global]
;;;   basic.qos-ok
;;;   basic.consume [ticket queue consumer-tag no-local no-ack exclusive no-wait]
;;;   basic.consume-ok [consumer-tag]
;;;   basic.cancel [consumer-tag no-wait]
;;;   basic.cancel-ok [consumer-tag]
;;;   basic.publish [ticket exchange routing-key mandatory immediate]
;;;   basic.return [reply-code reply-text exchange routing-key]
;;;   basic.deliver [consumer-tag delivery-tag redelivered exchange routing-key]
;;;   basic.get [ticket queue no-ack]
;;;   basic.get-ok [delivery-tag redelivered exchange routing-key message-count]
;;;   basic.get-empty [cluster-id]
;;;   basic.ack [delivery-tag multiple]
;;;   basic.reject [delivery-tag requeue]
;;;   basic.recover [requeue]

(def-amqp-class amqp-1-1-0-8-0:basic (amqp-1-1-0-8-0:object amqp:basic)
  ((id :initform 60 :allocation :class)
   (method-names :initform
    '(amqp:qos amqp:qos-ok amqp:consume amqp:consume-ok amqp:cancel amqp:cancel-ok amqp:publish amqp:return
      amqp:deliver amqp:get amqp:get-ok amqp:get-empty amqp:ack amqp:reject amqp:recover)
    :allocation :class))
   ((content-type :initform (field-type-initform content-type amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "MIME content type")
   (content-encoding
     :initform
     (field-type-initform content-encoding amqp-1-1-0-8-0::shortstr)
     :type
     amqp-1-1-0-8-0::shortstr
     :documentation
     "MIME content encoding")
   (headers :initform (field-type-initform headers amqp-1-1-0-8-0::table) :type amqp-1-1-0-8-0::table :documentation
    "Message header field table")
   (delivery-mode :initform (field-type-initform delivery-mode amqp-1-1-0-8-0::octet) :type amqp-1-1-0-8-0::octet
    :documentation "Non-persistent (1) or persistent (2)")
   (priority :initform (field-type-initform priority amqp-1-1-0-8-0::octet) :type amqp-1-1-0-8-0::octet :documentation
    "The message priority, 0 to 9")
   (correlation-id :initform (field-type-initform correlation-id amqp-1-1-0-8-0::shortstr) :type
    amqp-1-1-0-8-0::shortstr :documentation "The application correlation identifier")
   (reply-to :initform (field-type-initform reply-to amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "The destination to reply to")
   (expiration :initform (field-type-initform expiration amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "Message expiration specification")
   (message-id :initform (field-type-initform message-id amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "The application message identifier")
   (timestamp :initform (field-type-initform timestamp amqp-1-1-0-8-0::timestamp) :type amqp-1-1-0-8-0::timestamp
    :documentation "The message timestamp")
   (type :initform (field-type-initform type amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr :documentation
    "The message type name")
   (user-id :initform (field-type-initform user-id amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "The creating user id")
   (app-id :initform (field-type-initform app-id amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "The creating application id")
   (cluster-id :initform (field-type-initform cluster-id amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "Intra-cluster routing identifier"))
  ((prefetch-size :initform (field-type-initform prefetch-size amqp-1-1-0-8-0::long) :type amqp-1-1-0-8-0::long
    :documentation "prefetch window in octets

 The client can request that messages be sent in advance so that
 when the client finishes processing a message, the following
 message is already held locally, rather than needing to be sent
 down the channel. Prefetching gives a performance improvement.
 This field specifies the prefetch window size in octets. The
 server will send a message in advance if it is equal to or
 smaller in size than the available prefetch size (and also falls
 into other prefetch limits). May be set to zero, meaning 'no
 specific limit', although other prefetch limits may still apply.
 The prefetch-size is ignored if the no-ack option is set.

 The server MUST ignore this setting when the client is not
 processing any messages - i.e. the prefetch size does not limit
 the transfer of single messages to a client, only the sending in
 advance of more messages while the client still has one or more
 unacknowledged messages.")
   (prefetch-count :initform (field-type-initform prefetch-count amqp-1-1-0-8-0::short) :type amqp-1-1-0-8-0::short
    :documentation "prefetch window in messages

 Specifies a prefetch window in terms of whole messages. This
 field may be used in combination with the prefetch-size field;
 a message will only be sent in advance if both prefetch windows
 (and those at the channel and connection level) allow it.
 The prefetch-count is ignored if the no-ack option is set.

 The server MAY send less data in advance than allowed by the
 client's specified prefetch windows but it MUST NOT send more.")
   (global :initform (field-type-initform global amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "apply to entire connection

 By default the QoS settings apply to the current channel only. If
 this field is set, they are applied to the entire connection.")
   (no-local :initform (field-type-initform no-local amqp-1-1-0-8-0::no-local) :type amqp-1-1-0-8-0::no-local)
   (exclusive :initform (field-type-initform exclusive amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "request exclusive access

 Request exclusive consumer access, meaning only this consumer can
 access the queue.

 If the server cannot grant exclusive access to the queue when asked,
 - because there are other consumers active - it MUST raise a channel
 exception with return code 403 (access refused).")
   (no-wait :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception.")
   (mandatory :initform (field-type-initform mandatory amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "indicate mandatory routing

 This flag tells the server how to react if the message cannot be
 routed to a queue. If this flag is set, the server will return an
 unroutable message with a Return method. If this flag is zero, the
 server silently drops the message.

 The server SHOULD implement the mandatory flag.")
   (immediate :initform (field-type-initform immediate amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "request immediate delivery

 This flag tells the server how to react if the message cannot be
 routed to a queue consumer immediately. If this flag is set, the
 server will return an undeliverable message with a Return method.
 If this flag is zero, the server will queue the message, but with
 no guarantee that it will ever be consumed.

 The server SHOULD implement the immediate flag.")
   (reply-code :initform (field-type-initform reply-code amqp-1-1-0-8-0::reply-code) :type amqp-1-1-0-8-0::reply-code)
   (reply-text :initform (field-type-initform reply-text amqp-1-1-0-8-0::reply-text) :type amqp-1-1-0-8-0::reply-text)
   (consumer-tag :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag) :type
    amqp-1-1-0-8-0::consumer-tag)
   (ticket :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket) :type amqp-1-1-0-8-0::access-ticket
    :documentation "The client MUST provide a valid access ticket giving 'read'
 access rights to the realm for the queue.")
   (queue :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name) :type amqp-1-1-0-8-0::queue-name
    :documentation "Specifies the name of the queue to consume from. If the queue name
 is null, refers to the current queue for the channel, which is the
 last declared queue.

 If the client did not previously declare a queue, and the queue name
 in this method is empty, the server MUST raise a connection exception
 with reply code 530 (not allowed).")
   (no-ack :initform (field-type-initform no-ack amqp-1-1-0-8-0::no-ack) :type amqp-1-1-0-8-0::no-ack)
   (redelivered :initform (field-type-initform redelivered amqp-1-1-0-8-0::redelivered) :type
    amqp-1-1-0-8-0::redelivered)
   (exchange :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name) :type amqp-1-1-0-8-0::exchange-name
    :documentation "Specifies the name of the exchange that the message was originally
 published to. If empty, the message was published to the default
 exchange.")
   (routing-key :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "Message routing key

 Specifies the routing key name specified when the message was
 published.")
   (message-count :initform (field-type-initform message-count amqp-1-1-0-8-0::long) :type amqp-1-1-0-8-0::long
    :documentation "number of messages pending

 This field reports the number of messages pending on the queue,
 excluding the message being delivered. Note that this figure is
 indicative, not reliable, and can change arbitrarily as messages
 are added to the queue and removed by other clients.")
   (cluster-id :initform (field-type-initform cluster-id amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "Cluster id

 For use by cluster applications, should not be used by
 client applications.")
   (multiple :initform (field-type-initform multiple amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "acknowledge multiple messages

 If set to 1, the delivery tag is treated as 'up to and including',
 so that the client can acknowledge multiple messages with a single
 method. If set to zero, the delivery tag refers to a single
 message. If the multiple field is 1, and the delivery tag is zero,
 tells the server to acknowledge all outstanding mesages.

 The server MUST validate that a non-zero delivery-tag refers to an
 delivered message, and raise a channel exception if this is not the
 case.")
   (delivery-tag :initform (field-type-initform delivery-tag amqp-1-1-0-8-0::delivery-tag) :type
    amqp-1-1-0-8-0::delivery-tag)
   (requeue :initform (field-type-initform requeue amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "requeue the message

 If this field is zero, the message will be redelivered to the original recipient. If this bit
 is 1, the server will attempt to requeue the message, potentially then delivering it to an
 alternative subscriber."))
  (:documentation "roles: server MUST; client MAY.

  The Basic class provides methods that support an industry-standard
  messaging model.

    basic               = C:QOS S:QOS-OK
                        / C:CONSUME S:CONSUME-OK
                        / C:CANCEL S:CANCEL-OK
                        / C:PUBLISH content
                        / S:RETURN content
                        / S:DELIVER content
                        / C:GET ( S:GET-OK content / S:GET-EMPTY )
                        / C:ACK
                        / C:REJECT

  The server SHOULD respect the persistent property of basic messages
  and SHOULD make a best-effort to hold persistent basic messages on a
  reliable storage mechanism.

  The server MUST NOT discard a persistent basic message in case of a
  queue overflow. The server MAY use the Channel.Flow method to slow
  or stop a basic message publisher when necessary.

  The server MAY overflow non-persistent basic messages to persistent
  storage and MAY discard or dead-letter non-persistent basic messages
  on a priority basis if the queue size exceeds some configured limit.

  The server MUST implement at least 2 priority levels for basic
  messages, where priorities 0-4 and 5-9 are treated as two distinct
  levels. The server MAY implement up to 10 priority levels.

  The server MUST deliver messages of the same priority in order
  irrespective of their individual persistence.

  The server MUST support both automatic and explicit acknowledgements
  on Basic content.
"))


(def-amqp-method (amqp-1-1-0-8-0:basic amqp:qos) (amqp:qos amqp-1-1-0-8-0:method)
  ((id :initform 10))
  ((prefetch-size
   :initform (field-type-initform prefetch-size amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "prefetch window in octets

 The client can request that messages be sent in advance so that
 when the client finishes processing a message, the following
 message is already held locally, rather than needing to be sent
 down the channel. Prefetching gives a performance improvement.
 This field specifies the prefetch window size in octets. The
 server will send a message in advance if it is equal to or
 smaller in size than the available prefetch size (and also falls
 into other prefetch limits). May be set to zero, meaning 'no
 specific limit', although other prefetch limits may still apply.
 The prefetch-size is ignored if the no-ack option is set.

 The server MUST ignore this setting when the client is not
 processing any messages - i.e. the prefetch size does not limit
 the transfer of single messages to a client, only the sending in
 advance of more messages while the client still has one or more
 unacknowledged messages.")
  (prefetch-count
   :initform (field-type-initform prefetch-count amqp-1-1-0-8-0::short)
   :type amqp-1-1-0-8-0::short
   :documentation "prefetch window in messages

 Specifies a prefetch window in terms of whole messages. This
 field may be used in combination with the prefetch-size field;
 a message will only be sent in advance if both prefetch windows
 (and those at the channel and connection level) allow it.
 The prefetch-count is ignored if the no-ack option is set.

 The server MAY send less data in advance than allowed by the
 client's specified prefetch windows but it MUST NOT send more.")
  (global
   :initform (field-type-initform global amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "apply to entire connection

 By default the QoS settings apply to the current channel only. If
 this field is set, they are applied to the entire connection."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:qos-ok) (amqp:qos-ok amqp-1-1-0-8-0:method)
  ((id :initform 11))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:consume) (amqp:consume amqp-1-1-0-8-0:method)
  ((id :initform 20))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "The client MUST provide a valid access ticket giving 'read' access
 rights to the realm for the queue.")
  (queue
   :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name)
   :type amqp-1-1-0-8-0::queue-name
   :documentation "Specifies the name of the queue to consume from. If the queue name
 is null, refers to the current queue for the channel, which is the
 last declared queue.

 If the client did not previously declare a queue, and the queue name
 in this method is empty, the server MUST raise a connection exception
 with reply code 530 (not allowed).")
  (consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag
   :documentation "Specifies the identifier for the consumer. The consumer tag is
 local to a connection, so two clients can use the same consumer
 tags. If this field is empty the server will generate a unique
 tag.

 The tag MUST NOT refer to an existing consumer. If the client
 attempts to create two consumers with the same non-empty tag
 the server MUST raise a connection exception with reply code
 530 (not allowed).")
  (no-local
   :initform (field-type-initform no-local amqp-1-1-0-8-0::no-local)
   :type amqp-1-1-0-8-0::no-local)
  (no-ack
   :initform (field-type-initform no-ack amqp-1-1-0-8-0::no-ack)
   :type amqp-1-1-0-8-0::no-ack)
  (exclusive
   :initform (field-type-initform exclusive amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request exclusive access

 Request exclusive consumer access, meaning only this consumer can
 access the queue.

 If the server cannot grant exclusive access to the queue when asked,
 - because there are other consumers active - it MUST raise a channel
 exception with return code 403 (access refused).")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:consume-ok) (amqp:consume-ok amqp-1-1-0-8-0:method)
  ((id :initform 21))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag
   :documentation "Holds the consumer tag specified by the client or provided by
 the server."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:cancel) (amqp:cancel amqp-1-1-0-8-0:method)
  ((id :initform 30))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag)
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:cancel-ok) (amqp:cancel-ok amqp-1-1-0-8-0:method)
  ((id :initform 31))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:publish) (amqp:publish amqp-1-1-0-8-0:method)
  ((id :initform 40))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "The client MUST provide a valid access ticket giving 'write'
 access rights to the access realm for the exchange.")
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "Specifies the name of the exchange to publish to. The exchange
 name can be empty, meaning the default exchange. If the exchange
 name is specified, and that exchange does not exist, the server
 will raise a channel exception.

 The server MUST accept a blank exchange name to mean the default
 exchange.

 If the exchange was declared as an internal exchange, the server
 MUST raise a channel exception with a reply code 403 (access
 refused).

 The exchange MAY refuse basic content in which case it MUST raise
 a channel exception with reply code 540 (not implemented).")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "Message routing key

 Specifies the routing key for the message. The routing key is
 used for routing messages depending on the exchange configuration.")
  (mandatory
   :initform (field-type-initform mandatory amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "indicate mandatory routing

 This flag tells the server how to react if the message cannot be
 routed to a queue. If this flag is set, the server will return an
 unroutable message with a Return method. If this flag is zero, the
 server silently drops the message.

 The server SHOULD implement the mandatory flag.")
  (immediate
   :initform (field-type-initform immediate amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request immediate delivery

 This flag tells the server how to react if the message cannot be
 routed to a queue consumer immediately. If this flag is set, the
 server will return an undeliverable message with a Return method.
 If this flag is zero, the server will queue the message, but with
 no guarantee that it will ever be consumed.

 The server SHOULD implement the immediate flag."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:return) (amqp:return amqp-1-1-0-8-0:method)
  ((id :initform 50))
  ((reply-code
   :initform (field-type-initform reply-code amqp-1-1-0-8-0::reply-code)
   :type amqp-1-1-0-8-0::reply-code)
  (reply-text
   :initform (field-type-initform reply-text amqp-1-1-0-8-0::reply-text)
   :type amqp-1-1-0-8-0::reply-text)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "Specifies the name of the exchange that the message was
 originally published to.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "Message routing key

 Specifies the routing key name specified when the message was
 published."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:deliver) (amqp:deliver amqp-1-1-0-8-0:method)
  ((id :initform 60))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag)
  (delivery-tag
   :initform (field-type-initform delivery-tag amqp-1-1-0-8-0::delivery-tag)
   :type amqp-1-1-0-8-0::delivery-tag)
  (redelivered
   :initform (field-type-initform redelivered amqp-1-1-0-8-0::redelivered)
   :type amqp-1-1-0-8-0::redelivered)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "Specifies the name of the exchange that the message was
 originally published to.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "Message routing key

 Specifies the routing key name specified when the message was
 published."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:get) (amqp:get amqp-1-1-0-8-0:method)
  ((id :initform 70))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "The client MUST provide a valid access ticket giving 'read'
 access rights to the realm for the queue.")
  (queue
   :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name)
   :type amqp-1-1-0-8-0::queue-name
   :documentation "Specifies the name of the queue to consume from. If the queue name
 is null, refers to the current queue for the channel, which is the
 last declared queue.

 If the client did not previously declare a queue, and the queue name
 in this method is empty, the server MUST raise a connection exception
 with reply code 530 (not allowed).")
  (no-ack
   :initform (field-type-initform no-ack amqp-1-1-0-8-0::no-ack)
   :type amqp-1-1-0-8-0::no-ack))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:get-ok) (amqp:get-ok amqp-1-1-0-8-0:method)
  ((id :initform 71))
  ((delivery-tag
   :initform (field-type-initform delivery-tag amqp-1-1-0-8-0::delivery-tag)
   :type amqp-1-1-0-8-0::delivery-tag)
  (redelivered
   :initform (field-type-initform redelivered amqp-1-1-0-8-0::redelivered)
   :type amqp-1-1-0-8-0::redelivered)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "Specifies the name of the exchange that the message was originally
 published to. If empty, the message was published to the default
 exchange.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "Message routing key

 Specifies the routing key name specified when the message was
 published.")
  (message-count
   :initform (field-type-initform message-count amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "number of messages pending

 This field reports the number of messages pending on the queue,
 excluding the message being delivered. Note that this figure is
 indicative, not reliable, and can change arbitrarily as messages
 are added to the queue and removed by other clients."))
  (:documentation "roles: client MAY."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:get-empty) (amqp:get-empty amqp-1-1-0-8-0:method)
  ((id :initform 72))
  ((cluster-id
   :initform (field-type-initform cluster-id amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "Cluster id

 For use by cluster applications, should not be used by
 client applications."))
  (:documentation "roles: client MAY."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:ack) (amqp:ack amqp-1-1-0-8-0:method)
  ((id :initform 80))
  ((delivery-tag
   :initform (field-type-initform delivery-tag amqp-1-1-0-8-0::delivery-tag)
   :type amqp-1-1-0-8-0::delivery-tag)
  (multiple
   :initform (field-type-initform multiple amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "acknowledge multiple messages

 If set to 1, the delivery tag is treated as 'up to and including',
 so that the client can acknowledge multiple messages with a single
 method. If set to zero, the delivery tag refers to a single
 message. If the multiple field is 1, and the delivery tag is zero,
 tells the server to acknowledge all outstanding mesages.

 The server MUST validate that a non-zero delivery-tag refers to an
 delivered message, and raise a channel exception if this is not the
 case."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:reject) (amqp:reject amqp-1-1-0-8-0:method)
  ((id :initform 90))
  ((delivery-tag
   :initform (field-type-initform delivery-tag amqp-1-1-0-8-0::delivery-tag)
   :type amqp-1-1-0-8-0::delivery-tag)
  (requeue
   :initform (field-type-initform requeue amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "requeue the message

 If this field is zero, the message will be discarded. If this bit
 is 1, the server will attempt to requeue the message.

 The server MUST NOT deliver the message to the same client within
 the context of the current channel. The recommended strategy is
 to attempt to deliver the message to an alternative consumer, and
 if that is not possible, to move the message to a dead-letter
 queue. The server MAY use more sophisticated tracking to hold
 the message on the queue and redeliver it to the same client at
 a later stage."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:basic amqp:recover) (amqp:recover amqp-1-1-0-8-0:method)
  ((id :initform 100))
  ((requeue
   :initform (field-type-initform requeue amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "requeue the message

 If this field is zero, the message will be redelivered to the original recipient. If this bit
 is 1, the server will attempt to requeue the message, potentially then delivering it to an
 alternative subscriber."))
  (:documentation "roles: server MUST."))

;;; class: file [id method-names content-type content-encoding headers priority reply-to message-id filename timestamp cluster-id]
;;;   file.qos [prefetch-size prefetch-count global]
;;;   file.qos-ok
;;;   file.consume [ticket queue consumer-tag no-local no-ack exclusive no-wait]
;;;   file.consume-ok [consumer-tag]
;;;   file.cancel [consumer-tag no-wait]
;;;   file.cancel-ok [consumer-tag]
;;;   file.open [identifier content-size]
;;;   file.open-ok [staged-size]
;;;   file.stage
;;;   file.publish [ticket exchange routing-key mandatory immediate identifier]
;;;   file.return [reply-code reply-text exchange routing-key]
;;;   file.deliver [consumer-tag delivery-tag redelivered exchange routing-key identifier]
;;;   file.ack [delivery-tag multiple]
;;;   file.reject [delivery-tag requeue]

(def-amqp-class amqp-1-1-0-8-0:file (amqp-1-1-0-8-0:object amqp:file)
  ((id :initform 70 :allocation :class)
   (method-names :initform
    '(amqp:qos amqp:qos-ok amqp:consume amqp:consume-ok amqp:cancel amqp:cancel-ok amqp:open amqp:open-ok amqp::stage
      amqp:publish amqp:return amqp:deliver amqp:ack amqp:reject)
    :allocation :class))
  ((content-type :initform (field-type-initform content-type amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "MIME content type")
   (content-encoding
     :initform
     (field-type-initform content-encoding amqp-1-1-0-8-0::shortstr)
     :type
     amqp-1-1-0-8-0::shortstr
     :documentation
     "MIME content encoding")
   (headers :initform (field-type-initform headers amqp-1-1-0-8-0::table) :type amqp-1-1-0-8-0::table :documentation
    "Message header field table")
   (priority :initform (field-type-initform priority amqp-1-1-0-8-0::octet) :type amqp-1-1-0-8-0::octet :documentation
    "The message priority, 0 to 9")
   (reply-to :initform (field-type-initform reply-to amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "The destination to reply to")
   (message-id :initform (field-type-initform message-id amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "The application message identifier")
   (filename :initform (field-type-initform filename amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "The message filename")
   (timestamp :initform (field-type-initform timestamp amqp-1-1-0-8-0::timestamp) :type amqp-1-1-0-8-0::timestamp
    :documentation "The message timestamp")
   (cluster-id :initform (field-type-initform cluster-id amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "Intra-cluster routing identifier"))
  ((prefetch-size :initform (field-type-initform prefetch-size amqp-1-1-0-8-0::long) :type amqp-1-1-0-8-0::long
    :documentation "prefetch window in octets

 The client can request that messages be sent in advance so that
 when the client finishes processing a message, the following
 message is already held locally, rather than needing to be sent
 down the channel. Prefetching gives a performance improvement.
 This field specifies the prefetch window size in octets. May be
 set to zero, meaning 'no specific limit'. Note that other
 prefetch limits may still apply. The prefetch-size is ignored
 if the no-ack option is set.")
   (prefetch-count :initform (field-type-initform prefetch-count amqp-1-1-0-8-0::short) :type amqp-1-1-0-8-0::short
    :documentation "prefetch window in messages

 Specifies a prefetch window in terms of whole messages. This
 is compatible with some file API implementations. This field
 may be used in combination with the prefetch-size field; a
 message will only be sent in advance if both prefetch windows
 (and those at the channel and connection level) allow it.
 The prefetch-count is ignored if the no-ack option is set.

 The server MAY send less data in advance than allowed by the
 client's specified prefetch windows but it MUST NOT send more.")
   (global :initform (field-type-initform global amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "apply to entire connection

 By default the QoS settings apply to the current channel only. If
 this field is set, they are applied to the entire connection.")
   (queue :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name) :type amqp-1-1-0-8-0::queue-name
    :documentation "Specifies the name of the queue to consume from. If the queue name
 is null, refers to the current queue for the channel, which is the
 last declared queue.

 If the client did not previously declare a queue, and the queue name
 in this method is empty, the server MUST raise a connection exception
 with reply code 530 (not allowed).")
   (no-local :initform (field-type-initform no-local amqp-1-1-0-8-0::no-local) :type amqp-1-1-0-8-0::no-local)
   (no-ack :initform (field-type-initform no-ack amqp-1-1-0-8-0::no-ack) :type amqp-1-1-0-8-0::no-ack)
   (exclusive :initform (field-type-initform exclusive amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "request exclusive access

 Request exclusive consumer access, meaning only this consumer can
 access the queue.

 If the server cannot grant exclusive access to the queue when asked,
 - because there are other consumers active - it MUST raise a channel
 exception with return code 405 (resource locked).")
   (no-wait :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception.")
   (content-size :initform (field-type-initform content-size amqp-1-1-0-8-0::longlong) :type amqp-1-1-0-8-0::longlong
    :documentation "message content size

 The size of the content in octets. The recipient may use this
 information to allocate or check available space in advance, to
 avoid 'disk full' errors during staging of very large messages.

 The sender MUST accurately fill the content-size field.
 Zero-length content is permitted.")
   (staged-size :initform (field-type-initform staged-size amqp-1-1-0-8-0::longlong) :type amqp-1-1-0-8-0::longlong
    :documentation "already staged amount

 The amount of previously-staged content in octets. For a new
 message this will be zero.

 The sender MUST start sending data from this octet offset in the
 message, counting from zero.

 The recipient MAY decide how long to hold partially-staged content
 and MAY implement staging by always discarding partially-staged
 content. However if it uses the file content type it MUST support
 the staging methods.")
   (ticket :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket) :type amqp-1-1-0-8-0::access-ticket
    :documentation "The client MUST provide a valid access ticket giving 'write'
 access rights to the access realm for the exchange.")
   (mandatory :initform (field-type-initform mandatory amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "indicate mandatory routing

 This flag tells the server how to react if the message cannot be
 routed to a queue. If this flag is set, the server will return an
 unroutable message with a Return method. If this flag is zero, the
 server silently drops the message.

 The server SHOULD implement the mandatory flag.")
   (immediate :initform (field-type-initform immediate amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "request immediate delivery

 This flag tells the server how to react if the message cannot be
 routed to a queue consumer immediately. If this flag is set, the
 server will return an undeliverable message with a Return method.
 If this flag is zero, the server will queue the message, but with
 no guarantee that it will ever be consumed.

 The server SHOULD implement the immediate flag.")
   (reply-code :initform (field-type-initform reply-code amqp-1-1-0-8-0::reply-code) :type amqp-1-1-0-8-0::reply-code)
   (reply-text :initform (field-type-initform reply-text amqp-1-1-0-8-0::reply-text) :type amqp-1-1-0-8-0::reply-text)
   (consumer-tag :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag) :type
    amqp-1-1-0-8-0::consumer-tag)
   (redelivered :initform (field-type-initform redelivered amqp-1-1-0-8-0::redelivered) :type
    amqp-1-1-0-8-0::redelivered)
   (exchange :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name) :type amqp-1-1-0-8-0::exchange-name
    :documentation "Specifies the name of the exchange that the message was originally
 published to.")
   (routing-key :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "Message routing key

 Specifies the routing key name specified when the message was
 published.")
   (identifier :initform (field-type-initform identifier amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "staging identifier

 This is the staging identifier of the message to deliver. The
 message must have been staged. Note that a server can send the
 Deliver method asynchronously without waiting for staging to
 finish.")
   (multiple :initform (field-type-initform multiple amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "acknowledge multiple messages

 If set to 1, the delivery tag is treated as 'up to and including',
 so that the client can acknowledge multiple messages with a single
 method. If set to zero, the delivery tag refers to a single
 message. If the multiple field is 1, and the delivery tag is zero,
 tells the server to acknowledge all outstanding mesages.

 The server MUST validate that a non-zero delivery-tag refers to an
 delivered message, and raise a channel exception if this is not the
 case.")
   (delivery-tag :initform (field-type-initform delivery-tag amqp-1-1-0-8-0::delivery-tag) :type
    amqp-1-1-0-8-0::delivery-tag)
   (requeue :initform (field-type-initform requeue amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "requeue the message

 If this field is zero, the message will be discarded. If this bit
 is 1, the server will attempt to requeue the message.

 The server MUST NOT deliver the message to the same client within
 the context of the current channel. The recommended strategy is
 to attempt to deliver the message to an alternative consumer, and
 if that is not possible, to move the message to a dead-letter
 queue. The server MAY use more sophisticated tracking to hold
 the message on the queue and redeliver it to the same client at
 a later stage."))
  (:documentation "roles: server MAY; client MAY.

  The file class provides methods that support reliable file transfer.
  File messages have a specific set of properties that are required for
  interoperability with file transfer applications. File messages and
  acknowledgements are subject to channel transactions.  Note that the
  file class does not provide message browsing methods; these are not
  compatible with the staging model.  Applications that need browsable
  file transfer should use Basic content and the Basic class.

    file                = C:QOS S:QOS-OK
                        / C:CONSUME S:CONSUME-OK
                        / C:CANCEL S:CANCEL-OK
                        / C:OPEN S:OPEN-OK C:STAGE content
                        / S:OPEN C:OPEN-OK S:STAGE content
                        / C:PUBLISH
                        / S:DELIVER
                        / S:RETURN
                        / C:ACK
                        / C:REJECT

  The server MUST make a best-effort to hold file messages on a
  reliable storage mechanism.

  The server MUST NOT discard a file message in case of a queue
  overflow. The server MUST use the Channel.Flow method to slow or stop
  a file message publisher when necessary.

  The server MUST implement at least 2 priority levels for file
  messages, where priorities 0-4 and 5-9 are treated as two distinct
  levels. The server MAY implement up to 10 priority levels.

  The server MUST support both automatic and explicit acknowledgements
  on file content.
"))


(def-amqp-method (amqp-1-1-0-8-0:file amqp:qos) (amqp:qos amqp-1-1-0-8-0:method)
  ((id :initform 10))
  ((prefetch-size
   :initform (field-type-initform prefetch-size amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "prefetch window in octets

 The client can request that messages be sent in advance so that
 when the client finishes processing a message, the following
 message is already held locally, rather than needing to be sent
 down the channel. Prefetching gives a performance improvement.
 This field specifies the prefetch window size in octets. May be
 set to zero, meaning 'no specific limit'. Note that other
 prefetch limits may still apply. The prefetch-size is ignored
 if the no-ack option is set.")
  (prefetch-count
   :initform (field-type-initform prefetch-count amqp-1-1-0-8-0::short)
   :type amqp-1-1-0-8-0::short
   :documentation "prefetch window in messages

 Specifies a prefetch window in terms of whole messages. This
 is compatible with some file API implementations. This field
 may be used in combination with the prefetch-size field; a
 message will only be sent in advance if both prefetch windows
 (and those at the channel and connection level) allow it.
 The prefetch-count is ignored if the no-ack option is set.

 The server MAY send less data in advance than allowed by the
 client's specified prefetch windows but it MUST NOT send more.")
  (global
   :initform (field-type-initform global amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "apply to entire connection

 By default the QoS settings apply to the current channel only. If
 this field is set, they are applied to the entire connection."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp:qos-ok) (amqp:qos-ok amqp-1-1-0-8-0:method)
  ((id :initform 11))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp:consume) (amqp:consume amqp-1-1-0-8-0:method)
  ((id :initform 20))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "The client MUST provide a valid access ticket giving 'read' access
 rights to the realm for the queue.")
  (queue
   :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name)
   :type amqp-1-1-0-8-0::queue-name
   :documentation "Specifies the name of the queue to consume from. If the queue name
 is null, refers to the current queue for the channel, which is the
 last declared queue.

 If the client did not previously declare a queue, and the queue name
 in this method is empty, the server MUST raise a connection exception
 with reply code 530 (not allowed).")
  (consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag
   :documentation "Specifies the identifier for the consumer. The consumer tag is
 local to a connection, so two clients can use the same consumer
 tags. If this field is empty the server will generate a unique
 tag.

 The tag MUST NOT refer to an existing consumer. If the client
 attempts to create two consumers with the same non-empty tag
 the server MUST raise a connection exception with reply code
 530 (not allowed).")
  (no-local
   :initform (field-type-initform no-local amqp-1-1-0-8-0::no-local)
   :type amqp-1-1-0-8-0::no-local)
  (no-ack
   :initform (field-type-initform no-ack amqp-1-1-0-8-0::no-ack)
   :type amqp-1-1-0-8-0::no-ack)
  (exclusive
   :initform (field-type-initform exclusive amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request exclusive access

 Request exclusive consumer access, meaning only this consumer can
 access the queue.

 If the server cannot grant exclusive access to the queue when asked,
 - because there are other consumers active - it MUST raise a channel
 exception with return code 405 (resource locked).")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp:consume-ok) (amqp:consume-ok amqp-1-1-0-8-0:method)
  ((id :initform 21))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag
   :documentation "Holds the consumer tag specified by the client or provided by
 the server."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp:cancel) (amqp:cancel amqp-1-1-0-8-0:method)
  ((id :initform 30))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag)
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp:cancel-ok) (amqp:cancel-ok amqp-1-1-0-8-0:method)
  ((id :initform 31))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp:open) (amqp:open amqp-1-1-0-8-0:method)
  ((id :initform 40))
  ((identifier
   :initform (field-type-initform identifier amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "staging identifier

 This is the staging identifier. This is an arbitrary string chosen
 by the sender. For staging to work correctly the sender must use
 the same staging identifier when staging the same message a second
 time after recovery from a failure. A good choice for the staging
 identifier would be the SHA1 hash of the message properties data
 (including the original filename, revised time, etc.).")
  (content-size
   :initform (field-type-initform content-size amqp-1-1-0-8-0::longlong)
   :type amqp-1-1-0-8-0::longlong
   :documentation "message content size

 The size of the content in octets. The recipient may use this
 information to allocate or check available space in advance, to
 avoid 'disk full' errors during staging of very large messages.

 The sender MUST accurately fill the content-size field.
 Zero-length content is permitted."))
  (:documentation "roles: server MUST; client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp:open-ok) (amqp:open-ok amqp-1-1-0-8-0:method)
  ((id :initform 41))
  ((staged-size
   :initform (field-type-initform staged-size amqp-1-1-0-8-0::longlong)
   :type amqp-1-1-0-8-0::longlong
   :documentation "already staged amount

 The amount of previously-staged content in octets. For a new
 message this will be zero.

 The sender MUST start sending data from this octet offset in the
 message, counting from zero.

 The recipient MAY decide how long to hold partially-staged content
 and MAY implement staging by always discarding partially-staged
 content. However if it uses the file content type it MUST support
 the staging methods."))
  (:documentation "roles: server MUST; client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp::stage) (amqp::stage amqp-1-1-0-8-0:method)
  ((id :initform 50))
  ()
  (:documentation "roles: server MUST; client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp:publish) (amqp:publish amqp-1-1-0-8-0:method)
  ((id :initform 60))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "The client MUST provide a valid access ticket giving 'write'
 access rights to the access realm for the exchange.")
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "Specifies the name of the exchange to publish to. The exchange
 name can be empty, meaning the default exchange. If the exchange
 name is specified, and that exchange does not exist, the server
 will raise a channel exception.

 The server MUST accept a blank exchange name to mean the default
 exchange.

 If the exchange was declared as an internal exchange, the server
 MUST respond with a reply code 403 (access refused) and raise a
 channel exception.

 The exchange MAY refuse file content in which case it MUST respond
 with a reply code 540 (not implemented) and raise a channel
 exception.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "Message routing key

 Specifies the routing key for the message. The routing key is
 used for routing messages depending on the exchange configuration.")
  (mandatory
   :initform (field-type-initform mandatory amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "indicate mandatory routing

 This flag tells the server how to react if the message cannot be
 routed to a queue. If this flag is set, the server will return an
 unroutable message with a Return method. If this flag is zero, the
 server silently drops the message.

 The server SHOULD implement the mandatory flag.")
  (immediate
   :initform (field-type-initform immediate amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request immediate delivery

 This flag tells the server how to react if the message cannot be
 routed to a queue consumer immediately. If this flag is set, the
 server will return an undeliverable message with a Return method.
 If this flag is zero, the server will queue the message, but with
 no guarantee that it will ever be consumed.

 The server SHOULD implement the immediate flag.")
  (identifier
   :initform (field-type-initform identifier amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "staging identifier

 This is the staging identifier of the message to publish. The
 message must have been staged. Note that a client can send the
 Publish method asynchronously without waiting for staging to
 finish."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp:return) (amqp:return amqp-1-1-0-8-0:method)
  ((id :initform 70))
  ((reply-code
   :initform (field-type-initform reply-code amqp-1-1-0-8-0::reply-code)
   :type amqp-1-1-0-8-0::reply-code)
  (reply-text
   :initform (field-type-initform reply-text amqp-1-1-0-8-0::reply-text)
   :type amqp-1-1-0-8-0::reply-text)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "Specifies the name of the exchange that the message was
 originally published to.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "Message routing key

 Specifies the routing key name specified when the message was
 published."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp:deliver) (amqp:deliver amqp-1-1-0-8-0:method)
  ((id :initform 80))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag)
  (delivery-tag
   :initform (field-type-initform delivery-tag amqp-1-1-0-8-0::delivery-tag)
   :type amqp-1-1-0-8-0::delivery-tag)
  (redelivered
   :initform (field-type-initform redelivered amqp-1-1-0-8-0::redelivered)
   :type amqp-1-1-0-8-0::redelivered)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "Specifies the name of the exchange that the message was originally
 published to.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "Message routing key

 Specifies the routing key name specified when the message was
 published.")
  (identifier
   :initform (field-type-initform identifier amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "staging identifier

 This is the staging identifier of the message to deliver. The
 message must have been staged. Note that a server can send the
 Deliver method asynchronously without waiting for staging to
 finish."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp:ack) (amqp:ack amqp-1-1-0-8-0:method)
  ((id :initform 90))
  ((delivery-tag
   :initform (field-type-initform delivery-tag amqp-1-1-0-8-0::delivery-tag)
   :type amqp-1-1-0-8-0::delivery-tag)
  (multiple
   :initform (field-type-initform multiple amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "acknowledge multiple messages

 If set to 1, the delivery tag is treated as 'up to and including',
 so that the client can acknowledge multiple messages with a single
 method. If set to zero, the delivery tag refers to a single
 message. If the multiple field is 1, and the delivery tag is zero,
 tells the server to acknowledge all outstanding mesages.

 The server MUST validate that a non-zero delivery-tag refers to an
 delivered message, and raise a channel exception if this is not the
 case."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:file amqp:reject) (amqp:reject amqp-1-1-0-8-0:method)
  ((id :initform 100))
  ((delivery-tag
   :initform (field-type-initform delivery-tag amqp-1-1-0-8-0::delivery-tag)
   :type amqp-1-1-0-8-0::delivery-tag)
  (requeue
   :initform (field-type-initform requeue amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "requeue the message

 If this field is zero, the message will be discarded. If this bit
 is 1, the server will attempt to requeue the message.

 The server MUST NOT deliver the message to the same client within
 the context of the current channel. The recommended strategy is
 to attempt to deliver the message to an alternative consumer, and
 if that is not possible, to move the message to a dead-letter
 queue. The server MAY use more sophisticated tracking to hold
 the message on the queue and redeliver it to the same client at
 a later stage."))
  (:documentation "roles: server MUST."))

;;; class: stream [id method-names content-type content-encoding headers priority timestamp]
;;;   stream.qos [prefetch-size prefetch-count consume-rate global]
;;;   stream.qos-ok
;;;   stream.consume [ticket queue consumer-tag no-local exclusive no-wait]
;;;   stream.consume-ok [consumer-tag]
;;;   stream.cancel [consumer-tag no-wait]
;;;   stream.cancel-ok [consumer-tag]
;;;   stream.publish [ticket exchange routing-key mandatory immediate]
;;;   stream.return [reply-code reply-text exchange routing-key]
;;;   stream.deliver [consumer-tag delivery-tag exchange queue]

(def-amqp-class amqp-1-1-0-8-0:stream (amqp-1-1-0-8-0:object amqp:stream)
  ((id :initform 80 :allocation :class)
   (method-names :initform
    '(amqp:qos amqp:qos-ok amqp:consume amqp:consume-ok amqp:cancel amqp:cancel-ok amqp:publish amqp:return
      amqp:deliver)
    :allocation :class))
  ((content-type :initform (field-type-initform content-type amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "MIME content type")
   (content-encoding
     :initform
     (field-type-initform content-encoding amqp-1-1-0-8-0::shortstr)
     :type
     amqp-1-1-0-8-0::shortstr
     :documentation
     "MIME content encoding")
   (headers :initform (field-type-initform headers amqp-1-1-0-8-0::table) :type amqp-1-1-0-8-0::table :documentation
    "Message header field table")
   (priority :initform (field-type-initform priority amqp-1-1-0-8-0::octet) :type amqp-1-1-0-8-0::octet :documentation
    "The message priority, 0 to 9")
   (timestamp :initform (field-type-initform timestamp amqp-1-1-0-8-0::timestamp) :type amqp-1-1-0-8-0::timestamp
    :documentation "The message timestamp"))
  ((prefetch-size :initform (field-type-initform prefetch-size amqp-1-1-0-8-0::long) :type amqp-1-1-0-8-0::long
    :documentation "prefetch window in octets

 The client can request that messages be sent in advance so that
 when the client finishes processing a message, the following
 message is already held locally, rather than needing to be sent
 down the channel. Prefetching gives a performance improvement.
 This field specifies the prefetch window size in octets. May be
 set to zero, meaning 'no specific limit'. Note that other
 prefetch limits may still apply.")
   (prefetch-count :initform (field-type-initform prefetch-count amqp-1-1-0-8-0::short) :type amqp-1-1-0-8-0::short
    :documentation "prefetch window in messages

 Specifies a prefetch window in terms of whole messages. This
 field may be used in combination with the prefetch-size field;
 a message will only be sent in advance if both prefetch windows
 (and those at the channel and connection level) allow it.")
   (consume-rate :initform (field-type-initform consume-rate amqp-1-1-0-8-0::long) :type amqp-1-1-0-8-0::long
    :documentation "transfer rate in octets/second

 Specifies a desired transfer rate in octets per second. This is
 usually determined by the application that uses the streaming
 data. A value of zero means 'no limit', i.e. as rapidly as
 possible.

 The server MAY ignore the prefetch values and consume rates,
 depending on the type of stream and the ability of the server
 to queue and/or reply it. The server MAY drop low-priority
 messages in favour of high-priority messages.")
   (global :initform (field-type-initform global amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "apply to entire connection

 By default the QoS settings apply to the current channel only. If
 this field is set, they are applied to the entire connection.")
   (no-local :initform (field-type-initform no-local amqp-1-1-0-8-0::no-local) :type amqp-1-1-0-8-0::no-local)
   (exclusive :initform (field-type-initform exclusive amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "request exclusive access

 Request exclusive consumer access, meaning only this consumer can
 access the queue.

 If the server cannot grant exclusive access to the queue when asked,
 - because there are other consumers active - it MUST raise a channel
 exception with return code 405 (resource locked).")
   (no-wait :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception.")
   (ticket :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket) :type amqp-1-1-0-8-0::access-ticket
    :documentation "The client MUST provide a valid access ticket giving 'write'
 access rights to the access realm for the exchange.")
   (mandatory :initform (field-type-initform mandatory amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "indicate mandatory routing

 This flag tells the server how to react if the message cannot be
 routed to a queue. If this flag is set, the server will return an
 unroutable message with a Return method. If this flag is zero, the
 server silently drops the message.

 The server SHOULD implement the mandatory flag.")
   (immediate :initform (field-type-initform immediate amqp-1-1-0-8-0::bit) :type amqp-1-1-0-8-0::bit :documentation
    "request immediate delivery

 This flag tells the server how to react if the message cannot be
 routed to a queue consumer immediately. If this flag is set, the
 server will return an undeliverable message with a Return method.
 If this flag is zero, the server will queue the message, but with
 no guarantee that it will ever be consumed.

 The server SHOULD implement the immediate flag.")
   (reply-code :initform (field-type-initform reply-code amqp-1-1-0-8-0::reply-code) :type amqp-1-1-0-8-0::reply-code)
   (reply-text :initform (field-type-initform reply-text amqp-1-1-0-8-0::reply-text) :type amqp-1-1-0-8-0::reply-text)
   (routing-key :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "Message routing key

 Specifies the routing key name specified when the message was
 published.")
   (consumer-tag :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag) :type
    amqp-1-1-0-8-0::consumer-tag)
   (delivery-tag :initform (field-type-initform delivery-tag amqp-1-1-0-8-0::delivery-tag) :type
    amqp-1-1-0-8-0::delivery-tag)
   (exchange :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name) :type amqp-1-1-0-8-0::exchange-name
    :documentation "Specifies the name of the exchange that the message was originally
 published to.")
   (queue :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name) :type amqp-1-1-0-8-0::queue-name
    :documentation "Specifies the name of the queue that the message came from. Note
 that a single channel can start many consumers on different
 queues."))
  (:documentation "roles: server MAY; client MAY.

  The stream class provides methods that support multimedia streaming.
  The stream class uses the following semantics: one message is one
  packet of data; delivery is unacknowleged and unreliable; the consumer
  can specify quality of service parameters that the server can try to
  adhere to; lower-priority messages may be discarded in favour of high
  priority messages.

    stream              = C:QOS S:QOS-OK
                        / C:CONSUME S:CONSUME-OK
                        / C:CANCEL S:CANCEL-OK
                        / C:PUBLISH content
                        / S:RETURN
                        / S:DELIVER content

  The server SHOULD discard stream messages on a priority basis if
  the queue size exceeds some configured limit.

  The server MUST implement at least 2 priority levels for stream
  messages, where priorities 0-4 and 5-9 are treated as two distinct
  levels. The server MAY implement up to 10 priority levels.

  The server MUST implement automatic acknowledgements on stream
  content.  That is, as soon as a message is delivered to a client
  via a Deliver method, the server must remove it from the queue.
"))


(def-amqp-method (amqp-1-1-0-8-0:stream amqp:qos) (amqp:qos amqp-1-1-0-8-0:method)
  ((id :initform 10))
  ((prefetch-size
   :initform (field-type-initform prefetch-size amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "prefetch window in octets

 The client can request that messages be sent in advance so that
 when the client finishes processing a message, the following
 message is already held locally, rather than needing to be sent
 down the channel. Prefetching gives a performance improvement.
 This field specifies the prefetch window size in octets. May be
 set to zero, meaning 'no specific limit'. Note that other
 prefetch limits may still apply.")
  (prefetch-count
   :initform (field-type-initform prefetch-count amqp-1-1-0-8-0::short)
   :type amqp-1-1-0-8-0::short
   :documentation "prefetch window in messages

 Specifies a prefetch window in terms of whole messages. This
 field may be used in combination with the prefetch-size field;
 a message will only be sent in advance if both prefetch windows
 (and those at the channel and connection level) allow it.")
  (consume-rate
   :initform (field-type-initform consume-rate amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "transfer rate in octets/second

 Specifies a desired transfer rate in octets per second. This is
 usually determined by the application that uses the streaming
 data. A value of zero means 'no limit', i.e. as rapidly as
 possible.

 The server MAY ignore the prefetch values and consume rates,
 depending on the type of stream and the ability of the server
 to queue and/or reply it. The server MAY drop low-priority
 messages in favour of high-priority messages.")
  (global
   :initform (field-type-initform global amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "apply to entire connection

 By default the QoS settings apply to the current channel only. If
 this field is set, they are applied to the entire connection."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:stream amqp:qos-ok) (amqp:qos-ok amqp-1-1-0-8-0:method)
  ((id :initform 11))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:stream amqp:consume) (amqp:consume amqp-1-1-0-8-0:method)
  ((id :initform 20))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "The client MUST provide a valid access ticket giving 'read' access
 rights to the realm for the queue.")
  (queue
   :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name)
   :type amqp-1-1-0-8-0::queue-name
   :documentation "Specifies the name of the queue to consume from. If the queue name
 is null, refers to the current queue for the channel, which is the
 last declared queue.

 If the client did not previously declare a queue, and the queue name
 in this method is empty, the server MUST raise a connection exception
 with reply code 530 (not allowed).")
  (consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag
   :documentation "Specifies the identifier for the consumer. The consumer tag is
 local to a connection, so two clients can use the same consumer
 tags. If this field is empty the server will generate a unique
 tag.

 The tag MUST NOT refer to an existing consumer. If the client
 attempts to create two consumers with the same non-empty tag
 the server MUST raise a connection exception with reply code
 530 (not allowed).")
  (no-local
   :initform (field-type-initform no-local amqp-1-1-0-8-0::no-local)
   :type amqp-1-1-0-8-0::no-local)
  (exclusive
   :initform (field-type-initform exclusive amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request exclusive access

 Request exclusive consumer access, meaning only this consumer can
 access the queue.

 If the server cannot grant exclusive access to the queue when asked,
 - because there are other consumers active - it MUST raise a channel
 exception with return code 405 (resource locked).")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:stream amqp:consume-ok) (amqp:consume-ok amqp-1-1-0-8-0:method)
  ((id :initform 21))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag
   :documentation "Holds the consumer tag specified by the client or provided by
 the server."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:stream amqp:cancel) (amqp:cancel amqp-1-1-0-8-0:method)
  ((id :initform 30))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag)
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "do not send a reply method

 If set, the server will not respond to the method. The client should
 not wait for a reply method. If the server could not complete the
 method it will raise a channel or connection exception."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:stream amqp:cancel-ok) (amqp:cancel-ok amqp-1-1-0-8-0:method)
  ((id :initform 31))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:stream amqp:publish) (amqp:publish amqp-1-1-0-8-0:method)
  ((id :initform 40))
  ((ticket
   :initform (field-type-initform ticket amqp-1-1-0-8-0::access-ticket)
   :type amqp-1-1-0-8-0::access-ticket
   :documentation "The client MUST provide a valid access ticket giving 'write'
 access rights to the access realm for the exchange.")
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "Specifies the name of the exchange to publish to. The exchange
 name can be empty, meaning the default exchange. If the exchange
 name is specified, and that exchange does not exist, the server
 will raise a channel exception.

 The server MUST accept a blank exchange name to mean the default
 exchange.

 If the exchange was declared as an internal exchange, the server
 MUST respond with a reply code 403 (access refused) and raise a
 channel exception.

 The exchange MAY refuse stream content in which case it MUST
 respond with a reply code 540 (not implemented) and raise a
 channel exception.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "Message routing key

 Specifies the routing key for the message. The routing key is
 used for routing messages depending on the exchange configuration.")
  (mandatory
   :initform (field-type-initform mandatory amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "indicate mandatory routing

 This flag tells the server how to react if the message cannot be
 routed to a queue. If this flag is set, the server will return an
 unroutable message with a Return method. If this flag is zero, the
 server silently drops the message.

 The server SHOULD implement the mandatory flag.")
  (immediate
   :initform (field-type-initform immediate amqp-1-1-0-8-0::bit)
   :type amqp-1-1-0-8-0::bit
   :documentation "request immediate delivery

 This flag tells the server how to react if the message cannot be
 routed to a queue consumer immediately. If this flag is set, the
 server will return an undeliverable message with a Return method.
 If this flag is zero, the server will queue the message, but with
 no guarantee that it will ever be consumed.

 The server SHOULD implement the immediate flag."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:stream amqp:return) (amqp:return amqp-1-1-0-8-0:method)
  ((id :initform 50))
  ((reply-code
   :initform (field-type-initform reply-code amqp-1-1-0-8-0::reply-code)
   :type amqp-1-1-0-8-0::reply-code)
  (reply-text
   :initform (field-type-initform reply-text amqp-1-1-0-8-0::reply-text)
   :type amqp-1-1-0-8-0::reply-text)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "Specifies the name of the exchange that the message was
 originally published to.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "Message routing key

 Specifies the routing key name specified when the message was
 published."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:stream amqp:deliver) (amqp:deliver amqp-1-1-0-8-0:method)
  ((id :initform 60))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-8-0::consumer-tag)
   :type amqp-1-1-0-8-0::consumer-tag)
  (delivery-tag
   :initform (field-type-initform delivery-tag amqp-1-1-0-8-0::delivery-tag)
   :type amqp-1-1-0-8-0::delivery-tag)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-8-0::exchange-name)
   :type amqp-1-1-0-8-0::exchange-name
   :documentation "Specifies the name of the exchange that the message was originally
 published to.")
  (queue
   :initform (field-type-initform queue amqp-1-1-0-8-0::queue-name)
   :type amqp-1-1-0-8-0::queue-name
   :documentation "Specifies the name of the queue that the message came from. Note
 that a single channel can start many consumers on different
 queues."))
  (:documentation "roles: client MUST."))

;;; class: tx [id method-names]
;;;   tx.select
;;;   tx.select-ok
;;;   tx.commit
;;;   tx.commit-ok
;;;   tx.rollback
;;;   tx.rollback-ok

(def-amqp-class amqp-1-1-0-8-0:tx (amqp-1-1-0-8-0:object amqp:tx)
  ((id :initform 90 :allocation :class)
   (method-names :initform '(amqp:select amqp:select-ok amqp:commit amqp:commit-ok amqp:rollback amqp:rollback-ok)
    :allocation :class))
  ()
  nil
  (:documentation "roles: server SHOULD; client MAY.

  Standard transactions provide so-called \"1.5 phase commit\".  We can
  ensure that work is never lost, but there is a chance of confirmations
  being lost, so that messages may be resent.  Applications that use
  standard transactions must be able to detect and ignore duplicate
  messages.

    tx                  = C:SELECT S:SELECT-OK
                        / C:COMMIT S:COMMIT-OK
                        / C:ROLLBACK S:ROLLBACK-OK
"))


(def-amqp-method (amqp-1-1-0-8-0:tx amqp:select) (amqp:select amqp-1-1-0-8-0:method)
  ((id :initform 10))
  ()
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:tx amqp:select-ok) (amqp:select-ok amqp-1-1-0-8-0:method)
  ((id :initform 11))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:tx amqp:commit) (amqp:commit amqp-1-1-0-8-0:method)
  ((id :initform 20))
  ()
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:tx amqp:commit-ok) (amqp:commit-ok amqp-1-1-0-8-0:method)
  ((id :initform 21))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:tx amqp:rollback) (amqp:rollback amqp-1-1-0-8-0:method)
  ((id :initform 30))
  ()
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:tx amqp:rollback-ok) (amqp:rollback-ok amqp-1-1-0-8-0:method)
  ((id :initform 31))
  ()
  (:documentation "roles: client MUST."))

;;; class: dtx [id method-names]
;;;   dtx.select
;;;   dtx.select-ok
;;;   dtx.start [dtx-identifier]
;;;   dtx.start-ok

(def-amqp-class amqp-1-1-0-8-0:dtx (amqp-1-1-0-8-0:object amqp:dtx)
  ((id :initform 100 :allocation :class)
   (method-names :initform '(amqp:select amqp:select-ok amqp:start amqp:start-ok) :allocation :class))
  ()
  ((dtx-identifier :initform (field-type-initform dtx-identifier amqp-1-1-0-8-0::shortstr) :type
    amqp-1-1-0-8-0::shortstr :documentation "transaction identifier

 The distributed transaction key. This identifies the transaction
 so that the AMQP server can coordinate with the distributed
 transaction coordinator."))
  (:documentation "roles: server MAY; client MAY.

  Distributed transactions provide so-called \"2-phase commit\".    The
  AMQP distributed transaction model supports the X-Open XA
  architecture and other distributed transaction implementations.
  The Dtx class assumes that the server has a private communications
  channel (not AMQP) to a distributed transaction coordinator.

    dtx                 = C:SELECT S:SELECT-OK
                          C:START S:START-OK
"))


(def-amqp-method (amqp-1-1-0-8-0:dtx amqp:select) (amqp:select amqp-1-1-0-8-0:method)
  ((id :initform 10))
  ()
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:dtx amqp:select-ok) (amqp:select-ok amqp-1-1-0-8-0:method)
  ((id :initform 11))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-8-0:dtx amqp:start) (amqp:start amqp-1-1-0-8-0:method)
  ((id :initform 20))
  ((dtx-identifier
   :initform (field-type-initform dtx-identifier amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "transaction identifier

 The distributed transaction key. This identifies the transaction
 so that the AMQP server can coordinate with the distributed
 transaction coordinator."))
  (:documentation "roles: server MAY."))

(def-amqp-method (amqp-1-1-0-8-0:dtx amqp:start-ok) (amqp:start-ok amqp-1-1-0-8-0:method)
  ((id :initform 21))
  ()
  (:documentation "roles: client MUST."))

;;; class: tunnel [id method-names headers proxy-name data-name durable broadcast]
;;;   tunnel.request [meta-data]

(def-amqp-class amqp-1-1-0-8-0:tunnel (amqp-1-1-0-8-0:object amqp:tunnel)
  ((id :initform 110 :allocation :class) (method-names :initform '(amqp:request) :allocation :class))
  ()
  ((headers :initform (field-type-initform headers amqp-1-1-0-8-0::table) :type amqp-1-1-0-8-0::table :documentation
            "Message header field table")
   (proxy-name :initform (field-type-initform proxy-name amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
               :documentation "The identity of the tunnelling proxy")
   (data-name :initform (field-type-initform data-name amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
              :documentation "The name or type of the message being tunnelled")
   (durable :initform (field-type-initform durable amqp-1-1-0-8-0::octet) :type amqp-1-1-0-8-0::octet :documentation
            "The message durability indicator")
   (broadcast :initform (field-type-initform broadcast amqp-1-1-0-8-0::octet) :type amqp-1-1-0-8-0::octet
              :documentation "The message broadcast mode")
   (meta-data :initform (field-type-initform meta-data amqp-1-1-0-8-0::table) :type amqp-1-1-0-8-0::table
              :documentation "meta data for the tunnelled block

 This field table holds arbitrary meta-data that the sender needs
 to pass to the recipient."))
  (:documentation "roles: server MAY; client MAY.

  The tunnel methods are used to send blocks of binary data - which
  can be serialised AMQP methods or other protocol frames - between
  AMQP peers.

    tunnel              = C:REQUEST
                        / S:REQUEST
"))


(def-amqp-method (amqp-1-1-0-8-0:tunnel amqp:request) (amqp:request amqp-1-1-0-8-0:method)
  ((id :initform 10))
  ((meta-data
   :initform (field-type-initform meta-data amqp-1-1-0-8-0::table)
   :type amqp-1-1-0-8-0::table
   :documentation "meta data for the tunnelled block

 This field table holds arbitrary meta-data that the sender needs
 to pass to the recipient."))
  (:documentation "roles: server MUST."))

;;; class: test [id method-names]
;;;   test.integer [integer-1 integer-2 integer-3 integer-4 operation]
;;;   test.integer-ok [result]
;;;   test.string [string-1 string-2 operation]
;;;   test.string-ok [result]
;;;   test.table [table integer-op string-op]
;;;   test.table-ok [integer-result string-result]
;;;   test.content
;;;   test.content-ok [content-checksum]

(def-amqp-class amqp-1-1-0-8-0:test (amqp-1-1-0-8-0:object amqp:test)
  ((id :initform 120 :allocation :class)
   (method-names :initform
    '(amqp::integer amqp::integer-ok amqp:string amqp::string-ok amqp:table amqp::table-ok amqp::content
      amqp::content-ok)
    :allocation :class))
  ()
  ((integer-1 :initform (field-type-initform integer-1 amqp-1-1-0-8-0::octet) :type amqp-1-1-0-8-0::octet
    :documentation "octet test value

 An octet integer test value.")
   (integer-2 :initform (field-type-initform integer-2 amqp-1-1-0-8-0::short) :type amqp-1-1-0-8-0::short
    :documentation "short test value

 A short integer test value.")
   (integer-3 :initform (field-type-initform integer-3 amqp-1-1-0-8-0::long) :type amqp-1-1-0-8-0::long :documentation
    "long test value

 A long integer test value.")
   (integer-4 :initform (field-type-initform integer-4 amqp-1-1-0-8-0::longlong) :type amqp-1-1-0-8-0::longlong
    :documentation "long-long test value

 A long long integer test value.")
   (string-1 :initform (field-type-initform string-1 amqp-1-1-0-8-0::shortstr) :type amqp-1-1-0-8-0::shortstr
    :documentation "short string test value

 An short string test value.")
   (string-2 :initform (field-type-initform string-2 amqp-1-1-0-8-0::longstr) :type amqp-1-1-0-8-0::longstr
    :documentation "long string test value

 A long string test value.")
   (operation :initform (field-type-initform operation amqp-1-1-0-8-0::octet) :type amqp-1-1-0-8-0::octet
    :documentation "operation to test

 The client must execute this operation on the provided string
 test fields and return the result.

 return concatentation of test strings
 return shortest of test strings
 return longest of test strings")
   (result :initform (field-type-initform result amqp-1-1-0-8-0::longstr) :type amqp-1-1-0-8-0::longstr :documentation
    "result value

 The result of the tested operation.")
   (table :initform (field-type-initform table amqp-1-1-0-8-0::table) :type amqp-1-1-0-8-0::table :documentation
    "field table of test values

 A field table of test values.")
   (integer-op :initform (field-type-initform integer-op amqp-1-1-0-8-0::octet) :type amqp-1-1-0-8-0::octet
    :documentation "operation to test on integers

 The client must execute this operation on the provided field
 table integer values and return the result.

 return sum of numeric field values
 return min of numeric field values
 return max of numeric field values")
   (string-op :initform (field-type-initform string-op amqp-1-1-0-8-0::octet) :type amqp-1-1-0-8-0::octet
    :documentation "operation to test on strings

 The client must execute this operation on the provided field
 table string values and return the result.

 return concatenation of string field values
 return shortest of string field values
 return longest of string field values")
   (integer-result :initform (field-type-initform integer-result amqp-1-1-0-8-0::longlong) :type
    amqp-1-1-0-8-0::longlong :documentation "integer result value

 The result of the tested integer operation.")
   (string-result :initform (field-type-initform string-result amqp-1-1-0-8-0::longstr) :type amqp-1-1-0-8-0::longstr
    :documentation "string result value

 The result of the tested string operation.")
   (content-checksum :initform (field-type-initform content-checksum amqp-1-1-0-8-0::long) :type amqp-1-1-0-8-0::long
    :documentation "content hash

 The 32-bit checksum of the content, calculated by adding the
 content into a 32-bit accumulator."))
  (:documentation "roles: server MUST; client SHOULD.

  The test class provides methods for a peer to test the basic
  operational correctness of another peer. The test methods are
  intended to ensure that all peers respect at least the basic
  elements of the protocol, such as frame and content organisation
  and field types. We assume that a specially-designed peer, a
  \"monitor client\" would perform such tests.

    test                = C:INTEGER S:INTEGER-OK
                        / S:INTEGER C:INTEGER-OK
                        / C:STRING S:STRING-OK
                        / S:STRING C:STRING-OK
                        / C:TABLE S:TABLE-OK
                        / S:TABLE C:TABLE-OK
                        / C:CONTENT S:CONTENT-OK
                        / S:CONTENT C:CONTENT-OK
"))


(def-amqp-method (amqp-1-1-0-8-0:test amqp::integer) (amqp::test-integer amqp-1-1-0-8-0:method)
  ((id :initform 10))
  ((integer-1
   :initform (field-type-initform integer-1 amqp-1-1-0-8-0::octet)
   :type amqp-1-1-0-8-0::octet
   :documentation "octet test value

 An octet integer test value.")
  (integer-2
   :initform (field-type-initform integer-2 amqp-1-1-0-8-0::short)
   :type amqp-1-1-0-8-0::short
   :documentation "short test value

 A short integer test value.")
  (integer-3
   :initform (field-type-initform integer-3 amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "long test value

 A long integer test value.")
  (integer-4
   :initform (field-type-initform integer-4 amqp-1-1-0-8-0::longlong)
   :type amqp-1-1-0-8-0::longlong
   :documentation "long-long test value

 A long long integer test value.")
  (operation
   :initform (field-type-initform operation amqp-1-1-0-8-0::octet)
   :type amqp-1-1-0-8-0::octet
   :documentation "operation to test

 The client must execute this operation on the provided integer
 test fields and return the result.

 return sum of test values
 return lowest of test values
 return highest of test values"))
  (:documentation "roles: client MUST; server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:test amqp::integer-ok) (amqp::test-integer-ok amqp-1-1-0-8-0:method)
  ((id :initform 11))
  ((result
   :initform (field-type-initform result amqp-1-1-0-8-0::longlong)
   :type amqp-1-1-0-8-0::longlong
   :documentation "result value

 The result of the tested operation."))
  (:documentation "roles: client MUST; server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:test amqp:string) (amqp::test-string amqp-1-1-0-8-0:method)
  ((id :initform 20))
  ((string-1
   :initform (field-type-initform string-1 amqp-1-1-0-8-0::shortstr)
   :type amqp-1-1-0-8-0::shortstr
   :documentation "short string test value

 An short string test value.")
  (string-2
   :initform (field-type-initform string-2 amqp-1-1-0-8-0::longstr)
   :type amqp-1-1-0-8-0::longstr
   :documentation "long string test value

 A long string test value.")
  (operation
   :initform (field-type-initform operation amqp-1-1-0-8-0::octet)
   :type amqp-1-1-0-8-0::octet
   :documentation "operation to test

 The client must execute this operation on the provided string
 test fields and return the result.

 return concatentation of test strings
 return shortest of test strings
 return longest of test strings"))
  (:documentation "roles: client MUST; server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:test amqp::string-ok) (amqp::test-string-ok amqp-1-1-0-8-0:method)
  ((id :initform 21))
  ((result
   :initform (field-type-initform result amqp-1-1-0-8-0::longstr)
   :type amqp-1-1-0-8-0::longstr
   :documentation "result value

 The result of the tested operation."))
  (:documentation "roles: client MUST; server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:test amqp:table) (amqp::test-table amqp-1-1-0-8-0:method)
  ((id :initform 30))
  ((table
   :initform (field-type-initform table amqp-1-1-0-8-0::table)
   :type amqp-1-1-0-8-0::table
   :documentation "field table of test values

 A field table of test values.")
  (integer-op
   :initform (field-type-initform integer-op amqp-1-1-0-8-0::octet)
   :type amqp-1-1-0-8-0::octet
   :documentation "operation to test on integers

 The client must execute this operation on the provided field
 table integer values and return the result.

 return sum of numeric field values
 return min of numeric field values
 return max of numeric field values")
  (string-op
   :initform (field-type-initform string-op amqp-1-1-0-8-0::octet)
   :type amqp-1-1-0-8-0::octet
   :documentation "operation to test on strings

 The client must execute this operation on the provided field
 table string values and return the result.

 return concatenation of string field values
 return shortest of string field values
 return longest of string field values"))
  (:documentation "roles: client MUST; server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:test amqp::table-ok) (amqp::test-table-ok amqp-1-1-0-8-0:method)
  ((id :initform 31))
  ((integer-result
   :initform (field-type-initform integer-result amqp-1-1-0-8-0::longlong)
   :type amqp-1-1-0-8-0::longlong
   :documentation "integer result value

 The result of the tested integer operation.")
  (string-result
   :initform (field-type-initform string-result amqp-1-1-0-8-0::longstr)
   :type amqp-1-1-0-8-0::longstr
   :documentation "string result value

 The result of the tested string operation."))
  (:documentation "roles: client MUST; server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:test amqp::content) (amqp::test-content amqp-1-1-0-8-0:method)
  ((id :initform 40))
  ()
  (:documentation "roles: client MUST; server MUST."))

(def-amqp-method (amqp-1-1-0-8-0:test amqp::content-ok) (amqp::test-content-ok amqp-1-1-0-8-0:method)
  ((id :initform 41))
  ((content-checksum
   :initform (field-type-initform content-checksum amqp-1-1-0-8-0::long)
   :type amqp-1-1-0-8-0::long
   :documentation "content hash

 The 32-bit checksum of the content, calculated by adding the
 content into a 32-bit accumulator."))
  (:documentation "roles: client MUST; server MUST."))