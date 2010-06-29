;;; -*- Package: DE.SETF.AMQP.IMPLEMENTATION; -*-
;;;
;;; version: amqp-1-1-0-9-0
;;; generated 20100127T210212

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
    (ensure-export #:ack)
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
    (ensure-export #:exchange)
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
    (ensure-export #:recover-async)
    (ensure-export #:recover-ok)
    (ensure-export #:reject)
    (ensure-export #:return)
    (ensure-export #:rollback)
    (ensure-export #:rollback-ok)
    (ensure-export #:secure)
    (ensure-export #:secure-ok)
    (ensure-export #:select)
    (ensure-export #:select-ok)
    (ensure-export #:start)
    (ensure-export #:start-ok)
    (ensure-export #:tune)
    (ensure-export #:tune-ok)
    (ensure-export #:tx)
    (ensure-export #:unbind)
    (ensure-export #:unbind-ok)))

(eval-when (:compile-toplevel :load-toplevel :execute)
  (macrolet ((ensure-export (x) `(export (intern ,(string x) :amqp-1-1-0-9-0) :amqp-1-1-0-9-0))) 
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
    (ensure-export #:basic.recover-async)
    (ensure-export #:basic.recover-ok)
    (ensure-export #:basic.reject)
    (ensure-export #:basic.return)
    (ensure-export #:channel)
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
    (ensure-export #:connection.secure)
    (ensure-export #:connection.secure-ok)
    (ensure-export #:connection.start)
    (ensure-export #:connection.start-ok)
    (ensure-export #:connection.tune)
    (ensure-export #:connection.tune-ok)
    (ensure-export #:exchange)
    (ensure-export #:exchange.declare)
    (ensure-export #:exchange.declare-ok)
    (ensure-export #:exchange.delete)
    (ensure-export #:exchange.delete-ok)
    (ensure-export #:queue)
    (ensure-export #:queue.bind)
    (ensure-export #:queue.bind-ok)
    (ensure-export #:queue.declare)
    (ensure-export #:queue.declare-ok)
    (ensure-export #:queue.delete)
    (ensure-export #:queue.delete-ok)
    (ensure-export #:queue.purge)
    (ensure-export #:queue.purge-ok)
    (ensure-export #:queue.unbind)
    (ensure-export #:queue.unbind-ok)
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
;;;   connection.open [virtual-host reserved-1 reserved-2]
;;;   connection.open-ok [reserved-1]
;;;   connection.close [reply-code reply-text class-id method-id]
;;;   connection.close-ok

(def-amqp-class amqp-1-1-0-9-0:connection (amqp-1-1-0-9-0:object amqp:connection)
  ((id :initform 10 :allocation :class)
   (method-names :initform
    '(amqp:start amqp:start-ok amqp:secure amqp:secure-ok amqp:tune amqp:tune-ok amqp:open amqp:open-ok amqp:close
      amqp:close-ok)
    :allocation :class))
  ()
  ((version-major :initform (field-type-initform version-major amqp-1-1-0-9-0:octet) :type amqp-1-1-0-9-0:octet
    :documentation "protocol major version

 The major version number can take any value from 0 to 99 as defined in the
 AMQP specification.")
   (version-minor :initform (field-type-initform version-minor amqp-1-1-0-9-0:octet) :type amqp-1-1-0-9-0:octet
    :documentation "protocol minor version

 The minor version number can take any value from 0 to 99 as defined in the
 AMQP specification.")
   (server-properties :initform (field-type-initform server-properties amqp-1-1-0-9-0:peer-properties) :type
    amqp-1-1-0-9-0:peer-properties :documentation "server properties

 The properties SHOULD contain at least these fields: 'host', specifying the
 server host name or address, 'product', giving the name of the server product,
 'version', giving the name of the server version, 'platform', giving the name
 of the operating system, 'copyright', if appropriate, and 'information', giving
 other general information.

 Client connects to server and inspects the server properties. It checks for
 the presence of the required fields.")
   (mechanisms :initform (field-type-initform mechanisms amqp-1-1-0-9-0:longstr) :type amqp-1-1-0-9-0:longstr
    :documentation "available security mechanisms

 A list of the security mechanisms that the server supports, delimited by spaces.")
   (locales :initform (field-type-initform locales amqp-1-1-0-9-0:longstr) :type amqp-1-1-0-9-0:longstr :documentation
    "available message locales

 A list of the message locales that the server supports, delimited by spaces. The
 locale defines the language in which the server will send reply texts.

 The server MUST support at least the en_US locale.

 Client connects to server and inspects the locales field. It checks for
 the presence of the required locale(s).")
   (client-properties :initform (field-type-initform client-properties amqp-1-1-0-9-0:peer-properties) :type
    amqp-1-1-0-9-0:peer-properties :documentation "client properties

 The properties SHOULD contain at least these fields: 'product', giving the name
 of the client product, 'version', giving the name of the client version, 'platform',
 giving the name of the operating system, 'copyright', if appropriate, and
 'information', giving other general information.")
   (mechanism :initform (field-type-initform mechanism amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr
    :documentation "selected security mechanism

 A single security mechanisms selected by the client, which must be one of those
 specified by the server.

 The client SHOULD authenticate using the highest-level security profile it
 can handle from the list provided by the server.

 If the mechanism field does not contain one of the security mechanisms
 proposed by the server in the Start method, the server MUST close the
 connection without sending any further data.

 Client connects to server and sends an invalid security mechanism. The
 server must respond by closing the connection (a socket close, with no
 connection close negotiation).")
   (locale :initform (field-type-initform locale amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr :documentation
    "selected message locale

 A single message locale selected by the client, which must be one of those
 specified by the server.")
   (challenge :initform (field-type-initform challenge amqp-1-1-0-9-0:longstr) :type amqp-1-1-0-9-0:longstr
    :documentation "security challenge data

 Challenge information, a block of opaque binary data passed to the security
 mechanism.")
   (response :initform (field-type-initform response amqp-1-1-0-9-0:longstr) :type amqp-1-1-0-9-0:longstr
    :documentation "security response data

 A block of opaque data passed to the security mechanism. The contents of this
 data are defined by the SASL security mechanism.")
   (channel-max :initform (field-type-initform channel-max amqp-1-1-0-9-0:short) :type amqp-1-1-0-9-0:short
    :documentation "negotiated maximum channels

 The maximum total number of channels that the client will use per connection.

 If the client specifies a channel max that is higher than the value provided
 by the server, the server MUST close the connection without attempting a
 negotiated close. The server may report the error in some fashion to assist
 implementors.")
   (frame-max :initform (field-type-initform frame-max amqp-1-1-0-9-0:long) :type amqp-1-1-0-9-0:long :documentation
    "negotiated maximum frame size

 The largest frame size that the client and server will use for the connection.
 Zero means that the client does not impose any specific limit but may reject
 very large frames if it cannot allocate resources for them. Note that the
 frame-max limit applies principally to content frames, where large contents can
 be broken into frames of arbitrary size.

 Until the frame-max has been negotiated, both peers MUST accept frames of up
 to frame-min-size octets large, and the minimum negotiated value for frame-max
 is also frame-min-size.

 If the client specifies a frame max that is higher than the value provided
 by the server, the server MUST close the connection without attempting a
 negotiated close. The server may report the error in some fashion to assist
 implementors.")
   (heartbeat :initform (field-type-initform heartbeat amqp-1-1-0-9-0:short) :type amqp-1-1-0-9-0:short :documentation
    "desired heartbeat delay

 The delay, in seconds, of the connection heartbeat that the client wants. Zero
 means the client does not want a heartbeat.")
   (virtual-host :initform (field-type-initform virtual-host amqp-1-1-0-9-0:path) :type amqp-1-1-0-9-0:path
    :documentation "virtual host name

 The name of the virtual host to work with.

 If the server supports multiple virtual hosts, it MUST enforce a full
 separation of exchanges, queues, and all associated entities per virtual
 host. An application, connected to a specific virtual host, MUST NOT be able
 to access resources of another virtual host.

 The server SHOULD verify that the client has permission to access the
 specified virtual host.")
   (reserved-2 :initform (field-type-initform reserved-2 amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :allocation
    :class)
   (reserved-1 :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr
    :allocation :class)
   (reply-code :initform (field-type-initform reply-code amqp-1-1-0-9-0:reply-code) :type amqp-1-1-0-9-0:reply-code)
   (reply-text :initform (field-type-initform reply-text amqp-1-1-0-9-0:reply-text) :type amqp-1-1-0-9-0:reply-text)
   (class-id :initform (field-type-initform class-id amqp-1-1-0-9-0:class-id) :type amqp-1-1-0-9-0:class-id
    :documentation "failing method class

 When the close is provoked by a method exception, this is the class of the
 method.")
   (method-id :initform (field-type-initform method-id amqp-1-1-0-9-0:method-id) :type amqp-1-1-0-9-0:method-id
    :documentation "failing method ID

 When the close is provoked by a method exception, this is the ID of the method."))
  (:documentation "roles: server MUST; client MUST.

      The connection class provides methods for a client to establish a network connection to
      a server, and for both peers to operate the connection thereafter.
    
      connection          = open-connection *use-connection close-connection
      open-connection     = C:protocol-header
                            S:START C:START-OK
                            *challenge
                            S:TUNE C:TUNE-OK
                            C:OPEN S:OPEN-OK
      challenge           = S:SECURE C:SECURE-OK
      use-connection      = *channel
      close-connection    = C:CLOSE S:CLOSE-OK
                          / S:CLOSE C:CLOSE-OK
    "))


(def-amqp-method (amqp-1-1-0-9-0:connection amqp:start) (amqp:start amqp-1-1-0-9-0:method)
  ((id :initform 10))
  ((version-major
   :initform (field-type-initform version-major amqp-1-1-0-9-0:octet)
   :type amqp-1-1-0-9-0:octet
   :documentation "protocol major version

 The major version number can take any value from 0 to 99 as defined in the
 AMQP specification.")
  (version-minor
   :initform (field-type-initform version-minor amqp-1-1-0-9-0:octet)
   :type amqp-1-1-0-9-0:octet
   :documentation "protocol minor version

 The minor version number can take any value from 0 to 99 as defined in the
 AMQP specification.")
  (server-properties
   :initform (field-type-initform server-properties amqp-1-1-0-9-0:peer-properties)
   :type amqp-1-1-0-9-0:peer-properties
   :documentation "server properties

 The properties SHOULD contain at least these fields: 'host', specifying the
 server host name or address, 'product', giving the name of the server product,
 'version', giving the name of the server version, 'platform', giving the name
 of the operating system, 'copyright', if appropriate, and 'information', giving
 other general information.

 Client connects to server and inspects the server properties. It checks for
 the presence of the required fields.")
  (mechanisms
   :initform (field-type-initform mechanisms amqp-1-1-0-9-0:longstr)
   :type amqp-1-1-0-9-0:longstr
   :documentation "available security mechanisms

 A list of the security mechanisms that the server supports, delimited by spaces.")
  (locales
   :initform (field-type-initform locales amqp-1-1-0-9-0:longstr)
   :type amqp-1-1-0-9-0:longstr
   :documentation "available message locales

 A list of the message locales that the server supports, delimited by spaces. The
 locale defines the language in which the server will send reply texts.

 The server MUST support at least the en_US locale.

 Client connects to server and inspects the locales field. It checks for
 the presence of the required locale(s)."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:connection amqp:start-ok) (amqp:start-ok amqp-1-1-0-9-0:method)
  ((id :initform 11))
  ((client-properties
   :initform (field-type-initform client-properties amqp-1-1-0-9-0:peer-properties)
   :type amqp-1-1-0-9-0:peer-properties
   :documentation "client properties

 The properties SHOULD contain at least these fields: 'product', giving the name
 of the client product, 'version', giving the name of the client version, 'platform',
 giving the name of the operating system, 'copyright', if appropriate, and
 'information', giving other general information.")
  (mechanism
   :initform (field-type-initform mechanism amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :documentation "selected security mechanism

 A single security mechanisms selected by the client, which must be one of those
 specified by the server.

 The client SHOULD authenticate using the highest-level security profile it
 can handle from the list provided by the server.

 If the mechanism field does not contain one of the security mechanisms
 proposed by the server in the Start method, the server MUST close the
 connection without sending any further data.

 Client connects to server and sends an invalid security mechanism. The
 server must respond by closing the connection (a socket close, with no
 connection close negotiation).")
  (response
   :initform (field-type-initform response amqp-1-1-0-9-0:longstr)
   :type amqp-1-1-0-9-0:longstr
   :documentation "security response data

 A block of opaque data passed to the security mechanism. The contents of this
 data are defined by the SASL security mechanism.")
  (locale
   :initform (field-type-initform locale amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :documentation "selected message locale

 A single message locale selected by the client, which must be one of those
 specified by the server."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:connection amqp:secure) (amqp:secure amqp-1-1-0-9-0:method)
  ((id :initform 20))
  ((challenge
   :initform (field-type-initform challenge amqp-1-1-0-9-0:longstr)
   :type amqp-1-1-0-9-0:longstr
   :documentation "security challenge data

 Challenge information, a block of opaque binary data passed to the security
 mechanism."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:connection amqp:secure-ok) (amqp:secure-ok amqp-1-1-0-9-0:method)
  ((id :initform 21))
  ((response
   :initform (field-type-initform response amqp-1-1-0-9-0:longstr)
   :type amqp-1-1-0-9-0:longstr
   :documentation "security response data

 A block of opaque data passed to the security mechanism. The contents of this
 data are defined by the SASL security mechanism."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:connection amqp:tune) (amqp:tune amqp-1-1-0-9-0:method)
  ((id :initform 30))
  ((channel-max
   :initform (field-type-initform channel-max amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :documentation "proposed maximum channels

 Specifies highest channel number that the server permits. Usable channel numbers
 are in the range 1..channel-max. Zero indicates no specified limit.")
  (frame-max
   :initform (field-type-initform frame-max amqp-1-1-0-9-0:long)
   :type amqp-1-1-0-9-0:long
   :documentation "proposed maximum frame size

 The largest frame size that the server proposes for the connection, including
 frame header and end-byte. The client can negotiate a lower value. Zero means
 that the server does not impose any specific limit but may reject very large
 frames if it cannot allocate resources for them.

 Until the frame-max has been negotiated, both peers MUST accept frames of up
 to frame-min-size octets large, and the minimum negotiated value for frame-max
 is also frame-min-size.

 Client connects to server and sends a large properties field, creating a frame
 of frame-min-size octets. The server must accept this frame.")
  (heartbeat
   :initform (field-type-initform heartbeat amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :documentation "desired heartbeat delay

 The delay, in seconds, of the connection heartbeat that the server wants.
 Zero means the server does not want a heartbeat."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:connection amqp:tune-ok) (amqp:tune-ok amqp-1-1-0-9-0:method)
  ((id :initform 31))
  ((channel-max
   :initform (field-type-initform channel-max amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :documentation "negotiated maximum channels

 The maximum total number of channels that the client will use per connection.

 If the client specifies a channel max that is higher than the value provided
 by the server, the server MUST close the connection without attempting a
 negotiated close. The server may report the error in some fashion to assist
 implementors.")
  (frame-max
   :initform (field-type-initform frame-max amqp-1-1-0-9-0:long)
   :type amqp-1-1-0-9-0:long
   :documentation "negotiated maximum frame size

 The largest frame size that the client and server will use for the connection.
 Zero means that the client does not impose any specific limit but may reject
 very large frames if it cannot allocate resources for them. Note that the
 frame-max limit applies principally to content frames, where large contents can
 be broken into frames of arbitrary size.

 Until the frame-max has been negotiated, both peers MUST accept frames of up
 to frame-min-size octets large, and the minimum negotiated value for frame-max
 is also frame-min-size.

 If the client specifies a frame max that is higher than the value provided
 by the server, the server MUST close the connection without attempting a
 negotiated close. The server may report the error in some fashion to assist
 implementors.")
  (heartbeat
   :initform (field-type-initform heartbeat amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :documentation "desired heartbeat delay

 The delay, in seconds, of the connection heartbeat that the client wants. Zero
 means the client does not want a heartbeat."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:connection amqp:open) (amqp:open amqp-1-1-0-9-0:method)
  ((id :initform 40))
  ((virtual-host
   :initform (field-type-initform virtual-host amqp-1-1-0-9-0:path)
   :type amqp-1-1-0-9-0:path
   :documentation "virtual host name

 The name of the virtual host to work with.

 If the server supports multiple virtual hosts, it MUST enforce a full
 separation of exchanges, queues, and all associated entities per virtual
 host. An application, connected to a specific virtual host, MUST NOT be able
 to access resources of another virtual host.

 The server SHOULD verify that the client has permission to access the
 specified virtual host.")
  (reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :allocation :class)
  (reserved-2
   :initform (field-type-initform reserved-2 amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :allocation :class))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:connection amqp:open-ok) (amqp:open-ok amqp-1-1-0-9-0:method)
  ((id :initform 41))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :allocation :class))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:connection amqp:close) (amqp:close amqp-1-1-0-9-0:method)
  ((id :initform 50))
  ((reply-code
   :initform (field-type-initform reply-code amqp-1-1-0-9-0:reply-code)
   :type amqp-1-1-0-9-0:reply-code)
  (reply-text
   :initform (field-type-initform reply-text amqp-1-1-0-9-0:reply-text)
   :type amqp-1-1-0-9-0:reply-text)
  (class-id
   :initform (field-type-initform class-id amqp-1-1-0-9-0:class-id)
   :type amqp-1-1-0-9-0:class-id
   :documentation "failing method class

 When the close is provoked by a method exception, this is the class of the
 method.")
  (method-id
   :initform (field-type-initform method-id amqp-1-1-0-9-0:method-id)
   :type amqp-1-1-0-9-0:method-id
   :documentation "failing method ID

 When the close is provoked by a method exception, this is the ID of the method."))
  (:documentation "roles: client MUST; server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:connection amqp:close-ok) (amqp:close-ok amqp-1-1-0-9-0:method)
  ((id :initform 51))
  ()
  (:documentation "roles: client MUST; server MUST."))

;;; class: channel [id method-names]
;;;   channel.open [reserved-1]
;;;   channel.open-ok [reserved-1]
;;;   channel.flow [active]
;;;   channel.flow-ok [active]
;;;   channel.close [reply-code reply-text class-id method-id]
;;;   channel.close-ok

(def-amqp-class amqp-1-1-0-9-0:channel (amqp-1-1-0-9-0:object amqp:channel)
  ((id :initform 20 :allocation :class)
   (method-names :initform '(amqp:open amqp:open-ok amqp:flow amqp:flow-ok amqp:close amqp:close-ok) :allocation
    :class))
  ()
  ((reserved-1 :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:longstr) :type amqp-1-1-0-9-0:longstr
    :allocation :class)
   (active :initform (field-type-initform active amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "current flow setting

 Confirms the setting of the processed flow method: 1 means the peer will start
 sending or continue to send content frames; 0 means it will not.")
   (reply-code :initform (field-type-initform reply-code amqp-1-1-0-9-0:reply-code) :type amqp-1-1-0-9-0:reply-code)
   (reply-text :initform (field-type-initform reply-text amqp-1-1-0-9-0:reply-text) :type amqp-1-1-0-9-0:reply-text)
   (class-id :initform (field-type-initform class-id amqp-1-1-0-9-0:class-id) :type amqp-1-1-0-9-0:class-id
    :documentation "failing method class

 When the close is provoked by a method exception, this is the class of the
 method.")
   (method-id :initform (field-type-initform method-id amqp-1-1-0-9-0:method-id) :type amqp-1-1-0-9-0:method-id
    :documentation "failing method ID

 When the close is provoked by a method exception, this is the ID of the method."))
  (:documentation "roles: server MUST; client MUST.

      The channel class provides methods for a client to establish a channel to a
      server and for both peers to operate the channel thereafter.
    
      channel             = open-channel *use-channel close-channel
      open-channel        = C:OPEN S:OPEN-OK
      use-channel         = C:FLOW S:FLOW-OK
                          / S:FLOW C:FLOW-OK
                          / functional-class
      close-channel       = C:CLOSE S:CLOSE-OK
                          / S:CLOSE C:CLOSE-OK
    "))


(def-amqp-method (amqp-1-1-0-9-0:channel amqp:open) (amqp:open amqp-1-1-0-9-0:method)
  ((id :initform 10))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :allocation :class))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:channel amqp:open-ok) (amqp:open-ok amqp-1-1-0-9-0:method)
  ((id :initform 11))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:longstr)
   :type amqp-1-1-0-9-0:longstr
   :allocation :class))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:channel amqp:flow) (amqp:flow amqp-1-1-0-9-0:method)
  ((id :initform 20))
  ((active
   :initform (field-type-initform active amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "start/stop content frames

 If 1, the peer starts sending content frames. If 0, the peer stops sending
 content frames."))
  (:documentation "roles: server MUST; client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:channel amqp:flow-ok) (amqp:flow-ok amqp-1-1-0-9-0:method)
  ((id :initform 21))
  ((active
   :initform (field-type-initform active amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "current flow setting

 Confirms the setting of the processed flow method: 1 means the peer will start
 sending or continue to send content frames; 0 means it will not."))
  (:documentation "roles: server MUST; client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:channel amqp:close) (amqp:close amqp-1-1-0-9-0:method)
  ((id :initform 40))
  ((reply-code
   :initform (field-type-initform reply-code amqp-1-1-0-9-0:reply-code)
   :type amqp-1-1-0-9-0:reply-code)
  (reply-text
   :initform (field-type-initform reply-text amqp-1-1-0-9-0:reply-text)
   :type amqp-1-1-0-9-0:reply-text)
  (class-id
   :initform (field-type-initform class-id amqp-1-1-0-9-0:class-id)
   :type amqp-1-1-0-9-0:class-id
   :documentation "failing method class

 When the close is provoked by a method exception, this is the class of the
 method.")
  (method-id
   :initform (field-type-initform method-id amqp-1-1-0-9-0:method-id)
   :type amqp-1-1-0-9-0:method-id
   :documentation "failing method ID

 When the close is provoked by a method exception, this is the ID of the method."))
  (:documentation "roles: client MUST; server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:channel amqp:close-ok) (amqp:close-ok amqp-1-1-0-9-0:method)
  ((id :initform 41))
  ()
  (:documentation "roles: client MUST; server MUST."))

;;; class: exchange [id method-names]
;;;   exchange.declare [reserved-1 exchange type passive durable reserved-2 reserved-3 no-wait arguments]
;;;   exchange.declare-ok
;;;   exchange.delete [reserved-1 exchange if-unused no-wait]
;;;   exchange.delete-ok

(def-amqp-class amqp-1-1-0-9-0:exchange (amqp-1-1-0-9-0:object amqp:exchange)
  ((id :initform 40 :allocation :class)
   (method-names :initform '(amqp:declare amqp:declare-ok amqp:delete amqp:delete-ok) :allocation :class))
  ()
  ((type :initform (field-type-initform type amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr :documentation
    "exchange type

 Each exchange belongs to one of a set of exchange types implemented by the
 server. The exchange types define the functionality of the exchange - i.e. how
 messages are routed through it. It is not valid or meaningful to attempt to
 change the type of an existing exchange.

 Exchanges cannot be redeclared with different types. The client MUST not
 attempt to redeclare an existing exchange with a different type than used
 in the original Exchange.Declare method.

 TODO.

 The client MUST NOT attempt to declare an exchange with a type that the
 server does not support.

 TODO.")
   (passive :initform (field-type-initform passive amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "do not create exchange

 If set, the server will reply with Declare-Ok if the exchange already
 exists with the same name, and raise an error if not. The client can
 use this to check whether an exchange exists without modifying the
 server state. When set, all other method fields except name and no-wait
 are ignored. A declare with both passive and no-wait has no effect.
 Arguments are compared for semantic equivalence.

 If set, and the exchange does not already exist, the server MUST
 raise a channel exception with reply code 404 (not found).

 TODO.

 If not set and the exchange exists, the server MUST check that the
 existing exchange has the same values for type, durable, and arguments
 fields. The server MUST respond with Declare-Ok if the requested
 exchange matches these fields, and MUST raise a channel exception if
 not.

 TODO.")
   (durable :initform (field-type-initform durable amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "request a durable exchange

 If set when creating a new exchange, the exchange will be marked as durable.
 Durable exchanges remain active when a server restarts. Non-durable exchanges
 (transient exchanges) are purged if/when a server restarts.

 The server MUST support both durable and transient exchanges.

 TODO.")
   (reserved-2 :initform (field-type-initform reserved-2 amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :allocation
    :class)
   (reserved-3 :initform (field-type-initform reserved-3 amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :allocation
    :class)
   (arguments :initform (field-type-initform arguments amqp-1-1-0-9-0:table) :type amqp-1-1-0-9-0:table :documentation
    "arguments for declaration

 A set of arguments for the declaration. The syntax and semantics of these
 arguments depends on the server implementation.")
   (reserved-1 :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:short) :type amqp-1-1-0-9-0:short :allocation
    :class)
   (exchange :initform (field-type-initform exchange amqp-1-1-0-9-0:exchange-name) :type amqp-1-1-0-9-0:exchange-name
    :documentation "The client MUST NOT attempt to delete an exchange that does not exist.")
   (if-unused :initform (field-type-initform if-unused amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "delete only if unused

 If set, the server will only delete the exchange if it has no queue bindings. If
 the exchange has queue bindings the server does not delete it but raises a
 channel exception instead.

 The server MUST NOT delete an exchange that has bindings on it, if the if-unused
 field is true.

 The client declares an exchange, binds a queue to it, then tries to delete it
 setting if-unused to true.")
   (no-wait :initform (field-type-initform no-wait amqp-1-1-0-9-0:no-wait) :type amqp-1-1-0-9-0:no-wait))
  (:documentation "roles: server MUST; client MUST.

      Exchanges match and distribute messages across queues. Exchanges can be configured in
      the server or declared at runtime.
    
      exchange            = C:DECLARE  S:DECLARE-OK
                          / C:DELETE   S:DELETE-OK
    "))


(def-amqp-method (amqp-1-1-0-9-0:exchange amqp:declare) (amqp:declare amqp-1-1-0-9-0:method)
  ((id :initform 10))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :allocation :class)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-9-0:exchange-name)
   :type amqp-1-1-0-9-0:exchange-name
   :documentation "Exchange names starting with 'amq.' are reserved for pre-declared and
 standardised exchanges. The client MAY declare an exchange starting with
 'amq.' if the passive option is set, or the exchange already exists.

 The client attempts to declare a non-existing exchange starting with
 'amq.' and with the passive option set to zero.

 The exchange name consists of a non-empty sequence of these characters:
 letters, digits, hyphen, underscore, period, or colon.

 The client attempts to declare an exchange with an illegal name.")
  (type
   :initform (field-type-initform type amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :documentation "exchange type

 Each exchange belongs to one of a set of exchange types implemented by the
 server. The exchange types define the functionality of the exchange - i.e. how
 messages are routed through it. It is not valid or meaningful to attempt to
 change the type of an existing exchange.

 Exchanges cannot be redeclared with different types. The client MUST not
 attempt to redeclare an existing exchange with a different type than used
 in the original Exchange.Declare method.

 TODO.

 The client MUST NOT attempt to declare an exchange with a type that the
 server does not support.

 TODO.")
  (passive
   :initform (field-type-initform passive amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "do not create exchange

 If set, the server will reply with Declare-Ok if the exchange already
 exists with the same name, and raise an error if not. The client can
 use this to check whether an exchange exists without modifying the
 server state. When set, all other method fields except name and no-wait
 are ignored. A declare with both passive and no-wait has no effect.
 Arguments are compared for semantic equivalence.

 If set, and the exchange does not already exist, the server MUST
 raise a channel exception with reply code 404 (not found).

 TODO.

 If not set and the exchange exists, the server MUST check that the
 existing exchange has the same values for type, durable, and arguments
 fields. The server MUST respond with Declare-Ok if the requested
 exchange matches these fields, and MUST raise a channel exception if
 not.

 TODO.")
  (durable
   :initform (field-type-initform durable amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "request a durable exchange

 If set when creating a new exchange, the exchange will be marked as durable.
 Durable exchanges remain active when a server restarts. Non-durable exchanges
 (transient exchanges) are purged if/when a server restarts.

 The server MUST support both durable and transient exchanges.

 TODO.")
  (reserved-2
   :initform (field-type-initform reserved-2 amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :allocation :class)
  (reserved-3
   :initform (field-type-initform reserved-3 amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :allocation :class)
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-9-0:no-wait)
   :type amqp-1-1-0-9-0:no-wait)
  (arguments
   :initform (field-type-initform arguments amqp-1-1-0-9-0:table)
   :type amqp-1-1-0-9-0:table
   :documentation "arguments for declaration

 A set of arguments for the declaration. The syntax and semantics of these
 arguments depends on the server implementation."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:exchange amqp:declare-ok) (amqp:declare-ok amqp-1-1-0-9-0:method)
  ((id :initform 11))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:exchange amqp:delete) (amqp:delete amqp-1-1-0-9-0:method)
  ((id :initform 20))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :allocation :class)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-9-0:exchange-name)
   :type amqp-1-1-0-9-0:exchange-name
   :documentation "The client MUST NOT attempt to delete an exchange that does not exist.")
  (if-unused
   :initform (field-type-initform if-unused amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "delete only if unused

 If set, the server will only delete the exchange if it has no queue bindings. If
 the exchange has queue bindings the server does not delete it but raises a
 channel exception instead.

 The server MUST NOT delete an exchange that has bindings on it, if the if-unused
 field is true.

 The client declares an exchange, binds a queue to it, then tries to delete it
 setting if-unused to true.")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-9-0:no-wait)
   :type amqp-1-1-0-9-0:no-wait))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:exchange amqp:delete-ok) (amqp:delete-ok amqp-1-1-0-9-0:method)
  ((id :initform 21))
  ()
  (:documentation "roles: client MUST."))

;;; class: queue [id method-names]
;;;   queue.declare [reserved-1 queue passive durable exclusive auto-delete no-wait arguments]
;;;   queue.declare-ok [queue message-count consumer-count]
;;;   queue.bind [reserved-1 queue exchange routing-key no-wait arguments]
;;;   queue.bind-ok
;;;   queue.unbind [reserved-1 queue exchange routing-key arguments]
;;;   queue.unbind-ok
;;;   queue.purge [reserved-1 queue no-wait]
;;;   queue.purge-ok [message-count]
;;;   queue.delete [reserved-1 queue if-unused if-empty no-wait]
;;;   queue.delete-ok [message-count]

(def-amqp-class amqp-1-1-0-9-0:queue (amqp-1-1-0-9-0:object amqp:queue)
  ((id :initform 50 :allocation :class)
   (method-names :initform
    '(amqp:declare amqp:declare-ok amqp:bind amqp:bind-ok amqp:unbind amqp:unbind-ok amqp:purge amqp:purge-ok
      amqp:delete amqp:delete-ok)
    :allocation :class))
  ()
  ((passive :initform (field-type-initform passive amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "do not create queue

 If set, the server will reply with Declare-Ok if the queue already
 exists with the same name, and raise an error if not. The client can
 use this to check whether a queue exists without modifying the
 server state. When set, all other method fields except name and no-wait
 are ignored. A declare with both passive and no-wait has no effect.
 Arguments are compared for semantic equivalence.

 The client MAY ask the server to assert that a queue exists without
 creating the queue if not. If the queue does not exist, the server
 treats this as a failure.

 Client declares an existing queue with the passive option and expects
 the server to respond with a declare-ok. Client then attempts to declare
 a non-existent queue with the passive option, and the server must close
 the channel with the correct reply-code.

 If not set and the queue exists, the server MUST check that the
 existing queue has the same values for durable, exclusive, auto-delete,
 and arguments fields. The server MUST respond with Declare-Ok if the
 requested queue matches these fields, and MUST raise a channel exception
 if not.

 TODO.")
   (durable :initform (field-type-initform durable amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "request a durable queue

 If set when creating a new queue, the queue will be marked as durable. Durable
 queues remain active when a server restarts. Non-durable queues (transient
 queues) are purged if/when a server restarts. Note that durable queues do not
 necessarily hold persistent messages, although it does not make sense to send
 persistent messages to a transient queue.

 The server MUST recreate the durable queue after a restart.

 Client declares a durable queue. The server is then restarted. The client
 then attempts to send a message to the queue. The message should be successfully
 delivered.

 The server MUST support both durable and transient queues.

 A client declares two named queues, one durable and one transient.")
   (exclusive :initform (field-type-initform exclusive amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "request an exclusive queue

 Exclusive queues may only be accessed by the current connection, and are
 deleted when that connection closes. Passive declaration of an exclusive
 queue by other connections are not allowed.

 The server MUST support both exclusive (private) and non-exclusive (shared)
 queues.

 A client declares two named queues, one exclusive and one non-exclusive.

 The client MAY NOT attempt to use a queue that was declared as exclusive
 by another still-open connection.

 One client declares an exclusive queue. A second client on a different
 connection attempts to declare, bind, consume, purge, delete, or declare
 a queue of the same name.")
   (auto-delete :initform (field-type-initform auto-delete amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "auto-delete queue when unused

 If set, the queue is deleted when all consumers have finished using it. The last
 consumer can be cancelled either explicitly or because its channel is closed. If
 there was no consumer ever on the queue, it won't be deleted. Applications can
 explicitly delete auto-delete queues using the Delete method as normal.

 The server MUST ignore the auto-delete field if the queue already exists.

 Client declares two named queues, one as auto-delete and one explicit-delete.
 Client then attempts to declare the two queues using the same names again,
 but reversing the value of the auto-delete field in each case. Verify that the
 queues still exist with the original auto-delete flag values.")
   (consumer-count :initform (field-type-initform consumer-count amqp-1-1-0-9-0:long) :type amqp-1-1-0-9-0:long
    :documentation "number of consumers

 Reports the number of active consumers for the queue. Note that consumers can
 suspend activity (Channel.Flow) in which case they do not appear in this count.")
   (exchange :initform (field-type-initform exchange amqp-1-1-0-9-0:exchange-name) :type amqp-1-1-0-9-0:exchange-name
    :documentation "The name of the exchange to unbind from.

 The client MUST NOT attempt to unbind a queue from an exchange that
 does not exist.

 The client attempts to unbind a queue from a non-existent exchange.

 The server MUST accept a blank exchange name to mean the default exchange.

 The client declares a queue and binds it to a blank exchange name.")
   (routing-key :initform (field-type-initform routing-key amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr
    :documentation "routing key of binding

 Specifies the routing key of the binding to unbind.")
   (arguments :initform (field-type-initform arguments amqp-1-1-0-9-0:table) :type amqp-1-1-0-9-0:table :documentation
    "arguments of binding

 Specifies the arguments of the binding to unbind.")
   (reserved-1 :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:short) :type amqp-1-1-0-9-0:short :allocation
    :class)
   (queue :initform (field-type-initform queue amqp-1-1-0-9-0:queue-name) :type amqp-1-1-0-9-0:queue-name
    :documentation "Specifies the name of the queue to delete.

 The client MUST either specify a queue name or have previously declared a
 queue on the same channel

 The client opens a channel and attempts to delete an unnamed queue.

 The client MUST NOT attempt to delete a queue that does not exist.

 The client attempts to delete a non-existent queue.")
   (if-unused :initform (field-type-initform if-unused amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "delete only if unused

 If set, the server will only delete the queue if it has no consumers. If the
 queue has consumers the server does does not delete it but raises a channel
 exception instead.

 The server MUST NOT delete a queue that has consumers on it, if the if-unused
 field is true.

 The client declares a queue, and consumes from it, then tries to delete it
 setting if-unused to true.")
   (if-empty :initform (field-type-initform if-empty amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "delete only if empty

 If set, the server will only delete the queue if it has no messages.

 The server MUST NOT delete a queue that has messages on it, if the
 if-empty field is true.

 The client declares a queue, binds it and publishes some messages into it,
 then tries to delete it setting if-empty to true.")
   (no-wait :initform (field-type-initform no-wait amqp-1-1-0-9-0:no-wait) :type amqp-1-1-0-9-0:no-wait)
   (message-count :initform (field-type-initform message-count amqp-1-1-0-9-0:message-count) :type
    amqp-1-1-0-9-0:message-count :documentation "Reports the number of messages deleted."))
  (:documentation "roles: server MUST; client MUST.

      Queues store and forward messages. Queues can be configured in the server or created at
      runtime. Queues must be attached to at least one exchange in order to receive messages
      from publishers.
    
      queue               = C:DECLARE  S:DECLARE-OK
                          / C:BIND     S:BIND-OK
                          / C:UNBIND   S:UNBIND-OK
                          / C:PURGE    S:PURGE-OK
                          / C:DELETE   S:DELETE-OK
    "))


(def-amqp-method (amqp-1-1-0-9-0:queue amqp:declare) (amqp:declare amqp-1-1-0-9-0:method)
  ((id :initform 10))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :allocation :class)
  (queue
   :initform (field-type-initform queue amqp-1-1-0-9-0:queue-name)
   :type amqp-1-1-0-9-0:queue-name
   :documentation "The queue name MAY be empty, in which case the server MUST create a new
 queue with a unique generated name and return this to the client in the
 Declare-Ok method.

 Client attempts to declare several queues with an empty name. The client then
 verifies that the server-assigned names are unique and different.

 Queue names starting with 'amq.' are reserved for pre-declared and
 standardised queues. The client MAY declare a queue starting with
 'amq.' if the passive option is set, or the queue already exists.

 The client attempts to declare a non-existing queue starting with
 'amq.' and with the passive option set to zero.

 The queue name can be empty, or a sequence of these characters:
 letters, digits, hyphen, underscore, period, or colon.

 The client attempts to declare a queue with an illegal name.")
  (passive
   :initform (field-type-initform passive amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "do not create queue

 If set, the server will reply with Declare-Ok if the queue already
 exists with the same name, and raise an error if not. The client can
 use this to check whether a queue exists without modifying the
 server state. When set, all other method fields except name and no-wait
 are ignored. A declare with both passive and no-wait has no effect.
 Arguments are compared for semantic equivalence.

 The client MAY ask the server to assert that a queue exists without
 creating the queue if not. If the queue does not exist, the server
 treats this as a failure.

 Client declares an existing queue with the passive option and expects
 the server to respond with a declare-ok. Client then attempts to declare
 a non-existent queue with the passive option, and the server must close
 the channel with the correct reply-code.

 If not set and the queue exists, the server MUST check that the
 existing queue has the same values for durable, exclusive, auto-delete,
 and arguments fields. The server MUST respond with Declare-Ok if the
 requested queue matches these fields, and MUST raise a channel exception
 if not.

 TODO.")
  (durable
   :initform (field-type-initform durable amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "request a durable queue

 If set when creating a new queue, the queue will be marked as durable. Durable
 queues remain active when a server restarts. Non-durable queues (transient
 queues) are purged if/when a server restarts. Note that durable queues do not
 necessarily hold persistent messages, although it does not make sense to send
 persistent messages to a transient queue.

 The server MUST recreate the durable queue after a restart.

 Client declares a durable queue. The server is then restarted. The client
 then attempts to send a message to the queue. The message should be successfully
 delivered.

 The server MUST support both durable and transient queues.

 A client declares two named queues, one durable and one transient.")
  (exclusive
   :initform (field-type-initform exclusive amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "request an exclusive queue

 Exclusive queues may only be accessed by the current connection, and are
 deleted when that connection closes. Passive declaration of an exclusive
 queue by other connections are not allowed.

 The server MUST support both exclusive (private) and non-exclusive (shared)
 queues.

 A client declares two named queues, one exclusive and one non-exclusive.

 The client MAY NOT attempt to use a queue that was declared as exclusive
 by another still-open connection.

 One client declares an exclusive queue. A second client on a different
 connection attempts to declare, bind, consume, purge, delete, or declare
 a queue of the same name.")
  (auto-delete
   :initform (field-type-initform auto-delete amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "auto-delete queue when unused

 If set, the queue is deleted when all consumers have finished using it. The last
 consumer can be cancelled either explicitly or because its channel is closed. If
 there was no consumer ever on the queue, it won't be deleted. Applications can
 explicitly delete auto-delete queues using the Delete method as normal.

 The server MUST ignore the auto-delete field if the queue already exists.

 Client declares two named queues, one as auto-delete and one explicit-delete.
 Client then attempts to declare the two queues using the same names again,
 but reversing the value of the auto-delete field in each case. Verify that the
 queues still exist with the original auto-delete flag values.")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-9-0:no-wait)
   :type amqp-1-1-0-9-0:no-wait)
  (arguments
   :initform (field-type-initform arguments amqp-1-1-0-9-0:table)
   :type amqp-1-1-0-9-0:table
   :documentation "arguments for declaration

 A set of arguments for the declaration. The syntax and semantics of these
 arguments depends on the server implementation."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:queue amqp:declare-ok) (amqp:declare-ok amqp-1-1-0-9-0:method)
  ((id :initform 11))
  ((queue
   :initform (field-type-initform queue amqp-1-1-0-9-0:queue-name)
   :type amqp-1-1-0-9-0:queue-name
   :documentation "Reports the name of the queue. If the server generated a queue name, this field
 contains that name.")
  (message-count
   :initform (field-type-initform message-count amqp-1-1-0-9-0:message-count)
   :type amqp-1-1-0-9-0:message-count)
  (consumer-count
   :initform (field-type-initform consumer-count amqp-1-1-0-9-0:long)
   :type amqp-1-1-0-9-0:long
   :documentation "number of consumers

 Reports the number of active consumers for the queue. Note that consumers can
 suspend activity (Channel.Flow) in which case they do not appear in this count."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:queue amqp:bind) (amqp:bind amqp-1-1-0-9-0:method)
  ((id :initform 20))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :allocation :class)
  (queue
   :initform (field-type-initform queue amqp-1-1-0-9-0:queue-name)
   :type amqp-1-1-0-9-0:queue-name
   :documentation "Specifies the name of the queue to bind.

 The client MUST either specify a queue name or have previously declared a
 queue on the same channel

 The client opens a channel and attempts to bind an unnamed queue.

 The client MUST NOT attempt to bind a queue that does not exist.

 The client attempts to bind a non-existent queue.")
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-9-0:exchange-name)
   :type amqp-1-1-0-9-0:exchange-name
   :documentation "name of the exchange to bind to

 A client MUST NOT be allowed to bind a queue to a non-existent exchange.

 A client attempts to bind an named queue to a undeclared exchange.

 The server MUST accept a blank exchange name to mean the default exchange.

 The client declares a queue and binds it to a blank exchange name.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :documentation "message routing key

 Specifies the routing key for the binding. The routing key is used for routing
 messages depending on the exchange configuration. Not all exchanges use a
 routing key - refer to the specific exchange documentation. If the queue name
 is empty, the server uses the last queue declared on the channel. If the
 routing key is also empty, the server uses this queue name for the routing
 key as well. If the queue name is provided but the routing key is empty, the
 server does the binding with that empty routing key. The meaning of empty
 routing keys depends on the exchange implementation.

 If a message queue binds to a direct exchange using routing key K and a
 publisher sends the exchange a message with routing key R, then the message
 MUST be passed to the message queue if K = R.")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-9-0:no-wait)
   :type amqp-1-1-0-9-0:no-wait)
  (arguments
   :initform (field-type-initform arguments amqp-1-1-0-9-0:table)
   :type amqp-1-1-0-9-0:table
   :documentation "arguments for binding

 A set of arguments for the binding. The syntax and semantics of these arguments
 depends on the exchange class."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:queue amqp:bind-ok) (amqp:bind-ok amqp-1-1-0-9-0:method)
  ((id :initform 21))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:queue amqp:unbind) (amqp:unbind amqp-1-1-0-9-0:method)
  ((id :initform 50))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :allocation :class)
  (queue
   :initform (field-type-initform queue amqp-1-1-0-9-0:queue-name)
   :type amqp-1-1-0-9-0:queue-name
   :documentation "Specifies the name of the queue to unbind.

 The client MUST either specify a queue name or have previously declared a
 queue on the same channel

 The client opens a channel and attempts to unbind an unnamed queue.

 The client MUST NOT attempt to unbind a queue that does not exist.

 The client attempts to unbind a non-existent queue.")
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-9-0:exchange-name)
   :type amqp-1-1-0-9-0:exchange-name
   :documentation "The name of the exchange to unbind from.

 The client MUST NOT attempt to unbind a queue from an exchange that
 does not exist.

 The client attempts to unbind a queue from a non-existent exchange.

 The server MUST accept a blank exchange name to mean the default exchange.

 The client declares a queue and binds it to a blank exchange name.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :documentation "routing key of binding

 Specifies the routing key of the binding to unbind.")
  (arguments
   :initform (field-type-initform arguments amqp-1-1-0-9-0:table)
   :type amqp-1-1-0-9-0:table
   :documentation "arguments of binding

 Specifies the arguments of the binding to unbind."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:queue amqp:unbind-ok) (amqp:unbind-ok amqp-1-1-0-9-0:method)
  ((id :initform 51))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:queue amqp:purge) (amqp:purge amqp-1-1-0-9-0:method)
  ((id :initform 30))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :allocation :class)
  (queue
   :initform (field-type-initform queue amqp-1-1-0-9-0:queue-name)
   :type amqp-1-1-0-9-0:queue-name
   :documentation "Specifies the name of the queue to purge.

 The client MUST either specify a queue name or have previously declared a
 queue on the same channel

 The client opens a channel and attempts to purge an unnamed queue.

 The client MUST NOT attempt to purge a queue that does not exist.

 The client attempts to purge a non-existent queue.")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-9-0:no-wait)
   :type amqp-1-1-0-9-0:no-wait))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:queue amqp:purge-ok) (amqp:purge-ok amqp-1-1-0-9-0:method)
  ((id :initform 31))
  ((message-count
   :initform (field-type-initform message-count amqp-1-1-0-9-0:message-count)
   :type amqp-1-1-0-9-0:message-count
   :documentation "Reports the number of messages purged."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:queue amqp:delete) (amqp:delete amqp-1-1-0-9-0:method)
  ((id :initform 40))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :allocation :class)
  (queue
   :initform (field-type-initform queue amqp-1-1-0-9-0:queue-name)
   :type amqp-1-1-0-9-0:queue-name
   :documentation "Specifies the name of the queue to delete.

 The client MUST either specify a queue name or have previously declared a
 queue on the same channel

 The client opens a channel and attempts to delete an unnamed queue.

 The client MUST NOT attempt to delete a queue that does not exist.

 The client attempts to delete a non-existent queue.")
  (if-unused
   :initform (field-type-initform if-unused amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "delete only if unused

 If set, the server will only delete the queue if it has no consumers. If the
 queue has consumers the server does does not delete it but raises a channel
 exception instead.

 The server MUST NOT delete a queue that has consumers on it, if the if-unused
 field is true.

 The client declares a queue, and consumes from it, then tries to delete it
 setting if-unused to true.")
  (if-empty
   :initform (field-type-initform if-empty amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "delete only if empty

 If set, the server will only delete the queue if it has no messages.

 The server MUST NOT delete a queue that has messages on it, if the
 if-empty field is true.

 The client declares a queue, binds it and publishes some messages into it,
 then tries to delete it setting if-empty to true.")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-9-0:no-wait)
   :type amqp-1-1-0-9-0:no-wait))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:queue amqp:delete-ok) (amqp:delete-ok amqp-1-1-0-9-0:method)
  ((id :initform 41))
  ((message-count
   :initform (field-type-initform message-count amqp-1-1-0-9-0:message-count)
   :type amqp-1-1-0-9-0:message-count
   :documentation "Reports the number of messages deleted."))
  (:documentation "roles: client MUST."))

;;; class: basic [id method-names content-type content-encoding headers delivery-mode priority correlation-id reply-to expiration message-id timestamp type user-id app-id reserved]
;;;   basic.qos [prefetch-size prefetch-count global]
;;;   basic.qos-ok
;;;   basic.consume [reserved-1 queue consumer-tag no-local no-ack exclusive no-wait arguments]
;;;   basic.consume-ok [consumer-tag]
;;;   basic.cancel [consumer-tag no-wait]
;;;   basic.cancel-ok [consumer-tag]
;;;   basic.publish [reserved-1 exchange routing-key mandatory immediate]
;;;   basic.return [reply-code reply-text exchange routing-key]
;;;   basic.deliver [consumer-tag delivery-tag redelivered exchange routing-key]
;;;   basic.get [reserved-1 queue no-ack]
;;;   basic.get-ok [delivery-tag redelivered exchange routing-key message-count]
;;;   basic.get-empty [reserved-1]
;;;   basic.ack [delivery-tag multiple]
;;;   basic.reject [delivery-tag requeue]
;;;   basic.recover-async [requeue]
;;;   basic.recover [requeue]
;;;   basic.recover-ok

(def-amqp-class amqp-1-1-0-9-0:basic (amqp-1-1-0-9-0:object amqp:basic)
  ((id :initform 60 :allocation :class)
   (method-names :initform
    '(amqp:qos amqp:qos-ok amqp:consume amqp:consume-ok amqp:cancel amqp:cancel-ok amqp:publish amqp:return
      amqp:deliver amqp:get amqp:get-ok amqp:get-empty amqp:ack amqp:reject amqp:recover-async amqp:recover
      amqp:recover-ok)
    :allocation :class))
  ((content-type :initform (field-type-initform content-type amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr
    :documentation "MIME content type")
   (content-encoding
     :initform
     (field-type-initform content-encoding amqp-1-1-0-9-0:shortstr)
     :type
     amqp-1-1-0-9-0:shortstr
     :documentation
     "MIME content encoding")
   (headers :initform (field-type-initform headers amqp-1-1-0-9-0:table) :type amqp-1-1-0-9-0:table :documentation
    "message header field table")
   (delivery-mode :initform (field-type-initform delivery-mode amqp-1-1-0-9-0:octet) :type amqp-1-1-0-9-0:octet
    :documentation "non-persistent (1) or persistent (2)")
   (priority :initform (field-type-initform priority amqp-1-1-0-9-0:octet) :type amqp-1-1-0-9-0:octet :documentation
    "message priority, 0 to 9")
   (correlation-id :initform (field-type-initform correlation-id amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr
    :documentation "application correlation identifier")
   (reply-to :initform (field-type-initform reply-to amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr
    :documentation "address to reply to")
   (expiration :initform (field-type-initform expiration amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr
    :documentation "message expiration specification")
   (message-id :initform (field-type-initform message-id amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr
    :documentation "application message identifier")
   (timestamp :initform (field-type-initform timestamp amqp-1-1-0-9-0:timestamp) :type amqp-1-1-0-9-0:timestamp
    :documentation "message timestamp")
   (type :initform (field-type-initform type amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr :documentation
    "message type name")
   (user-id :initform (field-type-initform user-id amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr
    :documentation "creating user id")
   (app-id :initform (field-type-initform app-id amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr :documentation
    "creating application id")
   (reserved :initform (field-type-initform reserved amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr
    :documentation "reserved, must be empty" :allocation :class))
  ((prefetch-size :initform (field-type-initform prefetch-size amqp-1-1-0-9-0:long) :type amqp-1-1-0-9-0:long
    :documentation "prefetch window in octets

 The client can request that messages be sent in advance so that when the client
 finishes processing a message, the following message is already held locally,
 rather than needing to be sent down the channel. Prefetching gives a performance
 improvement. This field specifies the prefetch window size in octets. The server
 will send a message in advance if it is equal to or smaller in size than the
 available prefetch size (and also falls into other prefetch limits). May be set
 to zero, meaning 'no specific limit', although other prefetch limits may still
 apply. The prefetch-size is ignored if the no-ack option is set.

 The server MUST ignore this setting when the client is not processing any
 messages - i.e. the prefetch size does not limit the transfer of single
 messages to a client, only the sending in advance of more messages while
 the client still has one or more unacknowledged messages.

 Define a QoS prefetch-size limit and send a single message that exceeds
 that limit. Verify that the message arrives correctly.")
   (prefetch-count :initform (field-type-initform prefetch-count amqp-1-1-0-9-0:short) :type amqp-1-1-0-9-0:short
    :documentation "prefetch window in messages

 Specifies a prefetch window in terms of whole messages. This field may be used
 in combination with the prefetch-size field; a message will only be sent in
 advance if both prefetch windows (and those at the channel and connection level)
 allow it. The prefetch-count is ignored if the no-ack option is set.

 The server may send less data in advance than allowed by the client's
 specified prefetch windows but it MUST NOT send more.

 Define a QoS prefetch-size limit and a prefetch-count limit greater than
 one. Send multiple messages that exceed the prefetch size. Verify that
 no more than one message arrives at once.")
   (global :initform (field-type-initform global amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "apply to entire connection

 By default the QoS settings apply to the current channel only. If this field is
 set, they are applied to the entire connection.")
   (no-local :initform (field-type-initform no-local amqp-1-1-0-9-0:no-local) :type amqp-1-1-0-9-0:no-local)
   (exclusive :initform (field-type-initform exclusive amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "request exclusive access

 Request exclusive consumer access, meaning only this consumer can access the
 queue.

 The client MAY NOT gain exclusive access to a queue that already has
 active consumers.

 Open two connections to a server, and in one connection declare a shared
 (non-exclusive) queue and then consume from the queue. In the second
 connection attempt to consume from the same queue using the exclusive
 option.")
   (arguments :initform (field-type-initform arguments amqp-1-1-0-9-0:table) :type amqp-1-1-0-9-0:table :documentation
    "arguments for declaration

 A set of arguments for the consume. The syntax and semantics of these
 arguments depends on the server implementation.")
   (no-wait :initform (field-type-initform no-wait amqp-1-1-0-9-0:no-wait) :type amqp-1-1-0-9-0:no-wait)
   (mandatory :initform (field-type-initform mandatory amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "indicate mandatory routing

 This flag tells the server how to react if the message cannot be routed to a
 queue. If this flag is set, the server will return an unroutable message with a
 Return method. If this flag is zero, the server silently drops the message.

 The server SHOULD implement the mandatory flag.

 TODO.")
   (immediate :initform (field-type-initform immediate amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "request immediate delivery

 This flag tells the server how to react if the message cannot be routed to a
 queue consumer immediately. If this flag is set, the server will return an
 undeliverable message with a Return method. If this flag is zero, the server
 will queue the message, but with no guarantee that it will ever be consumed.

 The server SHOULD implement the immediate flag.

 TODO.")
   (reply-code :initform (field-type-initform reply-code amqp-1-1-0-9-0:reply-code) :type amqp-1-1-0-9-0:reply-code)
   (reply-text :initform (field-type-initform reply-text amqp-1-1-0-9-0:reply-text) :type amqp-1-1-0-9-0:reply-text)
   (consumer-tag :initform (field-type-initform consumer-tag amqp-1-1-0-9-0:consumer-tag) :type
    amqp-1-1-0-9-0:consumer-tag)
   (queue :initform (field-type-initform queue amqp-1-1-0-9-0:queue-name) :type amqp-1-1-0-9-0:queue-name
    :documentation "Specifies the name of the queue to get a message from.")
   (no-ack :initform (field-type-initform no-ack amqp-1-1-0-9-0:no-ack) :type amqp-1-1-0-9-0:no-ack)
   (redelivered :initform (field-type-initform redelivered amqp-1-1-0-9-0:redelivered) :type
    amqp-1-1-0-9-0:redelivered)
   (exchange :initform (field-type-initform exchange amqp-1-1-0-9-0:exchange-name) :type amqp-1-1-0-9-0:exchange-name
    :documentation "Specifies the name of the exchange that the message was originally published to.
 If empty, the message was published to the default exchange.")
   (routing-key :initform (field-type-initform routing-key amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr
    :documentation "Message routing key

 Specifies the routing key name specified when the message was published.")
   (message-count :initform (field-type-initform message-count amqp-1-1-0-9-0:message-count) :type
    amqp-1-1-0-9-0:message-count)
   (reserved-1 :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:shortstr) :type amqp-1-1-0-9-0:shortstr
    :allocation :class)
   (multiple :initform (field-type-initform multiple amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "acknowledge multiple messages

 If set to 1, the delivery tag is treated as 'up to and including', so that the
 client can acknowledge multiple messages with a single method. If set to zero,
 the delivery tag refers to a single message. If the multiple field is 1, and the
 delivery tag is zero, tells the server to acknowledge all outstanding messages.

 The server MUST validate that a non-zero delivery-tag refers to a delivered
 message, and raise a channel exception if this is not the case. On a transacted
 channel, this check MUST be done immediately and not delayed until a Tx.Commit.
 Specifically, a client MUST not acknowledge the same message more than once.

 TODO.")
   (delivery-tag :initform (field-type-initform delivery-tag amqp-1-1-0-9-0:delivery-tag) :type
    amqp-1-1-0-9-0:delivery-tag)
   (requeue :initform (field-type-initform requeue amqp-1-1-0-9-0:bit) :type amqp-1-1-0-9-0:bit :documentation
    "requeue the message

 If this field is zero, the message will be redelivered to the original
 recipient. If this bit is 1, the server will attempt to requeue the message,
 potentially then delivering it to an alternative subscriber."))
  (:documentation "roles: server MUST; client MAY.

      The Basic class provides methods that support an industry-standard messaging model.
    
      basic               = C:QOS S:QOS-OK
                          / C:CONSUME S:CONSUME-OK
                          / C:CANCEL S:CANCEL-OK
                          / C:PUBLISH content
                          / S:RETURN content
                          / S:DELIVER content
                          / C:GET ( S:GET-OK content / S:GET-EMPTY )
                          / C:ACK
                          / C:REJECT
                          / C:RECOVER-ASYNC
                          / C:RECOVER S:RECOVER-OK
    "))


(def-amqp-method (amqp-1-1-0-9-0:basic amqp:qos) (amqp:qos amqp-1-1-0-9-0:method)
  ((id :initform 10))
  ((prefetch-size
   :initform (field-type-initform prefetch-size amqp-1-1-0-9-0:long)
   :type amqp-1-1-0-9-0:long
   :documentation "prefetch window in octets

 The client can request that messages be sent in advance so that when the client
 finishes processing a message, the following message is already held locally,
 rather than needing to be sent down the channel. Prefetching gives a performance
 improvement. This field specifies the prefetch window size in octets. The server
 will send a message in advance if it is equal to or smaller in size than the
 available prefetch size (and also falls into other prefetch limits). May be set
 to zero, meaning 'no specific limit', although other prefetch limits may still
 apply. The prefetch-size is ignored if the no-ack option is set.

 The server MUST ignore this setting when the client is not processing any
 messages - i.e. the prefetch size does not limit the transfer of single
 messages to a client, only the sending in advance of more messages while
 the client still has one or more unacknowledged messages.

 Define a QoS prefetch-size limit and send a single message that exceeds
 that limit. Verify that the message arrives correctly.")
  (prefetch-count
   :initform (field-type-initform prefetch-count amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :documentation "prefetch window in messages

 Specifies a prefetch window in terms of whole messages. This field may be used
 in combination with the prefetch-size field; a message will only be sent in
 advance if both prefetch windows (and those at the channel and connection level)
 allow it. The prefetch-count is ignored if the no-ack option is set.

 The server may send less data in advance than allowed by the client's
 specified prefetch windows but it MUST NOT send more.

 Define a QoS prefetch-size limit and a prefetch-count limit greater than
 one. Send multiple messages that exceed the prefetch size. Verify that
 no more than one message arrives at once.")
  (global
   :initform (field-type-initform global amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "apply to entire connection

 By default the QoS settings apply to the current channel only. If this field is
 set, they are applied to the entire connection."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:qos-ok) (amqp:qos-ok amqp-1-1-0-9-0:method)
  ((id :initform 11))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:consume) (amqp:consume amqp-1-1-0-9-0:method)
  ((id :initform 20))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :allocation :class)
  (queue
   :initform (field-type-initform queue amqp-1-1-0-9-0:queue-name)
   :type amqp-1-1-0-9-0:queue-name
   :documentation "Specifies the name of the queue to consume from.")
  (consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-9-0:consumer-tag)
   :type amqp-1-1-0-9-0:consumer-tag
   :documentation "Specifies the identifier for the consumer. The consumer tag is local to a
 channel, so two clients can use the same consumer tags. If this field is
 empty the server will generate a unique tag.

 The client MUST NOT specify a tag that refers to an existing consumer.

 Attempt to create two consumers with the same non-empty tag, on the
 same channel.

 The consumer tag is valid only within the channel from which the
 consumer was created. I.e. a client MUST NOT create a consumer in one
 channel and then use it in another.

 Attempt to create a consumer in one channel, then use in another channel,
 in which consumers have also been created (to test that the server uses
 unique consumer tags).")
  (no-local
   :initform (field-type-initform no-local amqp-1-1-0-9-0:no-local)
   :type amqp-1-1-0-9-0:no-local)
  (no-ack
   :initform (field-type-initform no-ack amqp-1-1-0-9-0:no-ack)
   :type amqp-1-1-0-9-0:no-ack)
  (exclusive
   :initform (field-type-initform exclusive amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "request exclusive access

 Request exclusive consumer access, meaning only this consumer can access the
 queue.

 The client MAY NOT gain exclusive access to a queue that already has
 active consumers.

 Open two connections to a server, and in one connection declare a shared
 (non-exclusive) queue and then consume from the queue. In the second
 connection attempt to consume from the same queue using the exclusive
 option.")
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-9-0:no-wait)
   :type amqp-1-1-0-9-0:no-wait)
  (arguments
   :initform (field-type-initform arguments amqp-1-1-0-9-0:table)
   :type amqp-1-1-0-9-0:table
   :documentation "arguments for declaration

 A set of arguments for the consume. The syntax and semantics of these
 arguments depends on the server implementation."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:consume-ok) (amqp:consume-ok amqp-1-1-0-9-0:method)
  ((id :initform 21))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-9-0:consumer-tag)
   :type amqp-1-1-0-9-0:consumer-tag
   :documentation "Holds the consumer tag specified by the client or provided by the server."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:cancel) (amqp:cancel amqp-1-1-0-9-0:method)
  ((id :initform 30))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-9-0:consumer-tag)
   :type amqp-1-1-0-9-0:consumer-tag)
  (no-wait
   :initform (field-type-initform no-wait amqp-1-1-0-9-0:no-wait)
   :type amqp-1-1-0-9-0:no-wait))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:cancel-ok) (amqp:cancel-ok amqp-1-1-0-9-0:method)
  ((id :initform 31))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-9-0:consumer-tag)
   :type amqp-1-1-0-9-0:consumer-tag))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:publish) (amqp:publish amqp-1-1-0-9-0:method)
  ((id :initform 40))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :allocation :class)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-9-0:exchange-name)
   :type amqp-1-1-0-9-0:exchange-name
   :documentation "Specifies the name of the exchange to publish to. The exchange name can be
 empty, meaning the default exchange. If the exchange name is specified, and that
 exchange does not exist, the server will raise a channel exception.

 The client MUST NOT attempt to publish a content to an exchange that
 does not exist.

 The client attempts to publish a content to a non-existent exchange.

 The server MUST accept a blank exchange name to mean the default exchange.

 The client declares a queue and binds it to a blank exchange name.

 If the exchange was declared as an internal exchange, the server MUST raise
 a channel exception with a reply code 403 (access refused).

 TODO.

 The exchange MAY refuse basic content in which case it MUST raise a channel
 exception with reply code 540 (not implemented).

 TODO.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :documentation "Message routing key

 Specifies the routing key for the message. The routing key is used for routing
 messages depending on the exchange configuration.")
  (mandatory
   :initform (field-type-initform mandatory amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "indicate mandatory routing

 This flag tells the server how to react if the message cannot be routed to a
 queue. If this flag is set, the server will return an unroutable message with a
 Return method. If this flag is zero, the server silently drops the message.

 The server SHOULD implement the mandatory flag.

 TODO.")
  (immediate
   :initform (field-type-initform immediate amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "request immediate delivery

 This flag tells the server how to react if the message cannot be routed to a
 queue consumer immediately. If this flag is set, the server will return an
 undeliverable message with a Return method. If this flag is zero, the server
 will queue the message, but with no guarantee that it will ever be consumed.

 The server SHOULD implement the immediate flag.

 TODO."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:return) (amqp:return amqp-1-1-0-9-0:method)
  ((id :initform 50))
  ((reply-code
   :initform (field-type-initform reply-code amqp-1-1-0-9-0:reply-code)
   :type amqp-1-1-0-9-0:reply-code)
  (reply-text
   :initform (field-type-initform reply-text amqp-1-1-0-9-0:reply-text)
   :type amqp-1-1-0-9-0:reply-text)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-9-0:exchange-name)
   :type amqp-1-1-0-9-0:exchange-name
   :documentation "Specifies the name of the exchange that the message was originally published
 to. May be empty, meaning the default exchange.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :documentation "Message routing key

 Specifies the routing key name specified when the message was published."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:deliver) (amqp:deliver amqp-1-1-0-9-0:method)
  ((id :initform 60))
  ((consumer-tag
   :initform (field-type-initform consumer-tag amqp-1-1-0-9-0:consumer-tag)
   :type amqp-1-1-0-9-0:consumer-tag)
  (delivery-tag
   :initform (field-type-initform delivery-tag amqp-1-1-0-9-0:delivery-tag)
   :type amqp-1-1-0-9-0:delivery-tag)
  (redelivered
   :initform (field-type-initform redelivered amqp-1-1-0-9-0:redelivered)
   :type amqp-1-1-0-9-0:redelivered)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-9-0:exchange-name)
   :type amqp-1-1-0-9-0:exchange-name
   :documentation "Specifies the name of the exchange that the message was originally published to.
 May be empty, indicating the default exchange.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :documentation "Message routing key

 Specifies the routing key name specified when the message was published."))
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:get) (amqp:get amqp-1-1-0-9-0:method)
  ((id :initform 70))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:short)
   :type amqp-1-1-0-9-0:short
   :allocation :class)
  (queue
   :initform (field-type-initform queue amqp-1-1-0-9-0:queue-name)
   :type amqp-1-1-0-9-0:queue-name
   :documentation "Specifies the name of the queue to get a message from.")
  (no-ack
   :initform (field-type-initform no-ack amqp-1-1-0-9-0:no-ack)
   :type amqp-1-1-0-9-0:no-ack))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:get-ok) (amqp:get-ok amqp-1-1-0-9-0:method)
  ((id :initform 71))
  ((delivery-tag
   :initform (field-type-initform delivery-tag amqp-1-1-0-9-0:delivery-tag)
   :type amqp-1-1-0-9-0:delivery-tag)
  (redelivered
   :initform (field-type-initform redelivered amqp-1-1-0-9-0:redelivered)
   :type amqp-1-1-0-9-0:redelivered)
  (exchange
   :initform (field-type-initform exchange amqp-1-1-0-9-0:exchange-name)
   :type amqp-1-1-0-9-0:exchange-name
   :documentation "Specifies the name of the exchange that the message was originally published to.
 If empty, the message was published to the default exchange.")
  (routing-key
   :initform (field-type-initform routing-key amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :documentation "Message routing key

 Specifies the routing key name specified when the message was published.")
  (message-count
   :initform (field-type-initform message-count amqp-1-1-0-9-0:message-count)
   :type amqp-1-1-0-9-0:message-count))
  (:documentation "roles: client MAY."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:get-empty) (amqp:get-empty amqp-1-1-0-9-0:method)
  ((id :initform 72))
  ((reserved-1
   :initform (field-type-initform reserved-1 amqp-1-1-0-9-0:shortstr)
   :type amqp-1-1-0-9-0:shortstr
   :allocation :class))
  (:documentation "roles: client MAY."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:ack) (amqp:ack amqp-1-1-0-9-0:method)
  ((id :initform 80))
  ((delivery-tag
   :initform (field-type-initform delivery-tag amqp-1-1-0-9-0:delivery-tag)
   :type amqp-1-1-0-9-0:delivery-tag)
  (multiple
   :initform (field-type-initform multiple amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "acknowledge multiple messages

 If set to 1, the delivery tag is treated as 'up to and including', so that the
 client can acknowledge multiple messages with a single method. If set to zero,
 the delivery tag refers to a single message. If the multiple field is 1, and the
 delivery tag is zero, tells the server to acknowledge all outstanding messages.

 The server MUST validate that a non-zero delivery-tag refers to a delivered
 message, and raise a channel exception if this is not the case. On a transacted
 channel, this check MUST be done immediately and not delayed until a Tx.Commit.
 Specifically, a client MUST not acknowledge the same message more than once.

 TODO."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:reject) (amqp:reject amqp-1-1-0-9-0:method)
  ((id :initform 90))
  ((delivery-tag
   :initform (field-type-initform delivery-tag amqp-1-1-0-9-0:delivery-tag)
   :type amqp-1-1-0-9-0:delivery-tag)
  (requeue
   :initform (field-type-initform requeue amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "requeue the message

 If requeue is true, the server will attempt to requeue the message. If requeue
 is false or the requeue attempt fails the messages are discarded or dead-lettered.

 The server MUST NOT deliver the message to the same client within the
 context of the current channel. The recommended strategy is to attempt to
 deliver the message to an alternative consumer, and if that is not possible,
 to move the message to a dead-letter queue. The server MAY use more
 sophisticated tracking to hold the message on the queue and redeliver it to
 the same client at a later stage.

 TODO."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:recover-async) (amqp:recover-async amqp-1-1-0-9-0:method)
  ((id :initform 100))
  ((requeue
   :initform (field-type-initform requeue amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "requeue the message

 If this field is zero, the message will be redelivered to the original
 recipient. If this bit is 1, the server will attempt to requeue the message,
 potentially then delivering it to an alternative subscriber."))
  (:documentation "roles: server MAY."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:recover) (amqp:recover amqp-1-1-0-9-0:method)
  ((id :initform 110))
  ((requeue
   :initform (field-type-initform requeue amqp-1-1-0-9-0:bit)
   :type amqp-1-1-0-9-0:bit
   :documentation "requeue the message

 If this field is zero, the message will be redelivered to the original
 recipient. If this bit is 1, the server will attempt to requeue the message,
 potentially then delivering it to an alternative subscriber."))
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:basic amqp:recover-ok) (amqp:recover-ok amqp-1-1-0-9-0:method)
  ((id :initform 111))
  ()
  (:documentation "roles: client MUST."))

;;; class: tx [id method-names]
;;;   tx.select
;;;   tx.select-ok
;;;   tx.commit
;;;   tx.commit-ok
;;;   tx.rollback
;;;   tx.rollback-ok

(def-amqp-class amqp-1-1-0-9-0:tx (amqp-1-1-0-9-0:object amqp:tx)
  ((id :initform 90 :allocation :class)
   (method-names :initform '(amqp:select amqp:select-ok amqp:commit amqp:commit-ok amqp:rollback amqp:rollback-ok)
    :allocation :class))
  ()
  ()
  (:documentation "roles: server SHOULD; client MAY.

      The Tx class allows publish and ack operations to be batched into atomic
      units of work.  The intention is that all publish and ack requests issued
      within a transaction will complete successfully or none of them will.
      Servers SHOULD implement atomic transactions at least where all publish
      or ack requests affect a single queue.  Transactions that cover multiple
      queues may be non-atomic, given that queues can be created and destroyed
      asynchronously, and such events do not form part of any transaction.
      Further, the behaviour of transactions with respect to the immediate and
      mandatory flags on Basic.Publish methods is not defined.
    
      tx                  = C:SELECT S:SELECT-OK
                          / C:COMMIT S:COMMIT-OK
                          / C:ROLLBACK S:ROLLBACK-OK
    "))


(def-amqp-method (amqp-1-1-0-9-0:tx amqp:select) (amqp:select amqp-1-1-0-9-0:method)
  ((id :initform 10))
  ()
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:tx amqp:select-ok) (amqp:select-ok amqp-1-1-0-9-0:method)
  ((id :initform 11))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:tx amqp:commit) (amqp:commit amqp-1-1-0-9-0:method)
  ((id :initform 20))
  ()
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:tx amqp:commit-ok) (amqp:commit-ok amqp-1-1-0-9-0:method)
  ((id :initform 21))
  ()
  (:documentation "roles: client MUST."))

(def-amqp-method (amqp-1-1-0-9-0:tx amqp:rollback) (amqp:rollback amqp-1-1-0-9-0:method)
  ((id :initform 30))
  ()
  (:documentation "roles: server MUST."))

(def-amqp-method (amqp-1-1-0-9-0:tx amqp:rollback-ok) (amqp:rollback-ok amqp-1-1-0-9-0:method)
  ((id :initform 31))
  ()
  (:documentation "roles: client MUST."))