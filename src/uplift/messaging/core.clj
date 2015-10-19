(ns uplift.messaging.core
  (:import [java.nio.channels ServerSocketChannel SocketChannel Selector SelectionKey]
           [java.nio CharBuffer ByteBuffer]
           [java.nio.charset Charset]
           [java.net InetSocketAddress]
           [java.util NoSuchElementException]
           [clojure.lang PersistentArrayMap]
           (java.util NoSuchElementException))
  (:require [schema.core :as s])
  )

(def chan-type "channel-type")
(def server-chan "server-channel")
(def client-chan "client-channel")

(s/defrecord Server
  [host :- s/Str
   port :- s/Int
   blocking?
   server-chan :- ServerSocketChannel])

(defn attach-chan
  "Attach SelectorKey to a channel type"
  [{:keys [chan select-key]}]
  (.attach select-key {chan-type server-chan}))


(defn make-server-socket-chan
  "Creates a ServerSocketChannel.
   1. binding it to address and a port.
   2. sets it to non-blocking
   3. Either creates a Selector or uses existing to register Channel with the Selector
   4. Attaches the SelectorKey to a channel type

   returns a map of the Channel, the SelectionKey and the Selector"
  [host port & {:keys [selector block?]
                :or   {block? false}}]
  (let [ss-chan (ServerSocketChannel/open)
        _ (doto ss-chan
            (.bind (java.net.InetSocketAddress. host port))
            (.configureBlocking block?))
        selector (if selector
                   selector
                   (Selector/open))
        selkey (.register ss-chan selector (SelectionKey/OP_ACCEPT))
        info {:chan ss-chan :select-key selkey :selector selector}]
    (attach-chan info)
    info))

;; TODO: turn this into a defprotocol or multimethod
(defn pump-buffer [channel buff]
  (loop [more? (.hasRemaining buff)]
    (when more?
      (let [charset (Charset/defaultCharset)
            encoded (.encode charset buff)]
        (.write channel encoded))
      (recur (.hasRemaining buff)))))


(defn read-chan
  "Reading from a channel is consistent across channel and buffer types"
  [chan buf]
  (.read chan buf))


;; Message Types
;;
;; Type              OpCode    Length  Description
;; Connect           00        2       Connect and register with the Controller
;; ServiceList       01        4       Get all connected service
;; ServiceRequest    02        4       Call a service function and get data back
;; SendData          03        8       Sends arbitrary data to server
;; GetData           04        8       Retrieves arbitrary data from server
;; SendEvent         05        8       Sends an Event type to the server
;; Subscribe         06        4       Sends a subscription request to the server to listen for a topic
;;
(deftype UpliftMessage
  [opcode                                                   ;; Determines msg type and length
   source-id                                                ;; Address of source
   source-port                                              ;; Port is the service type (akin to IP port)
   dest-id                                                  ;; Address of destination
   dest-port                                                ;; The port of the destination
   length                                                   ;; number of bytes of params + data
   data                                                     ;; transit data
   ])


(defprotocol UBuffer
  "Encapsulates how to read and write from a buffer

  The type of the Buffer determines the actual implementation of reading and writing to it.
  This protocol should be implemented by other types to encapsulate the implementation"
  (read-channel [this channel])
  (write-channel [this channel]))


(defn decode-buff
  "Decode the raw byte buffer into a data structure"
  [data]
  )


(defrecord UByteBuffer [size]
  UBuffer
    (read-channel [this chan]
      (loop [bytes-read (read-chan chan this)]
        (cond
          (> bytes-read 0)
            (do
              (.flip this)
              ;; decode what's in the buffer
              )
          (= -1 bytes-read) )))
  )


(defn make-buffer []
  (CharBuffer/wrap "Client is connected"))


(defn register-client-chan
  "Registers the client SocketChannel with the Selector and confirms to the client
  it is connected"
  [client-channel selector]
  (.configureBlocking client-channel false)
  (let [buff (make-buffer)
        client-key (.register client-channel selector SelectionKey/OP_READ SelectionKey/OP_WRITE)]
    (.attach client-key {chan-type client-channel})
    (pump-buffer client-channel buff)))


(defn- accept
  "Called when a new connection has been obtained.

  Gets the ServerSocketChannel from the SelectionKey (key), then accepts the connection.
  If the accept isn't null, we have the SocketChannel from the client.  It registers the
  client SocketChannel with the selector"
  [key selector]
  (let [server-sock-chan (.channel key)
        client-sock-chan (.accept server-sock-chan)]
    ;; Check if the .accept returns nil, since the ServerSocketChannel was marked non-blocking
    (when (not (nil? client-sock-chan))
      (register-client-chan client-sock-chan selector))))


(defn iterate-keys
  [selector]
  (let [selected-keys (.selectedKeys selector)
        _ (println (type selected-keys))
        iter (.iterator selected-keys)
        get-next #(try
                   (.next iter)
                   (catch NoSuchElementException e
                     nil))]
    (loop [next- (get-next)
           acc []]
       (if next-
         ;;
         (let [selection-key next-
               sc (.attachment selection-key)
               channel-type (get sc chan-type)]
           (if (= server-chan channel-type)
             ;; In this case we have a new connection from a client
             (accept selection-key selector)
             ;; Otherwise we have data available on the socket
             nil
             )
           (recur (get-next) (conj acc sc)))
         acc))))


(defn serve
  "Starts the ServerSocketChannel to listen for incoming requests"
  [selector]
  (loop [continue? true]  ;; flag to stop looping
    ;; .select is a blocking method which returns when one of the registered channels is
    ;; selected.  A socket client will be added to the list of registered channels
    (let [selection (.select selector)]
      (if (and continue? (not= selection 0))
        (recur true)
        (do
          (iterate-keys selector))))))


(defn client
  "Creates a client to a ServerSocketChannel and connects it"
  [^String host ^int port]
  (let [chan (SocketChannel/open)
        sock-addr (InetSocketAddress. host port)]
    (doto chan
      (.configureBlocking false)
      (.connect sock-addr))
    chan))


(defn client-loop
  [^SocketChannel chan ^InputStream is]

  )


(defn main-
  "Starts a ServerSocketChannel"
  [host port]
  (let [server-sock-chan (make-server-socket-chan host port)
        {:keys [chan select-key selector]} server-sock-chan
        client (client host port)]
    ;; Start the ServerSocketChannel to listen for incoming data/connections
    (serve selector)))
