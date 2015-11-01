(ns uplift.messaging.core
  (:import [java.nio.channels ServerSocketChannel SocketChannel Selector SelectionKey]
           [java.nio CharBuffer ByteBuffer]
           [java.nio.charset Charset]
           [java.net InetSocketAddress StandardSocketOptions]
           [java.util NoSuchElementException]
           [uplift.messaging UpliftSelect]
           (java.util NoSuchElementException))
  (:require [schema.core :as s]
            [uplift.utils.repl-utils :refer [ptable]]
            [taoensso.timbre :as timbre :refer [log info debug spy]]))

(def chan-type "channelType")
(def server-chan "serverChannel")
(def client-chan "clientChannel")

(s/defrecord Server
  [host :- s/Str
   port :- s/Int
   block?
   channel :- ServerSocketChannel
   selector :- Selector
   sel-key])

(defn attach-chan
  "Attach SelectorKey to a channel type"
  [{:keys [sel-key]}]
  ;; when SelectionKey.attachment() is called, it will get this map
  (.attach sel-key {chan-type server-chan}))


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
            (.configureBlocking block?)
            (.setOption  (. StandardSocketOptions SO_REUSEADDR) true))
        selector (if selector
                   selector
                   (Selector/open))
        ;; register the channel to the selector
        selkey (.register ss-chan selector (SelectionKey/OP_ACCEPT))
        _ (info "registered selector:" selector "to channel: " ss-chan)
        sinfo (map->Server {:host host :port port :block? block?
                           :channel ss-chan :sel-key selkey :selector selector})]
    (attach-chan sinfo)
    (info "attaching data to SelectionKey")
    sinfo))

;; TODO: turn this into a defprotocol or multimethod
(defn pump-buffer [channel buff]
  (info "in pump-buffer getting data")
  (loop [more? (.hasRemaining buff)]
    (when more?
      (let [charset (Charset/defaultCharset)
            encoded (.encode charset buff)]
        (.write channel encoded))
      (recur (.hasRemaining buff)))))

;; Reading from a channel is consistent across channel and buffer types
(defn read-chan
  ""
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
          (> bytes-read 0) (do
                             (.flip this)
                             ;; decode what's in the buffer
                             (recur (read-chan chan this)))
          (= -1 bytes-read) chan))))


(defn make-buffer []
  (CharBuffer/wrap "Client is connected"))


(defn register-client-chan
  "Registers the client SocketChannel with the Selector and confirms to the client
  it is connected"
  [client-channel selector]
  (info "registering client")
  (.configureBlocking client-channel false)
  (let [buff (make-buffer)
        client-key (.register client-channel selector SelectionKey/OP_READ SelectionKey/OP_WRITE)]
    (.attach client-key {chan-type client-chan})
    (pump-buffer client-channel buff)))


(defn- accept
  "Called when a new connection has been obtained.

  Gets the ServerSocketChannel from the SelectionKey (key), then accepts the connection.
  If the accept isn't null, we have the SocketChannel from the client.  It registers the
  client SocketChannel with the selector"
  [key selector]
  (info "accepting incoming connection")
  (let [server-sock-chan (.channel key)
        _ (info "retrieved server socket channel from:" key)
        client-sock-chan (.accept server-sock-chan)]
    ;; Check if the .accept returns nil, since the ServerSocketChannel was marked non-blocking
    (when (not (nil? client-sock-chan))
      (register-client-chan client-sock-chan selector))))


;; FIXME: broken due http://dev.clojure.org/jira/browse/CLJ-1243
;; rewrite this in java
(comment
  (defn select
    ""
    [^Selector selector]
    (let [selected-keys (.selectedKeys selector)
          get-next #(try
                     (.next %)
                     (catch NoSuchElementException e
                       nil))]
      (loop [next- (get-next (.iterator selected-keys))]
        (if next-
          ;;
          (let [selection-key next-
                sc (.attachment selection-key)
                channel-type (get sc chan-type)
                ;; FIXME: remove key after its been handled
                new-iter (.iterator selected-keys)
                _ (ptable new-iter)]
            (if (= server-chan channel-type)
              ;; In this case we have a new connection from a client
              (accept selection-key selector)
              ;; Otherwise we have data available on the socket
              nil
              )
            (.remove new-iter)
            (recur (get-next new-iter))))))))


(defn serve
  "Starts the ServerSocketChannel to listen for incoming requests

  It picks a "
  [selector]
  (loop [continue? true]  ;; flag to stop looping
    ;; .select is a blocking method which returns when one of the registered channels is
    ;; selected.  A socket client will be added to the list of registered channels
    (let [selection (.select selector)]
      (info "select is done blocking")
      (if (not= selection 0)
        (do
          (log :info "got IO event")
          (UpliftSelect/select selector))))
    (recur true)))


(defn client
  "Creates a client to a ServerSocketChannel and connects it"
  [^String host ^long port]
  (let [chan (SocketChannel/open)
        sock-addr (InetSocketAddress. host port)]
    (doto chan
      (.configureBlocking false)
      (.connect sock-addr))
    chan))


(defn get-chan-data
  [chan buff]
  (let [charset (Charset/defaultCharset)]
    (loop [count (.read chan buff)
           msg ""]
      (if (> count 0)
        (do
          (info count "bytes to read in " chan)
          (.flip buff)
          (recur (.read chan buff) (str msg (.decode charset buff))))
        msg))))


(defn client-loop
  [^SocketChannel chan]
  (while (not (.finishConnect chan))
    (println "Waiting to connect..."))

  ;; read data from the channel
  (let [buff (ByteBuffer/allocate 256)]
    (loop [chan-msg (get-chan-data chan buff)]
      (when (> (count chan-msg) 0)
        (info chan-msg)
        (let [charset (Charset/defaultCharset)
              out-buf (CharBuffer/wrap "Hello server")]
          (while (.hasRemaining out-buf)
            (info "writing to server")
            (.write chan (.encode charset out-buf)))))
      (recur (get-chan-data chan buff)))))


(defn main-
  "Starts a ServerSocketChannel"
  [host port]
  (let [server-sock-chan (make-server-socket-chan host port)
        {:keys [selector]} server-sock-chan
        client-chan (client host port)]
    ;; Start the ServerSocketChannel to listen for incoming data/connections in its own thread
    (future (serve selector))
    ;; start the client in the main thread
    (client-loop client-chan)))
