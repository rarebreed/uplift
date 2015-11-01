/**
 * Due to bug http://dev.clojure.org/jira/browse/CLJ-1243
 * Creating a class here that does the functionality needed to
 */

package uplift.messaging;

import java.io.IOException;
import java.nio.ByteBuffer;
import java.nio.CharBuffer;
import java.nio.channels.ServerSocketChannel;
import java.nio.channels.SocketChannel;
import java.nio.channels.Selector;
import java.nio.channels.SelectionKey;
import java.nio.charset.Charset;
import java.util.HashMap;
import java.util.Iterator;
import java.util.Map;
import java.util.Set;


public class UpliftSelect {
    private static String clientChan = "clientChannel";
    private static String serverChan = "serverChannel";
    private static String channelType = "channelType";

    public static void select(Selector selector) {
        try {

            Set<SelectionKey> selectedKeys = selector.selectedKeys();
            Iterator<SelectionKey> iter = selectedKeys.iterator();

            while (iter.hasNext()) {
                System.out.println("Handling next event");
                SelectionKey key = iter.next();

                // Check the selection key, and see if we have either a new connection or
                // a client ready for read/write ops
                Map<?, ?> att = (Map<?, ?>) key.attachment();
                String t = (String) att.get(channelType);
                System.out.println("Channel type: " + t);
                if (t.equals(serverChan)) {
                    // In this case, we have a new connection, so the channel has a socket server
                    // and have the server socket channel accept the connection.  If no client
                    // is connected, the channel will return null
                    ServerSocketChannel ssc = (ServerSocketChannel) key.channel();
                    SocketChannel csc = ssc.accept();
                    System.out.println("Got an incoming connection");

                    if (csc != null) {
                        csc.configureBlocking(false);
                        SelectionKey clientKey = csc.register(selector, SelectionKey.OP_READ, SelectionKey.OP_WRITE);
                        Map<String, String> clientProps = new HashMap<>();
                        clientProps.put(channelType, clientChan);
                        clientKey.attach(clientProps);

                        // respond to the client
                        CharBuffer buff = CharBuffer.wrap("Welcome client!");
                        while (buff.hasRemaining()) {
                            csc.write(Charset.defaultCharset().encode(buff));
                        }
                    }
                }
                else {
                    System.out.println("Data is available to be read from client");
                    ByteBuffer buffer = ByteBuffer.allocate(20);
                    SocketChannel clientChannel = (SocketChannel) key.channel();
                    int bytesRead = 0;
                    if (key.isReadable()) {
                        // the channel is non blocking so keep it open till the count is >=0
                        if ((bytesRead = clientChannel.read(buffer)) > 0) {
                            buffer.flip();
                            System.out.println(Charset.defaultCharset().decode(buffer));
                            buffer.clear();
                        }
                        if (bytesRead < 0) {
                            // the key is automatically invalidated once the channel is closed
                            clientChannel.close();
                        }
                    }
                }
                iter.remove();
            }
        } catch (IOException e) {
            e.printStackTrace();
        }
    }
}