package org.example.distributedproject.service;


import com.ericsson.otp.erlang.*;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.springframework.stereotype.Service;
import java.io.IOException;

@Service
public class ErlangService {
    private OtpNode node;
    private OtpMbox mbox;
    private final String erlangNodeName = "DS_auction_monitor"; //for now its modified
    private final String cookie = "mypassword"; //MODIFY!!

    @PostConstruct
    public void init() throws IOException {
        // Creates Java Node
        node = new OtpNode(erlangNodeName, cookie);
        // creates the mailbox with same name as macro Erlang (?JAVA_LISTENER)
        mbox = node.createMbox("java_listener");
        //starts the listening threads for incoming messages (modification or auction ending)
        Thread listenerThread = new Thread(this::listen);
        listenerThread.setDaemon(true);
        listenerThread.start();
    }
    //sends a message to the chat at a specific auction
    public void sendChatMessage(int auctionId, String user, String message) {
        try {
            String chatProcessName = "chat_" + auctionId;
            OtpErlangObject[] msgPayload = new OtpErlangObject[]{
                    new OtpErlangAtom("post_message"),
                    new OtpErlangString(user),
                    new OtpErlangString(message)
            };
            mbox.send(chatProcessName, erlangNodeName, new OtpErlangTuple(msgPayload));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    //thread that receives messages from erlang
    private void listen() {
        while (true) {
            try {
                OtpErlangObject msg = mbox.receive();
                if (msg instanceof OtpErlangTuple) {
                    OtpErlangTuple tuple = (OtpErlangTuple) msg;
                    //handling incoming messages
                    // {chat_msg, #{auction_id => ..., user => ..., text => ...}}
                    System.out.println("Message received from Erlang: " + tuple);

                    // TODO: Inoltrare il messaggio ai WebSocket dei client (Spring SimpMessagingTemplate)
                }
            } catch (Exception e) {
                System.err.println("Error receiving Erlang: " + e.getMessage());
            }
        }
    }

    @PreDestroy
    public void shutdown() {
        if (node != null) node.close();
    }

}
