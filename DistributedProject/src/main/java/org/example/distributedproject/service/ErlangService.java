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
    private final String erlangNodeName = "server@localhost"; //MODIFICARE
    private final String cookie = "mypassword"; //DA MODIFICARE

    @PostConstruct
    public void init() throws IOException {
        // Creates Java Node
        node = new OtpNode("java_node@localhost", cookie);
        // Crea la mailbox con lo stesso nome usato nelle macro Erlang (?JAVA_LISTENER)
        mbox = node.createMbox("java_listener");

        // Avvia il thread in ascolto per i messaggi in arrivo (es. notifiche chat o fine asta)
        Thread listenerThread = new Thread(this::listen);
        listenerThread.setDaemon(true);
        listenerThread.start();
    }

    // Invia un messaggio alla chat di una specifica asta
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

    // Thread che riceve messaggi da Erlang
    private void listen() {
        while (true) {
            try {
                OtpErlangObject msg = mbox.receive();
                if (msg instanceof OtpErlangTuple) {
                    OtpErlangTuple tuple = (OtpErlangTuple) msg;
                    // Qui gestisci i messaggi ricevuti, ad esempio:
                    // {chat_msg, #{auction_id => ..., user => ..., text => ...}}
                    System.out.println("Messaggio ricevuto da Erlang: " + tuple);

                    // TODO: Inoltrare il messaggio ai WebSocket dei client (Spring SimpMessagingTemplate)
                }
            } catch (Exception e) {
                System.err.println("Errore ricezione Erlang: " + e.getMessage());
            }
        }
    }

    @PreDestroy
    public void shutdown() {
        if (node != null) node.close();
    }

}
