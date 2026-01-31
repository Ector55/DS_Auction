package org.example.distributedproject.service;

import com.ericsson.otp.erlang.*;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.example.distributedproject.model.Item;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;
import java.io.IOException;
import java.util.List;


@Service
public class ErlangService {

    @Autowired
    private ItemService itemService;
    private OtpNode node;
    private OtpMbox mbox;
    private final String erlangNodeName = "java_node@127.0.0.1";
    private final String erlangServerNode = "auction_service@127.0.0.1";
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
        System.out.println("Sono partito bro");
        System.out.println(node);
        System.out.println(mbox.getName());
        System.out.println("COOKIE JAVA: '" + node.cookie() + "'");
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
            mbox.send(chatProcessName, erlangServerNode, new OtpErlangTuple(msgPayload));
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    // In ErlangService.java

    private void listen() {
        while (true) {
            try {
                OtpErlangObject msg = mbox.receive();
                if (msg instanceof OtpErlangTuple) {
                    OtpErlangTuple tuple = (OtpErlangTuple) msg;

                    // Erlang invia: {SenderPid, MsgId, get_next_auctions, Count}
                    // La tupla ha 4 elementi.
                    if (tuple.arity() == 4) {
                        OtpErlangObject term3 = tuple.elementAt(2); // L'atomo del comando

                        if (term3 instanceof OtpErlangAtom && ((OtpErlangAtom) term3).atomValue().equals("get_next_auctions")) {
                            handleGetNextAuctions(tuple);
                        }
                    } else {
                        // Gestione altri messaggi (es. chat)
                        System.out.println("Messaggio generico ricevuto: " + tuple);
                    }
                }
            } catch (Exception e) {
                System.err.println("Error receiving Erlang: " + e.getMessage());
            }
        }
    }

    private void handleGetNextAuctions(OtpErlangTuple tuple) {
        try {
            // 1. Estrai ID messaggio e PID
            OtpErlangPid senderPid = (OtpErlangPid) tuple.elementAt(0);
            OtpErlangRef msgId = (OtpErlangRef) tuple.elementAt(1);

            System.out.println("Richiesta 'get_next_auctions' ricevuta.");

            // 2. Chiama il service (Transazionale)
            // Nota: Assicurati di aver iniettato il service: @Autowired private ItemService itemService;
            Item item = itemService.activateAndGetNextItem();

            OtpErlangList auctionList;

            if (item != null) {
                // Trovato! Convertiamolo in Tupla Erlang: {Id, Name, Price}
                OtpErlangTuple itemTuple = new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangLong(item.getId()),        // ID
                        new OtpErlangString(item.getName()),    // Nome
                        new OtpErlangDouble(item.getStartingPrice()) // Prezzo (Double per sicurezza)
                });

                // Mettilo dentro una lista (Erlang si aspetta una lista di aste)
                auctionList = new OtpErlangList(new OtpErlangObject[]{ itemTuple });
                System.out.println("Attivato item ID: " + item.getId());
            } else {
                // Nessun item trovato: Mandiamo una lista vuota []
                auctionList = new OtpErlangList();
                System.out.println("Nessun item PENDING trovato.");
            }

            // 3. Rispondi a Erlang: {MsgId, java_response, AuctionList}
            OtpErlangTuple response = new OtpErlangTuple(new OtpErlangObject[]{
                    msgId,
                    new OtpErlangAtom("java_response"),
                    auctionList
            });

            mbox.send(senderPid, response);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    @PreDestroy
    public void shutdown() {
        if (node != null) node.close();
    }

    public String placeBid(Long auctionId, String userId, Double amount) {
        try {
            // Il nome del processo asta in Erlang è registrato come "auction_ID"
            String auctionProcessName = "auction_" + auctionId;

            // Creiamo il messaggio: {self(), bid, UserId, Amount}
            // self() viene aggiunto automaticamente da mbox.send() se usiamo send RPC style
            // oppure dobbiamo inviare il nostro PID affinché Erlang possa rispondere.

            OtpErlangObject[] payload = new OtpErlangObject[]{
                    mbox.self(),                // ClientPid (Il PID della mailbox Java)
                    new OtpErlangAtom("bid"),   // Atom 'bid'
                    new OtpErlangString(userId),// UserId
                    new OtpErlangDouble(amount) // Amount
            };

            OtpErlangTuple message = new OtpErlangTuple(payload);

            // Invio al nodo Erlang specifico (definito nei passaggi precedenti)
            mbox.send(auctionProcessName, erlangServerNode, message);

            // Java aspetta la risposta dall'asta (es. {bid_accepted, ...} o {bid_rejected, ...})
            // Timeout di 5 secondi
            OtpErlangObject response = mbox.receive(5000);

            if (response instanceof OtpErlangTuple) {
                OtpErlangTuple respTuple = (OtpErlangTuple) response;
                String status = respTuple.elementAt(0).toString(); // 'bid_accepted' o 'bid_rejected'
                return status;
            }

        } catch (Exception e) {
            e.printStackTrace();
            return "error_communication";
        }
        return "timeout";
    }

}
