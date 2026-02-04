package org.example.distributedproject.service;

import com.ericsson.otp.erlang.*;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.example.distributedproject.model.Item; // Assicurati che questo model esista
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.stereotype.Service;

import java.io.IOException;

@Service
public class ErlangService {

    @Autowired
    private ItemService itemService;

    private SimpMessagingTemplate messagingTemplate;

    private OtpNode node;
    private OtpMbox mainMbox; // Mailbox principale per ricevere eventi asincroni (chat, richieste DB)

    // Configurazione (Idealmente dovrebbero stare in application.properties)
    private final String erlangNodeName = "java_node@127.0.0.1";
    private final String erlangServerNode = "auction_service@127.0.0.1";
    private final String cookie = "mypassword";

    @PostConstruct
    public void init() throws IOException {
        // Crea il nodo Java
        node = new OtpNode(erlangNodeName, cookie);

        // Crea la mailbox principale "java_listener" nota a Erlang
        mainMbox = node.createMbox("java_listener");

        // Avvia il thread di ascolto continuo
        Thread listenerThread = new Thread(this::listen);
        listenerThread.setDaemon(true); // Si chiude se l'app muore
        listenerThread.start();

        System.out.println("✅ Java Node Started: " + node.node());
        System.out.println("✅ Cookie: " + node.cookie());
        System.out.println("✅ Mailbox 'java_listener' ready.");
    }

    public void sendChatMessage(int auctionId, String user, String message) {
        try {
            String chatProcessName = "chat_" + auctionId;
            OtpErlangObject[] msgPayload = new OtpErlangObject[]{
                    new OtpErlangAtom("post_message"),
                    new OtpErlangString(user),
                    new OtpErlangString(message)
            };
            mainMbox.send(chatProcessName, erlangServerNode, new OtpErlangTuple(msgPayload));
        } catch (Exception e) {
            System.err.println("❌ Errore invio chat: " + e.getMessage());
        }
    }

    public String placeBid(Long auctionId, String userId, Double amount) {
        OtpMbox tempMbox = null;
        try {
            // 1. Crea mailbox effimera solo per questa richiesta
            tempMbox = node.createMbox();

            String auctionProcessName = "auction_" + auctionId;

            // Messaggio: {TempPid, bid, UserId, Amount}
            // Erlang dovrà rispondere a TempPid (tempMbox.self())
            OtpErlangObject[] payload = new OtpErlangObject[]{
                    tempMbox.self(),
                    new OtpErlangAtom("bid"),
                    new OtpErlangString(userId),
                    new OtpErlangDouble(amount)
            };

            // Invia
            tempMbox.send(auctionProcessName, erlangServerNode, new OtpErlangTuple(payload));

            // 2. Attende risposta SOLO su questa mailbox (Timeout 5s)
            OtpErlangObject response = tempMbox.receive(5000);

            if (response instanceof OtpErlangTuple respTuple) {
                // Aspettiamo {status, ...} es: {bid_accepted, NewPrice} o {bid_rejected, Reason}
                // Ritorniamo il primo elemento come stringa (status)
                return respTuple.elementAt(0).toString();
            }

        } catch (Exception e) {
            System.err.println("❌ Errore placeBid: " + e.getMessage());
            return "error_communication";
        } finally {
            // 3. Importante: distruggere la mailbox temporanea
            if (tempMbox != null) {
                node.closeMbox(tempMbox);
            }
        }
        return "timeout";
    }

    // ... (tutto il resto del codice rimane uguale)

    private void listen() {
        while (!Thread.currentThread().isInterrupted()) {
            try {
                OtpErlangObject msg = mainMbox.receive();

                if (msg instanceof OtpErlangTuple tuple) {
                    if (tuple.arity() == 4 && isAtom(tuple.elementAt(2), "get_next_auctions")) {
                        handleGetNextAuctions(tuple);
                    }
                    else if (tuple.arity() == 4 && isAtom(tuple.elementAt(0), "chat_msg")) {
                        handleIncomingChatMessage(tuple);
                    }
                    // Richiama il metodo che implementiamo qui sotto
                    else if (tuple.arity() == 4 && isAtom(tuple.elementAt(0), "auction_closed")) {
                        handleAuctionClosed(tuple);
                    }
                    else {
                        System.out.println("⚠️ Messaggio Erlang ignorato: " + tuple);
                    }
                }
            } catch (OtpErlangExit e) {
                System.err.println("❌ Processo Erlang terminato.");
                break;
            } catch (Exception e) {
                System.err.println("❌ Errore nel listener: " + e.getMessage());
            }
        }
    }

    // AGGIUNGI QUESTO METODO MANCANTE
    private void handleAuctionClosed(OtpErlangTuple tuple) {
        try {
            // Estrae dati da: {auction_closed, AuctionId, Winner, Price}
            Long auctionId = ((OtpErlangLong) tuple.elementAt(1)).longValue();
            String winner = extractString(tuple.elementAt(2));
            Double price = ((OtpErlangDouble) tuple.elementAt(3)).doubleValue();

            System.out.println("📥 Notifica chiusura asta " + auctionId + ". Vincitore: " + winner);

            // Chiama il service per aggiornare lo stato nel DB a SOLD
            itemService.closeItem(auctionId, winner, price);

        } catch (Exception e) {
            System.err.println("❌ Errore nel processare auction_closed: " + e.getMessage());
        }
    }

// ... (handleGetNextAuctions e le altre utility rimangono uguali)


    private void handleGetNextAuctions(OtpErlangTuple tuple) {
        try {
            OtpErlangPid senderPid = (OtpErlangPid) tuple.elementAt(0);
            OtpErlangRef msgId = (OtpErlangRef) tuple.elementAt(1);

            System.out.println("📥 Richiesta DB ricevuta da Erlang.");

            // Chiama il service DB
            Item item = itemService.activateAndGetNextItem();

            OtpErlangList auctionList;
            if (item != null) {
                // Crea tupla {Id, Name, Price}
                OtpErlangTuple itemTuple = new OtpErlangTuple(new OtpErlangObject[]{
                        new OtpErlangLong(item.getId()),
                        new OtpErlangString(item.getName()),
                        new OtpErlangDouble(item.getStartingPrice())
                });
                // Mette in lista
                auctionList = new OtpErlangList(new OtpErlangObject[]{ itemTuple });
                System.out.println("✅ Item trovato: " + item.getName());
            } else {
                // Lista vuota
                auctionList = new OtpErlangList();
                System.out.println("⚠️ Nessun item disponibile.");
            }

            // Risposta: {MsgId, java_response, AuctionList}
            OtpErlangTuple response = new OtpErlangTuple(new OtpErlangObject[]{
                    msgId,
                    new OtpErlangAtom("java_response"),
                    auctionList
            });

            mainMbox.send(senderPid, response);

        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void handleIncomingChatMessage(OtpErlangTuple tuple) {
        // tuple: {chat_msg, AuctionId, User, Text}
        try {
            // Convertiamo gli oggetti Erlang in Java String sicure
            String auctionId = tuple.elementAt(1).toString(); // ID numerico o stringa
            String user = extractString(tuple.elementAt(2));
            String text = extractString(tuple.elementAt(3));

            System.out.println("💬 CHAT RX [" + auctionId + "] " + user + ": " + text);

            // Invia al Frontend via WebSocket
            // Assumiamo che il client sia sottoscritto a: /topic/auction/{id}
            String destination = "/topic/auction/" + auctionId;

            // Creiamo un oggetto semplice per il JSON (puoi usare una classe dedicata DTO)
            ChatMessageDto msgDto = new ChatMessageDto(user, text);

            messagingTemplate.convertAndSend(destination, msgDto);

        } catch (Exception e) {
            System.err.println("❌ Errore parsing messaggio chat: " + e.getMessage());
        }
    }

    // Utility per controllare gli atomi
    private boolean isAtom(OtpErlangObject obj, String val) {
        return obj instanceof OtpErlangAtom atom && atom.atomValue().equals(val);
    }

    // Utility CRUCIALE: Erlang invia stringhe come Binary o List, dobbiamo gestire entrambi
    private String extractString(OtpErlangObject obj) {
        if (obj instanceof OtpErlangString) {
            return ((OtpErlangString) obj).stringValue();
        } else if (obj instanceof OtpErlangBinary) {
            return new String(((OtpErlangBinary) obj).binaryValue());
        } else if (obj instanceof OtpErlangAtom) {
            return ((OtpErlangAtom) obj).atomValue();
        }
        return obj.toString(); // Fallback
    }

    @PreDestroy
    public void shutdown() {
        if (node != null) {
            node.close();
            System.out.println("🛑 Java Node Closed.");
        }
    }

    public record ChatMessageDto(String user, String message) {

    }
}