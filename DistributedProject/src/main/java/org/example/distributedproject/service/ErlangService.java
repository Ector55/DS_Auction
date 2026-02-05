package org.example.distributedproject.service;

import com.ericsson.otp.erlang.*;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.example.distributedproject.model.Item;
import org.example.distributedproject.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.io.IOException;

@Service
public class ErlangService {

    @Autowired
    private ItemService itemService;

    @Autowired
    private UserService userService;

    @Autowired
    private SimpMessagingTemplate messagingTemplate;

    private OtpNode node;
    private OtpMbox mainMbox;

    private final String erlangNodeName = "java_node@127.0.0.1";
    private final String erlangServerNode = "auction_service@127.0.0.1"; // Replace with your manager node name
    private final String cookie = "mypassword"; // Must match Erlang -setcookie

    @PostConstruct
    public void init() throws IOException {
        node = new OtpNode(erlangNodeName, cookie);
        mainMbox = node.createMbox("java_listener");

        Thread listenerThread = new Thread(this::listen);
        listenerThread.setDaemon(true);
        listenerThread.start();

        System.out.println("Java Node Started: " + node.node());
        System.out.println("Mailbox 'java_listener' ready.");
    }

    /**
     * Sends a chat message from Web Frontend to the Erlang Chat Handler.
     */
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
            System.err.println("Error sending chat to Erlang: " + e.getMessage());
        }
    }

    /**
     * Handles Bid requests from the Web Frontend.
     */
    public String placeBid(Long auctionId, Double amount) {
        OtpMbox tempMbox = null;
        Authentication auth = SecurityContextHolder.getContext().getAuthentication();
        User user = (User) auth.getPrincipal();
        Long userId = user.getId();

        try {
            tempMbox = node.createMbox();
            String auctionProcessName = "auction_" + auctionId;

            OtpErlangObject[] payload = new OtpErlangObject[]{
                    tempMbox.self(),
                    new OtpErlangAtom("bid"),
                    new OtpErlangString(String.valueOf(userId)),
                    new OtpErlangDouble(amount)
            };

            tempMbox.send(auctionProcessName, erlangServerNode, new OtpErlangTuple(payload));
            OtpErlangObject response = tempMbox.receive(5000);

            if (response instanceof OtpErlangTuple respTuple) {
                return respTuple.elementAt(0).toString(); // e.g., "bid_accepted"
            }
        } catch (Exception e) {
            return "error_communication";
        } finally {
            if (tempMbox != null) node.closeMbox(tempMbox);
        }
        return "timeout";
    }

    private void listen() {
        while (!Thread.currentThread().isInterrupted()) {
            try {
                OtpErlangObject msg = mainMbox.receive();
                if (msg instanceof OtpErlangTuple tuple) {
                    // 1. Handle Auction Manager requesting new items from DB
                    if (tuple.arity() == 4 && isAtom(tuple.elementAt(2), "get_next_auctions")) {
                        handleGetNextAuctions(tuple);
                    }
                    // 2. Handle Chat Messages coming FROM Erlang terminals
                    else if (isAtom(tuple.elementAt(0), "chat_msg")) {
                        handleIncomingChatMessage(tuple);
                    }
                    // 3. Handle Auction Completion
                    else if (isAtom(tuple.elementAt(0), "auction_closed")) {
                        handleAuctionClosed(tuple);
                    }
                }
            } catch (Exception e) {
                System.err.println("Listener Error: " + e.getMessage());
            }
        }
    }

    private void handleIncomingChatMessage(OtpErlangTuple tuple) {
        try {
            // Expected tuple: {chat_msg, AuctionId, User, Text}
            String auctionId = tuple.elementAt(1).toString();
            String user = extractString(tuple.elementAt(2));
            String text = extractString(tuple.elementAt(3));

            System.out.println("Chat from Erlang [" + auctionId + "] " + user + ": " + text);

            // Push to Web Frontend via WebSocket
            messagingTemplate.convertAndSend("/topic/auction/" + auctionId, new ChatMessageDto(user, text));
        } catch (Exception e) {
            System.err.println("Error parsing chat msg: " + e.getMessage());
        }
    }

    private void handleAuctionClosed(OtpErlangTuple tuple) {
        try {
            // Expected: {auction_closed, AuctionId, Winner, Price}
            Long auctionId = ((OtpErlangLong) tuple.elementAt(1)).longValue();
            String winner = extractString(tuple.elementAt(2));
            Double price = ((OtpErlangDouble) tuple.elementAt(3)).doubleValue();

            itemService.closeItem(auctionId, winner, price);
            System.out.println("Auction " + auctionId + " closed. Winner: " + winner);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void handleGetNextAuctions(OtpErlangTuple tuple) {
        try {
            OtpErlangPid senderPid = (OtpErlangPid) tuple.elementAt(0);
            OtpErlangRef msgId = (OtpErlangRef) tuple.elementAt(1);

            Item item = itemService.activateAndGetNextItem();
            OtpErlangList auctionList = (item != null) ?
                    new OtpErlangList(new OtpErlangTuple(new OtpErlangObject[]{
                            new OtpErlangLong(item.getId()), new OtpErlangString(item.getName()), new OtpErlangDouble(item.getStartingPrice())
                    })) : new OtpErlangList();

            mainMbox.send(senderPid, new OtpErlangTuple(new OtpErlangObject[]{
                    msgId, new OtpErlangAtom("java_response"), auctionList
            }));
        } catch (Exception e) { e.printStackTrace(); }
    }

    private boolean isAtom(OtpErlangObject obj, String val) {
        return obj instanceof OtpErlangAtom atom && atom.atomValue().equals(val);
    }

    private String extractString(OtpErlangObject obj) {
        if (obj instanceof OtpErlangString) return ((OtpErlangString) obj).stringValue();
        if (obj instanceof OtpErlangBinary) return new String(((OtpErlangBinary) obj).binaryValue());
        if (obj instanceof OtpErlangAtom) return ((OtpErlangAtom) obj).atomValue();
        return obj.toString();
    }

    @PreDestroy
    public void shutdown() {
        if (node != null) node.close();
    }

    public record ChatMessageDto(String user, String message) {}
}