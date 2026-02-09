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
        String username = auth.getName();
        User user = userService.findByUserName(username);
        Long userId = user.getId();

        try {
            tempMbox = node.createMbox();
            String auctionProcessName = "auction_" + auctionId;

            OtpErlangObject[] payload = new OtpErlangObject[]{
                    tempMbox.self(),
                    new OtpErlangAtom("bid"),
                    new OtpErlangLong(userId),
                    new OtpErlangDouble(amount)
            };
            tempMbox.send(auctionProcessName, erlangServerNode, new OtpErlangTuple(payload));
            OtpErlangObject response = tempMbox.receive(5000);

            if (response instanceof OtpErlangTuple respTuple) {
                String status = ((OtpErlangAtom) respTuple.elementAt(0)).atomValue();

                if ("bid_accepted".equals(status)) {
                    return "SUCCESS: Bid accepted.";
                } else if ("bid_rejected".equals(status)) {
                    String reason = ((OtpErlangAtom) respTuple.elementAt(1)).atomValue();
                    return "ERROR: " + reason.replace("_", " ");
                }
            }
            return "ERROR: Invalid response from auction server.";
        } catch (Exception e) {
            return "ERROR: Connection lost or timeout.";
        } finally {
            if (tempMbox != null) node.closeMbox(tempMbox);
        }
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
                    else if (isAtom(tuple.elementAt(0), "new_bid")) {
                        handleNewBid(tuple);
                    }

                    else if (isAtom(tuple.elementAt(0), "bid_rejected")){
                        handleBidRejected(tuple);
                    }
                    else if(isAtom(tuple.elementAt(0), "bid_accepted")){
                        handleTimer(tuple);
                    }
                }
            } catch (Exception e) {
                System.err.println("Listener Error: " + e.getMessage());
            }
        }
    }

    private void handleNewBid(OtpErlangTuple tuple) {
        try {
            // {new_bid, AuctionId, NewPrice, BidderName}
            String auctionId = tuple.elementAt(1).toString();
            Double price = extractDouble(tuple.elementAt(2));
            String userId = extractString(tuple.elementAt(3));

            messagingTemplate.convertAndSend("/topic/auction/" + auctionId + "/updates",
                    new BidUpdateDto(price, userId));
            System.out.println("New Bid for Auction [" + auctionId + "] - Bidder: " + userId + ", Amount: " + price);
        } catch (Exception e) {
            System.err.println("Error handling new_bid: " + e.getMessage());
        }
    }

    private void handleBidRejected(OtpErlangTuple tuple) {
        try {
            String auctionId = tuple.elementAt(1).toString();
            String user = extractString(tuple.elementAt(2));
            String reason = extractString(tuple.elementAt(3));

            System.err.println("Bid rejected for User " + user + " on Auction " + auctionId + ": " + reason);
            messagingTemplate.convertAndSend("/topic/auction/" + auctionId + "/errors",
                    "Bid rejected for user " + user + ": " + reason.replace("_", " "));

        } catch (Exception e) {
            System.err.println("Error handling bid_rejected: " + e.getMessage());
        }
    }

    private void handleTimer(OtpErlangTuple tuple){
        try {
            Double amount = extractDouble(tuple.elementAt(1));
            Double time = extractDouble(tuple.elementAt(2));
            System.out.println("Timer Extended by 30 seconds");
        } catch (Exception e) {
            System.err.println("Error handling bid_accepted msg");
        }
    }

    private void handleIncomingChatMessage(OtpErlangTuple tuple) {
        try {
            // Expected tuple: {chat_msg, AuctionId, User, Text}
            String auctionId = tuple.elementAt(1).toString();
            String user = extractString(tuple.elementAt(2));
            String text = extractString(tuple.elementAt(3));

            System.out.println("Chat from Erlang [" + auctionId + "] " + user + ": " + text);

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
            System.out.println("+++ [ACTIVATING] Manager requested next item. Starting Auction #" + item.getId() + " (" + item.getName() + ")");
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

    private Double extractDouble(OtpErlangObject obj) {
        if (obj instanceof OtpErlangDouble d) return d.doubleValue();
        if (obj instanceof OtpErlangLong l) return (double) l.longValue();
        return 0.0;
    }

    private Long extractLong(OtpErlangObject obj) {
        if (obj instanceof OtpErlangLong l) return l.longValue();
        if (obj instanceof OtpErlangDouble d) return (long) d.doubleValue();
        return 0L;
    }

    @PreDestroy
    public void shutdown() {
        if (node != null) node.close();
    }

    public record ChatMessageDto(String user, String message) {}
    public record BidUpdateDto(Double price, String bidder) {}

}