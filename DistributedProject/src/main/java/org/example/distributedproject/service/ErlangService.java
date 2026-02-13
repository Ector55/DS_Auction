package org.example.distributedproject.service;

import com.ericsson.otp.erlang.*;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.example.distributedproject.model.Auction;
import org.example.distributedproject.model.Item;
import org.example.distributedproject.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

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

    public void sendChatMessage(int auctionId, String user, String message) {
        try {
            String chatProcessName = "chat_" + auctionId;
            OtpErlangObject[] msgPayload = new OtpErlangObject[]{
                    new OtpErlangAtom("post_message"),
                    new OtpErlangString(user),
                    new OtpErlangString(message)
            };
            mainMbox.send(chatProcessName, erlangServerNode, new OtpErlangTuple(msgPayload));
            System.out.println("Chat message sent to " + chatProcessName + "@" + erlangServerNode);
        } catch (Exception e) {
            System.err.println("Error sending chat to Erlang: " + e.getMessage());
        }
    }


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
                    new OtpErlangDouble(amount),
                    new OtpErlangString(username)
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
                    // 3. Handle Auction Completion i.e
                    else if (tuple.arity() == 5 &&isAtom(tuple.elementAt(0), "auction_closed")) {
                        handleAuctionClosed(tuple);
                    }
                    // 4. Auction unsold (no bids)
                    else if (tuple.arity() == 2 && isAtom(tuple.elementAt(0), "auction_unsold")) {
                        handleAuctionUnsold(tuple);
                    }
                    else if (isAtom(tuple.elementAt(0), "new_bid")) {
                        handleNewBid(tuple);
                    }

                    else if (isAtom(tuple.elementAt(0), "bid_rejected")){
                        handleBidRejected(tuple);
                    }
                    else if(isAtom(tuple.elementAt(0), "bid_accepted")){
                        handleTimer(tuple);
                    }  //else {
//                        System.out.println(" UNKNOWN MESSAGE TYPE: " + tuple.elementAt(0));
//                    }
                }
            } catch (OtpErlangException e) {
                System.err.println("Erlang connection losy.");
                break;
            }
            catch (Exception e) {
                System.err.println("Listener Error: " + e.getMessage());
            }
        }

    }

    // handle unsold items
    private void handleAuctionUnsold(OtpErlangTuple tuple) {

        try {
            Long itemId = ((OtpErlangLong) tuple.elementAt(1)).longValue();

            // mark item as pending again so it can be re-auctioned
            itemService.markItemPending(itemId);

        } catch (Exception e) {
            System.err.println("Error in processing auction_unsold: " + e.getMessage());
        }
    }

    private void handleNewBid(OtpErlangTuple tuple) {
        try {
            // {new_bid, AuctionId, NewPrice, BidderName}
            String auctionId = tuple.elementAt(1).toString();
            Double price = extractDouble(tuple.elementAt(2));
            String bidderName = extractString(tuple.elementAt(3));

            messagingTemplate.convertAndSend("/topic/auction/" + auctionId + "/updates",
                    new BidUpdateDto(price, bidderName));
            System.out.println("New Bid for Auction [" + auctionId + "] - Bidder: " + bidderName + ", Amount: " + price);
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
            // Expected: {auction_closed, AuctionId, JavaWinner, WinnderName, Price}
            Long itemId = ((OtpErlangLong) tuple.elementAt(1)).longValue();
            String winner = extractString(tuple.elementAt(2));
            String userName = extractString((tuple.elementAt(3)));
            Double price = ((OtpErlangDouble) tuple.elementAt(4)).doubleValue();

            itemService.closeItem(itemId, winner, price);
            System.out.println("Item " + itemId + " sold. Winner: " + userName + ", Price:" + price);
        } catch (Exception e) {
            e.printStackTrace();
        }
    }

    private void handleGetNextAuctions(OtpErlangTuple tuple) {
        try {
            OtpErlangPid senderPid = (OtpErlangPid) tuple.elementAt(0);
            OtpErlangRef msgId = (OtpErlangRef) tuple.elementAt(1);

            int requestedCount = ((OtpErlangLong) tuple.elementAt(3)).intValue();
            System.out.println("[JAVA] Requested " + requestedCount + " items");

//            Item item = itemService.activateAndGetNextItem();
//            System.out.println("+++ [ACTIVATING] Manager requested next item. Starting Auction #" + item.getId() + " (" + item.getName() + ")");
//            OtpErlangList auctionList = (item != null) ?
//                    new OtpErlangList(new OtpErlangTuple(new OtpErlangObject[]{
//                            new OtpErlangLong(item.getId()), new OtpErlangString(item.getName()), new OtpErlangDouble(item.getStartingPrice())
//                    })) : new OtpErlangList();
//
//            mainMbox.send(senderPid, new OtpErlangTuple(new OtpErlangObject[]{
//                    msgId, new OtpErlangAtom("java_response"), auctionList
//            }));

            if (requestedCount > 0) {
                Item item = itemService.activateAndGetNextItem();

                OtpErlangList auctionList;
                if (item != null) {
                    System.out.println("+++ [ACTIVATING] Manager requested next item. Starting Auction with Item #" + item.getId() + " (" + item.getName() + ") for next available slot");
                    auctionList = new OtpErlangList(new OtpErlangTuple(new OtpErlangObject[]{
                            new OtpErlangLong(item.getId()),
                            new OtpErlangString(item.getName()),
                            new OtpErlangDouble(item.getStartingPrice())
                    }));
                } else {
                    System.out.println("[JAVA] No PENDING Items found");
                    auctionList = new OtpErlangList();
                }
                mainMbox.send(senderPid, new OtpErlangTuple(new OtpErlangObject[]{
                        msgId, new OtpErlangAtom("java_response"), auctionList
                }));
            }
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


    public List<Auction> fetchActiveAuctionsFromErlang() {
        OtpMbox tempMbox = null;
        try {
            tempMbox = node.createMbox(); // Create a temporary mailbox for the reply
            OtpErlangObject[] request = new OtpErlangObject[]{
                    tempMbox.self(),
                    node.createRef(),
                    new OtpErlangAtom("get_active_auctions")
            };

            // Send to the manager
            tempMbox.send("DS_auction_manager", erlangServerNode, new OtpErlangTuple(request));
            // Wait for reply (timeout 5 seconds)
            OtpErlangObject response = tempMbox.receive(5000);
            System.out.println("DEBUG: Risposta grezza da Erlang: " + response); // <--- AGGIUNGI QUESTO

            if (response instanceof OtpErlangTuple respTuple) {
                // Expected: {Ref, active_auctions_response, [List]}
                OtpErlangAtom status = (OtpErlangAtom) respTuple.elementAt(1);

                if ("active_auctions_response".equals(status.atomValue())) {
                    OtpErlangList auctionList = (OtpErlangList) respTuple.elementAt(2);
                    return mapErlangListToAuctions(auctionList);
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (tempMbox != null) node.closeMbox(tempMbox);
        }
        return new ArrayList<>(); // Return empty if failed
    }


    private List<Auction> mapErlangListToAuctions(OtpErlangList erlangList) {
        List<Auction> result = new ArrayList<>();

        for (OtpErlangObject obj : erlangList) {
            if (obj instanceof OtpErlangTuple auctionTuple) {
                try {
                    // La tupla ora è: {AuctionId, ItemId, TimeLeft}
                    Long auctionId = extractLong(auctionTuple.elementAt(0));
                    Long itemId = extractLong(auctionTuple.elementAt(1));

                    // NUOVO: Estrai il tempo rimanente (3° elemento, indice 2)
                    Long timeLeft = extractLong(auctionTuple.elementAt(2));

                    Item item = itemService.getItemById(itemId);

                    if (item != null) {
                        Auction auction = new Auction(
                                auctionId,
                                item,
                                timeLeft, // <--- Qui usiamo il tempo vero ricevuto da Erlang
                                item.getStartingPrice(),
                                null
                        );
                        result.add(auction);
                    }
                } catch (Exception e) {
                    System.err.println("Failed to map auction tuple: " + obj);
                    e.printStackTrace();
                }
            }
        }
        return result;
    }
}