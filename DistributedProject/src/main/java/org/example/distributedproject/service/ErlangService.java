package org.example.distributedproject.service;

import java.io.IOException;
import java.util.ArrayList;
import java.util.List;

import org.example.distributedproject.model.Auction;
import org.example.distributedproject.model.Item;
import org.example.distributedproject.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import com.ericsson.otp.erlang.OtpErlangAtom;
import com.ericsson.otp.erlang.OtpErlangBinary;
import com.ericsson.otp.erlang.OtpErlangDouble;
import com.ericsson.otp.erlang.OtpErlangException;
import com.ericsson.otp.erlang.OtpErlangList;
import com.ericsson.otp.erlang.OtpErlangLong;
import com.ericsson.otp.erlang.OtpErlangObject;
import com.ericsson.otp.erlang.OtpErlangPid;
import com.ericsson.otp.erlang.OtpErlangRef;
import com.ericsson.otp.erlang.OtpErlangString;
import com.ericsson.otp.erlang.OtpErlangTuple;
import com.ericsson.otp.erlang.OtpMbox;
import com.ericsson.otp.erlang.OtpNode;

import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
//Service responsible for managing communication between the Java application and the Erlang auction server, 
//handling bid placements, chat messages, and synchronization of auction state.
@Service
public class ErlangService {

    @Autowired
    private ItemService itemService;

    @Autowired
    private UserService userService;

    @Autowired
    private AuctionBidService auctionBidService;

    @Autowired
    private SimpMessagingTemplate messagingTemplate;

    private OtpNode node;
    private OtpMbox mainMbox;

    private final String erlangNodeName = "java_node@10.2.1.25";
    private final String erlangServerNode = "auction_service@10.2.1.48"; //IP of the container with the Erlang server
    private final String erlangWorkerNode = "worker@10.2.1.13"; //IP of the container with the Erlang worker
    private final String cookie = "mypassword";

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
            mainMbox.send(chatProcessName, erlangWorkerNode, new OtpErlangTuple(msgPayload));            
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
            //send bid request to the specific auction process in Erlang
            tempMbox.send(auctionProcessName, erlangWorkerNode, new OtpErlangTuple(payload));
            OtpErlangObject response = tempMbox.receive(5000);

            if (response instanceof OtpErlangTuple respTuple) {
                String status = ((OtpErlangAtom) respTuple.elementAt(0)).atomValue();

                if ("bid_accepted".equals(status)) {
                    return "bid_accepted";
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
                    //handle Auction Manager requesting new items from DB and so on
                    if (tuple.arity() == 4 && isAtom(tuple.elementAt(2), "get_next_auctions")) {
                        handleGetNextAuctions(tuple);
                    }
                    else if (isAtom(tuple.elementAt(0), "chat_msg")) {
                        handleIncomingChatMessage(tuple);
                    }
                    else if (tuple.arity() == 5 &&isAtom(tuple.elementAt(0), "auction_closed")) {
                        handleAuctionClosed(tuple);
                    }
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
                    }
                    else if (isAtom(tuple.elementAt(0), "timer_tick")) {
                        handleTimerTick(tuple);
                    }
                }
            } catch (OtpErlangException e) {
                System.err.println("Erlang connection lost.");
                break;
            }
            catch (Exception e) {
                System.err.println("Listener Error: " + e.getMessage());
            }
        }

    }
//handle timer ticks to update remaining time for auctions
    private void handleTimerTick(OtpErlangTuple tuple) {
        try {
            //tuple:{timer_tick, AuctionId, NewTime}
            String auctionId = tuple.elementAt(1).toString();
            Long timeLeft = extractLong(tuple.elementAt(2));
            messagingTemplate.convertAndSend("/topic/auction/" + auctionId + "/time", timeLeft);

            //System.out.println("Auction [" + auctionId + "] Time remaining: " + timeLeft + "s");
        } catch (Exception e) {
            System.err.println("Error parsing timer_tick msg: " + e.getMessage());
        }
    }

    //handle unsold items
    private void handleAuctionUnsold(OtpErlangTuple tuple) {
        try {
            Long itemId = ((OtpErlangLong) tuple.elementAt(1)).longValue();
            //mark item as pending again so it can be re-auctioned
            itemService.markItemPending(itemId);
        } catch (Exception e) {
            System.err.println("Error in processing auction_unsold: " + e.getMessage());
        }
    }

    private void handleNewBid(OtpErlangTuple tuple) {
        try {
            //{new_bid, AuctionId, NewPrice, BidderName}
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
            //Tuple {bid_accepted, AuctionID, Amount, UserName}
            String auctionId = tuple.elementAt(1).toString();
            Double amount = extractDouble(tuple.elementAt(2));
            String bidderName = extractString(tuple.elementAt(3));

            System.out.println("Timer Extended by 30 seconds on auction " + auctionId + " due to bid of " + amount + " by " + bidderName);
            messagingTemplate.convertAndSend("/topic/auction/" + auctionId + "/time", 30L);

        } catch (Exception e) {
            System.err.println("Error handling bid_accepted msg: " + e.getMessage());
        }
    }

    private void handleIncomingChatMessage(OtpErlangTuple tuple) {
        try {
            //tuple: {chat_msg, AuctionId, User, Text}
            String auctionIdStr = tuple.elementAt(1).toString();
            Long auctionId = Long.parseLong(auctionIdStr);
            String user = extractString(tuple.elementAt(2));
            String text = extractString(tuple.elementAt(3));

            System.out.println("Chat from Erlang [" + auctionId + "] " + user + ": " + text);
            //save to history 
            auctionBidService.addChatMessage(auctionId, user, text);
            //broadcast to current subscribers via WebSocket
            messagingTemplate.convertAndSend("/topic/auction/" + auctionId, new ChatMessageDto(user, text));
        } catch (Exception e) {
            System.err.println("Error parsing chat msg: " + e.getMessage());
        }
    }

    private void handleAuctionClosed(OtpErlangTuple tuple) {
        try {
            //tuple: {auction_closed, ItemId, JavaWinner, WinnderName, Price}
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
    //handle manager requesting new items to start auctions
    private void handleGetNextAuctions(OtpErlangTuple tuple) {
        try {
            OtpErlangPid senderPid = (OtpErlangPid) tuple.elementAt(0);
            OtpErlangRef msgId = (OtpErlangRef) tuple.elementAt(1);
            int requestedCount = ((OtpErlangLong) tuple.elementAt(3)).intValue();
            System.out.println("[JAVA] Requested " + requestedCount + " items");
            if (requestedCount > 0) {
                Item item = itemService.activateAndGetNextItem();
                OtpErlangList auctionList;
                if (item != null) {
                    System.out.println("[ACTIVATING] Manager requested next item. Starting Auction with Item #" + item.getId() + " (" + item.getName() + ") for next available slot");
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
    //utility methods to parse different Erlang types safely
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
    //Clean up resources on shutdown
    @PreDestroy
    public void shutdown() {
        if (node != null) node.close();
    }

    
    public record ChatMessageDto(String user, String message) {}
    public record BidUpdateDto(Double price, String bidder) {}


    //method to fetch active auctions from Erlang and synchronize with Java state
    public List<Auction> fetchActiveAuctionsFromErlang() {
        OtpMbox tempMbox = null;
        OtpErlangList auctionList = null;
        try {
            tempMbox = node.createMbox(); //create a temporary mailbox for the reply
            OtpErlangObject[] request = new OtpErlangObject[]{
                    tempMbox.self(),
                    node.createRef(),
                    new OtpErlangAtom("get_active_auctions")
            };
            //send to the manager
            tempMbox.send("DS_auction_manager", erlangServerNode, new OtpErlangTuple(request));
            OtpErlangObject response = tempMbox.receive(5000); //waiting for a reply
            System.out.println("DEBUG: Raw Erlang response: " + response);

            if (response instanceof OtpErlangTuple respTuple) {
                //tuple: {Ref, active_auctions_response, [List]}
                OtpErlangAtom status = (OtpErlangAtom) respTuple.elementAt(1);
                if ("active_auctions_response".equals(status.atomValue())) {
                    auctionList = (OtpErlangList) respTuple.elementAt(2);
                    List<Auction> auctions = mapErlangListToAuctions(auctionList);
                    auctionBidService.syncWithErlangState(auctions);
                    return auctions;
                }
            }
        } catch (Exception e) {
            e.printStackTrace();
        } finally {
            if (tempMbox != null) node.closeMbox(tempMbox);
        }
        return new ArrayList<>();
    }

//method to convert Erlang list of auctions into Java Auction objects
    private List<Auction> mapErlangListToAuctions(OtpErlangList erlangList) {
        List<Auction> result = new ArrayList<>();
        for (OtpErlangObject obj : erlangList) {
            if (obj instanceof OtpErlangTuple auctionTuple) {
                try {
                    //tuple: {AuctionId, ItemId, TimeLeft}
                    Long auctionId = extractLong(auctionTuple.elementAt(0));
                    Long itemId = extractLong(auctionTuple.elementAt(1));
                    //extracting remaining time
                    Long timeLeft = extractLong(auctionTuple.elementAt(2));
                    Item item = itemService.getItemById(itemId);
                    if (item != null) {
                        Auction auction = new Auction(
                                auctionId,
                                item,
                                timeLeft, //Time received from erlang
                                item.getStartingPrice(),
                                null,
                                new ArrayList<>(),
                                new ArrayList<>()
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