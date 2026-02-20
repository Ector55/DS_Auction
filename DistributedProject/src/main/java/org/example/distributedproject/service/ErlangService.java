package org.example.distributedproject.service;

import com.ericsson.otp.erlang.*;
import jakarta.annotation.PostConstruct;
import jakarta.annotation.PreDestroy;
import org.example.distributedproject.model.Auction;
import org.example.distributedproject.model.Item;
import org.example.distributedproject.model.User;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.messaging.simp.SimpMessagingTemplate;
import org.springframework.scheduling.annotation.EnableScheduling;
import org.springframework.scheduling.annotation.Scheduled;
import org.springframework.security.core.Authentication;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.stereotype.Service;

import java.io.IOException;
import java.util.*;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.Executors;
import java.util.concurrent.ScheduledExecutorService;
import java.util.concurrent.TimeUnit;

@Service
@EnableScheduling
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
    private ScheduledExecutorService resyncExecutor;

    // cache for time synchonization data
    private final Map<Long, Map<String, Object>> auctionSyncCache = new ConcurrentHashMap<>();

    // cache for active auctions to avoid repeate Erlang calls
    private final Map<Long, Auction> activeAuctionsCache = new ConcurrentHashMap<>();

    // cache for time remaining
    private final Map<Long, Long> timeRemainingCache = new ConcurrentHashMap<>();

    private final String erlangNodeName = "java_node@10.2.1.25";
    private final String erlangServerNode = "auction_service@10.2.1.48";
    private final String erlangWorkerNode = "worker@10.2.1.13";
    private final String cookie = "mypassword";

    @PostConstruct
    public void init() throws IOException {
        node = new OtpNode(erlangNodeName, cookie);
        mainMbox = node.createMbox("java_listener");

        Thread listenerThread = new Thread(this::listen);
        listenerThread.setDaemon(true);
        listenerThread.start();

        initResync();

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
            Map<String, Object> SyncInfo = getAuctionSyncInfo(auctionId);
            if (SyncInfo == null) {
                return "ERROR: Unable to synchronize with auction server. Please try again.";
            }

            long offset = ((Number) SyncInfo.get("offset")).longValue();
            long rtt = ((Number) SyncInfo.get("rtt")).longValue();
            System.out.println("[BID] Using Offset: " + offset + "ms, RTT: " + rtt + "ms for auction " + auctionId);

            tempMbox = node.createMbox();
            String auctionProcessName = "auction_" + auctionId;

            OtpErlangObject[] payload = new OtpErlangObject[]{
                    tempMbox.self(),
                    new OtpErlangAtom("bid"),
                    new OtpErlangLong(userId),
                    new OtpErlangDouble(amount),
                    new OtpErlangString(username)
            };
            tempMbox.send(auctionProcessName, erlangWorkerNode, new OtpErlangTuple(payload));
            OtpErlangObject response = tempMbox.receive(5000);

            if (response instanceof OtpErlangTuple respTuple) {
                String status = ((OtpErlangAtom) respTuple.elementAt(0)).atomValue();

                if ("bid_accepted".equals(status)) {
                    // Update cache properly
                    timeRemainingCache.remove(auctionId);
                    Auction cached = activeAuctionsCache.get(auctionId);
                    if (cached != null) {
                        cached.setCurrentBid(amount);
                        cached.setHighBidder(username);
                    }
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
                    if (tuple.arity() == 4 && isAtom(tuple.elementAt(2), "get_next_auctions")) {
                        handleGetNextAuctions(tuple);
                    }
                    else if (isAtom(tuple.elementAt(0), "chat_msg")) {
                        handleIncomingChatMessage(tuple);
                    }
                    else if (tuple.arity() == 5 && isAtom(tuple.elementAt(0), "auction_closed")) {
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

    private void handleAuctionUnsold(OtpErlangTuple tuple) {
        try {
            Long itemId = ((OtpErlangLong) tuple.elementAt(1)).longValue();
            itemService.markItemPending(itemId);
            activeAuctionsCache.clear();
            timeRemainingCache.clear();
        } catch (Exception e) {
            System.err.println("Error in processing auction_unsold: " + e.getMessage());
        }
    }

    private void handleNewBid(OtpErlangTuple tuple) {
        try {
            Long auctionId = extractLong(tuple.elementAt(1));
            Double price = extractDouble(tuple.elementAt(2));
            String bidderName = extractString(tuple.elementAt(3));

            // Invalidate timer cache, update auction cache
            timeRemainingCache.remove(auctionId);
            Auction cached = activeAuctionsCache.get(auctionId);
            if (cached != null) {
                cached.setCurrentBid(price);
                cached.setHighBidder(bidderName);
            }

            long serverTime = getAbsoluteServerTime();
            long timeRemaining = getTimeRemaining(auctionId);
            long endTime = serverTime + (timeRemaining * 1000);

            // Unified WebSockets update
            Map<String, Object> update = new HashMap<>();
            update.put("type", "new_bid");
            update.put("price", price);
            update.put("bidder", bidderName);
            update.put("auctionId", auctionId);
            update.put("serverTime", serverTime);
            update.put("timeRemaining", timeRemaining);
            update.put("endTime", endTime);

            messagingTemplate.convertAndSend("/topic/auction/" + auctionId + "/updates", Optional.of(update));
            System.out.println("New Bid for Auction [" + auctionId + "] - Bidder: " + bidderName + ", Amount: " + price);
        } catch (Exception e) {
            System.err.println("Error handling new_bid: " + e.getMessage());
        }
    }

    private void handleBidRejected(OtpErlangTuple tuple) {
        try {
            Long auctionId = extractLong(tuple.elementAt(1));
            String user = extractString(tuple.elementAt(2));
            String reason = extractString(tuple.elementAt(3));

            System.err.println("Bid rejected for User " + user + " on Auction " + auctionId + ": " + reason);

            Map<String, Object> error = new HashMap<>();
            error.put("type", "bid_rejected");
            error.put("auctionId", auctionId);
            error.put("user", user);
            error.put("reason", reason);
            error.put("serverTime", getAbsoluteServerTime());
            messagingTemplate.convertAndSend("/topic/auction/" + auctionId + "/errors",
                    "Bid rejected for user " + user + ": " + reason.replace("_", " "));

        } catch (Exception e) {
            System.err.println("Error handling bid_rejected: " + e.getMessage());
        }
    }

    private void handleTimer(OtpErlangTuple tuple){
        try {
            Long auctionId = extractLong(tuple.elementAt(1));
            Double amount = extractDouble(tuple.elementAt(2));
            String bidderName = extractString(tuple.elementAt(3));

            timeRemainingCache.remove(auctionId);
            Auction cached = activeAuctionsCache.get(auctionId);
            if (cached != null) {
                cached.setCurrentBid(amount);
                cached.setHighBidder(bidderName);
            }

            long serverTime = getAbsoluteServerTime();
            long timeRemaining = getTimeRemaining(auctionId);
            long endTime = serverTime + (timeRemaining * 1000);

            Map<String, Object> update = new HashMap<>();
            update.put("type", "timer_extended");
            update.put("auctionId", auctionId);
            update.put("bidder", bidderName);
            update.put("price", amount);
            update.put("serverTime", serverTime);
            update.put("timeRemaining", timeRemaining);
            update.put("endTime", endTime);
            update.put("message", "Timer extended by 30 seconds");

            messagingTemplate.convertAndSend("/topic/auction/" + auctionId + "/updates", Optional.of(update));

            System.out.println("Timer Extended for Auction " + auctionId + " by 30 seconds");

        } catch (Exception e) {
            System.err.println("Error handling bid_accepted msg");
        }
    }

    private void handleIncomingChatMessage(OtpErlangTuple tuple) {
        try {
            String auctionIdStr = tuple.elementAt(1).toString();
            Long auctionId = Long.parseLong(auctionIdStr);
            String user = extractString(tuple.elementAt(2));
            String text = extractString(tuple.elementAt(3));

            System.out.println("Chat from Erlang [" + auctionId + "] " + user + ": " + text);

            auctionBidService.addChatMessage(auctionId, user, text);

            Map<String, Object> chatMsg = new HashMap<>();
            chatMsg.put("type", "chat");
            chatMsg.put("user", user);
            chatMsg.put("message", text);
            chatMsg.put("auctionId", auctionId);
            chatMsg.put("serverTime", getAbsoluteServerTime());

            messagingTemplate.convertAndSend("/topic/auction/" + auctionId, new ChatMessageDto(user, text));
        } catch (Exception e) {
            System.err.println("Error parsing chat msg: " + e.getMessage());
        }
    }

    private void handleAuctionClosed(OtpErlangTuple tuple) {
        try {
            Long itemId = ((OtpErlangLong) tuple.elementAt(1)).longValue();
            String winner = extractString(tuple.elementAt(2));
            String userName = extractString((tuple.elementAt(3)));
            Double price = ((OtpErlangDouble) tuple.elementAt(4)).doubleValue();

            itemService.closeItem(itemId, winner, price);
            activeAuctionsCache.clear();
            timeRemainingCache.clear();

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

                    activeAuctionsCache.clear();
                    timeRemainingCache.clear();
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

    public Map<String, Object> synchronizeWithAuctionServer(Long auctionId) {
        System.out.println("\n=== SYNC REQUEST for auction " + auctionId + " ===");
        OtpMbox tempMbox = null;
        try {
            System.out.println("Node status: " + (node != null ? node.node() : "null"));
            if (node == null) {
                System.err.println("ERROR: Node is null!");
                return null;
            }

            tempMbox = node.createMbox();
            String auctionProcessName = "auction_" + auctionId;

            long t1 = System.currentTimeMillis();
            System.out.println("T1 (client time vefore request): " + t1);

            OtpErlangObject[] msgPayload = new OtpErlangObject[]{
                    new OtpErlangAtom("get_time"), tempMbox.self()
            };

            OtpErlangTuple request = new OtpErlangTuple(msgPayload);
            tempMbox.send(auctionProcessName, erlangWorkerNode, request);

            OtpErlangObject response = tempMbox.receive(5000);

            long t2 = System.currentTimeMillis();
            System.out.println("T2 (client receive time): " + t2);

            if (response instanceof OtpErlangTuple respTuple) {
                if (respTuple.arity() == 2 &&
                        respTuple.elementAt(0) instanceof OtpErlangAtom &&
                        ((OtpErlangAtom) respTuple.elementAt(0)).atomValue().equals("time_response")) {
                    long serverTime = ((OtpErlangLong) respTuple.elementAt(1)).longValue();
                    System.out.println("Server time: " + serverTime);

                    long rtt = t2 - t1;
                    long offset = serverTime + (rtt / 2) - t2;

                    System.out.println("[JAVA SYNC] Auction " + auctionId +
                            ": RTT=" + rtt + "ms, Offset=" + offset + "ms");

                    return Map.of(
                            "offset", offset,
                            "rtt", rtt,
                            "server_time", serverTime,
                            "auction_id", auctionId,
                            "sync_time", System.currentTimeMillis()
                    );
                }
            }
        } catch (Exception e) {
            System.err.println("Sync failed for auction " + auctionId + ": " + e.getMessage());
        } finally {
            if (tempMbox != null) {
                node.closeMbox(tempMbox);
            }
        }
        return null;
    }

    public Map<String, Object> getAuctionSyncInfo(Long auctionId) {
        Map<String, Object> cached = auctionSyncCache.get(auctionId);
        if (cached != null) {
            long syncTime = ((Number) cached.get("sync_time")).longValue();
            if (System.currentTimeMillis() - syncTime < 10000) {
                return cached;
            }
        }
        Map<String, Object> synced = synchronizeWithAuctionServer(auctionId);
        if (synced != null) {
            auctionSyncCache.put(auctionId, synced);
        }
        return synced;
    }

    // FIX IMPORTANTE: Evita il blocco e timeout di 5 secondi se l'Asta 1 non esiste
    public long getAbsoluteServerTime() {
        if (!auctionSyncCache.isEmpty()) {
            Map<String, Object> anySync = auctionSyncCache.values().iterator().next();
            long offset = ((Number) anySync.get("offset")).longValue();
            return System.currentTimeMillis() + offset;
        }
        return System.currentTimeMillis();
    }

    public long getAuctionEndTime(Long auctionId) {
        long serverTime = getAbsoluteServerTime();
        long timeRemaining = getTimeRemaining(auctionId);
        return serverTime + (timeRemaining * 1000);
    }

    public long getTimeRemaining(Long auctionId) {
        Long cached = timeRemainingCache.get(auctionId);
        if (cached != null) {
            return cached;
        }
        OtpMbox tempMbox = null;
        try {
            tempMbox = node.createMbox();
            String auctionProcessName = "auction_" + auctionId;

            OtpErlangObject[] payload = new OtpErlangObject[]{
                    tempMbox.self(),
                    new OtpErlangAtom("get_remaining_time")
            };

            tempMbox.send(auctionProcessName, erlangWorkerNode, new OtpErlangTuple(payload));
            OtpErlangObject response = tempMbox.receive(2000);

            if (response instanceof OtpErlangTuple respTuple) {
                long timeRemaining = ((OtpErlangLong) respTuple.elementAt(1)).longValue();
                timeRemainingCache.put(auctionId, timeRemaining);
                return timeRemaining;
            }
        } catch (Exception e) {
            System.err.println("Error getting time remaining: " + e.getMessage());
        } finally {
            if (tempMbox != null) node.closeMbox(tempMbox);
        }
        return 0;
    }

    @Scheduled(fixedDelay = 1000)
    public void broadcastTimerUpdates() {
        List<Auction> auctions = fetchActiveAuctionsFromErlang();

        for (Auction auction : auctions) {
            Long auctionId = auction.getId();

            long serverTime = getAbsoluteServerTime();
            long timeRemaining = auction.getTimeRemaining();
            long endTime = serverTime + (timeRemaining * 1000);

            timeRemainingCache.put(auctionId, timeRemaining);

            Map<String, Object> timerUpdate = new HashMap<>();
            timerUpdate.put("type", "timer_update");
            timerUpdate.put("auctionId", auctionId);
            timerUpdate.put("timeRemaining", timeRemaining);
            timerUpdate.put("endTime", endTime);
            timerUpdate.put("serverTime", serverTime);
            timerUpdate.put("currentBid", auction.getCurrentBid());
            timerUpdate.put("highBidder", auction.getHighBidder());

            messagingTemplate.convertAndSend("/topic/auction/" + auctionId + "/timer", Optional.of(timerUpdate));
        }
    }

    private void initResync() {
        resyncExecutor = Executors.newScheduledThreadPool(1);
        resyncExecutor.scheduleAtFixedRate(() -> {
            long now = System.currentTimeMillis();
            auctionSyncCache.entrySet().removeIf(entry -> {
                long syncTime = ((Number) entry.getValue().get("sync_time")).longValue();
                boolean expired = (now - syncTime) > 30000;
                if (expired) {
                    System.out.println("[CACHE] Removing expired sync data for auction " + entry.getKey());
                }
                return expired;
            });
        }, 15, 15, TimeUnit.SECONDS);
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
        if (!activeAuctionsCache.isEmpty()) {
            return new ArrayList<>(activeAuctionsCache.values());
        }
        OtpMbox tempMbox = null;
        OtpErlangList auctionList = null;
        try {
            tempMbox = node.createMbox();
            OtpErlangObject[] request = new OtpErlangObject[]{
                    tempMbox.self(),
                    node.createRef(),
                    new OtpErlangAtom("get_active_auctions")
            };
            tempMbox.send("DS_auction_manager", erlangServerNode, new OtpErlangTuple(request));
            OtpErlangObject response = tempMbox.receive(5000);

            if (response instanceof OtpErlangTuple respTuple) {
                OtpErlangAtom status = (OtpErlangAtom) respTuple.elementAt(1);

                if ("active_auctions_response".equals(status.atomValue())) {
                    auctionList = (OtpErlangList) respTuple.elementAt(2);
                    List<Auction> auctions = mapErlangListToAuctions(auctionList);

                    for (Auction auction : auctions) {
                        activeAuctionsCache.put(auction.getId(), auction);
                    }

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

    public Auction getAuctionDetails(Long auctionId) {
        Auction cached = activeAuctionsCache.get(auctionId);
        if (cached != null) {
            return cached;
        }

        List<Auction> auctions = fetchActiveAuctionsFromErlang();
        for (Auction auction : auctions) {
            if (auction.getId().equals(auctionId)) {
                return auction;
            }
        }
        return null;
    }

    public Map<String, Object> getAuctionStatus(Long auctionId) {
        Map<String, Object> status = new HashMap<>();

        Map<String, Object> syncInfo = getAuctionSyncInfo(auctionId);
        long offset = ((Number) syncInfo.get("offset")).longValue();
        long serverTime = System.currentTimeMillis() + offset;
        long rtt = ((Number) syncInfo.get("rtt")).longValue();

        Auction auction = getAuctionDetails(auctionId);

        if (auction != null) {
            long timeRemaining = auction.getTimeRemaining();
            long endTime = serverTime + (timeRemaining * 1000);

            status.put("auctionId", auctionId);
            status.put("itemName", auction.getItem().getName());
            status.put("currentBid", auction.getCurrentBid());
            status.put("highBidder", auction.getHighBidder());
            status.put("timeRemaining", timeRemaining);
            status.put("endTime", endTime);
            status.put("serverTime", serverTime);

            status.put("timeSync", Map.of(
                    "offset", offset,
                    "rtt", rtt,
                    "algorithm", "Christian's Algorithm"
            ));
        }

        return status;
    }

    private List<Auction> mapErlangListToAuctions(OtpErlangList erlangList) {
        List<Auction> result = new ArrayList<>();

        for (OtpErlangObject obj : erlangList) {
            if (obj instanceof OtpErlangTuple auctionTuple) {
                try {
                    Long auctionId = extractLong(auctionTuple.elementAt(0));
                    Long itemId = extractLong(auctionTuple.elementAt(1));
                    Long timeLeft = extractLong(auctionTuple.elementAt(2));
                    Item item = itemService.getItemById(itemId);

                    if (item != null) {
                        // FIX: Mantiene l'offerta se già in cache per non farla resettare al prezzo base!
                        Auction existing = activeAuctionsCache.get(auctionId);
                        Double currentBid = (existing != null && existing.getCurrentBid() != null)
                                ? existing.getCurrentBid()
                                : item.getStartingPrice();
                        String highBidder = existing != null ? existing.getHighBidder() : null;

                        Auction auction = new Auction(
                                auctionId,
                                item,
                                timeLeft,
                                currentBid,
                                highBidder,
                                existing != null ? existing.getBidHistory() : new ArrayList<>(),
                                existing != null ? existing.getChatHistory() : new ArrayList<>()
                        );
                        result.add(auction);

                        timeRemainingCache.put(auctionId, timeLeft);
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