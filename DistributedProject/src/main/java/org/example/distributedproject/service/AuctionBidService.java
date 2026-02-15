package org.example.distributedproject.service;

import org.example.distributedproject.model.Auction;
import org.springframework.stereotype.Service;

import java.util.ArrayList;
import java.util.List;
import java.util.Map;
import java.util.concurrent.ConcurrentHashMap;
import java.util.concurrent.CopyOnWriteArrayList;

@Service
public class AuctionBidService {

    private final Map<Long, List<String>> bidHistory = new ConcurrentHashMap<>();
    private final Map<Long, List<String>> chatHistoryMap = new ConcurrentHashMap<>();
    private final Map<Long, Long> currentItemId = new ConcurrentHashMap<>();

    public void syncWithErlangState(List<Auction> activeAuctions) {
        for (Auction auction : activeAuctions) {
            Long auctionId = auction.getId();
            Long newItemId = auction.getItem().getId();

            //heck if the item ID exists and is different (The auction has moved to a new item)
            if (currentItemId.containsKey(auctionId) &&
                    !currentItemId.get(auctionId).equals(newItemId)) {

                bidHistory.put(auctionId, new CopyOnWriteArrayList<>());

                //reset Chat History
                chatHistoryMap.put(auctionId, new CopyOnWriteArrayList<>());

                System.out.println("Chat and Bid history reset for Auction ID: " + auctionId);
            }

            currentItemId.put(auctionId, newItemId);
            List<String> currentBids = bidHistory.getOrDefault(auctionId, new ArrayList<>());
            auction.setBidHistory(new ArrayList<>(currentBids));

            //same thing for the chat
            List<String> currentChat = chatHistoryMap.getOrDefault(auctionId, new ArrayList<>());
            auction.setChatHistory(new ArrayList<>(currentChat));
        }
    }

    public void addBidMessage(Long auctionId, String username, Double amount) {
        String message = String.format("%s offered %.2f", username, amount);
        bidHistory
                .computeIfAbsent(auctionId, k -> new CopyOnWriteArrayList<>())
                .add(message);
    }

    public void addChatMessage(Long auctionId, String username, String messageText) {
        String formattedMessage = username + ": " + messageText; //format username:message
        chatHistoryMap
                .computeIfAbsent(auctionId, k -> new CopyOnWriteArrayList<>())
                .add(formattedMessage);
    }
}
