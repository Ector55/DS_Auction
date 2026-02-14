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

    private final Map<Long, Long> currentItemId = new ConcurrentHashMap<>();

    /**
     * DA CHIAMARE SUBITO DOPO aver ottenuto le aste da Erlang
     * Resetta automaticamente la chat se l'item è cambiato
     */
    public void syncWithErlangState(List<Auction> activeAuctions) {
        for (Auction auction : activeAuctions) {
            Long auctionId = auction.getId();
            Long newItemId = auction.getItem().getId();

            // Resetta SOLO se l'ID era presente ED è diverso (l'asta è cambiata davvero)
            if (currentItemId.containsKey(auctionId) &&
                    !currentItemId.get(auctionId).equals(newItemId)) {

                bidHistory.put(auctionId, new CopyOnWriteArrayList<>());
                System.out.println("✅ Chat resettata per asta " + auctionId);
            }

            currentItemId.put(auctionId, newItemId);

            List<String> history = bidHistory.getOrDefault(auctionId, new ArrayList<>());
            auction.setBidHistory(new ArrayList<>(history));
        }
    }

    /**
     * Aggiunge un messaggio DOPO che Erlang ha accettato l'offerta
     */
    public void addBidMessage(Long auctionId, String username, Double amount) {
        String message = String.format("%s offered %.2f", username, amount);
        bidHistory
                .computeIfAbsent(auctionId, k -> new CopyOnWriteArrayList<>())
                .add(message);
    }
}
