package org.example.distributedproject.controller;

import org.example.distributedproject.model.Auction;
import org.example.distributedproject.model.Item;
import org.example.distributedproject.repository.ItemRepository;
import org.example.distributedproject.service.AuctionBidService;
import org.example.distributedproject.service.ErlangService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.util.ArrayList;
import java.util.HashMap;
import java.util.List;
import java.util.Map;

//Controller responsible for handling Auction-related operations.
//It acts as a bridge between the frontend, the database, and the Erlang backend node.
@RestController
@RequestMapping("/api/auctions")
public class AuctionController {

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private AuctionBidService auctionBidService;

   @Autowired
   private ErlangService erlangService;

    //Endpoint to finalize an auction.
    @PostMapping("/close")
    public void closeAuction(@RequestParam Long itemId, @RequestBody String winner, @RequestBody Double price) {
        System.out.println("Erlang closed auction " + itemId + ". Winner: " + winner + " Price: " + price);
        Item item = itemRepository.findById(itemId).orElse(null);
        //If the item exists in the database, update its status
        if (item != null) {
            item.setStatus("SOLD");
            itemRepository.save(item);
        }
    }

    @PostMapping("/{id}/bid")
    public ResponseEntity<?> bid(@PathVariable Long id, @RequestBody Double amount) {
        String currentUser = SecurityContextHolder.getContext().getAuthentication().getName();
        try {
            String response = erlangService.placeBid(id, amount);
            if ("bid_accepted".equals(response)) {
                auctionBidService.addBidMessage(id, currentUser, amount);
                // Get updated status with synchronized time
                Map<String, Object> status = erlangService.getAuctionStatus(id);

                Map<String, Object> result = new HashMap<>();
                result.put("status", "success");
                result.put("message", currentUser + " offered " + String.format("%.2f", amount));
                result.put("currentBid", amount);
                result.put("highBidder", currentUser);
                result.put("timeRemaining", status.get("timeRemaining"));
                result.put("endTime", status.get("endTime"));
                result.put("serverTime", status.get("serverTime"));
                result.put("timeSync", status.get("timeSync"));

                return ResponseEntity.ok(result);
            }
            return ResponseEntity.badRequest().body(Map.of(
                    "status", "error",
                    "message", response
            ));
        } catch (Exception e) {
            return ResponseEntity.internalServerError().body(Map.of(
                    "status", "error",
                    "message", "Bid processing failed"
            ));
        }
    }


  //Endpoint to handle sending chat messages within a specific auction.
    @PostMapping("/{id}/chat")
    public ResponseEntity<?> postChatMessage(@PathVariable int id, @RequestBody String message) {
        String currentUser = SecurityContextHolder.getContext().getAuthentication().getName();
        erlangService.sendChatMessage(id, currentUser, message);
        return ResponseEntity.ok(Map.of(
                "status", "ok",
                "message", "Chat sent",
                "serverTime", erlangService.getAbsoluteServerTime()
        ));
    }

    @GetMapping("/active")
    public ResponseEntity<?> getActiveAuctions() {
        List<Auction> auctions = erlangService.fetchActiveAuctionsFromErlang();

        List<Map<String, Object>> processedAuctions = new ArrayList<>();
        long serverTime = erlangService.getAbsoluteServerTime();

        for (Auction auction : auctions) {
            Map<String, Object> auctionData = new HashMap<>();
            auctionData.put("id", auction.getId());
            auctionData.put("itemName", auction.getItem().getName());
            auctionData.put("currentBid", auction.getCurrentBid());
            auctionData.put("highBidder", auction.getHighBidder());

            // Calculate absolute end time using synchronized server time
            long timeRemaining = auction.getTimeRemaining();
            long endTime = serverTime + (timeRemaining * 1000);

            auctionData.put("timeRemaining", timeRemaining);
            auctionData.put("endTime", endTime);
            auctionData.put("serverTime", serverTime);

            processedAuctions.add(auctionData);
        }

        return ResponseEntity.ok(Map.of(
                "auctions", processedAuctions,
                "serverTime", serverTime
        ));
        //return ResponseEntity.ok(erlangService.fetchActiveAuctionsFromErlang());
    }

    @GetMapping("/{id}")
    public ResponseEntity<?> getAuction(@PathVariable Long id) {
        Map<String, Object> status = erlangService.getAuctionStatus(id);

        if (!status.isEmpty()) {
            return ResponseEntity.ok(status);
        }

        return ResponseEntity.notFound().build();
    }

    @GetMapping("/{id}/time-sync")
    public ResponseEntity<?> getAuctionTimeSync(@PathVariable Long id) {
        Map<String, Object> syncInfo = erlangService.getAuctionSyncInfo(id);
        if (syncInfo != null) {
            return ResponseEntity.ok(syncInfo);
        }
        return ResponseEntity.status(500).body("Failed to synchronize with auction server");
    }

}