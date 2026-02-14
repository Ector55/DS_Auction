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

        // Retrieve the item from the database using the provided ID.
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
                // ✅ Aggiorna la chat IN MEMORIA (persiste finché l'asta non viene sovrascritta)
                auctionBidService.addBidMessage(id, currentUser, amount);

                return ResponseEntity.ok(Map.of(
                        "status", "success",
                        "message", currentUser + " offered " + String.format("%.2f", amount)
                ));
            }
            return ResponseEntity.badRequest().body("Bid rejected by auction system");
        } catch (Exception e) {
            return ResponseEntity.internalServerError().body("Bid processing failed");
        }
    }


  //Endpoint to handle sending chat messages within a specific auction.
    @PostMapping("/{id}/chat")
    public ResponseEntity<?> postChatMessage(@PathVariable int id, @RequestBody String message) {
        //Retrieve the currently authenticated username
        String currentUser = SecurityContextHolder.getContext().getAuthentication().getName();

        // Send the message to the specific auction's 'chat_handler' process on the Erlang node.
        erlangService.sendChatMessage(id, currentUser, message);

        //HTTP 200 OK
        return ResponseEntity.ok().build();
    }

    @GetMapping("/active")
    public ResponseEntity<List<Auction>> getActiveAuctions() {
        return ResponseEntity.ok(erlangService.fetchActiveAuctionsFromErlang());
    }

}