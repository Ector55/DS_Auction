package org.example.distributedproject.controller;

import java.util.List;
import java.util.Map;

import org.example.distributedproject.model.Auction;
import org.example.distributedproject.model.Item;
import org.example.distributedproject.repository.ItemRepository;
import org.example.distributedproject.service.AuctionBidService;
import org.example.distributedproject.service.ErlangService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

//Controller responsible for handling Auction-related operations.
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
    //endpoint to bid
    @PostMapping("/{id}/bid")
    public ResponseEntity<?> bid(@PathVariable Long id, @RequestBody Double amount) {
        String currentUser = SecurityContextHolder.getContext().getAuthentication().getName();
        try {
            String response = erlangService.placeBid(id, amount);
            if ("bid_accepted".equals(response)) {
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
        String currentUser = SecurityContextHolder.getContext().getAuthentication().getName();
        erlangService.sendChatMessage(id, currentUser, message);
        return ResponseEntity.ok().build(); //200 OK message
    }

    //endpoint to fetch active auctions
    @GetMapping("/active")
    public ResponseEntity<List<Auction>> getActiveAuctions() {
        return ResponseEntity.ok(erlangService.fetchActiveAuctionsFromErlang());
    }

}