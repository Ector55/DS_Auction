package org.example.distributedproject.controller;

import org.example.distributedproject.model.Auction;
import org.example.distributedproject.model.Item;
import org.example.distributedproject.repository.ItemRepository;
import org.example.distributedproject.service.ErlangService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

import java.util.List;

//Controller responsible for handling Auction-related operations.
@RestController
@RequestMapping("/api/auctions")
public class AuctionController {

    @Autowired
    private ItemRepository itemRepository;

   @Autowired
   private ErlangService erlangService;

    @PostMapping("/close")
    public void closeAuction(@RequestParam Long itemId, @RequestBody String winner, @RequestBody Double price) {

        System.out.println("Erlang closed auction " + itemId + ". Winner: " + winner + " Price: " + price);
        Item item = itemRepository.findById(itemId).orElse(null);
        if (item != null) {
            item.setStatus("SOLD");
            itemRepository.save(item);
        }
    }

    @PostMapping("/{Id}/bid")
    public ResponseEntity<String> bid(@PathVariable Long Id, @RequestBody Double amount) {
        try {
            erlangService.placeBid(Id, amount);
        } catch (Exception e){
            return ResponseEntity.noContent().build();
        }
       return ResponseEntity.ok("Offer sent to the handler: " + erlangService.placeBid(Id, amount));
    }

    @PostMapping("/{id}/chat")
    public ResponseEntity<?> postChatMessage(@PathVariable int id, @RequestBody String message) {
        String currentUser = SecurityContextHolder.getContext().getAuthentication().getName();
        erlangService.sendChatMessage(id, currentUser, message);
        return ResponseEntity.ok().build();
    }

    @GetMapping("/active")
    public List<Auction> getActiveAuctions() {
        return erlangService.fetchActiveAuctionsFromErlang();
    }

}