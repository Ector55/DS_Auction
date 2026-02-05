package org.example.distributedproject.controller;

import org.example.distributedproject.model.Item;
import org.example.distributedproject.repository.ItemRepository;
import org.example.distributedproject.service.ErlangService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

//Controller responsible for handling Auction-related operations.
//It acts as a bridge between the frontend, the database, and the Erlang backend node.
@RestController
@RequestMapping("/api/auctions")
public class AuctionController {

    @Autowired
    private ItemRepository itemRepository;

    //Service used to communicate with the distributed Erlang node/cluster
   @Autowired
   private ErlangService erlangService;

    //Endpoint to finalize an auction.
    @PostMapping("/close")
    public void closeAuction(@RequestParam Long itemId, @RequestParam String winner, @RequestParam Double price) {

        System.out.println("Erlang closed auction " + itemId + ". Winner: " + winner + " Price: " + price);

        // Retrieve the item from the database using the provided ID.
        Item item = itemRepository.findById(itemId).orElse(null);

        //If the item exists in the database, update its status
        if (item != null) {
            item.setStatus("SOLD");
            itemRepository.save(item);
        }
    }


    @PostMapping("/{itemId}/bid")
    public ResponseEntity<String> bid(@PathVariable Long itemId, @RequestParam Double amount) {
       erlangService.placeBid(itemId, amount);
       return ResponseEntity.ok("Offer sent to the handler!");
    }


  //Endpoint to handle sending chat messages within a specific auction.
    @PostMapping("/{id}/chat")
    public ResponseEntity<?> postChatMessage(@PathVariable int id, @RequestParam String message) {
        //Retrieve the currently authenticated username
        String currentUser = SecurityContextHolder.getContext().getAuthentication().getName();

        // Send the message to the specific auction's 'chat_handler' process on the Erlang node.
        erlangService.sendChatMessage(id, currentUser, message);

        //HTTP 200 OK
        return ResponseEntity.ok().build();
    }

}