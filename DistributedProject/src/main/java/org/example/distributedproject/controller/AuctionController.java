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


    @PostMapping("/{Id}/bid")
    public ResponseEntity<String> bid(@PathVariable Long Id, @RequestBody Double amount) {
        try {
            erlangService.placeBid(Id, amount);
        } catch (Exception e){
            return ResponseEntity.noContent().build();
        }

       return ResponseEntity.ok("Offer sent to the handler: " + erlangService.placeBid(Id, amount));
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

    // In AuctionController.java

    @GetMapping("/active")
    public List<Auction> getActiveAuctions() {
        // Chiama il metodo sincrono che interroga Erlang e aspetta la risposta
        return erlangService.fetchActiveAuctionsFromErlang();
    }

}