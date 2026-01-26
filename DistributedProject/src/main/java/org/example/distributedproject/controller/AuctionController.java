package org.example.distributedproject.controller;

import org.example.distributedproject.model.Item;
import org.example.distributedproject.repository.ItemRepository;
import org.example.distributedproject.service.ErlangService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.ResponseEntity;
import org.springframework.security.core.context.SecurityContextHolder;
import org.springframework.web.bind.annotation.*;

@RestController
@RequestMapping("/api/auctions")
public class AuctionController {

    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    private ErlangService erlangService;

    @PostMapping("/close")
    public void closeAuction(@RequestParam Long itemId, @RequestParam String winner, @RequestParam Double price) {
        System.out.println("Erlang ha chiuso l'asta " + itemId + ". Vin: " + winner + "Price: " + price);
        Item item = itemRepository.findById(itemId).orElse(null);
        if(item != null) {
            item.setStatus("SOLD");
            itemRepository.save(item);
        }
    }

    @PostMapping("/{id}/chat")
    public ResponseEntity<?> postChatMessage(@PathVariable int id, @RequestParam String message) {
        String currentUser = SecurityContextHolder.getContext().getAuthentication().getName();

        // Invia il messaggio al processo Erlang chat_handler dell'asta specifica
        erlangService.sendChatMessage(id, currentUser, message);

        return ResponseEntity.ok().build();
    }
}
