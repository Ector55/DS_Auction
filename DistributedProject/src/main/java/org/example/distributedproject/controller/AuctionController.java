package org.example.distributedproject.controller;

import org.example.distributedproject.model.Item;
import org.example.distributedproject.repository.ItemRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RequestParam;
import org.springframework.web.bind.annotation.RestController;

@RestController
@RequestMapping("/api/auctions")
public class AuctionController {

    @Autowired
    private ItemRepository itemRepository;

    @PostMapping("/close")
    public void closeAuction(@RequestParam Long itemId, @RequestParam String winner, @RequestParam Double price) {
        System.out.println("Erlang ha chiuso l'asta " + itemId + ". Vin: " + winner + "Price: " + price);
        Item item = itemRepository.findById(itemId).orElse(null);
        if(item != null) {
            item.setStatus("SOLD");
            itemRepository.save(item);
        }
    }
}
