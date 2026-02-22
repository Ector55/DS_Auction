package org.example.distributedproject.controller;
import java.util.List;

import org.example.distributedproject.model.Item;
import org.example.distributedproject.service.ItemService;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.http.HttpStatus;
import org.springframework.http.ResponseEntity;
import org.springframework.web.bind.annotation.GetMapping;
import org.springframework.web.bind.annotation.PathVariable;
import org.springframework.web.bind.annotation.PostMapping;
import org.springframework.web.bind.annotation.PutMapping;
import org.springframework.web.bind.annotation.RequestBody;
import org.springframework.web.bind.annotation.RequestMapping;
import org.springframework.web.bind.annotation.RestController;
//Controller responsible for handling Item-related operations.

@RestController
@RequestMapping("/api/items")
public class ItemController {
    @Autowired
    private ItemService itemService;

    @GetMapping
    public ResponseEntity<List<Item>> getAllItems() {
        return ResponseEntity.ok(itemService.findAll());
    }
    //endpoint to fetch the  item 
    @GetMapping("/{itemId}")
    public ResponseEntity<Item> getItemById(@PathVariable Long itemId) {
        Item item = itemService.getItemById(itemId);
        if (item != null) {
            return ResponseEntity.ok(item);
        } else {
            return ResponseEntity.notFound().build();
        }
    }

    //to save the items
    @PostMapping()
    public ResponseEntity<Item> saveItem(@RequestBody Item item) {
        item.setStatus("PENDING");
        Item item1 = itemService.save(item);
        return new ResponseEntity<>(item1, HttpStatus.CREATED);
    }

    //update item details
    @PutMapping("/{itemId}")
    public ResponseEntity<Item> updateItem(@PathVariable Long itemId, @RequestBody Item item) {
        Item item1 = itemService.update(itemId, item);
        return ResponseEntity.ok(item1);
    }

}
