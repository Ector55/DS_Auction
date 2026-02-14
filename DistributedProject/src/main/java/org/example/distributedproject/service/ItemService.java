package org.example.distributedproject.service;

import jakarta.transaction.Transactional;
import org.example.distributedproject.model.Item;
import org.example.distributedproject.repository.ItemRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.context.annotation.Lazy;
import org.springframework.stereotype.Service;

import java.io.PrintWriter;
import java.net.Socket;
import java.util.Collections;
import java.util.List;
import java.util.Optional;

@Service
public class ItemService {
    @Autowired
    private ItemRepository itemRepository;

    @Autowired
    @Lazy
    private ErlangService erlangService;

    public List<Item> findAll(){
        List<Item> items = itemRepository.findAll();
        if (items.isEmpty() || items == null){
            return Collections.emptyList();
        }
        return items;
    }

    public Item getItemById(Long id){
        return itemRepository.findById(id).orElse(null);
    }

    public Item save(Item item) {
        return itemRepository.save(item);
    }

    public Item update (Long id, Item itemEntrata) {
        Item itemEsistente = itemRepository.findById(id).orElse(null);
        if (itemEsistente == null){
            return null;
        }
        itemEsistente.setName(itemEntrata.getName());
        itemEsistente.setType(itemEntrata.getType());
        itemEsistente.setDescription(itemEntrata.getDescription());
        return itemRepository.save(itemEsistente);
    }

    @Transactional
    public void markItemPending(Long itemId) {

        try {
            Item item = itemRepository.findById(itemId).orElse(null);
            if (item != null) {
                item.setStatus("PENDING");

                Item saved = itemRepository.saveAndFlush(item);

                //itemRepository.save(item);
                System.out.println("Item " + itemId + " marked as PENDING (will be re-auctioned)");
            } else {
                System.err.println(" Item " + itemId + " not found!");
            }
        }  catch (Exception e) {
            System.err.println(" ERROR marking item as PENDING:");

            throw new RuntimeException(e);
        }
    }

    @Transactional
    public Item activateAndGetNextItem() {
        Optional<Item> itemOpt = itemRepository.findFirstByStatusOrderByIdAsc("PENDING");
        if (itemOpt.isPresent()) {
            Item item = itemOpt.get();
            item.setStatus("ACTIVE");
            return itemRepository.save(item); //updated object
        }
        return null; //no items available
    }

    @Transactional
    public void closeItem(Long itemId, String winner, Double price) {
        Item item = itemRepository.findById(itemId).orElse(null);
        if (item != null) {
            item.setStatus("SOLD");
            itemRepository.save(item);
            System.out.println("Item " + itemId + " marked as SOLD in the DB");
        }
    }

}
