package org.example.distributedproject.service;


import jakarta.transaction.Transactional;
import org.example.distributedproject.model.Item;
import org.example.distributedproject.repository.ItemRepository;
import org.springframework.beans.factory.annotation.Autowired;
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

    public boolean placeBid(Long itemId, Double amount, String userId) {
        String erlangHost = "localhost"; //65
        int erlangPort = 9000;

        try (Socket socket = new Socket(erlangHost, erlangPort)) {
            PrintWriter out = new PrintWriter(socket.getOutputStream(), true);
            String command = "BID:" + itemId + ":" + amount + ":" + userId;
            out.println(command);
            System.out.println(">> Command sent to Erlang: " + command);
            return true;
        } catch (Exception e) {
            System.err.println("ERROR: It is not possible to contact the Erlang Server!");
            e.printStackTrace();
            return false;
        }
    }

    @Transactional
    public String getNextPendingItem() {
        Optional<Item> itemOpt = itemRepository.findFirstByStatusOrderByIdAsc("PENDING");
        if (itemOpt.isPresent()) {
            Item item = itemOpt.get();
            item.setStatus("ACTIVE");
            itemRepository.save(item);
            return item.getId() + "," + item.getName() + "," + item.getStartingPrice();
        }
        return "NO_ITEMS";
    }

}
