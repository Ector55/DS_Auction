package org.example.distributedproject.service;


import org.example.distributedproject.model.Item;
import org.example.distributedproject.repository.ItemRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

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
}
