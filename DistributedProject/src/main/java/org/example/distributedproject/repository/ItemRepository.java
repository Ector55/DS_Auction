package org.example.distributedproject.repository;

import org.example.distributedproject.model.Item;
import org.springframework.data.jpa.repository.JpaRepository;

import java.util.Optional;

public interface ItemRepository extends JpaRepository<Item, Long> {

    Optional<Item> findFirstByStatusOrderByIdAsc(String pending);
}
