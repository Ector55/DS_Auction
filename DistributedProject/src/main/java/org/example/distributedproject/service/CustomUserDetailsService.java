package org.example.distributedproject.service;

import org.example.distributedproject.model.User;
import org.example.distributedproject.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.core.userdetails.UserDetails;
import org.springframework.security.core.userdetails.UserDetailsService;
import org.springframework.security.core.userdetails.UsernameNotFoundException;
import org.springframework.stereotype.Service;

import java.util.ArrayList;

@Service
public class CustomUserDetailsService implements UserDetailsService {

    @Autowired
    private UserRepository userRepository;

    @Override
    public UserDetails loadUserByUsername(String username) throws UsernameNotFoundException {
        // Cerca l'utente nel tuo DB (assicurati che UserRepository abbia findByUsername o simile)
        // Se nel tuo User.java il campo si chiama 'email' o 'name', usa quello.
        User user = userRepository.findAll().stream()
                .filter(u -> u.getName().equals(username)) // O u.getEmail()
                .findFirst()
                .orElseThrow(() -> new UsernameNotFoundException("Utente non trovato: " + username));

        // Ritorna l'oggetto User di Spring Security
        return new org.springframework.security.core.userdetails.User(
                user.getName(),
                user.getPassword(), // Deve essere criptata nel DB!
                new ArrayList<>() // Lista dei ruoli (vuota per ora)
        );
    }
}