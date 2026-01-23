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
    public UserDetails loadUserByUsername(String usernameInput) throws UsernameNotFoundException {
        User user = userRepository.findAll().stream()
                .filter(u -> u.getUserName().equals(usernameInput))
                .findFirst()
                .orElseThrow(() -> new UsernameNotFoundException("Utente non trovato con username: " + usernameInput));

        // Costruiamo l'utente per Spring
        return new org.springframework.security.core.userdetails.User(
                user.getUserName(),  // Usiamo userName come identificativo
                user.getPassword(),
                new ArrayList<>()
        );
    }
}