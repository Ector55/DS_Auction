package org.example.distributedproject.service;

import org.example.distributedproject.model.User;
import org.example.distributedproject.repository.UserRepository;
import org.springframework.beans.factory.annotation.Autowired;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.stereotype.Service;

import java.util.Collections;
import java.util.List;

@Service
public class UserService {
    @Autowired
    private UserRepository userRepository;
    @Autowired
    private PasswordEncoder passwordEncoder;

    public List<User> findAll() {
        List<User> users = userRepository.findAll();
        if(users.isEmpty() || users == null){
            return Collections.emptyList();
        }
        return users;
    }

    public User findById(Long id){
        return userRepository.findById(id).orElse(null);
    }

    public User save(User user) {
        // Cripta la password prima di salvare
        String encodedPassword = passwordEncoder.encode(user.getPassword());
        user.setPassword(encodedPassword);
        return userRepository.save(user);
    }

    public void delete(Long id){
        userRepository.deleteById(id);
    }

    public User update(Long id, User user){
        User user1 = userRepository.findById(id).orElse(null);
        if(user1 == null){
            return null;
        }
        user1.setName(user.getName());
        user1.setPassword(user.getPassword());
        user1.setSurname(user.getSurname());
        user1.setAge(user.getAge());
        user1.setPaymentInfo(user.getPaymentInfo());
        user1.setEmail(user.getEmail());
        return userRepository.save(user1);
    }

    public User findByUserName(String userName) {
        return userRepository.findByUserName(userName).orElse(null);
    }

}
