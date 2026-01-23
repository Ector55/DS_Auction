package org.example.distributedproject.config;

import org.springframework.context.annotation.Bean;
import org.springframework.context.annotation.Configuration;
import org.springframework.security.config.Customizer;
import org.springframework.security.config.annotation.web.builders.HttpSecurity;
import org.springframework.security.config.annotation.web.configuration.EnableWebSecurity;
import org.springframework.security.config.annotation.web.configurers.AbstractHttpConfigurer;
import org.springframework.security.crypto.bcrypt.BCryptPasswordEncoder;
import org.springframework.security.crypto.password.PasswordEncoder;
import org.springframework.security.web.SecurityFilterChain;

@Configuration
@EnableWebSecurity
public class SpringSecurity {

    @Bean
    public SecurityFilterChain filterChain(HttpSecurity http) throws Exception {
        http
                .csrf(AbstractHttpConfigurer::disable) // Disabilita CSRF (necessario per POST da Postman/Erlang)
                .authorizeHttpRequests(auth -> auth
                        // 1. Endpoint pubblici (Registrazione e API per Erlang)
                        .requestMatchers("/api/users/register", "/api/items/next", "/api/auctions/close").permitAll()
                        // 2. Tutto il resto richiede autenticazione (Basic Auth)
                        .anyRequest().authenticated()
                )
                .httpBasic(Customizer.withDefaults()); // Abilita Basic Auth
        return http.build();
    }
    // Bean per criptare le password (Obbligatorio)
    @Bean
    public PasswordEncoder passwordEncoder() {
        return new BCryptPasswordEncoder();
    }
}
