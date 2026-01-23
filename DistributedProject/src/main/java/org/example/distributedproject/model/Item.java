package org.example.distributedproject.model;


import jakarta.persistence.*;

@Entity
@Table(name = "oggetti")
public class Item {
    @Id
    @GeneratedValue(strategy = GenerationType.IDENTITY)
    private Long id;

    @Column(nullable = false)
    private String name;

    @Column(nullable = false)
    private String type;

    @Column(nullable = false)
    private String status;

    @Column(name = "starting_price", nullable = false)
    private Double startingPrice;

    @Column(nullable = false, length = 1000)
    private String description;

    public Item(){

    }

    public Item(Long id, String name, String type, String description, String status, Double startingPrice) {
        this.id = id;
        this.name = name;
        this.type = type;
        this.description = description;
        this.status = status;
        this.startingPrice = startingPrice;
    }

    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public String getName() {
        return name;
    }

    public void setName(String name) {
        this.name = name;
    }

    public String getType() {
        return type;
    }

    public void setType(String type) {
        this.type = type;
    }

    public String getDescription() {
        return description;
    }

    public void setDescription(String description) {
        this.description = description;
    }

    public String getStatus() {return status;}

    public void setStatus(String status) {this.status = status;}

    public Double getStartingPrice() {return startingPrice;}

    public void setStartingPrice(Double startingPrice) {this.startingPrice = startingPrice;}
}
