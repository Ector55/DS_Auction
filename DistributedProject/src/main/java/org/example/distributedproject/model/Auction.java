package org.example.distributedproject.model;

public class Auction {

    private Long id;
    private Item item;
    private Long duration;
    private Double startingPrice;
    private User winner;


    public Long getId() {
        return id;
    }

    public void setId(Long id) {
        this.id = id;
    }

    public Item getItem() {
        return item;
    }

    public void setItem(Item item) {
        this.item = item;
    }

    public Long getDuration() {
        return duration;
    }

    public void setDuration(Long duration) {
        this.duration = duration;
    }

    public Double getStartingPrice() {
        return startingPrice;
    }

    public void setStartingPrice(Double startingPrice) {
        this.startingPrice = startingPrice;
    }

    public User getWinner() {
        return winner;
    }

    public void setWinner(User winner) {
        this.winner = winner;
    }

    public Auction(Long id, Item item, Long duration, Double startingPrice, User winner) {
        this.id = id;
        this.item = item;
        this.duration = duration;
        this.startingPrice = startingPrice;
        this.winner = winner;
    }
}
