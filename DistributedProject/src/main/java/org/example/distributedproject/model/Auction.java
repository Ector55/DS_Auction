package org.example.distributedproject.model;

import java.util.List;

public class Auction {

    private Long id;
    private Item item;
    private Long duration;
    private Double startingPrice;
    private User winner;
    private List<String> chatHistory;
    private List<String> bidHistory;

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

    public List<String> getChatHistory() {
        return chatHistory;
    }

    public void setChatHistory(List<String> chatHistory) {
        this.chatHistory = chatHistory;
    }

    public List<String> getBidHistory() {
        return bidHistory;
    }

    public void setBidHistory(List<String> bidHistory) {
        this.bidHistory = bidHistory;
    }

    public Auction(Long id, Item item, Long duration, Double startingPrice, User winner, List<String> chatHistory, List<String> bidHistory) {
        this.id = id;
        this.item = item;
        this.duration = duration;
        this.startingPrice = startingPrice;
        this.winner = winner;
        this.chatHistory = chatHistory;
        this.bidHistory = bidHistory;
    }
}
