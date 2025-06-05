# Class for Categorizing Cards in a Deck
# Created by Shaheer Ziya on UTC+08 17:01

class Card:
    """A Class representing Cards"""
    
    
    def __init__(self, rank, suit):
        self.rank = rank
        self.suit = suit
    
    
    def getRank(self):
        return self.rank
    
    
    def getSuit(self):
        return self.suit
    
    
    def value(self):
        # If Ace to 10
        if self.getRank() <= 10:
            return self.getRank()
        # Jacks, Queens, Kings valued at 10
        else: return 10
    
    
    def __str__(self):
        # Determine Suit of Cards
        if self.getSuit() == 'd':
            Suit = "Diamonds"
        elif self.getSuit() == 'c':
            Suit = "Clubs"
        elif self.getSuit() == 'h':
            Suit = "Hearts"
        else:
            Suit = "Spades"
        # Map b/w Rank & Value of Cards
        valueDict = {
            1: "Ace", 2: "Two", 3: "Three", 4: "Four", 5: "Five", 6: "Six",
            7: "Seven", 8: "Eight", 9: "Nine", 10: "Ten", 11: "Jack", 12: "Queen",
            13: "King"
        }
        # Return Value of Card in String
        return valueDict[self.getRank()] + " of " + Suit
    
    
    def __repr__(self):
        return f"Card({self.getRank()},\'{self.getSuit()}\')"