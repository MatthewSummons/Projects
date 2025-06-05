/**
 * This hand consists of five cards, with two having the same rank and three
 * having another same rank. The card in the triplet with the highest suit in a
 * full house is referred to as the top card of this full house. A full house
 * always beats any straights and flushes. A full house having a top card with a
 * higher rank beats a full house having a top card with a lower rank.
 */
public class FullHouse extends Hand {

  /**
   * A constructor for building a hand with the specified player and list of cards
   * 
   * @param player The player holding the cards
   * @param cards  The cards in the player's hand
   */
  public FullHouse(CardGamePlayer player, CardList cards) {
    super(player, cards);
  }

  /**
   * A method for checking if this hand beats a specified hand.
   * 
   * @param hand The hand being compared to.
   * @return True if the given hand beats player's hand, False otherwise
   */
  public boolean beats(Hand hand) {
    if (hand == null || !hand.isValid() || !this.isValid()) {
      return false;
    } else if (this.getType() != hand.getType()) {
      if (this.size() != 5 || hand.size() != 5) {
        return false;
      } else if (hand.getType() == "Straight") {
        return true;
      } else if (hand.getType() == "Flush") {
        return true;
      } else {
        return false;
      }
    } else if (this.getTopCard().compareTo(hand.getTopCard()) == 0) {
      return this.getTopCard().getSuit() > hand.getTopCard().getSuit();
    } else {
      return (this.getTopCard().compareTo(hand.getTopCard()) == 1);
    }
  }

  /*
   * (non-Javadoc)
   * 
   * @see Hand#getTopCard()
   */
  public Card getTopCard() {
    if (this.isValid()) {
      this.sort();

      // The middle card will have the same rank as cards in the triple
      int tripleRank = this.getCard(2).getRank();

      Card topCard = this.getCard(2);
      for (int i = 0; i < this.size(); i++) {
        if (this.getCard(i).getRank() == tripleRank &&
            this.getCard(i).getSuit() > topCard.getSuit()) {
          topCard = this.getCard(i);
        }
      }

      return topCard;
    }

    else {
      return null;
    }
  }


  /**
   * A method for checking if this is a valid hand.
   * 
   * @return True if a valid hand, False otherwise
   */
  public boolean isValid() {
    if (this.size() != 5) {
      return false;
    }
    
    // We sort so cards of the same rank are together.
    this.sort();
    // Two cards of same rank, three card of another rank
    // Case 1. First two cards are same, last three are same
    if (this.getCard(0).getRank() == this.getCard(1).getRank() &&
        this.getCard(2).getRank() == this.getCard(3).getRank() &&
        this.getCard(3).getRank() == this.getCard(4).getRank()) {
      return true;
    }
    // Case 2. First three cards of the same, last two are same
    if (this.getCard(0).getRank() == this.getCard(1).getRank() &&
        this.getCard(1).getRank() == this.getCard(2).getRank() &&
        this.getCard(3).getRank() == this.getCard(4).getRank()) {
          return true;
        }
    return false;
  }


  /**
   * A method for returning a string specifying the type of
   * this hand.
   * 
   * @return String specifying the type of the hand
   */
  public String getType() {
    return "FullHouse";
  }
  
}
