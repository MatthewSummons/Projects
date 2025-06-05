/**
 * This hand consists of five cards with the same suit. The card with the
 * highest rank in a flush is referred to as the top card of this flush. A flush
 * always beats any straights. A flush with a higher suit beats a flush with a
 * lower suit. For flushes with the same suit, the one having a top card with a
 * higher rank beats the one having a top card with a lower rank.
 */
public class Flush extends Hand {
  /**
   * A constructor for building a hand with the specified player and list of cards
   * 
   * @param player The player holding the cards
   * @param cards  The cards in the player's hand
   */
  public Flush(CardGamePlayer player, CardList cards) {
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
      } else if (this.getType() == "Straight") {
        return false;
      } else if (hand.getType() == "Straight") {
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

  /**
   * A method for checking if this is a valid hand.
   * 
   * @return True if a valid hand, False otherwise
   */
  public boolean isValid() {
    if (this.size() != 5) {
      return false;
    }

    // All cards must be of the same suit
    int firstCardSuit = this.getCard(0).getSuit();
    for (int i=0; i < this.size()-1; i++) {
      if (firstCardSuit != this.getCard(i+1).getSuit()) {
        return false;
      }
    }

    return true;
  }

  /**
   * A method for returning a string specifying the type of
   * this hand.
   * 
   * @return String specifying the type of the hand
   */
  public String getType() {
    return "Flush";
  }
  
}
