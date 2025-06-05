/**
 * This hand consists of five cards with consecutive ranks and the same suit.
 * For the sake of simplicity, 2 and A can only form a straight flush with K but
 * not with 3. The card with the highest rank in a straight flush is referred to
 * as the top card of this straight flush. A straight flush always beats any
 * straights, flushes, full houses, and quads. A straight flush having a top
 * card with a higher rank beats a straight flush having a top card with a lower
 * rank. For straight flushes having top cards with the same rank, the one
 * having a top card with a higher suit beats the one having a top card with a
 * lower suit.
 * @author Shaheer Ziya
 */
public class StraightFlush extends Hand {

  /**
   * A constructor for building a hand with the specified player and list of cards
   * 
   * @param player The player holding the cards
   * @param cards  The cards in the player's hand
   */
  public StraightFlush(CardGamePlayer player, CardList cards) {
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
      } else if (hand.getType() == "FullHouse") {
        return true;
      } else if (hand.getType() == "Quad") {
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

    int firstCardSuit = this.getCard(0).getSuit();
    
    // All cards must have the same suit
    for (int i=0; i < this.size(); i++) {
      if (this.getCard(i).getSuit() != firstCardSuit) {
        return false;
      }
    }

    // All cards must differ by 1 if sorted
    this.sort();
    
    // Check for special case of 2 and A
    if ((this.getCard(0).getRank() == this.getCard(1).getRank() - 1) &&
        (this.getCard(1).getRank() == this.getCard(2).getRank() - 1) &&
        (this.getCard(2).getRank() == this.getCard(3).getRank() + 12) &&
        (this.getCard(3).getRank() == this.getCard(4).getRank() - 1)) {
      return true;
    }

    for (int i = 0; i < this.size() - 1; i++) {
      if (this.getCard(i).getRank() != this.getCard(i + 1).getRank() - 1) {
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
    return "StraightFlush";
  }
  
}
