/**
 * This hand consists of three cards with the same rank. The card with the
 * highest suit in a triple is referred to as the top card of this triple. A
 * triple with a higher rank beats a triple with a lower rank.
 * @author Shaheer Ziya
 */
public class Triple extends Hand {

  /**
   * A constructor for building a hand with the specified player and list of cards
   * 
   * @param player The player holding the cards
   * @param cards  The cards in the player's hand
   */
  public Triple(CardGamePlayer player, CardList cards) {
    super(player, cards);
  }

  /**
   * A method for checking if this is a valid hand.
   * 
   * @return True if a valid hand, False otherwise
   */
  public boolean isValid() {
    if (this.size() != 3) {
      return false;
    }

    // All cards must be of the same rank
    for (int i=0; i < this.size(); i++) {
      if (this.getCard(i).getRank() != this.getCard(0).getRank()) {
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
    return "Triple";
  }
  
}
