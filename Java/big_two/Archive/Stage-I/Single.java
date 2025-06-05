/**
 * This hand consists of only one single card. The only card in a single is
 * referred to as the top card of this single. A single with a higher rank beats
 * a single with a lower rank. For singles with the same rank,
 * the one with a higher suit beats the one with a lower suit.
 * @author Shaheer Ziya
 */
public class Single extends Hand {

  /**
   * A constructor for building a hand with the specified player and list of cards
   * 
   * @param player The player holding the cards
   * @param cards  The cards in the player's hand
   */
  public Single(CardGamePlayer player, CardList cards) {
    super(player, cards);
  }

  /**
   * A method for checking if this is a valid hand.
   * 
   * @return True if a valid hand, False otherwise
   */
  public boolean isValid() {
    if (this.size() != 1) {
      return false;
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
    return "Single";
  }
}
