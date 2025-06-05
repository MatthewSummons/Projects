/**
 * This hand consists of two cards with the same rank. The card with a higher suit 
 * in a pair is referred to as the top card of this pair. A pair with a higher rank beats a
 * pair with a lower rank. For pairs with the same rank, the one containing the highest
 * suit beats the other.
 * @author Shaheer Ziya
 */
public class Pair extends Hand {
  /**
   * A constructor for building a hand with the specified player and list of cards
   * 
   * @param player The player holding the cards
   * @param cards  The cards in the player's hand
   */
  public Pair(CardGamePlayer player, CardList cards) {
    super(player, cards);
  }

  /**
   * A method for checking if this is a valid hand.
   * 
   * @return True if a valid hand, False otherwise
   */
  public boolean isValid() {
    if (this.size() != 2) {
      return false;
    } else {
      Card firstCard = this.getCard(0);
      Card secondCard = this.getCard(1);
      if (firstCard.getRank() == secondCard.getRank()) {
        return true;
      } else {
        return false;
      }
    }
  }
  
  /**
   * A method for returning a string specifying the type of
   * this hand.
   * 
   * @return String specifying the type of the hand
   */
  public String getType() {
    return "Pair";
  }
}
