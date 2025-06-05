import java.util.ArrayList;

/**
 * The Hand class is a subclass of the CardList class and is used to model a
 * hand of cards. It has a private instance variable for storing the player
 * who plays this hand. It also has methods for getting the player of this
 * hand, checking if it is a valid hand, getting the type of this hand,
 * getting the top card of this hand, and checking if it beats a specified hand.
 * 
 * @author Shaheer Ziya
 */
abstract public class Hand extends CardList {
  /**
   * The player who plays this hand.
   */
  private final CardGamePlayer player;
  
  /**
   * A constructor for building a hand with the specified player and list of cards
   * 
   * @param player The player holding the cards
   * @param cards The cards in the player's hand
   */
  public Hand(CardGamePlayer player, CardList cards) {
    this.player = player;
    
    // Store the cards in in Hand's CardList (ArrayList<Card>)
    for (int i=0; i < cards.size(); i++) {
      this.addCard(cards.getCard(i));
    }    
  }

  /**
   * A method for retrieving the player of this hand
   * @return The player holding the cards
   */
  public CardGamePlayer getPlayer() {
    return this.player;
  }

  /**
   * A method for retrieving the top card of this hand.
   * @return The card with the highest value
   */
  public Card getTopCard() {
    if (this.isValid()) {

      // Sort the cards in descending order
      this.sort();

      // The final card will be the highest value card
      return this.getCard(this.size()-1);
    }
    
    else {
      return null;
    }
  }

  
  /**
   * A method for checking if this hand beats a specified hand.
   * @param hand The hand being compared to.
   * @return True if the given hand beats player's hand, False otherwise
   */
  public boolean beats(Hand hand) {
    if (hand == null || !hand.isValid() || !this.isValid() ||  this.getType() != hand.getType()) {
      return false;
    } else if (this.getTopCard().compareTo(hand.getTopCard()) == 0) {
      return this.getTopCard().getSuit() > hand.getTopCard().getSuit();
    } else {
      BigTwoCard thisCard = new BigTwoCard(this.getTopCard().getSuit(), this.getTopCard().getRank());
      BigTwoCard otherCard = new BigTwoCard(hand.getTopCard().getSuit(), hand.getTopCard().getRank());
      return thisCard.compareTo(otherCard) > 0;
    }
  }

  // Abstract Methods

  /**
   * A method for checking if this is a valid hand.
   * @return True if a valid hand, False otherwise
   */
  abstract public boolean isValid();
  
  /**
   * A method for returning a string specifying the type of
   * this hand.
   * @return String specifying the type of the hand
   */
  abstract public String getType();
}
