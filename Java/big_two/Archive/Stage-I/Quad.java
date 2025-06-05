/**
 * This hand consists of five cards, with four having the same rank. The card in
 * the quadruplet with the highest suit in a quad is referred to as the top card
 * of this quad. A quad always beats any straights, flushes, and full houses. A
 * quad having a top card with a higher rank beats a quad having a top card with
 * a lower rank.
 * @author Shaheer Ziya
 */
public class Quad extends Hand {

  /**
   * A constructor for building a hand with the specified player and list of cards
   * 
   * @param player The player holding the cards
   * @param cards  The cards in the player's hand
   */
  public Quad(CardGamePlayer player, CardList cards) {
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
      } else {
        return false;
      }
    } else if (this.getTopCard().compareTo(hand.getTopCard()) == 0) {
      return this.getTopCard().getSuit() > hand.getTopCard().getSuit();
    } else {
      return (this.getTopCard().compareTo(hand.getTopCard()) == 1);
    }
  }

  /* (non-Javadoc)
   * @see Hand#getTopCard()
   */
  public Card getTopCard() {
    if (this.isValid()) {
      this.sort();

      // The middle card will have the same rank as cards in ther quadruplet
      int quadRank = this.getCard(2).getRank();

      Card topCard = this.getCard(2);
      for (int i=0; i < this.size(); i++) {
        if (this.getCard(i).getRank() == quadRank &&
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
    
    // Four cards of same rank, one card of another rank
    int firstCardRank = this.getCard(0).getRank();
    int secondCardRank = this.getCard(1).getRank();
    int thirdCardRank = this.getCard(2).getRank();
    int fourthCardRank = this.getCard(3).getRank();
    int fifthCardRank = this.getCard(4).getRank();
    
    // Case 1. The first four cards are the same rank
    if (firstCardRank == secondCardRank) {
      if (thirdCardRank != firstCardRank || 
          fourthCardRank != firstCardRank) {
        return false;
      }
      return true;
    }
    
    // Case 2. The last four cards are the same rank
    else if (secondCardRank == thirdCardRank) {
      if (secondCardRank != fourthCardRank || 
          secondCardRank != fifthCardRank) {
        return false;
      }
      
      return true;
    }

    // Case 3. At least three different ranks in the hand
    return false;
  }


  /**
   * A method for returning a string specifying the type of
   * this hand.
   * 
   * @return String specifying the type of the hand
   */
  public String getType() {
    return "Quad";
  }
}
