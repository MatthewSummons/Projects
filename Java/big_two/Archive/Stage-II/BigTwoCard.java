/**
 * The BigTwoCard class is a subclass of the Card class and is used to model a card used in a 
 * Big Two card game.
 * 
 * @author Shaheer Ziya
 */
public class BigTwoCard extends Card {

  /**
   * A constructor for building a card with the specified suit and rank.
   * 
   * @param suit It is an integer between 0 and 3
   * @param rank It is is an integer between 0 and 12.
   */
  public BigTwoCard(int suit, int rank) {
    super(suit, rank);
  }

  /**
   * A method for comparing the order of this card with the specified card.
   * The order of ranks from high to low is 2, A, K, Q, J, 10, 9, 8, 7, 6, 5, 4,
   * 3.
   * 
   * @param card The card being compared to the instance of this BigTwo Card
   * @return Returns a negative integer, zero, or a positive integer when this
   *         card is less than, equal to, or greater than the specified card.
   */
  public int compareTo(Card card) {
    int thisRank = this.rank;
    int thatRank = card.getRank();
    // Case 1. Different Ranks
    if (thisRank != thatRank) {
      // Subcase 1.1 One of the cards is '2', which has Rank 1
      if (thisRank == 1) {
        return 1;
      }
      else if (thatRank == 1) {
        return -1;
      }
      // Subcase 1.2 One of the cards is 'A', which has rank 0
      else if (thisRank == 0) {
        return 1;
      }
      else if (thatRank == 0) {
        return -1;
      }
      // Subcase 1.3 None of the cards are 2, A
      else if (thisRank > thatRank) {
        return 1;
      }
      else {
        return -1;
      }
    }
    
    // Case 2. Same Rank
    // Subcase 2.1 Different Suit
    else if (this.suit > card.suit) {
			return 1;}
    else if (this.suit < card.suit) {
			return -1;}
    // Subcase 2.2 Same Suit
    else {
			return 0;}
  }
}
