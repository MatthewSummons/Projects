/**
 * This is subclass of Card class which models a card used in Big Two 
 * card game.Inherits all methods from Card class and overrides compareTo() 
 * method so that it can reflect the ordering of cards used in a Big Two card game.
 */
public class BigTwoCard extends Card {

	// *********** constructor ***************

	/**
	 * Constructor for building a card with the specified suit and rank.
	 * Suit is an integer between 0 to 3, and rank is an integer between 0 to 12.
	 * 
	 * @param suit Integer represents a suit with a value between 0 and 3
	 * @param rank Integer represents a rank with a value between 0 and 12
	 * 
	 */
	
	public BigTwoCard(int suit, int rank) {
		super(suit,rank);
	}
	
	/**
	 * Method for comparison of order of this BigTwo card with specified BigTwo card.
	 * Returns a negative integer, zero, or a positive integer when this card is 
	 * less than, equal to, or greater than the specified card.  
	 * 
	 * @param card The card that need to be compared with this card.
	 * @return A negative integer, zero, or a positive integer if this card is 
	 *         less than, equal to, or greater than the specified card.     
	 */
	public int compareTo(Card card) {
		// if the card is an A or 2 
		if (this.rank == 0 || this.rank == 1) {
			// If other cards are from 3 to K 
			// then check which is greater
			if (card.rank > 1) {
				return 1;
			}
			
			// If other cards are A or 2 
			
			else { 
				// then check which is greater
				if (this.rank > card.rank) { // if ranks are equal
					return 1;
					}
				
				else if (this.rank == card.rank) { // if other card's suit is less then this card's suit
					if (this.suit > card.suit) {
						return 1;
					}
					else if (this.suit == card.suit){ // if suits are equal
						return 0;
					}
					else { // if less than other card
						return -1;
					}
				}
				else { // if less than other card
					return -1;
				}
			}
		}
		else if (this.rank > 1 && card.rank > 1) { // checking if the card given index in the range of 2 to 12
			if (this.rank > card.rank) { // check which rank is greater
				return 1;
			}
			else if (this.rank == card.rank) { // if ranks are equal
				if (this.suit > card.suit) {   // if other card's suit is less then this card's suit
					return 1;
				}
				else if (this.suit == card.suit){ // if suits are equal
					return 0;
				}
				else { // if less than other card
					return -1;
				}
			}
			else { // if less than other card
				return -1;
			}
		}
		else { // if less than other card
			return -1;
		}
	}
}
