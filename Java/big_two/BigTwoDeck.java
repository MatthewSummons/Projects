/**
 * It is the subclass of Deck class which is used to model the deck of cards. 
 * This specific class will model the deck of cards being used in Big Two card game.
 * It inherits all the methods from the Deck class and overrides initialize() method
 * so that it can create a Big Two Card deck. 
 */
public class BigTwoDeck extends Deck{
	
	/**
	 * A method for initializing a deck of Big Two cards. 
	 * It removes all cards from the deck, create 52 
	 * Big Two cards and add them to the deck.
	 */
	public void initialize() {
		// removing all the cards 
		removeAllCards();
		
		// creating big two cards and adding them to deck
		// i loops for 4 time because 52/13 = 4
		// and j loops for 13 times as 13 cards need to be added each time
		for (int i = 0; i < 4; i++) {
			for (int j = 0; j < 13; j++) {
				// creating object
				BigTwoCard bigTwoCard = new BigTwoCard(i, j);
				// adding card. This method is in CardList
				addCard(bigTwoCard);
			}
		}
	}

}
