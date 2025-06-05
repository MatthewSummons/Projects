public class StraightTester {
  
  public static void main (String[] args) {
    // Create a cardList containig a StraightFlush and test if it is Valid
    CardList cards = new CardList();
    cards.addCard(new Card(0, 1));
    cards.addCard(new Card(2, 0));
    cards.addCard(new Card(1, 12));
    cards.addCard(new Card(0, 11));
    cards.addCard(new Card(3, 10));
    Straight straight = new Straight(null, cards);

    // Print out the cards to the screen
    for(int i=0; i<cards.size(); i++) {
      System.out.println(cards.getCard(i));
    }
    
    System.out.println("Straight: " + straight.isValid());
  }
}