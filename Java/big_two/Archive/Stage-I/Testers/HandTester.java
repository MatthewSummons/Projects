public class HandTester {
  public static void main(String[] args) {
    CardList cards = new CardList();
    cards.addCard(new BigTwoCard(1, 2));
    cards.addCard(new BigTwoCard(2, 12));
    cards.addCard(new BigTwoCard(3, 2));
    cards.addCard(new BigTwoCard(2, 12));
    cards.addCard(new BigTwoCard(0, 12));

    for (int i = 0; i < cards.size(); i++) {
      System.out.println(cards.getCard(i));
    }`
    
    FullHouse test = new FullHouse(null, cards);
    
    if (test.isValid()) {
      System.out.println("Valid");
      System.out.println(test.getTopCard());
    }
    else {
      System.out.println("Invalid");
    }
  }
  
}
