class HandBeatsTester {
  public static void main(String[] args) {
    
    testSingles();

    testPairs();

    testStraight();

    testFlush();

    testFullHouse();

    testQuad();

    // CardList hand1 = new CardList();
    // CardList hand2 = new CardList();
    // hand1.addCard(new BigTwoCard(0, 0));
    // hand2.addCard(new BigTwoCard(0, 2));

    // printHand(hand1);
    // printHand(hand2);

    // Single single1 = new Single(null, hand1);
    // Single single2 = new Single(null, hand2);

    // System.out.println(single1.getTopCard().compareTo(single2.getTopCard()) == 1);

  }

  static private void printHand(CardList hand) {
    for (int i = 0; i < hand.size(); i++) {
      System.out.print(hand.getCard(i).toString() + " ");
    }
    System.out.println();
  }

  static private void testSingles() {
    
    CardList hand1 = new CardList();
    CardList hand2 = new CardList();
    hand1.addCard(new BigTwoCard(2, 0));
    hand2.addCard(new BigTwoCard(0, 1));

    Single single1 = new Single(null, hand1);
    Single single2 = new Single(null, hand2);

    printHand(hand1);
    printHand(hand2);
    
    System.out.println("A beats 2? " + single1.beats(single2));

    hand1.setCard(0, new BigTwoCard(2, 1));
    hand2.setCard(0, new BigTwoCard(2, 0));
    single1 = new Single(null, hand1);
    single2 = new Single(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("2 beats A? " + single1.beats(single2));

    hand1.setCard(0, new BigTwoCard(2, 1));
    hand2.setCard(0, new BigTwoCard(3, 1));
    single1 = new Single(null, hand1);
    single2 = new Single(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("2 Hearts beats 2 Spades? " + single1.beats(single2));

    hand1.setCard(0, new BigTwoCard(3, 8));
    hand2.setCard(0, new BigTwoCard(2, 6));
    single1 = new Single(null, hand1);
    single2 = new Single(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("9 beats 7? " + single1.beats(single2));

    // Test if 2 beats 8?
    hand1.setCard(0, new BigTwoCard(2, 1));
    hand2.setCard(0, new BigTwoCard(3, 8));

    single1 = new Single(null, hand1);
    single2 = new Single(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("2 beats 8? " + single1.beats(single2));
  }

  static private void testPairs() {
    CardList hand1 = new CardList();
    CardList hand2 = new CardList();
    hand1.addCard(new BigTwoCard(2, 6));
    hand1.addCard(new BigTwoCard(3, 6));
    hand2.addCard(new BigTwoCard(1, 4));
    hand2.addCard(new BigTwoCard(0, 4));

    Pair pair1 = new Pair(null, hand1);
    Pair pair2 = new Pair(null, hand2);

    printHand(hand1);
    printHand(hand2);
    
    System.out.println("7, 7 beats 5, 5? " + pair1.beats(pair2));

    hand1.setCard(0, new BigTwoCard(0, 6));
    hand1.setCard(1, new BigTwoCard(1, 6));
    hand2.setCard(0, new BigTwoCard(2, 6));
    hand2.setCard(1, new BigTwoCard(3, 6));

    pair1 = new Pair(null, hand1);
    pair2 = new Pair(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("Suit 0,1 beats Suit 2, 3? " + pair1.beats(pair2));
  }

  public static void testStraight() {
    CardList hand1 = new CardList();
    CardList hand2 = new CardList();

    hand1.addCard(new BigTwoCard(0, 3));
    hand1.addCard(new BigTwoCard(1, 4));
    hand1.addCard(new BigTwoCard(2, 5));
    hand1.addCard(new BigTwoCard(3, 6));
    hand1.addCard(new BigTwoCard(0, 7));

    hand2.addCard(new BigTwoCard(2, 4));
    hand2.addCard(new BigTwoCard(3, 5));
    hand2.addCard(new BigTwoCard(0, 6));
    hand2.addCard(new BigTwoCard(1, 7));
    hand2.addCard(new BigTwoCard(1, 8));

    Straight straight1 = new Straight(null, hand1);
    Straight straight2 = new Straight(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("3, 4, 5, 6, 7 beats 4, 5, 6, 7, 8? " + straight1.beats(straight2));

    hand1.setCard(0, new BigTwoCard(0, 3));
    hand1.setCard(1, new BigTwoCard(1, 4));
    hand1.setCard(2, new BigTwoCard(2, 5));
    hand1.setCard(3, new BigTwoCard(0, 6));
    hand1.setCard(4, new BigTwoCard(1, 7));

    hand2.setCard(0, new BigTwoCard(2, 3));
    hand2.setCard(1, new BigTwoCard(3, 4));
    hand2.setCard(2, new BigTwoCard(0, 5));
    hand2.setCard(3, new BigTwoCard(1, 6));
    hand2.setCard(4, new BigTwoCard(2, 7));

    straight1 = new Straight(null, hand1);
    straight2 = new Straight(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("3, 4, 5, 6, 7 beats 3, 4, 5, 6, 7? " + straight1.beats(straight2));

    hand1.setCard(0, new BigTwoCard(0, 10));
    hand1.setCard(1, new BigTwoCard(1, 11));
    hand1.setCard(2, new BigTwoCard(2, 12));
    hand1.setCard(3, new BigTwoCard(0, 9));
    hand1.setCard(4, new BigTwoCard(1, 8));

    hand2.setCard(0, new BigTwoCard(2, 10));
    hand2.setCard(1, new BigTwoCard(3, 11));
    hand2.setCard(2, new BigTwoCard(0, 12));
    hand2.setCard(3, new BigTwoCard(1, 0));
    hand2.setCard(4, new BigTwoCard(2, 1));

    straight1 = new Straight(null, hand1);
    straight2 = new Straight(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("J, Q, K, 10, 9 beats 10, J, Q, K, A? " + straight1.beats(straight2));

    hand1.setCard(0, new BigTwoCard(0, 10));
    hand1.setCard(1, new BigTwoCard(1, 11));
    hand1.setCard(2, new BigTwoCard(2, 12));
    hand1.setCard(3, new BigTwoCard(0, 1));
    hand1.setCard(4, new BigTwoCard(1, 0));

    hand2.setCard(0, new BigTwoCard(2, 10));
    hand2.setCard(1, new BigTwoCard(3, 11));
    hand2.setCard(2, new BigTwoCard(0, 12));
    hand2.setCard(3, new BigTwoCard(1, 0));
    hand2.setCard(4, new BigTwoCard(2, 1));

    straight1 = new Straight(null, hand1);
    straight2 = new Straight(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("J, Q, K, A, 2 beats 10, J, Q, K, A? " + straight1.beats(straight2));





  }

  public static void testFlush() {
    CardList hand1 = new CardList();
    CardList hand2 = new CardList();

    hand1.addCard(new BigTwoCard(0, 3));
    hand1.addCard(new BigTwoCard(0, 4));
    hand1.addCard(new BigTwoCard(0, 5));
    hand1.addCard(new BigTwoCard(0, 6));
    hand1.addCard(new BigTwoCard(0, 7));

    hand2.addCard(new BigTwoCard(2, 4));
    hand2.addCard(new BigTwoCard(2, 5));
    hand2.addCard(new BigTwoCard(2, 6));
    hand2.addCard(new BigTwoCard(2, 7));
    hand2.addCard(new BigTwoCard(2, 8));

    Flush flush1 = new Flush(null, hand1);
    Flush flush2 = new Flush(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("3, 4, 5, 6, 7 beats 4, 5, 6, 7, 8? " + flush1.beats(flush2));
    System.out.println("3, 4, 5, 6, 7 beats 4, 5, 6, 7, 8? " + flush2.beats(flush1));

    hand1.setCard(0, new BigTwoCard(0, 3));
    hand1.setCard(1, new BigTwoCard(0, 4));
    hand1.setCard(2, new BigTwoCard(0, 5));
    hand1.setCard(3, new BigTwoCard(0, 6));
    hand1.setCard(4, new BigTwoCard(0, 7));

    hand2.setCard(0, new BigTwoCard(2, 3));
    hand2.setCard(1, new BigTwoCard(2, 4));
    hand2.setCard(2, new BigTwoCard(2, 5));
    hand2.setCard(3, new BigTwoCard(2, 6));
    hand2.setCard(4, new BigTwoCard(2, 7));

    flush1 = new Flush(null, hand1);
    flush2 = new Flush(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("4, 5, 6, 7, 8 beats 4, 5, 6, 7, 8? " + flush1.beats(flush2));
    System.out.println("4, 5, 6, 7, 8 beats 4, 5, 6, 7, 8? " + flush2.beats(flush1));

    hand1.setCard(0, new BigTwoCard(0, 10));
    hand1.setCard(1, new BigTwoCard(0, 11));
    hand1.setCard(2, new BigTwoCard(0, 12));
    hand1.setCard(3, new BigTwoCard(0, 9));
    hand1.setCard(4, new BigTwoCard(0, 8));

    hand2.setCard(0, new BigTwoCard(2, 10));
    hand2.setCard(1, new BigTwoCard(3, 11));
    hand2.setCard(2, new BigTwoCard(1, 12));
    hand2.setCard(3, new BigTwoCard(0, 0));
    hand2.setCard(4, new BigTwoCard(2, 1));

    flush1 = new Flush(null, hand1);
    Straight straight2 = new Straight(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("J, Q, K, 10, 9 beats 10, J, Q, K, A? " + flush1.beats(straight2));
    System.out.println("J, Q, K, 10, 9 beats 10, J, Q, K, A? " + straight2.beats(flush1));
  }

  public static void testFullHouse() {
    CardList hand1 = new CardList();
    CardList hand2 = new CardList();

    hand1.addCard(new BigTwoCard(0, 3));
    hand1.addCard(new BigTwoCard(1, 3));
    hand1.addCard(new BigTwoCard(2, 3));
    hand1.addCard(new BigTwoCard(0, 4));
    hand1.addCard(new BigTwoCard(1, 4));

    hand2.addCard(new BigTwoCard(2, 4));
    hand2.addCard(new BigTwoCard(3, 4));
    hand2.addCard(new BigTwoCard(0, 5));
    hand2.addCard(new BigTwoCard(1, 5));
    hand2.addCard(new BigTwoCard(2, 5));

    FullHouse fullHouse1 = new FullHouse(null, hand1);
    FullHouse fullHouse2 = new FullHouse(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("3, 3, 3, 4, 4 beats 4, 4, 5, 5, 5? " + fullHouse1.beats(fullHouse2));
    System.out.println("4, 4, 5, 5, 5 beats 3, 3, 3, 4, 4? " + fullHouse2.beats(fullHouse1));

    hand1.setCard(0, new BigTwoCard(0, 3));
    hand1.setCard(1, new BigTwoCard(1, 3));
    hand1.setCard(2, new BigTwoCard(2, 3));
    hand1.setCard(3, new BigTwoCard(0, 4));
    hand1.setCard(4, new BigTwoCard(1, 4));

    hand2.setCard(0, new BigTwoCard(2, 3));
    hand2.setCard(1, new BigTwoCard(3, 3));
    hand2.setCard(2, new BigTwoCard(0, 4));
    hand2.setCard(3, new BigTwoCard(1, 4));
    hand2.setCard(4, new BigTwoCard(2, 4));

    fullHouse1 = new FullHouse(null, hand1);
    fullHouse2 = new FullHouse(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("3, 3, 3, 4, 4 beats 3, 3, 4, 4, 4? " + fullHouse1.beats(fullHouse2));
    System.out.println("3, 3, 4, 4, 4 beats 3, 3, 3, 4, 4? " + fullHouse2.beats(fullHouse1));

  }

  public static void testQuad() {
    CardList hand1 = new CardList();
    CardList hand2 = new CardList();

    hand1.addCard(new BigTwoCard(0, 3));
    hand1.addCard(new BigTwoCard(1, 3));
    hand1.addCard(new BigTwoCard(2, 3));
    hand1.addCard(new BigTwoCard(3, 3));
    hand1.addCard(new BigTwoCard(0, 4));

    hand2.addCard(new BigTwoCard(2, 4));
    hand2.addCard(new BigTwoCard(3, 4));
    hand2.addCard(new BigTwoCard(0, 4));
    hand2.addCard(new BigTwoCard(1, 4));
    hand2.addCard(new BigTwoCard(2, 5));

    Quad quad1 = new Quad(null, hand1);
    Quad quad2 = new Quad(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("3, 3, 3, 3, 4 beats 4, 4, 4, 4, 5? " + quad1.beats(quad2));
    System.out.println("4, 4, 4, 4, 5 beats 3, 3, 3, 3, 4? " + quad2.beats(quad1));

    hand1.setCard(0, new BigTwoCard(0, 3));
    hand1.setCard(1, new BigTwoCard(1, 3));
    hand1.setCard(2, new BigTwoCard(2, 3));
    hand1.setCard(3, new BigTwoCard(3, 3));
    hand1.setCard(4, new BigTwoCard(0, 4));

    hand2.setCard(0, new BigTwoCard(2, 3));
    hand2.setCard(1, new BigTwoCard(3, 3));
    hand2.setCard(2, new BigTwoCard(0, 3));
    hand2.setCard(3, new BigTwoCard(1, 3));
    hand2.setCard(4, new BigTwoCard(2, 4));

    quad1 = new Quad(null, hand1);
    quad2 = new Quad(null, hand2);

    printHand(hand1);
    printHand(hand2);

    System.out.println("3, 3, 3, 3, 4 beats 3, 3, 3, 3, 4? " + quad1.beats(quad2));
    System.out.println("3, 3, 3, 3, 4 beats 3, 3, 3, 3, 4? " + quad2.beats(quad1));
    
  }
}