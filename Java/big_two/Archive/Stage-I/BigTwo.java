import java.util.ArrayList;

/**
 * The BigTwo class implements the CardGame interface and is used to model a Big
 * Two card
 * game.
 * 
 * @author Shaheer Ziya
 */
public class BigTwo implements CardGame {

  private int numOfPlayers = 4;
  private Deck deck;
  private ArrayList<CardGamePlayer> playerList;
  private ArrayList<Hand> handsOnTable = new ArrayList<Hand>();
  private int currentPlayerIdx;
  private BigTwoUI ui;
  
  
  /**
   * A constructor for creating a Big Two card game. You should (i) create 4
   * players and add them to the player list; and (ii) create a BigTwoUI object
   * for providing the user interface.
   */
  public BigTwo() {
    // Create 4 Players and them to the playerList
    this.playerList = new ArrayList<CardGamePlayer>();
    for (int i = 0; i < 4; i++) {
      this.playerList.add(new CardGamePlayer());
    }
    // Create a BigTwoUI object for providing the user interface
    this.ui = new BigTwoUI(this);

  }

  /**
   * A method for starting a Big Two card game. It should (i) create a Big Two
   * card game, (ii) create and shuffle a deck of cards, and (iii) start the game
   * with the deck of cards.
   */
  public static void main(String[] args) {
    // Create a Big Two card game
    BigTwo game = new BigTwo();

    // Create and shuffle a deck of cards
    game.deck = new Deck();
    game.deck.shuffle();

    // Start the game with the deck of cards
    game.start(game.deck);

    while ( !game.endOfGame() ) {
      game.ui.repaint();
      
      game.ui.printMsg("\n");
      
      game.ui.promptActivePlayer();
      
      game.ui.printMsg("\n");
    }

    game.showResults();
  }

  /**
   * A method for returning a valid hand from the specified list of cards of the
   * player. Returns null if no valid hand can be composed from the specified list
   * of cards.
   * 
   * @param player The player holding the cards
   * @param cards  The list of cards to be checked
   * @return A valid hand from the specified list of cards of the player. Null if
   *        no valid hand can be composed from the specified list of cards.
   */
  public static Hand composeHand(CardGamePlayer player, CardList cards) {

    Single single = new Single(player, cards);
    Pair pair = new Pair(player, cards);
    Triple triple = new Triple(player, cards);
    
    Straight straight = new Straight(player, cards);
    Flush flush = new Flush(player, cards);
    FullHouse fullHouse = new FullHouse(player, cards);
    Quad quad = new Quad(player, cards);
    StraightFlush straightFlush = new StraightFlush(player, cards);


    int numCards = cards.size();
    // Check the 5 Card Combinations
    if (numCards == 5) {
      if (straightFlush.isValid()) {
        return straightFlush;
      }

      if (quad.isValid()) {
        return quad;
      }

      if (fullHouse.isValid()) {
        return fullHouse;
      }

      if (flush.isValid()) {
        return flush;
      }

      if (straight.isValid()) {
        return straight;
      } 
    }

    // Check the other possible combinations
    if (single.isValid()) {
      return single;
    }

    if (pair.isValid()) {
      return pair;
    }

    if (triple.isValid()) {
      return triple;
    }
    
    // Return null if no valid hand can be composed from the specified list of cards
    return null;
  }




  /**
   * A method for getting the number of players.
   */
  public int getNumOfPlayers() {
    return numOfPlayers;
  }

  /**
   * A method for getting the deck of cards being used.
   */
  public Deck getDeck() {
    return deck;
  }

  /**
   * A method for getting the list of players.
   */
  public ArrayList<CardGamePlayer> getPlayerList() {
    return playerList;
  }

  /**
   * A method for getting the list of hands played on the table.
   */
  public ArrayList<Hand> getHandsOnTable() {
    return handsOnTable;
  }

  /**
   * A method for getting the index of the current player.
   */
  public int getCurrentPlayerIdx() {
    return currentPlayerIdx;
  }

  /**
   * A method for starting/restarting the game with a given shuffled deck of cards.
   * You should (i) remove all the cards from the players as well as from the table;
   * (ii) distribute the cards to the players; (iii) identify the player who holds the
   * Three of Diamonds; (iv) set both the currentPlayerIdx of the BigTwo object and
   * the activePlayer of the BigTwoUI object to the index of the player who holds the
   * Three of Diamonds; (v) call the repaint() method of the BigTwoUI object to show 
   * the cards on the table; and (vi) call the promptActivePlayer() method of the BigTwoUI
   * object to prompt user to select cards and make his/her move.
   */
  public void start(Deck deck) {
    // Remove all cards from the players and the table
    for (int i=0; i<4; i++) {
      playerList.get(i).removeAllCards();
    }
    
    handsOnTable.clear();
    
    // Deal cards to the players
    for (int i=0; i<13; i++) {
      for (int j=0; j<4; j++) {
        playerList.get(j).addCard(deck.getCard(i*4+j));
      }
    }

    // Sort the cards in the hand of each player
    for (int i=0; i < this.numOfPlayers; i++) {
      playerList.get(i).sortCardsInHand();
    }

    // Find the player who holds the 3 of Diamonds
    for (int i=0; i<4; i++) {
      for (int j=0; j<13; j++) {
        if (playerList.get(i).getCardsInHand().contains(new Card(0, 2))) {
          currentPlayerIdx = i;
          ui.setActivePlayer(i);
        }
      }
    }

    // Call repaint() and promptActivePlayer()
    ui.repaint();
    ui.promptActivePlayer();
  }

  /**
   * A method for making a move by a player with the specified index using the
   * cards specified by the list of indices. This method should be called from the
   * BigTwoUI after the active player has selected cards to make his/her move. You
   * should simply call the checkMove() method with the playerIdx and cardIdx as
   * the arguments.
   */
  public void makeMove(int playerIdx, int[] cardIdx) {
    checkMove(playerIdx, cardIdx);
  }

  /**
   * A method for checking a move made by a player. This method should be called
   * from the makeMove() method.
   */
  public void checkMove(int playerIdx, int[] cardIdx) {
    /*
     * In checkMove(), you should determine if the hand, specified by cardIdx,
     * played by the player, specified by playerIdx, is a legal move. If yes, the
     * hand of cards should be removed from the player and added to the table, and
     * the turn goes to the next player; if not, the current player should be
     * prompted to make a move again (please refer to the example game play in the
     * Appendix of the assignemnt).
     */
    
    // Make sure the 3 of Diamonds is played in the first move
    if (handsOnTable.size() == 0) {
      // Cannot pass on the first move
      if (cardIdx == null) {
       illegalMove();
       return;
      }

      // Check if the player has played the 3 of Diamonds
      CardList cards = playerList.get(playerIdx).play(cardIdx);
      for(int i=0; i < cards.size(); i++) {
        if (cards.getCard(i).getRank() == 2 && cards.getCard(i).getSuit() == 0) {
          
          CardGamePlayer currPlayer = this.playerList.get(playerIdx);
          CardList currPlayerCards = currPlayer.play(cardIdx);

          // Attempt to compose a Hand from the played cards
          Hand currPlayerHand = composeHand(this.playerList.get(playerIdx), currPlayerCards);

          if (currPlayerHand != null) {
            // Remove the cards from the player's hand and add them to the table
            // Update the active player
            currPlayer.removeCards(currPlayerHand);
            handsOnTable.add(currPlayerHand);
            this.currentPlayerIdx = (playerIdx + 1) % 4;
            this.ui.setActivePlayer(this.currentPlayerIdx);
            return;
          }
        }
      }
      
      // If the player has not played the 3 of Diamonds, prompt the player to play the 3 of Diamonds
      illegalMove();
      return;
    }
    
    
    // If a player has passed and it is the same player as the player who played the last hand,
    // then player must play a legal hand
    if (cardIdx == null && 
      handsOnTable.get(handsOnTable.size()-1).getPlayer() == playerList.get(playerIdx)) {
      illegalMove();
      return;
    } else if (handsOnTable.get(handsOnTable.size()-1).getPlayer() == playerList.get(playerIdx)) {
      CardGamePlayer currPlayer = this.playerList.get(playerIdx);
      CardList currPlayerCards = currPlayer.play(cardIdx);

      // Attempt to compose a Hand from the played cards
      Hand currPlayerHand = composeHand(this.playerList.get(playerIdx), currPlayerCards);

      // Allow them to play any legal move
      if (currPlayerHand != null) {
        // Remove the cards from the player's hand and add them to the table
        // Update the active player
        currPlayer.removeCards(currPlayerHand);
        handsOnTable.add(currPlayerHand);
        this.currentPlayerIdx = (playerIdx + 1) % 4;
        this.ui.setActivePlayer(this.currentPlayerIdx);
        return;
      } else {
        illegalMove();
        return;
      }
    }
    // Otherwise, if they attempt to pass, allow them to do so
    else if (cardIdx == null) {
      ui.printMsg("{Pass}\n");
      this.currentPlayerIdx = (playerIdx + 1) % 4;
      this.ui.setActivePlayer(this.currentPlayerIdx);
      return;
    }
    
    CardGamePlayer currPlayer = this.playerList.get(playerIdx);
    CardList currPlayerCards = currPlayer.play(cardIdx);
    
    // Attempt to compose a Hand from the played cards
    Hand currPlayerHand = composeHand(this.playerList.get(playerIdx), currPlayerCards);

    // At this stage we know that it is not the first move, and the player has not passed.
    // Then we can only allow the player to make a move if this hand beats the last hand.
    if (currPlayerHand.beats(handsOnTable.get(handsOnTable.size()-1))) {
      currPlayer.removeCards(currPlayerHand);
      handsOnTable.add(currPlayerHand);
      this.currentPlayerIdx = (playerIdx + 1) % 4;
      this.ui.setActivePlayer(this.currentPlayerIdx);
    }
      
    // Illegal Move
    else {
      ui.printMsg("Not a legal move!!!\n");
      ui.promptActivePlayer();
    }
  }

  /**
   * A method for checking if the game ends.
   * The game ends when any of the players has no more cards in his/her hand.
   */
  public boolean endOfGame() {
    for(int i=0; i < this.numOfPlayers; i++) {
      if(this.playerList.get(i).getNumOfCards() == 0) {
        return true;
      }
    }
    
    return false;
  }

  /**
   * A method for printing the end game results.
   */
  private void showResults() {
    ui.printMsg("Game ends\n");
    for (int i=0; i < numOfPlayers; i++) {
      if (playerList.get(i).getNumOfCards() == 0) {
        ui.printMsg("Player " + i + " wins the game.\n");
      } else {
        ui.printMsg("Player " + i + " has " + playerList.get(i).getNumOfCards() + " cards in hand.\n");
      }
    }
  }

  /**
   * A method exectued inside checkMove() when an illegal move is made
   */
  private void illegalMove() {
    ui.printMsg("Not a legal move!!!\n");
    ui.promptActivePlayer();
  }
}