import java.awt.*;
import java.awt.event.*;

import javax.swing.*;
import javax.swing.border.*;

/**
 * This is BigTwoGUI class which builds a GUI for the game and handles all
 * the user actions. This class implements the CardGameUI interface and has
 * a number of inner classes responsible for rendering the user interface and
 * handling user actions.
 *   
 * @author Shaheer Ziya
 */

public class BigTwoGUI implements CardGameUI {

	/** A Big Two card game associated with this GUI. */
	private BigTwo game;
	/** A CardList to identify the selected cards */
	private CardList selected = new CardList();
	/** An integer specifying the index of the active player. */
	private int activePlayer = -1;
	/** The main window of the application. */
	private JFrame frame;
    /** A panel for showing the cards of each player and the cards played on the table. */
	private JPanel bigTwoPanel;
 	/** A “Play” button for the active player to play the selected cards. */
	private JButton playButton;
    /** A “Pass” button for the active player to pass his/her turn to the next player. */
	private JButton passButton;
	/**
	 * A text area for showing the current game status as well as end of game
	 * messages.
	 */
	private JTextArea msgArea;
	/** A text area for showing chat messages sent by the players. */
	private JTextArea chatArea;
	/** A text field for players to input chat messages. */
	private JTextField chatInput;
	
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	// Self Defined Fields for BigTwoGUI
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	/** A panel that holds text-field and label. */
	private JPanel chatInputPanel;
	/** A 2D array to store images of the 52 playing BigTwo cards. */
	private Image[][] imagesOfCards;
	/** Image of the back face of the card */
	private Image backOfCard;
	/** An array to store the avataars for the players */
	private Image[] playerAvatars;
	/** Image of the table where the hands will be placed */
	private Image imageOfTable;
	/** An image of the application icon */
	private Image imageOfLogo;
	/** The panel to hold the Plan & Pass buttons */
	private JPanel bottomPanel;
	/** The top panel to indicate the section for the players and the hands */
	private JPanel header;
	/** The listener for the play button */
	private PlayButtonListener playButtonListener;
	/** The listener for the pass button */
	private PassButtonListener passButtonListener;
	/** The color of the table */
	// private Color color = new Color(76,157,105);
	private Color color = new Color(0, 50, 2);
	
	// ~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~~
	
	/**
	 * A constructor for creating a BigTwoGUI. The parameter game is a
	 * reference to a Big Two card game associates with this GUI.
	 * @param game a reference to a card game associates with this table.
	 */
	
	public BigTwoGUI (BigTwo game) {
		this.game = game;   // Link the BigTwo game to the GUI
		LoadImages();       // To load necessary images
		selected = new CardList();   // To make a list of cards
		setupGUI();     // Setup GUI
	}
	
	
	// *************** Public Methods of the class *************************************
	

	/**
	 * A method for setting the index of the 
	 * active player (i.e. the player having control of the GUI).  
	 * @param activePlayer  an integer value representing the index of the active player     
	 */
	
	public void setActivePlayer(int activePlayer) {
		this.activePlayer = activePlayer;  // setting active Player
	}
	
	/**
	 * A method for repainting the GUI. This method resets the list of selected cards,
	 * clears the message area, and redraws the card game table.
	 */
	public void repaint() {

		// Clears the panel inside the frame
		frame.getContentPane().removeAll();
		frame.setContentPane(new JPanel(new BorderLayout()));
		
		// Reset the selected list of cards
		resetSelected();

		// The panel to hold the chat & message area
		JPanel msgPanel = new JPanel();
		msgPanel.setLayout(new BoxLayout(msgPanel, BoxLayout.Y_AXIS));

		// Create a scroller for the msgArea having no horizontal scroller but a vertical scroller
		JScrollPane messageScroller = new JScrollPane(msgArea, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
		
		// Create a scroller for the chatArea having no horizontal scroller but a vertical scroller
		JScrollPane chatScroller = new JScrollPane(chatArea, JScrollPane.VERTICAL_SCROLLBAR_ALWAYS,
			JScrollPane.HORIZONTAL_SCROLLBAR_NEVER);
	
		// Set the size of the message and chat area 
		msgArea.setPreferredSize(new Dimension(400,10000));
		msgArea.setMaximumSize(new Dimension(20,10));
		chatArea.setPreferredSize(new Dimension(400,10000));
		chatArea.setMaximumSize(new Dimension(20,10));
		
		// Add the message and chat area to the msgPanel
		msgPanel.add(messageScroller, BorderLayout.NORTH);
		msgPanel.add(chatScroller, BorderLayout.CENTER);
		msgPanel.add(chatInputPanel, BorderLayout.SOUTH);
		
		// Create the BigTwoPanel which is the main panel of the game
		bigTwoPanel = new BigTwoPanel();
		bigTwoPanel.setLayout(new BoxLayout(bigTwoPanel, BoxLayout.Y_AXIS));
		
		// Add the background panel to the frame
		header = new JPanel();
		header.setBackground(color);
		header.setLayout(new GridLayout(1,2));
		header.setMaximumSize(new Dimension(10000, 300));
				
		// Create the labels for the header
		JLabel headerLabel1 = new JLabel(" Players:");
		headerLabel1.setForeground(Color.WHITE);
		headerLabel1.setFont(new Font("SansSerif", Font.BOLD, 20));
		header.add(headerLabel1);
				
		JLabel headerLabel2 = new JLabel("Hand:         ");
		headerLabel2.setForeground(Color.WHITE);
		headerLabel2.setFont(new Font("SansSerif", Font.BOLD, 20));
		header.add(headerLabel2);
				
		// Color of the border
		LineBorder whiteBorder = new LineBorder(Color.WHITE);
		
		// Add the header panel to the backgorund panel, 1st element.
		bigTwoPanel.add(header);
		
		bigTwoPanel.setAlignmentX(Component.LEFT_ALIGNMENT);
		frame.add(bigTwoPanel);
		
		// Create the four player panels for each player.
		JPanel player0 = new PlayerPanel(game.getPlayerList().get(0), 0);
		JPanel player1 = new PlayerPanel(game.getPlayerList().get(1), 1);
		JPanel player2 = new PlayerPanel(game.getPlayerList().get(2), 2);
		JPanel player3 = new PlayerPanel(game.getPlayerList().get(3), 3);
		
		// Set the layout and the border to highlight the active player
		JPanel player = null;

		if (activePlayer == 0) {player = player0; }
		else if (activePlayer == 1) {player = player1;}
		else if (activePlayer == 2) {player = player2;}
		else {player = player3;}
		
		player.setLayout(new BoxLayout(player, BoxLayout.X_AXIS));
		player.setPreferredSize(new Dimension(bigTwoPanel.getWidth(),180));
		player.setBorder(BorderFactory.createTitledBorder(whiteBorder, "You", 0, 0, new Font("OpenSans", Font.BOLD, 15), Color.GREEN));
		
		
		if (activePlayer != 0) {
			player0.setLayout(new FlowLayout());
			player0.setPreferredSize(new Dimension(bigTwoPanel.getWidth(),160));
			player0.setBorder(BorderFactory.createTitledBorder(whiteBorder, "Player 0", 0, 0, new Font("OpenSans", Font.BOLD, 15), Color.WHITE));
		}
		if (activePlayer != 1) {
			player1.setLayout(new FlowLayout());
			player1.setPreferredSize(new Dimension(bigTwoPanel.getWidth(),160));
			player1.setBorder(BorderFactory.createTitledBorder(whiteBorder, "Player 1", 0, 0, new Font("OpenSans", Font.BOLD, 15), Color.WHITE));
		}
		if (activePlayer != 2) {
			player2.setLayout(new FlowLayout());
			player2.setPreferredSize(new Dimension(bigTwoPanel.getWidth(),160));
			player2.setBorder(BorderFactory.createTitledBorder(whiteBorder, "Player 2", 0, 0, new Font("OpenSans", Font.BOLD, 15), Color.WHITE));
		}
		if (activePlayer != 3) {
			player3.setLayout(new FlowLayout());
			player3.setPreferredSize(new Dimension(bigTwoPanel.getWidth(),160));
			player3.setBorder(BorderFactory.createTitledBorder(whiteBorder, "Player 3", 0, 0, new Font("OpenSans", Font.BOLD, 15), Color.WHITE));
		}

		
		// If the player is active, add the clickable card panels accordingly 
		// Add the dummy panel first so that the position of the cards is correct.
		// If the player is not active, don't add the clickable panel.
		JPanel dummyPanel = new JPanel();
		dummyPanel.setPreferredSize(new Dimension(285, 20));
		dummyPanel.setOpaque(false);
		
		if (activePlayer == 0) {
			HandPanel handPanel = new HandPanel(game.getPlayerList().get(0));
			
			player0.add(dummyPanel);
			player0.add(handPanel);
		}
		else if (activePlayer == 1) {
			HandPanel handPanel = new HandPanel(game.getPlayerList().get(1));
			
			player1.add(dummyPanel);
			player1.add(handPanel);
		}
		else if (activePlayer == 2) {
			HandPanel handPanel = new HandPanel(game.getPlayerList().get(2));
			
			player2.add(dummyPanel);
			player2.add(handPanel);
		}
		else if (activePlayer == 3) {
			HandPanel handPanel = new HandPanel(game.getPlayerList().get(3));
			
			player3.add(dummyPanel);
			player3.add(handPanel);
		}
		
		// Add the player panel according to the setting if the player is active or not to the background panel
		bigTwoPanel.add(player0);
		bigTwoPanel.add(player1);
		bigTwoPanel.add(player2);
		bigTwoPanel.add(player3);
		
		// Getting name of the last player which played a valid legal hand.
		String lastPlayer = "";
		if (game.getHandsOnTable().size() != 0) {
			lastPlayer = game.getHandsOnTable().get(game.getHandsOnTable().size() - 1).getPlayer().getName();
		}
		
		// Create the table panel where played card would be placed
		JPanel tablePanel = new JPanel();
		tablePanel.setLayout(new BoxLayout(tablePanel, BoxLayout.Y_AXIS));
		tablePanel.setPreferredSize(new Dimension(500,200));
		
		// Add the label panel for the last player who played a valid legal hand, set the font 
		JLabel tableLabel = new JLabel("Last Hand: " + lastPlayer);
		tableLabel.setForeground(Color.WHITE);
		tableLabel.setFont(new Font("SansSerif", Font.BOLD, 18));
		tablePanel.add(tableLabel);
		tablePanel.setBackground(color);
		TablePanel table = new TablePanel();
		tablePanel.add(table);
		tablePanel.setBorder(BorderFactory.createLoweredBevelBorder());
		
		// Add the panel holding the table and the buttons
		bigTwoPanel.add(tablePanel);
		
		// Add the bottomPanel to bigTwoPanel
		bigTwoPanel.add(bottomPanel);
		
		frame.add(msgPanel, BorderLayout.EAST);  // added now
		frame.add(bigTwoPanel);  // added now
		
		// Disable the GUI buttons if there is winner.
		if (game.endOfGame()) {
			disable();
		}
		else {
			enable();
		}
		
		frame.setVisible(true);
	}

	
	/**
	 * A method for printing the specified string to the message area of the GUI.
	 * @param msg  the string to be printed to the message area of the Big Two Game                  
	 */
	public void printMsg(String msg) {
		msgArea.append(msg);
	}
	
	
	/**
	 * A method for clearing the message area of the GUI. 
	 */
	public void clearMsgArea() {
		msgArea.setText("");
	}
	
	/**
	 * A method for resetting the GUI. It will
	 * (i) reset the list of selected cards; 
	 * (ii) clear the message area; and 
	 * (iii) enable user interactions.
	 */
	
	public void reset() {
		resetSelected();
		clearMsgArea();
		enable();
	}
	

	/**
	 * A method for enabling user interactions with the GUI. It will
	 * (i) enable the "Play" button and "Pass" button (i.e., making them click-able); 
	 * (ii) enable the chat input; and 
	 * (iii) enable the BigTwoPanel for selection of cards through mouse clicks.
	 */
	public void enable() {
		playButton.setEnabled(true);
		passButton.setEnabled(true);
		chatInput.setEnabled(true);
		bigTwoPanel.setEnabled(true); 
	}
	
	/**
	 * A method for disabling user interactions with the GUI. It will
	 * (i) disable the "Play" button and "Pass" button (i.e. making them not click-able);  
	 * (ii) disable the chat input; and 
	 * (ii) disable the BigTwoPanel for selection of cards through mouse clicks.
	 *  
	 */
	public void disable() {
		playButton.setEnabled(false);  // (i)
		passButton.setEnabled(false);  // (i)
		chatInput.setEnabled(false);   // (ii)
		bigTwoPanel.setEnabled(false); // (iii)
	}
	
	/**
	 *  A method for prompting the active player to select cards 
	 *  and make his/her move. A message should be displayed in the message area showing it 
	 *  is the active player’s turn.
	 */
	public void promptActivePlayer() {
		printMsg(game.getPlayerList().get(activePlayer).getName() + "'s turn: \n");
	}	
	
	
    // A method to load and put essential Images
	private void LoadImages() {
		
		// Load the avatars for the players
		playerAvatars = new Image[4];
		
		playerAvatars[0] = new ImageIcon("Resources/Avatars/Player0.png").getImage();
		playerAvatars[1] = new ImageIcon("Resources/Avatars/Player1.png").getImage();
		playerAvatars[2] = new ImageIcon("Resources/Avatars/Player2.png").getImage();
		playerAvatars[3] = new ImageIcon("Resources/Avatars/Player3.png").getImage();
		
		// Set up the ImageIcon
		imageOfLogo = new ImageIcon("Resources/Logo/Logo.png").getImage();  // creating an ImageIcon object

		// To Put card images in the 2-dimensional list
		imagesOfCards = new Image[4][13];	
		for (int i = 0; i < 4; i++) {
			for (int j = 0; j < 13; j++) {
				imagesOfCards[i][j] = new ImageIcon("Resources/Cards/" + j + "-" + i + ".gif").getImage();
				}
			}		
		
		// To put Image of the back of the card
		backOfCard = new ImageIcon("Resources/Cards/backCard.gif").getImage();
		
		// To put Image of the table where cards played would be placed
		imageOfTable = new ImageIcon("Resources/Table.jpeg").getImage();
		
	}
	
	// A private method to setup the Graphical User Interface for the game
	private void setupGUI() {
		
		// Create the frame for the GUI
		frame = new JFrame("Big Two Game");
		frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
		frame.setIconImage(imageOfLogo);  // Change icon of frame
		frame.setLocation(130, 50);  // Set position on screen
		frame.setSize(1050,850);   // Decide the size of the frame
		
		// Initialize the menu-bar for the frame, including the menus and the menu-items.
		JMenuBar menuBar = new JMenuBar();
		JMenu menuGame = new JMenu("Game");
		JMenuItem restartItem = new JMenuItem("Restart");
		JMenuItem quitItem = new JMenuItem("Quit");
		
		// Register the listeners to the buttons
		RestartMenuItemListener restartListener = new RestartMenuItemListener();
		QuitMenuItemListener quitListener = new QuitMenuItemListener();
		restartItem.addActionListener(restartListener);
		quitItem.addActionListener(quitListener);
		
		// Add the menus items to the menus and the menu-bars
		menuGame.add(restartItem);
		menuGame.add(quitItem);
		menuBar.add(menuGame);
		
		// Adding Message component
		JMenu menuMessage = new JMenu("Message");
		JMenuItem clearChat = new JMenuItem("Clear Chat Box");
		ClearChatListener clearListener = new ClearChatListener();
		clearChat.addActionListener(clearListener);
		menuMessage.add(clearChat);
		menuBar.add(menuMessage);
				
		// Adding the menu-bar to the frame
		frame.setJMenuBar(menuBar);
				
		LineBorder blackBorder = new LineBorder(Color.BLACK);
				
	    // Initialize the message area, set the font, not editable,  the background, margins and wrap the lines and create a white titled border for it
		msgArea = new JTextArea("Welcome to the BigTwo game!\n");
		msgArea.setFont(new Font("Titillium", Font.PLAIN, 15));
		msgArea.setEditable(false);
		msgArea.setLineWrap(true); 
		msgArea.setWrapStyleWord(true);
		msgArea.setBorder(BorderFactory.createTitledBorder(blackBorder, "Card History", 0,
			0, new Font("Titillium", Font.BOLD, 20), Color.BLACK));
		msgArea.setBackground(Color.WHITE);
		msgArea.setMargin(new Insets(5,10,10,10));
		
		// Initialize the chat area and its properties.
		chatArea = new JTextArea();
		chatArea.setEditable(false);
		chatArea.setLineWrap(true);
		chatArea.setWrapStyleWord(true);
        Font font = new Font("Titillium", Font.PLAIN, 15);
        chatArea.setBorder(BorderFactory.createTitledBorder(blackBorder, "Chat Area", 0,
			0, new Font("Titillium", Font.BOLD, 20), Color.BLACK));
        chatArea.setFont(font);
        chatArea.setBackground(Color.WHITE);
		chatArea.setMargin(new Insets(5,10,10,10));
		
		// Initialize the panel for chat input
		chatInputPanel = new JPanel();
		chatInputPanel.setLayout(new FlowLayout());
		JLabel chatLabel = new JLabel("Message: ");
		
		chatInput = new JTextField();
		
		// Adding listener for chat panel
		chatInput.addActionListener(new ChatInputActionListener());
		chatInput.setPreferredSize(new Dimension(310,30));
		
		chatInputPanel.add(chatLabel);
		chatInputPanel.add(chatInput);
		chatInputPanel.setMaximumSize(new Dimension(400,30));
		
		
		// Initialize the panel for the buttons
		bottomPanel = new JPanel();
		bottomPanel.setLayout(new GridLayout());
		bottomPanel.setMaximumSize(new Dimension(9000,30));
				
		// Initialize the play and pass buttons for the GUI
		playButton = new JButton("Play");
		passButton = new JButton("Pass");
				
		// Add the the initialized buttons to the bottom panel
		bottomPanel.add(playButton);
		bottomPanel.add(passButton);
				
		// Initialize the listener for the buttons
		playButtonListener = new PlayButtonListener();
		passButtonListener = new PassButtonListener();
				
		// Add the listener for the buttons.
		playButton.addActionListener(playButtonListener);
		passButton.addActionListener(passButtonListener);
	}
	
	// A method that returns an array of indices of the cards selected.
	private int[] getSelected() {
		
		int amountSelected = 0;
		for (int i = 0; i < game.getPlayerList().get(activePlayer).getNumOfCards(); i++) {
			for (int j = 0; j < selected.size(); j++) {
				if (selected.getCard(j).equals(game.getPlayerList().get(activePlayer).getCardsInHand().getCard(i))) {
					amountSelected++;
				}
			}
		}
		// Initialize an array size according to the amount of card selected.
		int[] indexSelected = new int[amountSelected];
		
		// Put the index of the selected cards into the initialized array
		int counter = 0;
		for (int i = 0; i < game.getPlayerList().get(activePlayer).getNumOfCards(); i++) {
			for (int j = 0; j < selected.size(); j++) {
				if (selected.getCard(j).equals(game.getPlayerList().get(activePlayer).getCardsInHand().getCard(i))) {
					indexSelected[counter] = i;
					counter++;
				}
			}
		}
		return indexSelected;
	}
	
	// A method that resets the list of selected cards to an empty list.
	private void resetSelected() {
		
		selected.removeAllCards();  // resets the selected cards
	}
	
	
	/**
	 * An inner class that extends the JPanel class and implements the 
	 * MouseListener interface. Overrides the paintComponent() method inherited 
	 * from the JPanel class to draw the card game table.
	 */
	
	// @SuppressWarnings("serial")
	class BigTwoPanel extends JPanel{
		
		/**
		 * This method draws the image of the table on the bigTwoPanel.
		 * @param g  the Graphics object to protect
		 */
		public void paintComponent(Graphics g) {
			super.paintComponent(g);
			Image background = new ImageIcon("Resources/Table.jpeg").getImage();
			g.drawImage(background, 0, 0, bigTwoPanel.getWidth(), bigTwoPanel.getHeight(), this);
		}
		
	}
	
	/**
	 * An inner class that implements the ActionListener interface. Implements the 
	 * actionPerformed() method from the ActionListener interface to handle button-click 
	 * events for the "Play" button. When the "Play" button is clicked, it calls 
	 * the makeMove() method of your CardGame object to make a move.
	 */
	
	class PlayButtonListener implements ActionListener {
		/**
		 * This method allows the player to pick the card and make a move in the 
		 * big two game.It is invoked when an action occurs.
		 * @param e A semantic event which indicates that a component-defined action occurred. 
		 */
		public void actionPerformed(ActionEvent e) {
			if (getSelected().length != 0) {
				game.makeMove(activePlayer, getSelected());
			}
			else {
				printMsg("Not a legal move!!!\n");
			}
		}	
	}
	
	/**
	 * An inner class that implements the ActionListener interface. Implements the actionPerformed()
	 * method from the ActionListener interface to handle button-click events for the "Pass" button. 
	 * When the "Pass" button is clicked, it should call the makeMove() method of your CardGame object
	 *  to make a move.
	 */
	class PassButtonListener implements ActionListener {
		/**
		 * This method allows the player to pass a move in the 
		 * big two game. It is invoked when an action occurs.
		 * 
		 * @param e A semantic event which indicates that a component-defined action occurred. 
		 * 
		 */
		public void actionPerformed(ActionEvent e) {
			game.makeMove(activePlayer, null);
		}
	}
	
	
	/**
	 * An inner class that implements the ActionListener interface. Implements the 
	 * actionPerformed() method from the ActionListener interface to handle menu-item-click
	 * events for the "Restart" menu item. When the "Restart" menu item is selected, it should 
	 * (i) create a new BigTwoDeck object and call its shuffle() method; and 
	 * (ii) call the start() method of your CardGame object with the BigTwoDeck object as an argument.
	 */
	
	class RestartMenuItemListener implements ActionListener {
		/**
		 * This method handles events related to the restarting of the game
		 * when "Restart" is clicked.
		 * @param e A semantic event which indicates that a component-defined action occurred. 
		 */
		public void actionPerformed(ActionEvent e) {
			BigTwoDeck newDeck = new BigTwoDeck();  // new deck
			newDeck.initialize();  // initializing the deck
			newDeck.shuffle();   // shuffles the deck
			reset();  // resets everything
			printMsg("Welcome to the BigTwo game!\n");
			chatArea.setText("");  // clears the chat area
			game.start(newDeck);  // starts the game again
		}
		
	}
	
	/**
	 * An inner class that implements the ActionListener interface. Implements the 
	 * actionPerformed() method from the ActionListener interface to handle menu-item-click
	 * events for the "Quit" menu item. When the "Quit" menu item is selected, it terminates
	 * the application.
	 */
	class QuitMenuItemListener implements ActionListener {
		/**
		 * This method exits the game when "Quit" is clicked.
		 * @param e A semantic event which indicates that a component-defined action occurred. 
		 */
		public void actionPerformed(ActionEvent e) {
			System.exit(0);
		}
		
	}
	
	
	/**
	 * An inner class that extends JLayeredPane, which allows the panels to be stacked and overlap
	 * over each other. This panel holds the card panel which becomes the hand of the player.
	 * It also check if there is a winner, if the active player is a winner, 
	 * this class don't need to create the layered pane for the players.
	 */
	class HandPanel extends JLayeredPane {
	
		private CardGamePlayer player;
		
		/**
		 * A constructor that create layered panels for the cards.
		 * @param player The active player
		 */
		HandPanel(CardGamePlayer player){
			
			this.setOpaque(false);
			this.player = player;
			// Set the size of the panel
			this.setPreferredSize(new Dimension ((int) Toolkit.getDefaultToolkit().getScreenSize().getWidth(), 30));
			if (!game.endOfGame()) {
				for (int i = 0; i < player.getNumOfCards(); i++) {
					// Setting up card panel
					CardPanel card = new CardPanel(i, player);
					card.setBounds(50 + (i*30), 15, 81, 111);
					this.add(card, i);
				}
			}	
		}
	}
	
	/**
	 * An inner class that extends the JPanel class and implements the MouseListener interface. It Overrides
	 * the paintComponent() method inherited from the JPanel class to draw the card. It also Implements the
	 * mouseClicked() method from the MouseListener interface to handle mouse click events for each card.
	 * It will add the selected array according to the card if the card is selected and will bring the card
	 * upward if the card is selected.
	 */
	class CardPanel extends JPanel implements MouseListener{
		
		private int cardPosition;
		private CardGamePlayer player;
		private boolean clicked;
		
		/**
		 * A constructor that adds mouse listener to the cards to know 
		 * the position of the place where you can click the card.
		 * 
		 * @param cardPosition The position of the card using the index of the card in the list
		 * @param player  The active Player
		 * 
		 */
		CardPanel(int cardPosition, CardGamePlayer player){
			clicked = false;
			this.cardPosition = cardPosition;
			this.player = player;
			addMouseListener(this);
		}
		
		/**
		 * The method redraws the card.
		 * @param g  the Graphics object to protect
		 */
		public void paintComponent(Graphics g) {
			g.drawImage(imagesOfCards[player.getCardsInHand().getCard(cardPosition).getSuit()][player.getCardsInHand()
			.getCard(cardPosition).getRank()], 0, 0, 81, 111, this);
		}
		
		/**
		 * This method raises the card if the card is clicked.
		 * @param e A semantic event which indicates that a component-defined Mouse action occurred. 
		 */
		public void mouseClicked(MouseEvent e) {
			if (!clicked) {
				clicked = true;
				this.setBounds(50 + (cardPosition * 30), 0, 81, 111);
				selected.addCard(player.getCardsInHand().getCard(cardPosition)); 
			}
			else {
				clicked = false;
				this.setBounds(50 + (cardPosition * 30), 15, 81, 111);
				selected.removeCard(player.getCardsInHand().getCard(cardPosition));
			}
			frame.repaint();
		}

		/*
		 * This method does nothing in this class
		 * @param e A semantic event which indicates that a component-defined Mouse action occurred. 
		 */
		public void mousePressed(MouseEvent e) {}

		/*
		 * This method does nothing in this class
		 * 
		 * @param e A semantic event which indicates that a component-defined Mouse action occurred. 
		 */
		public void mouseReleased(MouseEvent e) {}

		/*
		 * This method changes the border so that we can know which card is selected.
		 * 
		 * @param e A semantic event which indicates that a component-defined Mouse action occurred. 
		 */
		public void mouseEntered(MouseEvent e) {
			this.setBorder(BorderFactory.createLineBorder(Color.BLACK));
		}

		/*
		 * This method removes border when mouse is not above the card.
		 * @param e A semantic event which indicates that a component-defined Mouse action occurred. 
		 */
		public void mouseExited(MouseEvent e) {
			this.setBorder(null);
		}
		
	}
	
	/**
	 * An inner class that extends JPanel. It creates a panel for each of the player.
	 * It override the paint component method to draw the avatar image of the player. 
	 * Also prints the winner image if the player is the winner. If not winner, draw 
	 * the remaining cards for the player.It draws the card backs of the players if the
	 * player is not active player to show number of cards left.
	 */
	class PlayerPanel extends JPanel {
		
		CardGamePlayer playerName;
		int playerNumber;
		
		/**
		 * A constructor which provides values to the instance variables.
		 * @param playerName  The name of the player
		 * @param playerNumber  The number of player
		 */
		PlayerPanel(CardGamePlayer playerName, int playerNumber) {
			this.playerName = playerName;
			this.playerNumber = playerNumber;
		}
		
		/**
		 * It redraws the image of the cards face-down for inactive players. If the winner is 
		 * found it redraws the cards face-up with winner label.
		 * @param g  the Graphics object to protect
		 */
		public void paintComponent(Graphics g) {
			g.drawImage(playerAvatars[playerNumber], 10, 30, 110, 110, this);
			
			if (game.endOfGame()) {
				for (int i = 0; i < playerName.getNumOfCards(); i++) {
					g.drawImage(imagesOfCards[playerName.getCardsInHand().getCard(i).getSuit()][playerName.getCardsInHand().getCard(i).getRank()], 150 + (30*i), 25, 81, 111, this);
				}
			}
			else if (playerNumber != activePlayer) {
				for (int i = 0; i < playerName.getNumOfCards(); i++) {
					g.drawImage(backOfCard, 150 + (30*i), 20, 81, 111, this);
				}
			}
		}
	}	
	
	
	/**
	 * An inner class that extends the JLayer, it is added at the last after the player panel.
	 * It overrides the paintComponent method to print the background image for the table.
	 * It also put the last hand played on table if there is one.
	 */
	class TablePanel extends JPanel {
		/**
		 * It redraws the table where cards would be placed and it 
		 * also redraws the card of the last hand played.
		 * 
		 * @param g the Graphics object to protect
		 */
		public void paintComponent(Graphics g) {
			g.drawImage(imageOfTable, 0, -70, bigTwoPanel.getWidth(), 400, this);
			if (game.getHandsOnTable().size() != 0) {
				for (int i = 0; i < game.getHandsOnTable().get(game.getHandsOnTable().size() - 1).size(); i++) {
					g.drawImage(imagesOfCards[game.getHandsOnTable().get(game.getHandsOnTable().size() - 1).getCard(i)
					.getSuit()][game.getHandsOnTable().get(game.getHandsOnTable().size() - 1).getCard(i).getRank()],
					200 + (i*30), 22, 81, 111, this);
				}
			}
		}
	}
	
	/**
	 * An inner class that implements ActionListener for clearing the 
	 * chat area of the big two game.
	 */
	class ClearChatListener implements ActionListener {
		
		/**
		 * This method clear the chat area of the Big Two Game.
		 * 
		 * @param e A semantic event which indicates that a component-defined action occurred.
		 * 
		 */
		public void actionPerformed(ActionEvent e) {
			chatArea.setText("");
		}
		
	}
	
	/**
	 * An inner class that implements ActionListener for putting messages from
	 * chat Input panel to the chat text area. 
	 */
	class ChatInputActionListener implements ActionListener {
		
		/**
		 * This chat appends the messages sent by the player to the chat area.
		 * 
		 * @param e A semantic event which indicates that a component-defined action occurred.
		 * 
		 */
		public void actionPerformed(ActionEvent e) {
			if (activePlayer == 0) {
				chatArea.append("Player 0: " + chatInput.getText() + "\n");
			}
			else if (activePlayer == 1) {
				chatArea.append("Player 1: " + chatInput.getText() + "\n");
			}
			else if (activePlayer == 2) {
				chatArea.append("Player 2: " + chatInput.getText() + "\n");
			}
			else if (activePlayer == 3) {
				chatArea.append("Player 3: " + chatInput.getText() + "\n");
			}
			chatInput.setText("");
		}		
	}
}
