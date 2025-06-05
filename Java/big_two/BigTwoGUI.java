// Libraries being Used
import java.awt.*;
import java.awt.event.ActionEvent;
import java.awt.event.ActionListener;
import java.awt.event.MouseEvent;
import java.awt.event.MouseListener;
import java.util.ArrayList;

import javax.swing.*;
import javax.swing.border.*;




/**
 * BigTwoGUI class builds a GUI for the game and handles all
 * the user actions. This implements CardGameUI interface and has
 * a number of inner classes responsible for rendering the user interface and
 * handling user actions.
 */
public class BigTwoGUI implements CardGameUI {
	
	
	/** Big Two card game associates with this GUI. */
	private BigTwo game;
	/** A boolean array indicating which cards are being selected. */
	private boolean[] selected;
	/** Integer specifying the index of the active player. */
	private int activePlayer;
	/** Main window of the application. */
	private JFrame frame;
	/** Panel for showing the cards of each player and the cards played on the table */
	private JPanel bigTwoPanel;
	/** A "Play" button for the active player to play the selected cards. */
	private JButton playButton;  // 
	/** A "Pass" button for the active player to pass his/her turn to the next player*/
	private JButton passButton;
	/** Text area for showing the current game status as well as end of game messages. */
	private JTextArea msgArea;
	/** Text area for showing chat messages sent by the players. */
	private JTextArea chatArea;
	/** Text field for players to input chat messages.  */
	private JTextField chatInput;
	
	
	/** Right panel to setup the message and chat area */
	private JPanel rightPanel;
	/** 2-dimensional array storing images of the 52 playing poker cards. */
	private Image[][] imagesOfCards;
	/** Instance variable stores the image of the back of the card. */
	private Image backOfCard; 
	/** Array to store avatars of the players */
	private Image[] playerAvatars;
	/** Image for empty player slot */
	private Image emptyAvatar;
	/** Instance variable that saves the image of the icon of the application */
	private Image imageOfLogo;
	/** Panel that holds text-field and label. */
	private JPanel chatInputPanel;
	private PlayButtonListener playButtonListener;
	private PassButtonListener passButtonListener;
		
	
	/**
	 * 
	 * Constructor for creating a BigTwoGUI. The parameter game is a
	 * reference to a Big Two card game associates with this GUI.
	 * @param game a reference to a card game associates with this table.
	 * 
	 */
	
	public BigTwoGUI(BigTwo game) {
		this.game = game;   // create the game object
		LoadImages();       //Load necessary images
		setupGUI();     // Setup GUI
		
	}
	
	
	/**
	 * Method for setting the index of the 
	 * active player (i.e. the player having control of the GUI).  
	 * @param activePlayer  an integer value representing the index of the active player
	 *            
	 */
	public void setActivePlayer(int activePlayer) {
		if (activePlayer < 0 || activePlayer >= game.getPlayerList().size()) {this.activePlayer = -1;
		} 
		else { this.activePlayer = activePlayer;     // setting active Player
		}
	}

	/**
	 * Method for repainting the GUI.
	 *   
	 */
	public void repaint() {
		frame.setContentPane(new JPanel(new GridLayout(1,2)));
		
		JMenuBar menuBar = new JMenuBar();
		JMenu menuGame = new JMenu("Game");
		JMenuItem quitItem, connectItem;
		
		quitItem = new JMenuItem("Quit");
		connectItem = new JMenuItem("Connect");
		
		ConnectMenuItemListener connectListener = new ConnectMenuItemListener();
		QuitMenuItemListener quitListener = new QuitMenuItemListener();
		
		connectItem.addActionListener(connectListener);
		quitItem.addActionListener(quitListener);
		
		menuGame.add(connectItem); menuGame.add(quitItem); 
		menuBar.add(menuGame);
		
		frame.setJMenuBar(menuBar);
		
		JMenu menuMessage = new JMenu("Message");
		JMenuItem clearChat = new JMenuItem("Clear Chat Box");
		
		ClearChatListener clearListener = new ClearChatListener();
		clearChat.addActionListener(clearListener);
		
		menuMessage.add(clearChat);
		
		menuBar.add(menuMessage);
		
		bigTwoPanel = new TableBackground("Resources/Table.jpeg");
		bigTwoPanel.setLayout(new BorderLayout());
		
		JPanel bottom = new JPanel();
		bottom.setLayout(new GridLayout(1,2));
		bottom.add(playButton); bottom.add(passButton);
		
		if(game.getCurrentPlayerIdx() == activePlayer) {enable();}
		else {disable();}
		
		bigTwoPanel.add(bottom, BorderLayout.SOUTH);
		
		JPanel playerPanel;
		playerPanel = new JPanel();
		playerPanel.setBackground(new Color(0, 0, 0, 0));
		playerPanel.setLayout(new GridLayout(5, 1));
		
		if(game.endOfGame() != true  && game.getStarted() == true) {
			resetSelected();
			if(game.getCurrentPlayerIdx() == activePlayer) {
				promptActivePlayer();
			} else { 
				printMsg(game.getPlayerList().get(game.getCurrentPlayerIdx()).getName() + "'s turn:");
			}
		} else {
			resetSelected();
			disable();
		}
		
		JPanel player0Panel; JPanel player1Panel; JPanel player2Panel; JPanel player3Panel;
		if(game.getPlayerList().get(0).getName() != null && game.getPlayerList().get(0).getName() != "") {
			player0Panel = new BigTwoPanel(playerAvatars[0]);
		} else {
			player0Panel = new BigTwoPanel(emptyAvatar);
		}
		
		player0Panel.setBackground(new Color(213, 134, 145, 50));
		player0Panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
		player0Panel.setLayout(new FlowLayout(FlowLayout.LEFT));
		JLabel player0Label = new JLabel();
		
		if(game.getPlayerList().get(0).getName() != null) {
			if(game.getClient().getPlayerID() == 0) { player0Label.setText("You");
			} else {
				player0Label.setText(game.getPlayerList().get(0).getName());
			}
		}
		
		player0Label.setForeground(Color.WHITE);
		player0Panel.add(player0Label);
		JLayeredPane layeredPane0 = new JLayeredPane();
		layeredPane0.setPreferredSize(new Dimension(1000, 1000));
		printerCardPanel(0, layeredPane0, player0Panel);
		if(game.getPlayerList().get(1).getName() != null && game.getPlayerList().get(1).getName() != "") {
			player1Panel = new BigTwoPanel(playerAvatars[1]);
		} else { player1Panel = new BigTwoPanel(emptyAvatar);
		}
		
		player1Panel.setBackground(new Color(0, 134, 145, 50));
		player1Panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
		player1Panel.setLayout(new FlowLayout(FlowLayout.LEFT));
		JLabel player1Label = new JLabel();
		
		if(game.getPlayerList().get(1).getName() != null) {
			if(game.getClient().getPlayerID() == 1) { player1Label.setText("You");
			} else { player1Label.setText(game.getPlayerList().get(1).getName());
			}
		}
		
		player1Label.setForeground(Color.WHITE);
		player1Panel.add(player1Label);
		JLayeredPane layeredPane1 = new JLayeredPane();
		layeredPane1.setPreferredSize(new Dimension(1000, 1000));
		printerCardPanel(1, layeredPane1, player1Panel);
		
		if(game.getPlayerList().get(2).getName() != null && game.getPlayerList().get(2).getName() != "") {
			player2Panel = new BigTwoPanel(playerAvatars[2]);
		} else { player2Panel = new BigTwoPanel(emptyAvatar);
		}
		
		player2Panel.setBackground(new Color(213, 0, 145, 50));
		player2Panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
		player2Panel.setLayout(new FlowLayout(FlowLayout.LEFT));
		JLabel player2Label = new JLabel();
		
		if(game.getPlayerList().get(2).getName() != null) {
			if(game.getClient().getPlayerID() == 2) { player2Label.setText("You");
			}
			else { player2Label.setText(game.getPlayerList().get(2).getName());
			}
		}
		
		player2Label.setForeground(Color.WHITE);
		player2Panel.add(player2Label);
		JLayeredPane layeredPane2 = new JLayeredPane();
		layeredPane2.setPreferredSize(new Dimension(1000, 1000));
		printerCardPanel(2, layeredPane2, player2Panel);
		
		if(game.getPlayerList().get(3).getName() != null && game.getPlayerList().get(3).getName() != "") {
			player3Panel = new BigTwoPanel(playerAvatars[3]);
		} else {
			player3Panel = new BigTwoPanel(emptyAvatar);
		}
		
		player3Panel.setBackground(new Color(213, 134, 0, 50));
		player3Panel.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
		player3Panel.setLayout(new FlowLayout(FlowLayout.LEFT));
		JLabel player3Label = new JLabel();
		
		if(game.getPlayerList().get(3).getName() != null) {
			if(game.getClient().getPlayerID() == 3) { player3Label.setText("You");
			} else {
				player3Label.setText(game.getPlayerList().get(3).getName());
			}
		}
		
		player3Label.setForeground(Color.WHITE);
		player3Panel.add(player3Label);
		JLayeredPane layeredPane3 = new JLayeredPane();
		layeredPane3.setPreferredSize(new Dimension(1000, 1000));
		printerCardPanel(3, layeredPane3, player3Panel);
		
		JPanel table = new JPanel();
		table.setBackground(new Color(213, 134, 0, 0));
		table.setBorder(BorderFactory.createBevelBorder(BevelBorder.RAISED));
		table.setLayout(new FlowLayout(FlowLayout.LEFT));
		
		JLabel tableLabel = new JLabel();
		tableLabel.setForeground(Color.WHITE);
		table.add(tableLabel);
		JLayeredPane layeredPaneTable = new JLayeredPane();
		Hand lastHandOnTable;
		
		if(game.getHandsOnTable().isEmpty()) {
			lastHandOnTable = null;
			tableLabel.setText("Empty");
		} else {
			lastHandOnTable = game.getHandsOnTable().get(game.getHandsOnTable().size() - 1);
			tableLabel.setText("Played by " + lastHandOnTable.getPlayer().getName());
		}
		
		layeredPaneTable.setPreferredSize(new Dimension(1000, 1000));
		
		if(lastHandOnTable != null) {
			for(int i = 0; i < lastHandOnTable.size(); i ++) {
				CardPanel cardPanel;
				Card card = lastHandOnTable.getCard(i);
				int x = 20 + 27 * i;
				int y = 5;
				cardPanel = new CardPanel(imagesOfCards[card.getSuit()][card.getRank()], x, y, activePlayer, card, i);	
				cardPanel.setBounds(x, y, 80, 110);
				layeredPaneTable.add(cardPanel, 0);
			}
			table.add(layeredPaneTable);
		}
		
		// Addplayer panels to playerPanel
		playerPanel.add(player0Panel);
		playerPanel.add(player1Panel);
		playerPanel.add(player2Panel);
		playerPanel.add(player3Panel);
		playerPanel.add(table);
		
		// Add playerPanel to bigTwoPanel
		bigTwoPanel.add(playerPanel);
		
		// Add bigTwoPanel to frame
		frame.add(bigTwoPanel);
		
		// Add scrollers
		JScrollPane msgScrollPane1 = new JScrollPane(msgArea);
		JScrollPane chatScrollPane2 = new JScrollPane(chatArea);
		

		// Add message and chat area
		JPanel otherPanel = new JPanel();
		rightPanel = new JPanel();
		rightPanel.setLayout(new GridLayout(2,1));
		rightPanel.add(msgScrollPane1);
		rightPanel.add(chatScrollPane2);
		
		otherPanel.setLayout(new BorderLayout());
		otherPanel.add(rightPanel);
		otherPanel.add(chatInputPanel, BorderLayout.SOUTH);
		
		frame.add(otherPanel);
		msgArea.setCaretPosition(msgArea.getDocument().getLength());
		chatArea.setCaretPosition(chatArea.getDocument().getLength());
		
		// Make the panel visible
		frame.setVisible(true);
	}
	
	/**
	 * Method for printing the specified string to the message area of the GUI.
	 * @param msg  the string to be printed to the message area of the Big Two Game          
	 *            
	 */
	public void printMsg(String msg) {
		msgArea.append(msg + "\n");
	}
	
	/**
	 * Method for clearing the message area of the GUI. 
	 *  
	 */
	public void clearMsgArea() {
		msgArea.setText(null);
	}
	
	/**
	 * Method for printing messages on the chat area of the GUI.
	 * @param msg  the string to be printed to the chat area of the Big Two Game 
	 */
	
	public void printChatMsg(String msg) {
		chatArea.append(msg + "\n");
	}
	
	/**
	 * Method for clearing the chat area of the GUI. 
	 *  
	 */
	
	public void clearChatMsgArea() {
		chatArea.setText(null);
	}
	
	/**
	 * Method for resetting the GUI. You should 
	 * reset the list of selected cards; 
	 * clear the message area; and 
	 * enable user interactions.
	 * 
	 */
	public void reset() {
		resetSelected(); // reset the list of selected cards; 
		clearMsgArea();  // clear the message area; and 
		enable();        // enable user interactions.
	}
	
	
	/**
	 * Method for enabling user interactions with the GUI. You should 
	 * enable the "Play" button and "Pass" button (i.e., making them click-able); 
	 * enable the BigTwoPanel for selection of cards through mouse clicks.
	 * 
	 */
	
	public void enable() {
		playButton.setEnabled(true);  
		passButton.setEnabled(true);  
		bigTwoPanel.setEnabled(true); 
	}
	
	
	/**
	 * Method for disabling user interactions with the GUI.  You should 
	 * disable the "Play" button and "Pass" button (i.e. making them not click-able);  
	 * disable the BigTwoPanel for selection of cards through mouse clicks.
	 *  
	 */
	public void disable() {
		playButton.setEnabled(false);
		passButton.setEnabled(false);  
		bigTwoPanel.setEnabled(false); 
		
	}
	
	/**
	 *  Method to display A message in the message area showing it 
	 *  is the the player's turn.
	 *  
	 */
	public void promptActivePlayer() {
		printMsg("Your turn:");
	}
		
	
	
    // Method to load and put essential Images
	private void LoadImages() {
		playerAvatars = new Image[4];
		
		playerAvatars[0] = new ImageIcon("Resources/Avatars/Player0.png").getImage();
		playerAvatars[1] = new ImageIcon("Resources/Avatars/Player1.png").getImage();
		playerAvatars[2] = new ImageIcon("Resources/Avatars/Player2.png").getImage();
		playerAvatars[3] = new ImageIcon("Resources/Avatars/Player3.png").getImage();
		
		// Put image of Empty avatar
		emptyAvatar = new ImageIcon("Resources/Avatars/Empty.png").getImage();
		
		// Set up the ImageIcon
		imageOfLogo = new ImageIcon("Resources/Logo/Logo.png").getImage();  // creating an ImageIcon object
		
		// Put card images in the 2-dimensional list
		imagesOfCards = new Image[4][13];
		for(int i = 0; i < 4; i ++) {
			for (int j = 0; j < 13; j++) {
				Image source = new ImageIcon("Resources/Cards/" + j + "-" + i + ".gif").getImage();
				Image resized = resizer(source, 80, 110);
				imagesOfCards[i][j] = new ImageIcon(resized).getImage();
			}
		}
		
		// Put Image of the back of the card
		Image cardBackSrc = new ImageIcon("Resources/Cards/backCard.gif").getImage();
		backOfCard = new ImageIcon(resizer(cardBackSrc, 80, 110)).getImage();
		
	}

	// Private method to setup the Graphical User Interface for the game
	private void setupGUI() {
	// Create the frame for the GUI
	frame = new JFrame("Big Two Game");
	frame.setDefaultCloseOperation(JFrame.EXIT_ON_CLOSE);
	frame.setIconImage(imageOfLogo);  //change icon of frame
	frame.setLocation(130, 100);      // setting the position on the screen
	frame.setSize(new Dimension(1200, 850));   // giving the frame size
	GridLayout layout = new GridLayout(1,2);
	frame.setLayout(layout);   // Setting GridLayout

	// Initialize the message area and it's properties
	msgArea = new JTextArea("Welcome to the BigTwo game!\n");
	msgArea.setFont(new Font("Titillium", Font.PLAIN, 15));
	msgArea.setLineWrap(true);
	msgArea.setEditable(false);
	msgArea.setBackground(Color.WHITE);
			
	// Initialize the chat area and its properties.
	chatArea = new JTextArea("Welcome to the Chat Box\n");
	chatArea.setLineWrap(true);
	chatArea.setEditable(false);
    Font font = new Font("Segoe Script", Font.PLAIN, 15);
    chatArea.setFont(font);
    chatArea.setBackground(Color.WHITE);
    
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
			
    // Initialize the play and pass buttons for the GUI
	playButton = new JButton("Play"); passButton = new JButton("Pass");
	
	// Initialize the listener for the buttons
	playButtonListener = new PlayButtonListener();
	passButtonListener = new PassButtonListener();
	
	// Add the listener for the buttons.
	playButton.addActionListener(playButtonListener);
	passButton.addActionListener(passButtonListener);
			
	}
	
	// Method to resize an image
	private Image resizer(Image source, int width, int height) {
		source = new ImageIcon(source).getImage();
		Image resized = source.getScaledInstance(width, height, Image.SCALE_SMOOTH);
		return resized;
	}
	
	// Method to print the cards of each player on each player panel
	private void printerCardPanel(int playerIdx, JLayeredPane layeredPane, JPanel eachPlayerPanel) {
		// getting number of cards in hand
		int numOfCardsInHand = game.getPlayerList().get(playerIdx).getNumOfCards();
		for(int i = 0; i < numOfCardsInHand; i ++) {
			CardPanel button;
			Card card = game.getPlayerList().get(playerIdx).getCardsInHand().getCard(i);
			int x = 170 + 27 * i;
			int y = 20;
			//opens up the cards of active player and makes them click-able
			if(playerIdx == activePlayer && game.getStarted() == true) { 
				button = new CardPanel(imagesOfCards[card.getSuit()][card.getRank()], x, y, playerIdx, card, i);
				if(playerIdx == game.getCurrentPlayerIdx()) { button.addMouseListener(button);
				}
			}
			else {
				//opens up all cards when the game ends
				if(game.endOfGame() == true) { button = new CardPanel(imagesOfCards[card.getSuit()][card.getRank()], x, y, playerIdx, card, i);
				}
				//close all cards for all non active players
				else { button = new CardPanel(backOfCard, x, y, playerIdx, card, i);
				}
			}
			button.setBounds(x, y, 80, 110);
			layeredPane.add(button, 0);
		}
		eachPlayerPanel.add(layeredPane);
	}

	// Method that returns an array of indices of the cards selected.
	private int[] getSelected() {
		ArrayList<Integer> intSelected = new ArrayList<Integer>();
		for(int i = 0; i < selected.length; i++) {
			if(selected[i] == true) { intSelected.add(i);
			}
		}
		
		// Put the index of the selected cards into the initialized array
		int size = intSelected.size();
		int[] intArraySelected = new int[size];
		for(int i = 0; i <size; i++) { intArraySelected[i] = intSelected.get(i); 
		}
		return intArraySelected;
	}

	// Method that resets the list of selected cards to an empty list.
	private void resetSelected() {
		 // resets the selected cards
		selected = new boolean[game.getPlayerList().get(activePlayer).getNumOfCards()];
	}
	
	
	/**
	 * An Inner class extends JPanel and implements MouseListener.
	 * Designs a card that is printed on the Graphical User Interface.
	 * printed on the GUI.

	 */
	class CardPanel extends JPanel implements MouseListener{
		// Instance variables
		private boolean raised = false;
		private Image image;
		private int x;
		private int y;
		private int playerIdx;
		private Card card;
		private int index;
		
		/**
		 * This is a constructor that allots values to the instance variabels
		 * 
		 * @param image Image of the card
		 * @param x  x-coordinate of the card
		 * @param y  y-coordinate of the card
		 * @param playerIdx Index of the player that holds the card
		 * @param card   Playing card object
		 * @param index  Playing card index
		 */
		public CardPanel(Image image, int x, int y, int playerIdx, Card card, int index) {
			this.card = card;
			this.index = index;
			this.playerIdx = playerIdx;
			this.x = x;
			this.y = y;
			this.image = image;
			
			Dimension size = new Dimension(80, 110);
			setPreferredSize(size);
			setMinimumSize(size);
	        setMaximumSize(size);
	        setSize(size);
		}
		
		/**
		 * Method to redraws the image of card.
		 * @param g  the Graphics object to protect
		 * 
		 */
		public void paintComponent(Graphics g) {
	        g.drawImage(image, 0, 0, getWidth(), getHeight(), null);
	    }
		/**
		 * Method raises the card if the card is clicked.
		 * 
		 * @param e A semantic event which indicates that a component-defined Mouse action occurred. 
		 * 
		 */
		public void mouseClicked(MouseEvent e) {
			
			if(!raised) {
				setBounds(x, 0, 80, 110);
				selected[index] = true;
				raised = true;
			}
			else {
				setBounds(x, 20, 80, 110);
				selected[index] = false;
				raised = false;
			}
			frame.repaint();
		}
		
		/*
		 * Method does nothing in this class
		 * @param e A semantic event which indicates that a component-defined Mouse action occurred. 
		 * 
		 */
		public void mousePressed(MouseEvent e) {}
		/*
		 * Method does nothing in this class
		 * @param e A semantic event which indicates that a component-defined Mouse action occurred. 
		 * 
		 */
		
		public void mouseReleased(MouseEvent e) {}

		/*
		 * Method changes the border so that we can know which card is selected.
		 * 
		 * @param e A semantic event which indicates that a component-defined Mouse action occurred. 
		 * 
		 */
		public void mouseEntered(MouseEvent e) {
			this.setBorder(BorderFactory.createLineBorder(Color.BLACK));
		}
		
		/*
		 * Method removes border when mouse is not above the card.
		 * @param e A semantic event which indicates that a component-defined Mouse action occurred. 
		 * 
		 */
		public void mouseExited(MouseEvent e) {
			this.setBorder(null);
		}
	}
	
	/**
	 * Inner class that extends the JPanel class and implements the 
	 * MouseListener interface. Overrides the paintComponent() method inherited 
	 * from the JPanel class to draw the card game table.
	 */
	class BigTwoPanel extends JPanel{
		private Image avatar;   // image of avatar
		/**
		 * It resizes and provides avatar image to the avatar 
		 * private instance variable.
		 * 
		 * @param avatar  Image of the player avatar.
		 * 
		 */
		BigTwoPanel(Image avatar) {
			Image resized = resizer(avatar, 150, 150);
			this.avatar = new ImageIcon(resized).getImage();
		}
		
		/**
		 * Method draws the image of the avatar on the bigTwoPanel.
		 * @param g  the Graphics object to protect
		 * 
		 */
		
		public void paintComponent(Graphics g) {
			g.drawImage(avatar, 5, 20, avatar.getWidth(this), avatar.getHeight(this), this);

		}
	}
	
	/**
	 * Inner class that extends the JPanel class. 
	 * Overrides the paintComponent() to draw the 
	 * table. 
	 */
	class TableBackground extends JPanel{
		
		private Image image;  // Store the image of the table
		/**
		 * Constructor that takes image of the table.
		 * @param image A string variable having the location of the image.
		 */
		
	    public TableBackground(String image) {
	        this(new ImageIcon(image).getImage());
	    }

	    /**
	     *  Constructor that takes image of the table.
	     * 
	     * @param image An image object to having the image of the table.
	     */
	    public TableBackground(Image image) {
	        this.image = image;
	        Dimension size = new Dimension(image.getWidth(null), image.getHeight(null));
	        setPreferredSize(size);
	        setMinimumSize(size);
	        setMaximumSize(size);
	        setSize(size);
	        setLayout(null);
	    }
	    
		/**
		 * Method to the draw the image of the table.
		 * @param g  the Graphics object to protect
		 */
	    public void paintComponent(Graphics g) {
	        g.drawImage(image, 0, 0, getWidth(), getHeight(), null);
	    }
	}
	
	/**
	 * Inner class implements the ActionListener interface. Implements the 
	 * actionPerformed() method from the ActionListener interface to handle button-click 
	 * events for the "Play" button. When the "Play" button is clicked, it calls 
	 * the makeMove() method of your CardGame object to make a move.
	 */
	
	class PlayButtonListener implements ActionListener {
		/**
		 * Method allows the player to pick the card and make a move in the 
		 * big two game.It is invoked when an action occurs.
		 * 
		 * @param e A semantic event which indicates that a component-defined action occurred. 
		 */
		public void actionPerformed(ActionEvent e) {
			game.makeMove(activePlayer, getSelected());
		}
	}
	
	/**
	 * Inner class implements the ActionListener interface. Implements the actionPerformed()
	 * method from the ActionListener interface to handle button-click events for the "Pass" button. 
	 * When the "Pass" button is clicked, it should call the makeMove() method of your CardGame object
	 *  to make a move.
	 */
	class PassButtonListener implements ActionListener {
		/**
		 * Method allows the player to pass a move in the 
		 * big two game. It is invoked when an action occurs.
		 * 
		 * @param e A semantic event which indicates that a component-defined action occurred. 
		 */
		public void actionPerformed(ActionEvent e) {
			game.makeMove(activePlayer, null);
		}
	}
	
	/**
	 * Inner class implements the ActionListener interface for handling the menu-item-click 
	 * events for the "Connect" menu item. When the "Connect" menu item is selected, it will call connect() 
	 * method from the client if the client is not currently connected to the server.
	 */
	class ConnectMenuItemListener implements ActionListener {
		/**
		 * Method handles connection of local client with server.
		 * 
		 * @param e A semantic event which indicates that a component-defined action occurred. 
		 */
		public void actionPerformed(ActionEvent e) {
			
			if(game.getClient().getConnected() == true) {
				printMsg("You are already connected to the server!");
				frame.repaint();
			} else { 
				game.getClient().connect();
			}	
		}
	}
	
	
	/**
	 * Inner class that implements ActionListener for putting messages from
	 * chat Input panel to the chat text area. 
	 */
	class ChatInputActionListener implements ActionListener {
		/**
		 * Method handles the input in the text box
		 * @param e A semantic event which indicates that a component-defined action occurred. 
		 */
		public void actionPerformed(ActionEvent e) {
			CardGameMessage message = new CardGameMessage(CardGameMessage.MSG, -1, chatInput.getText());
			chatInput.setText("");
			chatInput.requestFocus();
			game.getClient().sendMessage(message);
		}
	}
	
	/**
	 * Inner class implements the ActionListener interface. Implements the 
	 * actionPerformed() method from the ActionListener interface to handle menu-item-click
	 * events for the "Quit" menu item. When the "Quit" menu item is selected, it terminates
	 * the application.
	 */
	
	class QuitMenuItemListener implements ActionListener {
		/**
		 * Method exits the game when "Quit" is clicked.
		 * @param e A semantic event which indicates that a component-defined action occurred. 
		 * 
		 */
		public void actionPerformed(ActionEvent e) {
			System.exit(0);
		}
	}
	
	/**
	 * Inner class implements ActionListener for clearing the 
	 * chat area of the big two game.
	 */
	class ClearChatListener implements ActionListener {
		/**
		 * Method clear the chat area of the Big Two Game.
		 * @param e A semantic event which indicates that a component-defined action occurred.
		 */
		public void actionPerformed(ActionEvent e) {
			chatArea.setText("");
		}
	}
}
