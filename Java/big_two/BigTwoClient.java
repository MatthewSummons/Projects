import java.io.IOException;
import java.io.ObjectInputStream;
import java.io.ObjectOutputStream;
import java.net.Socket;
import java.net.SocketException;

import javax.swing.JOptionPane;

/**
 * BigTwoClient class implements NetworkGame interface. It models Big Two game client that has to
 * establish connection and communicate with Big Two game server.
 */

public class BigTwoClient implements NetworkGame {
	
	/** BigTwo object for Big Two card game. */
	private BigTwo game;
	/** BigTwoGUI object for the Big Two card game. */
	private BigTwoGUI gui;
	/** A socket connection to the game server. */
	private Socket sock;
	/** A string specifying the IP address of the game server. */
	private String serverIP;
	/** An integer specifying the TCP port of the game server. */
	private int serverPort;
	/** A boolean value to determine connection of client. */
	private boolean connected;
	/** ObjectOutputStream for sending messages to server. */
	private ObjectOutputStream oos;
	/** An integer specifying the playerID (i.e., index) of local player. */
	private int playerID;
	/** A string specifying the name of the local player. */
	private String playerName;
	
		
	
	/**
	 * The constructor for creating Big Two client. 
	 * First parameter is reference to BigTwo object related to this client 
	 * and the second parameter is a reference to a BigTwoGUI object related to BigTwo object.
	 */
	
	public BigTwoClient(BigTwo game, BigTwoGUI gui) {
		this.game = game;
		this.gui = gui;
		game.setStarted(false);
		gui.repaint();
		playerName = JOptionPane.showInputDialog(null,"Enter Name: ");
		
		if (playerName == null) { System.exit(0);
		}
		
		while(playerName.isEmpty()) {
			playerName = JOptionPane.showInputDialog("Enter A valid Name: ");
			if (playerName == null) { System.exit(0);
			}
		}

		setPlayerName(playerName);
		// Set up IP
		setServerIP("127.0.0.1");   
		// Setup port
		setServerPort(2396);       
		
		connect();                  

		
	}
		
	
	/**
	 * A method for getting connection status of the BigTwoClient
	 * @return a boolean value depicting connected
	 */
	
	public boolean getConnected() {
		return connected;
	}
	
	/**
	 * A method for getting the playerID (i.e., index) of the local player.
	 * @return playerID of the Local player
	 */
	public int getPlayerID() {
		return playerID;
	}
	
	/**
	 * A method for setting the playerID of the local player. 
	 * This will be called from the parseMessage() method when message 
	 * of type PLAYER_LIST is received from game server.
	 * @param playerID  ID of the local player
	 */
	public void setPlayerID(int playerID) {
		this.playerID = playerID;
	}
	
	/**
	 * A method for getting local player's name.
	 * @return It returns playerName
	 */
	public String getPlayerName() {
		return playerName;
	}
	
	/**
	 * A method for setting local player's name.
	 * @param playerName  Name of local player
	 */
	public void setPlayerName(String playerName) {
		this.playerName = playerName;
	}
	
	/**
	 * A method for getting game server's IP address.
	 * @return It returns serverIP
	 */
	public String getServerIP() {
		return serverIP;
	}
	
	/**
	 * A method for setting game server's IP address.
	 * @param serverIP IP address of the game server
	 */
	public void setServerIP(String serverIP) {
		this.serverIP = serverIP;
	}
	
	/**
	 * A method for getting game server's TCP Port.
	 * @return It returns serverPort
	 */
	public int getServerPort() {
		return serverPort;
	}
	
	/**
	 * A method for setting game server's TCP Port.
	 * @param serverPort  The TCP port of the server
	 */
	public void setServerPort(int serverPort) {
		this.serverPort = serverPort;
	}
	
	/**
	 * A method for making a socket connection with the game server. 
	 * Upon successful connection, it will create an ObjectOutputStream 
	 * for sending messages to the game server; and create a thread for receiving
	 * messages from the game server.
	 */
	public void connect() {
		try {
			sock = new Socket(getServerIP(), getServerPort());
			oos = new ObjectOutputStream(sock.getOutputStream());  // create object output-stream
			//thread
			Runnable job = new ServerHandler();
			Thread readerThread = new Thread(job);  // create a thread
			readerThread.start();
			
		} catch (Exception e) {
			e.printStackTrace();
		}
	}
	
	
	/**
	 * A method for parsing the messages received from game server. 
	 * This method should be called from the thread responsible for receiving 
	 * messages from game server. Different actions will be 
	 * carried out based on message type.
	 */
	public void parseMessage(GameMessage message) {
		switch (message.getType()) {
		case CardGameMessage.PLAYER_LIST:
			connected = true;
			setPlayerID(message.getPlayerID());
			String[] nameOfPlayers = (String[]) message.getData();
			for(int i = 0; i < nameOfPlayers.length; i ++) { game.getPlayerList().get(i).setName(nameOfPlayers[i]);
			}
			CardGameMessage join = new CardGameMessage(CardGameMessage.JOIN, -1, getPlayerName());
			sendMessage(join);
			break;
			
		case CardGameMessage.JOIN:
			String data = (String) message.getData();
			game.getPlayerList().get(message.getPlayerID()).setName(data);
			if(playerID == message.getPlayerID()) {
				CardGameMessage ready = new CardGameMessage(CardGameMessage.READY, -1, null);
				sendMessage(ready);
			}
			gui.printMsg(game.getPlayerList().get(message.getPlayerID()).getName() + " joins the game.");
			gui.repaint();
			break;
			
		case CardGameMessage.FULL:
			connected = false;
			gui.printMsg("The server is full and you cannot join the game.");
			break;
			
		case CardGameMessage.QUIT:
			gui.printMsg("Player "+ game.getPlayerList().get(message.getPlayerID()).getName() + " has left the game");
			game.getPlayerList().get(message.getPlayerID()).setName("");
			CardGameMessage readyMessage = new CardGameMessage(CardGameMessage.READY, -1, null);
			sendMessage(readyMessage);
			game.setStarted(false);
			gui.repaint();
			break;
			
		case CardGameMessage.READY:
			gui.printMsg(game.getPlayerList().get(message.getPlayerID()).getName() + " is ready.");
			break;
		
		case CardGameMessage.START:
			BigTwoDeck deck = (BigTwoDeck) message.getData();
			if (game.getGameEnded() == false) {
				game.start(deck);
				gui.repaint();
			}
			break;
			
		case CardGameMessage.MOVE:
			int[] cardIdx = (int[]) message.getData();
			game.checkMove(message.getPlayerID(), cardIdx);
			break;
			
		case CardGameMessage.MSG:
			String Chat = (String) message.getData();
			gui.printChatMsg(Chat);
			break;
		}

	}
	
	/**
	 * Method for sending specified message to game server. 
	 * Method should be called whenever the client wants to communicate 
	 * with the game server or other clients.
	 * 
	 * @param message message that needs to be sent
	 */
	public synchronized void sendMessage(GameMessage message) {
		try {
			oos.writeObject(message);
		} catch (IOException e) {
			e.printStackTrace();
		}
		
	}
	
	/**
	 * An Inner class implements the Runnable interface. 
	 * When message received, the parseMessage() method from the NetworkGame 
	 * interface will be called to parse the messages accordingly.
	 */
	class ServerHandler implements Runnable {
		/**
		 * A method to create a thread with instance of this class
		 * as its job in connect method from network game
		 * interface for recieving messages from game server.
		 */
		public void run() {
			CardGameMessage message;
			ObjectInputStream oistream;
			try {
				oistream = new ObjectInputStream(sock.getInputStream());
				while((message = (CardGameMessage) oistream.readObject()) != null) { parseMessage(message);
				}
				
			} catch(SocketException ex) {
				connected = false;
				
				game.setStarted(false);
				gui.printMsg("Your connection to the server is lost. Please click connect"
						+ " from the menu button.");
				gui.disable();
				ex.printStackTrace();
			} catch (Exception ex) {
				connected = false;
				ex.printStackTrace();
			}
		}
	}
}
