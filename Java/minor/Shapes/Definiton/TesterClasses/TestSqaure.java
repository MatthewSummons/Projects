import java.awt.Color;

/**
 * Tester class for the Square class.
 * 
 * @author Shaheer Ziya
 */
public class TestSqaure extends TestShape {
  
  /**
   * Main method for testing the Square class. It initializes an instance of 
   * the Square class and tests the methods of the Square class.
   * @param args Unused
   */
  public static void main(String[] args) {
    // Initialize a square with values for debugging
    Square s = new Square();
    s.color = Color.RED;
    s.filled = true;
    s.theta = 0;
    s.xc = 100;
    s.yc = 100;

    // Print out fields of the square
    printFields(s);

    // Testing the setVertices method
    System.out.println("Calling setVertices() with d = 50.");
    
    s.setVertices(50);
    
    System.out.print("The x coordinates of the vertices in the local cordinates are: ");
    printArr(s.xLocal);

    System.out.print("The y coordinates of the vertices in the local cordinates are: ");
    printArr(s.yLocal);

    System.out.println();

    // Testing the getX() and getY() methods
    System.out.print("The x coordinates of the vertices in the canvas cordinates are: ");
    printArr(s.getX());

    System.out.print("The y coordinates of the vertices in the canvas cordinates are: ");
    printArr(s.getY());

    System.out.println();

    // Test the other methods (inherited from the Shape class)
    testMethods(s);
  }
  
}
