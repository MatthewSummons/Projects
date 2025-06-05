import java.awt.Color;

/**
 * Tester class for the Circle class.
 * @author Shaheer Ziya
 */
public class TestCircle extends TestShape{
  /**
   * Main method for testing the Circle class. It initializes an instance of the
   * Circle class and tests the methods of the Circle class.
   * 
   * @param args Unused
   */
  public static void main(String[] args) {
    // Initialize a circle with values for debugging
    Circle c = new Circle();
    c.color = Color.RED;
    c.filled = true;
    c.theta = 0;
    c.xc = 100;
    c.yc = 100;

    printFields(c);

    // Testing the various methods

    // Testing the setVertices method
    System.out.println("Calling setVertices() with d = 50.");
    System.out.println("This method should asign the vertices of the bounding box for the circle");

    c.setVertices(50);

    System.out.print("The x coordinates of the vertices in the local cordinates are: ");
    printArr(c.xLocal);

    System.out.print("The y coordinates of the vertices in the local cordinates are: ");
    printArr(c.yLocal);

    System.out.println();

    // Testing the getX() and getY() methods
    System.out.print("The x coordinates of the vertices in the canvas cordinates are: ");
    printArr(c.getX());

    System.out.print("The y coordinates of the vertices in the canvas cordinates are: ");
    printArr(c.getY());
    
    System.out.println();

    // Test the other methods (inherited from the Shape class)
    testMethods(c);

    
  } // End of main()

} // End of TestCircle class

